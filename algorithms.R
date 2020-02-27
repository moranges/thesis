list.of.packages <- c("HapEstXXR", "arules", "data.tree")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

library("HapEstXXR")
library("arules")
library("data.tree")

source("domain.R")
source("common.R")
source("stats.R")

discretizeDataTableIn <- function(data.table, discretization.method, number.intervals) {
  message(sprintf("Discretizing data table: %s \nDiscretization method  : %s \nNumber of intervals    : %d", data.table$get_table_name(), discretization.method, number.intervals))
  
  params <- DiscretizationAlgorithmParams$new(a.discretization.method = discretization.method, a.number.intervals = number.intervals)
  
  time <- system.time(discretized.data <- discretizeDataTable(data.table, discretization.method, number.intervals))
  
  run <- DiscretizationAlgorithmRun$new(a.data.table = data.table, some.params = params, a.processing.time = time[["elapsed"]])
  
  discretized.data.table <- DiscretizedNetworkDataTable$new(a.data.table = data.table, a.discretized.data.table = discretized.data, a.discretization.algorithm.run = run)
  
  message("Done\n")

  rm(run)
  rm(time)
  rm(params)
  rm(data.table)
  rm(discretization.method)
  rm(number.intervals)
  gc()
  return (discretized.data.table)
}


discretizeDataTable <- function(data.table, discretization.method, number.intervals) {
  
  data <- data.table$get_data_table()
  
  if (discretization.method == "intervalo") {
    discretization.method <- "interval"
  } else {
    discretization.method <- "frequency"
  }
  
  discretized.data <-
    arules::discretize(data, discretization.method, categories = number.intervals)
  
  dim(discretized.data) <- dim(data)
  
  colnames(discretized.data) <- colnames(data)
  rownames(discretized.data) <- rownames(data)
  
  rm(data)
  rm(data.table)
  gc()
  
  return (discretized.data)
}


  
k2In <- function(original.network, discretized.data.table.name, k2.nodes.order, k2.max.number.parents) {
  message(sprintf("Executing K2 for data table: %s \nNode order                 : %s \nMax number of parents      : %d", discretized.data.table.name, paste(k2.nodes.order$get_order(), collapse = ", "), k2.max.number.parents))
  
  params <- K2AlgorithmParams$new(a.max.number.parents = k2.max.number.parents)
    
  time <- system.time(nodes.parents <- k2(discretized.data.table.name, k2.nodes.order$get_order(), k2.max.number.parents))
    
  run <- K2AlgorithmRun$new(a.discretized.data.table = get0(discretized.data.table.name, envir = .GlobalEnv), a.nodes.order = k2.nodes.order, some.params = params, a.processing.time = time[["elapsed"]])
    
  message("Done\n")

  learned.network <- LearnedNetwork$new(original.network, some.nodes.parents = List$new(nodes.parents), a.k2.algorithm.run = run)
    
  rm(time)
  rm(run)
  rm(params)
  rm(k2.nodes.order)
  gc()
  learned.network
}


#data rows are the variables
#data cols are the time values
k2 <- function(discretized.data.table.name, nodes.order, max.number.parents) {
  
  nodes.parents <- sapply(nodes.order, function (current.node) {    
    current.node.pos <- which(nodes.order == current.node)
    
    if (current.node.pos == 1) {
      return (list())
    }
    
    parents.current.node <- List$new()
    predecessors.current.node <- nodes.order[c(1:(current.node.pos - 1))]
    
    parents.current.node.score <- K2Score(parents.current.node$unList(), discretized.data.table.name, current.node)
    continue <- TRUE
    while (continue & (parents.current.node$length() < max.number.parents)) {
      
      potential.parents.current.node <- List$new(lapply(predecessors.current.node, function(predecessor.current.node) {
        parents.current.node.cloned <- parents.current.node$clone() 
        parents.current.node.cloned$add(predecessor.current.node)
        parents.current.node.cloned$unList()
      }))
      
      argmax.response <- argmax(K2Score, potential.parents.current.node, discretized.data.table.name, current.node)
      
      potential.parents.max <- argmax.response$get_value_list()
      potential.parents.max.score <- argmax.response$get_value_max()$unList()
      
      if (potential.parents.max$length() > 0 & (parents.current.node.score < potential.parents.max.score)) {
        parents.current.node.score <- potential.parents.max.score
        parents.current.node <- List$new(potential.parents.max$get()[[1]])
        predecessors.current.node <- setdiff(predecessors.current.node, potential.parents.max$unList())
        continue <- !(length(predecessors.current.node) == 0)
      } else {
        continue <- FALSE
      }
    }
    
    return (parents.current.node$unList()) 
    
  }, simplify = FALSE, USE.NAMES = TRUE)
  
  rm(nodes.order)
  gc()

  return (nodes.parents)
}


get_power_sets <- function(elements, max.elements, max.power.sets) {
  power.sets <- List$new()

  for (i in c(1:min(length(elements), max.elements))) {
    e <- combn(elements, i)
    power.sets$addAll(unname(split(e, rep(1:ncol(e), each = nrow(e)))))
  }
  
  power.sets.length <- power.sets$length()
  
  if (power.sets.length > max.power.sets) {
    ps <- power.sets$get()
    power.sets$clear()
    power.sets.capped <- sample(1:length(ps), max.power.sets, replace = F)
    power.sets$addAll(ps[power.sets.capped])
  }
  
  return(power.sets)
}


best <- function(current.node, predecessors.current.node, discretized.data.table.name, max.number.parents) {
  
  if (predecessors.current.node$length() > 0) {
    predecessors.power.sets <- get_power_sets(predecessors.current.node$unList(), max.number.parents, 50)
    
    if (predecessors.power.sets$length() > 0) {
      argmax.response <- argmax(K2Score, predecessors.power.sets, discretized.data.table.name, current.node)

      best.potential.parents <- argmax.response$get_value_list()

      rm(current.node)
      rm(predecessors.current.node)
      gc()
      return (best.potential.parents)
    }
  }  
  
  rm(current.node)
  rm(predecessors.current.node)
  gc()
  return (List$new())
}


gain <- function(nodes.order, current.node, parents.current.node, next.current.node, parents.next.current.node, discretized.data.table.name, max.number.parents) {
  
  position.current.node <- nodes.order$get_pos(current.node)
  if (position.current.node > 1) {
    predecessors.current.node <- nodes.order$get(c(1:(position.current.node - 1)))
  } else {
    predecessors.current.node <- List$new()
  }
  
  predecessors.current.node.plus.next <- predecessors.current.node$clone()
  predecessors.current.node.plus.next$add(next.current.node)

  predecessors.next.current.node.minus.current <- predecessors.current.node
  
  best.parents.current.node <- best(current.node, predecessors.current.node.plus.next, discretized.data.table.name, max.number.parents)
  
  best.parents.next.current.node <- best(next.current.node, predecessors.next.current.node.minus.current, discretized.data.table.name, max.number.parents)
  
  gain.current.node <- K2Score(best.parents.current.node$unList(), discretized.data.table.name, current.node) - K2Score(parents.current.node$unList(), discretized.data.table.name, current.node)
  
  gain.next.current.node <- K2Score(best.parents.next.current.node$unList(), discretized.data.table.name, next.current.node) - K2Score(parents.next.current.node$unList(), discretized.data.table.name, next.current.node)
  
  result <- gain.current.node + gain.next.current.node 
  
  rm(nodes.order)
  rm(current.node)
  rm(parents.current.node)
  rm(next.current.node)
  rm(parents.next.current.node)
  rm(position.current.node)
  rm(predecessors.current.node)
  rm(predecessors.current.node.plus.next)
  rm(predecessors.next.current.node.minus.current)
  rm(best.parents.current.node)
  rm(best.parents.next.current.node)
  rm(gain.current.node)
  rm(gain.next.current.node)
  gc()
  return (result)
}


osIn <- function(discretized.data.table.name, initial.nodes.order, os.max.number.parents) {
  message(sprintf("Executing OS for data table: %s \nInitial node order         : %s \nMax number of parents      : %d", discretized.data.table.name, paste(initial.nodes.order$get_order(), collapse = ", "), os.max.number.parents))
    
  params <- OsAlgorithmParams$new(a.max.number.parents = os.max.number.parents)

  time <- system.time(os.nodes.order <- os(discretized.data.table.name, initial.nodes.order, os.max.number.parents))
    
  run <- OsAlgorithmRun$new(a.discretized.data.table = get0(discretized.data.table.name, envir = .GlobalEnv), an.initial.nodes.order = initial.nodes.order, some.params = params, a.processing.time = time[["elapsed"]])
    
  message(sprintf("OS node order              : %s", paste(os.nodes.order$toString(), collapse = ", ")))
    
  message("Done\n")

  nodes.order <- NodesOrder$new(an.order.name = initial.nodes.order$get_order_name(), an.order = os.nodes.order$unList(), an.ordering.algorithm.run = run)

  rm(initial.nodes.order)
  gc()
  return (nodes.order)    
}


os <- function(discretized.data.table.name, nodes.order, max.number.parents) {
  nodes.order <- List$new(nodes.order$get_order())
  nodes.order.length <- nodes.order$length()

  nodes.parents <- nodes.order$lapply(function(current.node) {
    position.current.node <- nodes.order$get_pos(current.node)

    if (position.current.node > 1) {
      predecessors.current.node <- nodes.order$get(c(1:position.current.node-1))
      parents.current.node <- best(current.node, predecessors.current.node, discretized.data.table.name, max.number.parents)
    } else {
      parents.current.node <- List$new()
    }
    
    return (parents.current.node)
  }, names = unlist(nodes.order$unList()))

  gains <- nodes.order$lapply(function(current.node) {
    position.current.node <- nodes.order$get_pos(current.node)
    
    if (position.current.node < nodes.order.length) {
      next.current.node <- nodes.order$get(position.current.node + 1)$unList()
      
      gain(nodes.order, current.node, nodes.parents$get(position.current.node), next.current.node, 
           nodes.parents$get(position.current.node + 1), discretized.data.table.name, max.number.parents)
    } else {
      return (-Inf)
    }
  })

  tabu.list <- TabuList$new(TabuListEntry$new(a.gain=-1, a.nodes.order=nodes.order$clone(), some.nodes.parents = nodes.parents))

  proceed <- TRUE
  while (proceed) {
    gains.max.position <- gains$which.max()
    
    gains.max <- gains$get(gains.max.position)
    if (gains.max$unList() > 0) {
      nodes.order$swap(gains.max.position, gains.max.position + 1)
      
      current.node <- nodes.order$get(gains.max.position)$unList()

      if (gains.max.position > 1) {
        predecessors.current.node <- nodes.order$get(c(1:gains.max.position - 1))
      } else {
        predecessors.current.node <- List$new()
      }
       
      nodes.parents$set(current.node, best(current.node, predecessors.current.node, discretized.data.table.name, max.number.parents))

      if (gains.max.position > 1) {
        previous.current.node <- nodes.order$get(gains.max.position - 1)$unList()
        gains$set(gains.max.position - 1, gain(nodes.order, previous.current.node, nodes.parents$get(previous.current.node), 
                                               current.node, nodes.parents$get(current.node), discretized.data.table.name, max.number.parents))
        
      }
      

      next.current.node <- nodes.order$get(gains.max.position + 1)$unList()
      predecessors.next.current.node <- nodes.order$get(c(1:gains.max.position))
      
      nodes.parents$set(next.current.node, best(next.current.node, predecessors.next.current.node, discretized.data.table.name, max.number.parents))
      
      gains$set(gains.max.position, -gains$get(gains.max.position)$unList())
      
      if (gains.max.position < (nodes.order.length - 1)) {
        predecessors.next.next.current.node <- nodes.order$get(c(1:(gains.max.position + 1)))

        next.next.current.node <- nodes.order$get(gains.max.position + 2)$unList()
        
        nodes.parents$set(next.next.current.node, best(next.next.current.node, predecessors.next.next.current.node, discretized.data.table.name, max.number.parents))
        gains$set(gains.max.position + 1, gain(nodes.order, next.current.node, nodes.parents$get(next.current.node), 
                                               next.next.current.node, nodes.parents$get(next.next.current.node), discretized.data.table.name, max.number.parents))
      }
      
      if (!is.null(tabu.list.entry <- tabu.list$get_entry_by_nodes_order(nodes.order))) {
        argmax.response <- argmax(networkScore, List$new(list(tabu.list.entry$get_nodes_parents(), 
                                                List$new(sapply(nodes.parents$unList(), function(x) {x$unList()})))), discretized.data.table.name, K2Score)

        rm(nodes.parents)
        rm(gains)
        gc()
        
        if (!is.null(argmax.response$get_value_list()$get_name())) {
          return (tabu.list.entry$get_nodes_order())
        } else {
          return (nodes.order)
        }
        
      } else {
        tabu.list$add(TabuListEntry$new(a.gain=gains.max, a.nodes.order=nodes.order$clone(), some.nodes.parents = nodes.parents))
      }
      
    } else {
      proceed <- FALSE
    }
  }
  
  rm(gains)
  rm(gains.max)
  rm(gains.max.position)
  if (exists("next.current.node")) {
    rm(next.current.node)
  }
  
  if (exists("next.next.current.node")) {
    rm(next.next.current.node)
  }

  rm(nodes.parents)
  if (exists("predecessors.current.node")) {
    rm(predecessors.current.node)
  }
  if(exists("predecessors.next.current.node")) {
    rm(predecessors.next.current.node)
  }
  
  if(exists("predecessors.next.next.current.node")) {
    rm(predecessors.next.next.current.node)
  }
  if (exists("previous.current.node")) {
    rm(previous.current.node)
  }
  if (exists("tabu.list")) {
    rm(tabu.list)
  }
  if (exists("tabu.list.entry")) {
    rm(tabu.list.entry)
  }
  gc()
  
  return (nodes.order)
}