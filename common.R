list.of.packages <- c("igraph", "plyr", "memoise", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

library("igraph")
library("plyr")
library("memoise")
library("lubridate")

source("domain.R")

assign("nodes.at.level", List$new(), envir = .GlobalEnv)

#alfa cols are levels of current node
#alfa rows are combinations of current node parents
alfa <- function(discretized.data.table.name, current.node, parents.current.node) {
  discretized.data.table <- get(discretized.data.table.name, envir = .GlobalEnv)

  number.levels <- discretized.data.table$get_number_levels()
  levels <- discretized.data.table$get_levels()
  number.parents.current.node <- length(parents.current.node)
  number.parents.possible.combinations <- number.levels ^ number.parents.current.node
  number.instances <- discretized.data.table$get_number_instances()
  
  if (is.na(number.parents.possible.combinations) || number.parents.possible.combinations > 256) {
    stop()
  }
  
  alfa.matrix <- matrix(0, nrow = number.parents.possible.combinations, ncol = number.levels)
  
  if (number.parents.current.node == 0) {

    alfa.matrix <- sapply(c(1:number.levels), function(number.level) {

      possible.level.combination <- c(paste(c(current.node), number.level, sep = "-"))
          
      discretized.data.table$get_data_instance_count(possible.level.combination)
          
    })

    dim(alfa.matrix) <- c(1, number.levels)

  } else {
    
    parents.possible.level.combinations.matrix <- combn(rep(c(1:number.levels), number.parents.current.node), number.parents.current.node)
    
    if (number.parents.current.node > 0) {
      parents.possible.level.combinations.matrix <- matrix(parents.possible.level.combinations.matrix[, !duplicated(t(parents.possible.level.combinations.matrix))], nrow = number.parents.current.node)
    }
    
    possible.level.combination.matrices <- lapply(c(1:number.levels), function(current.node.level) apply(parents.possible.level.combinations.matrix, 2, function(parents.possible.level.combinations.matrix.col) c(current.node.level, parents.possible.level.combinations.matrix.col)))

    nodes <- c(current.node, parents.current.node)
    total = 0
    alfa.matrix <- sapply(c(1:number.levels), function(number.level) {
      apply (possible.level.combination.matrices[[number.level]], 2, function (possible.level.combination) {
        if (total < number.instances) {
          possible.level.combination <- paste(nodes, possible.level.combination, sep = "-")
          
          value = discretized.data.table$get_data_instance_count(possible.level.combination)

          total <<- total + value
        } else {
          value = 0
        }
        
        return (value)
      })
    })
    alfa.matrix
  }

  return(alfa.matrix)
}


networkScore <- function (nodes.parents, discretized.data.table.name, func) {
  nodes.parents <- nodes.parents$unList()
  seq.nodes.parents.names <- names(nodes.parents)
  
  score <- Reduce('+', mapply(function(seq.nodes.parents.name) {
    return (func(discretized.data.table.name = discretized.data.table.name, current.node = seq.nodes.parents.name, parents.current.node = nodes.parents[[seq.nodes.parents.name]]))
  }, seq.nodes.parents.names))
  
  rm(nodes.parents)
  rm(seq.nodes.parents.names)
  #gc()
  
  return (score)
}


K2Score <- memoise(function(parents.current.node, discretized.data.table.name, current.node) {
  discretized.data.table <- get0(discretized.data.table.name, envir = .GlobalEnv)

  alfas <- alfa(discretized.data.table.name, current.node, parents.current.node)

  number.levels <- discretized.data.table$get_number_levels()
  
  result <- sum((lfactorial(number.levels - 1) - lfactorial(apply(alfas, 1, sum) + number.levels - 1)) + apply(alfas, 1, function(x) sum(lfactorial(x))))

  rm(discretized.data.table)
  rm(alfas)

  return (result)
})


BdeuScore <- function(discretized.data.table.name, current.node, parents.current.node) {
  discretized.data.table <- get0(discretized.data.table.name, envir = .GlobalEnv)
  
  alfas <- alfa(discretized.data.table.name, current.node, parents.current.node)
  
  number.levels <- discretized.data.table$get_number_levels()
  number.parents.current.node <- length(parents.current.node)
  number.parents.possible.combinations <- number.levels ^ number.parents.current.node
  
  aijk = 10/(number.levels * number.parents.possible.combinations)
  aij = aijk*number.levels
  
  result <- sum(lgamma(aij) - lgamma(aij + apply(alfas, 1, sum))) + sum(lgamma(aijk + alfas)- lgamma(aijk))
  
  rm(alfas)
  rm(aij)
  rm(discretized.data.table)
  
  return (result)
}


argmax <- function(func, domain.values, ...) {
  value.images <- domain.values$lapply(func, ...)
  
  max.position <- value.images$which.max()
  
  result <- ArgMaxResponse$new(domain.values$get(max.position), value.images$get(max.position))
  
  rm(value.images)
  rm(max.position)

  return (result)
}


adjacencyMatrixToParents <- function (adjacency.matrix) {
  nodes.parents <- sapply(colnames(adjacency.matrix), function (current.node) {
    
    cols <- row.names(adjacency.matrix)[which(adjacency.matrix[,current.node] == 1, arr.ind = TRUE)]
    
    if (length(cols) == 0) {
      cols <- list()
    }
    
    return (cols)
    
  }, simplify = FALSE)
  
  return (nodes.parents)
}


getAbsolutePath <- function(working.directory, filename, extension) {
  return (paste(working.directory, '\\', filename, ".", extension, sep=""))
}


importNetwork <- function(working.directory, network.name) {
  return (Network$new(a.network.name = network.name, an.adjacency.matrix = importNetworkAdjacencyMatrix(working.directory = working.directory, filename = network.name, extension = "adj")))
} 


importNetworkAdjacencyMatrix <- function (working.directory, filename, extension) {
  adjacency.matrix <-
    as.matrix(read.table(
      getAbsolutePath(working.directory, filename, extension),
      header = TRUE,
      sep = "\t",
      row.names = 1,
      as.is = TRUE
    ))
  
  adjacency.matrix <- adjacency.matrix[, -ncol(adjacency.matrix)] # Drop last column
  adjacency.matrix <- adjacency.matrix[sort(rownames(adjacency.matrix)),sort(colnames(adjacency.matrix))]
  adjacency.matrix[adjacency.matrix == -1] <- 0 # Replace -1 with 0
  
  return (adjacency.matrix)
}


exportLearnedNetworkIn <- function (network, working.directory, number.sets, number.instances, original.network.name, extension) {
  file <- getAbsolutePath(working.directory, paste(original.network.name, "_", number.sets, "_", number.instances, "_learned_networks_stats", sep = ""), "csv")

  message(sprintf("Exporting matrix and stats for learned network %s", network$get_network_name()))
  browser()
  stats <- list(timestamp = now(),
       data.table = network$get_k2_algorithm_run()$get_discretized_data_table()$get_table_name(),
       network.file.path = paste(network$get_network_name(), ".", extension, sep = ""),
       discretization.method = network$get_k2_algorithm_run()$get_discretized_data_table()$get_discretization_algorithm_run()$get_params()$get_discretization_method(),
       discretization.number.intervals = network$get_k2_algorithm_run()$get_discretized_data_table()$get_discretization_algorithm_run()$get_params()$get_number_intervals(),
       order.name = network$get_k2_algorithm_run()$get_nodes_order()$get_order_name(),
       os.initial.order = getInitialOrder(network),
       os.max.number.parents = getMaxNumberParents(network), 
       k2.node.order = paste(network$get_k2_algorithm_run()$get_nodes_order()$get_order(), collapse = " - "),
       k2.max.number.parents = network$get_k2_algorithm_run()$get_params()$get_max_number_parents(),
       true.positives = network$get_stats()$get_true_positives(),
       false.positives = network$get_stats()$get_false_positives(), 
       false.negatives = network$get_stats()$get_false_negatives(), 
       accuracy = round(network$get_stats()$get_accuracy(), digits = 3), 
       sensitivity = round(network$get_stats()$get_sensitivity(), digits = 3), 
       k2.score = round(network$get_stats()$get_k2_score(), digits = 3),
       bdeu.score = round(network$get_stats()$get_bdeu_score(), digits = 3), 
       shd = network$get_stats()$get_shifted_hamming_distance(),
       processing.time = (getorderingProcessingTime(network) + network$get_k2_algorithm_run()$get_processing_time()))

  if(!file.exists(file)) {
    write.table(stats, file, sep = ",", col.names = c("timestamp", "data.table", "network.file.path", "método de discretización",	"número de intervalos", "combinación", "os.initial.order", "número máx de padres OS", "k2.node.order", "número máx de padres K2",	"true.positives",	"false.positives",	"false.negatives",	"accuracy",	"sensitivity", "k2.score", "puntaje log Bdeu", "shd", "processing.time"), row.names = FALSE, append = file.exists(file), quote = FALSE)
  } else {
    write.table(stats, file, sep = ",", col.names = FALSE, row.names = FALSE, append = file.exists(file), quote = FALSE)
  }
    
  message("Done\n")
}


getInitialOrder <- function(network) {
  if (length(grep("OS",network$get_k2_algorithm_run()$get_nodes_order()$get_order_name())) > 0) {
    return (paste(network$get_k2_algorithm_run()$get_nodes_order()$get_ordering_algorithm_run()$get_initial_order()$get_order(), collapse = " - ")) 
  } else {
    return ("NA")
  }
}


getMaxNumberParents <- function(network) {
  if (length(grep("OS", network$get_k2_algorithm_run()$get_nodes_order()$get_order_name())) > 0) {
    return (network$get_k2_algorithm_run()$get_nodes_order()$get_ordering_algorithm_run()$get_params()$get_max_number_parents())
  }else {
    return (0)
  }
}


getorderingProcessingTime <- function(network) { 
  if (length(grep("OS", network$get_k2_algorithm_run()$get_nodes_order()$get_order_name())) > 0) {
    return (network$get_k2_algorithm_run()$get_nodes_order()$get_ordering_algorithm_run()$get_processing_time())
  } else {
    return (0)
  }
}


exportNetworkAsGraph <- function(network, destination.directory, network.name) {
  network.graph <- graph_from_adjacency_matrix(network$get_adjacency_matrix(), mode = "directed")
  exportNetworkGraph(network.graph, working.directory = destination.directory, filename = network.name, extension = "gml")  
}


exportNetworkFromAdjacencyMatrix <- function (adjacency.matrix, working.directory, filename, extension) {
  network.graph <- graph_from_adjacency_matrix(adjacency.matrix, mode = "directed")
  exportNetworkGraph(network.graph, working.directory, filename, extension)
}


exportNetworkGraph <- function (network.graph, working.directory, filename, extension) {
  V(network.graph)$label <- V(network.graph)$name
  V(network.graph)$id <- V(network.graph)$label
  write.graph(graph = network.graph, getAbsolutePath(working.directory, filename, extension), extension)
}


getNetworkData <- function (network, working.directory, data.file, table.number, number.instances) {
  message(sprintf("Getting network data for table %d from %s...", table.number, (path <- getAbsolutePath(working.directory, data.file, "tsv"))))

  header <- read.table(path, nrows = 1, header = TRUE)
  header <- header[2:length(header)]
  
  data.table <- read.table(path, skip = 1 + (table.number-1)*(number.instances+1), nrows = number.instances)
  data.table <- data.table[, 2:ncol(data.table)]
  
  colnames(data.table) <- colnames(header)
  data.table <- t(data.table[,sort(colnames(data.table))])
  data <- NetworkData$new(header, NetworkDataTable$new(a.data.table = data.table, a.network = network, a.data.file = data.file, a.number.instances = number.instances, bulk.id = table.number))
  
  message("Network data obtained and processed")
  
  rm(data.table)
  rm(header)

  return (data)
}


getRandomOrderNotInList <- function(node.order, node.orders) {
  node.order.unlisted <- node.order$unList()
  
  random = sample(node.order.unlisted)
  
  while (sum(node.orders %in% list(random)) >= 1) {
    random = sample(node.order.unlisted)
  }
 
  rm(node.order.unlisted)
  return (List$new(random))
}