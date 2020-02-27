list.of.packages <- c("parallel", "lsr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

library("parallel")
library("lsr")
setwd(paste0(path.expand("~"), "/../Dropbox/Public/Tesis/Experiments"))

source("algorithms.R")

statExists <- function(working.directory, number.sets, number.instances, table.number, network, discretization.method, number.intervals, order.name, os.max.number.parents, k2.max.number.parents) {
  file <- getAbsolutePath(working.directory, paste(network$get_network_name(), "_", number.sets, "_", number.instances, "_learned_networks_stats", sep = ""), "csv")
  
  if (!file.exists(file)) {
    return (FALSE)
  }
   
  current.stats <- read.table(file, sep = ",", header = TRUE)
  exists <- apply(current.stats, 1, function(current.stat) {
    
    (current.stat[2] == paste0(network$get_network_name(), "_", number.sets, "_", number.instances, "_data_data_table_", table.number)) && (current.stat[4] == discretization.method) && (current.stat[5] == number.intervals) && (current.stat[6] == order.name) && (current.stat[8] == os.max.number.parents) && (current.stat[10] == k2.max.number.parents)
  })

  return (sum(exists) == 1)
}  


inferNetwork <- function(grid, statExists, getNetworkData,
                         network, working.directory, data.file, table.number, number.sets, number.instances, optimal.order) {
  discretization.method <- as.character(grid[["discretization.methods"]])
  number.intervals <- as.integer(levels(grid[["numbers.intervals"]])[grid[["numbers.intervals"]]])
  order.name <- as.character(grid[["order.name"]])
  os.max.number.parents <- as.integer(levels(grid[["os.max.numbers.parents"]])[grid[["os.max.numbers.parents"]]])
  k2.max.number.parents <- as.integer(levels(grid[["k2.max.numbers.parents"]])[grid[["k2.max.numbers.parents"]]])
  
  if (statExists(working.directory, number.sets, number.instances, table.number, network, discretization.method, number.intervals, order.name, os.max.number.parents, k2.max.number.parents)) {
    message("Already done")
    return (0);
  }
  
  discretized.data.table.name <- paste0(network$get_network_name(), "_" , number.sets, "_", number.instances, "_data_data_table_", table.number, "_discretized_table_", discretization.method, "_", number.intervals)
  
  if (is.null(get0(discretized.data.table.name, envir = .GlobalEnv))) {
    rm(list = ls(pattern = paste0(network$get_network_name(), "_" , number.sets, "_", number.instances, "_data_data_table_", table.number, "_discretized_table_"), envir = .GlobalEnv), envir = .GlobalEnv)
    gc()
    
    network.data <- getNetworkData(network, working.directory, data.file, table.number, number.instances)
    data.table <- network.data$get_data_table()
    assign(discretized.data.table.name, discretizeDataTableIn(data.table, discretization.method, number.intervals), envir = .GlobalEnv)

    rm(network.data)    
    rm(data.table)
    gc()
  }
  
  if (order.name == "óptimo OS+K2" || order.name == "al azar OS+K2") {
    
    if (is.null(k2.nodes.order <- get0(paste0(discretized.data.table.name, "_", order.name, "_", os.max.number.parents), envir = .GlobalEnv))) {
      browser()
      if (order.name == "óptimo OS+K2") {
        os.initial.nodes.order <- NodesOrder$new(an.order.name = "óptimo OS", an.order = optimal.order)
      } else if (order.name == "al azar OS+K2") {
        os.initial.nodes.order <- NodesOrder$new(an.order.name = "al azar OS", an.order = getRandomOrderNotInList(get0(discretized.data.table.name, envir = .GlobalEnv)$get_attribute_names(), optimal.order)$unList())
      } else {
        stop("Error")
      }
      
      k2.nodes.order <- osIn(discretized.data.table.name, os.initial.nodes.order, os.max.number.parents)
      
      assign(paste0(discretized.data.table.name, "_", order.name, "_", os.max.number.parents), k2.nodes.order, envir = .GlobalEnv)
    }
  } else {
    
    if (order.name == "óptimo K2") {
      k2.nodes.order <- NodesOrder$new(an.order.name = "óptimo K2", an.order = optimal.order)      
    } else {
      k2.nodes.order <- NodesOrder$new(an.order.name = "al azar K2", an.order = getRandomOrderNotInList(get0(discretized.data.table.name, envir = .GlobalEnv)$get_attribute_names(), optimal.order)$unList())
    }
  }

  learned.network <- k2In(network, discretized.data.table.name, k2.nodes.order, k2.max.number.parents)

  exportLearnedNetworkIn(learned.network, working.directory, number.sets, number.instances, network$get_network_name(), "gml")
  
  rm(learned.network)
  rm(k2.nodes.order)
  if (exists("os.initial.nodes.order")) {
    rm(os.initial.nodes.order)
  }
  rm(discretization.method)
  rm(number.intervals)
  rm(order.name)
  rm(os.max.number.parents)
  rm(k2.max.number.parents)
}


data.tables.function <- function(table.number, grid, getNetworkData, inferNetwork, statExists,
                                 network, working.directory, data.file, number.sets, number.instances, optimal.order) {
  lapply(grid, inferNetwork, statExists, getNetworkData,  
         network, working.directory, data.file, table.number, number.sets, number.instances, optimal.order)
  
  forget(K2Score)
  gc()
}

args <- commandArgs(TRUE)

network.name = args[1]
working.directory = paste0(path.expand("~"), "/../Dropbox/Public/Tesis/Experiments/", network.name)
number.instances = as.integer(args[2])
number.sets = as.integer(args[3])
data.file = paste0(network.name, "_", number.sets, "_", number.instances, "_data")
discretization.methods = c("intervalo", "frecuencia")
numbers.intervals = c(2)
optimal.order = as.vector(strsplit(args[4], ",")[[1]])
os.max.numbers.parents = c(2,3,4)
k2.max.numbers.parents = c(2)

network = importNetwork(working.directory, network.name)

grid1 <- expand.grid(discretization.methods, numbers.intervals, c("óptimo OS+K2", "al azar K2+OS"), os.max.numbers.parents, k2.max.numbers.parents, stringsAsFactors = FALSE)
grid2 <- expand.grid(discretization.methods, numbers.intervals, c("óptimo K2", "al azar K2"), c(0), k2.max.numbers.parents, stringsAsFactors = FALSE)
grid <- rbind(grid1, grid2)
colnames(grid) <- c("discretization.methods", "numbers.intervals", "order.name", "os.max.numbers.parents", "k2.max.numbers.parents" )
grid <- unname(tFrame(grid[order(grid[, 1], grid[, 2], grid[, 3], grid[, 4], grid[, 5]),]))
colnames(grid) <- c(1:ncol(grid))

no_cores <- as.integer(args[5])

cl <- makeCluster(no_cores, outfile = "debug.log")

clusterEvalQ(cl, source("algorithms.R"))

parLapplyLB(cl, c(as.integer(args[6]):as.integer(args[7])), data.tables.function, grid, getNetworkData, inferNetwork, statExists,
            network, working.directory, data.file, number.sets, number.instances, optimal.order)

stopCluster(cl)