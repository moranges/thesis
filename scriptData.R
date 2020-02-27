source("http://bioconductor.org/biocLite.R")
list.of.packages <- c("igraph", "proftools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

biocLite(c("graph", "RBGL", "Rgraphviz"))
library("igraph")
library("proftools")

source("script41.R")
source("domain1.R")
source("common11.R")
source("stats.R")

args = commandArgs(trailingOnly=TRUE)

if (length(args) != 2) {
  stop("Two arguments must be supplied (network_adjacency_matrix_file).adj and (network_data_file).tsv", call.=FALSE)
}

network.name = "studfarm"
working.directory = "C:\\Users\\maria_000\\Dropbox\\Public\\Tesis\\Experiments\\Phase 3"
number.instances = 1000
discretization.methods = list("interval")
number.intervals = list(4,5)
max.number.parents = list(2,3)
optimal.order = c("A1", "A2", "Alan", "Alice", "Ann", "Benny", "Betsy", "Bill", "Bonnie", "Carl", "Cecily", "Dennis")
#optimal.order = c("X1", "X2", "X3")

#From Cytoscape to GNW
# 1. Create network in Cytoscape
# 2. Export network adjacency matrix 
# 3. Import network adjacency matrix in R
network <- importNetwork(working.directory, network.name)

#adjacency.matrix <- importNetworkAdjacencyMatrix(working.directory, network.name, "adj")

# 4. Get graph from adjacency matrix and export it
exportNetworkAsGraph(network, working.directory, network.name)

#exportNetworkFromAdjacencyMatrix(adjacency.matrix, working.directory, network.name, "gml")

# 5. Import gml into GNW
# 6. Generate data set, import it into R
#data.tables <- getNetworkData(adjacency.matrix, working.directory, network.name, number.instances, "tsv")

data.tables <- getNetworkData(network, working.directory, network.name, number.instances)

# 7. Discretize
Rprof(tmp <- tempfile(), line.profiling = TRUE)
discretized.data.tables <- discretizeDataTableInBulk(data.tables, discretization.methods, numbers.intervals)
Rprof()
summaryRprof(tmp, lines = "show")

# 8. Inference
Rprof(tmp <- tempfile(), line.profiling = TRUE)
os.nodes.orders = osInBulk(discretized.data.tables, max.number.parents)
Rprof()
summaryRprof(tmp, lines = "show")

#data2 <- data[os.nodes.orders[[1]],]
#write.table(data2, paste("t_", "N1_dream4_timeseries_10.tsv"), sep = "\t", quote = FALSE, col.names = NA, row.names = rownames(l))

learned.networks <- k2InBulk(discretized.data.tables, os.nodes.orders, optimal.order, TRUE, max.number.parents)

# 8. Compare adjacency matrixes
#learned.networks.with.stats <- getLearnedNetworkStatisticsInBulk(adjacency.matrix, learned.networks)

# 9. Export network so it can be read in Cytoscape
exportNetworkInBulk(learned.networks, working.directory, network.name, "gml")
