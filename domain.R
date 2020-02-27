list.of.packages <- c("R6", "data.tree")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library("R6")
library("data.tree")

Network <- R6Class(
  "Network",
  public = list(
    initialize = function(a.network.name = NULL,
                          an.adjacency.matrix = NULL,
                          some.nodes.parents = NULL) {
      private$network.name <- a.network.name
      private$nodes.parents <- some.nodes.parents
      private$adjacency.matrix <- an.adjacency.matrix
      
      if (!is.null(private$nodes.parents) &&
          is.null(private$adjacency.matrix)) {
        private$nodes.names <- names(private$nodes.parents$unList())
        private$number.nodes = length(private$nodes.names)
        private$adjacency.matrix <- matrix(0, ncol = private$number.nodes, nrow = private$number.nodes, byrow = TRUE, dimnames <- list(private$nodes.names, private$nodes.names))
        
        sapply(private$nodes.names, function(current.node) {
          parents.current.node <- private$nodes.parents$get(current.node)
          if (parents.current.node$length() > 0) {
            for (parent.current.node in parents.current.node$unList()) {
              if (length(parent.current.node) > 0) {
                private$adjacency.matrix[parent.current.node, current.node] <- 1
              }
            }
          }
        })
      } else {
        private$nodes.names <- List$new(rownames(private$adjacency.matrix))
        private$number.nodes = private$nodes.names$length()
      }
    },
    get_network_name = function() {
      return (private$network.name)
    },
    get_adjacency_matrix = function() {
      return (private$adjacency.matrix)
    },
    get_nodes_parents = function() {
      return (private$nodes.parents)
    },
    get_nodes_names = function() {
      return (private$nodes.names)
    },
    get_number_nodes = function() {
      return (private$number.nodes)
    }
  ),
  private = list(
    network.name = NULL,
    adjacency.matrix = NULL,
    nodes.parents = NULL,
    nodes.names = NULL,
    number.nodes = NULL
  )
)


NetworkData <- R6Class(
  "NetworkData",
  public = list(
    initialize = function(a.header, a.data.table) {
      private$header = a.header
      private$data.table = a.data.table
    },
    get_header = function() {
      return (private$header)
    },
    get_data_table = function() {
      return (private$data.table)
    }
  ),
  private = list(
    header = NULL,
    data.table = NULL
  )
)


NetworkDataTable <- R6Class(
  "NetworkDataTable",
  public = list(
    initialize = function(a.data.table = NULL,
                          a.network = NULL,
                          a.data.file,
                          a.number.instances = NULL,
                          bulk.id = NULL) {
      if (!is.null(a.network) && !is.null(a.number.instances)) {
        private$table.name <- paste0(a.data.file, "_data_table_", bulk.id)
      }
      
      private$network <- a.network
      if (!is.null(private$network)) {
        private$attribute.names <- private$network$get_nodes_names()
        node.names.unlisted <- private$attribute.names$unList()
        
        seq.attributes <- seq_along(node.names.unlisted)
        attributes <- sapply (seq.attributes, function(seq.attributes.index) {
          Attribute$new(node.names.unlisted[[seq.attributes.index]], seq.attributes.index)
        })
        names(attributes) <- node.names.unlisted
        
        private$attributes <- List$new(attributes)
        private$number.attributes <- private$attributes$length()      
      }
      
      private$data.table <- a.data.table
      private$number.instances <- a.number.instances
    },
    get_data_table = function() {
      return (private$data.table)
    },
    get_table_name = function() {
      return (private$table.name)
    },
    get_network = function() {
      return (private$network)
    },
    get_attributes = function() {
      return (private$attributes)
    },
    get_attribute_names = function() {
      return (private$attribute.names)
    },
    get_number_attributes = function() {
      return (private$number.attributes)
    },
    get_number_instances = function() {
      return (private$number.instances)
    }
  ),
  private = list(
    table.name = NULL,
    data.table = NULL,
    attributes = NULL,
    attribute.names = NULL,
    number.attributes = NULL,
    network = NULL,
    number.instances = NULL
  )
)


DiscretizationAlgorithmParams <- R6Class(
  "discretizationAlgorithmParams",
  public = list (
    initialize = function(a.discretization.method = NULL,
                          a.number.intervals = NULL) {
      private$discretization.method <- a.discretization.method
      private$number.intervals <- a.number.intervals
    },
    get_discretization_method = function() {
      return (private$discretization.method)
    },
    get_number_intervals = function() {
      return (private$number.intervals)
    }
  ),
  private = list(
    discretization.method = NULL,
    number.intervals = NULL
  )
)


DiscretizationAlgorithmRun <- R6Class(
  "DiscretizationAlgorithmRun",
  public = list(
    initialize = function(a.data.table = NULL,
                          some.params = NULL,
                          a.processing.time = NULL) {
      private$data.table <- a.data.table
      private$params <- some.params
      private$processing.time <- a.processing.time
    },
    get_data_table = function() {
      return (private$data.table)
    },
    get_params = function() {
      return (private$params)
    },
    get_processing_time = function() {
      return (private$processing.time)
    }
  ),
  private = list(
    data.table = NULL,
    params = NULL,
    processing.time = NULL
  )
)


DiscretizedNetworkDataTable <- R6Class(
  "DiscretizedNetworkDataTable",
  public = list(
    initialize = function(a.data.table = NULL,
                          a.discretized.data.table = NULL,
                          a.discretization.algorithm.run = NULL) {
      private$discretization.algorithm.run <- a.discretization.algorithm.run

      if (!is.null(a.data.table)) {
        private$attributes <- a.data.table$get_attributes()
        private$attribute.names <- a.data.table$get_attribute_names()
        private$number.attributes <- a.data.table$get_number_attributes()
        private$number.instances <- a.data.table$get_number_instances()
        private$table.name <- a.data.table$get_table_name()
      }
      
      private$timestamp <- format(now(), "%Y_%m_%d_%H_%M_%S")
      
      private$discretized.table.name = NULL
      if (!is.null(a.data.table)) {
        private$discretized.table.name <- paste0(a.data.table$get_table_name(), "_discretized_table_", a.discretization.algorithm.run$get_params()$get_discretization_method(), "_", a.discretization.algorithm.run$get_params()$get_number_intervals())
      }
      
      private$number.levels <- nlevels(a.discretized.data.table)
      private$levels <- levels(a.discretized.data.table)
      
      attributes <- private$attributes$unList()
      seq.attributes <- seq_along(attributes)

      private$build_ad_tree(a.discretized.data.table)
    },
    get_ad_tree = function() {
      return (private$ad.tree)
    },
    get_discretized_table_name = function() {
      return (private$discretized.table.name)
    },
    get_discretization_algorithm_run = function() {
      return (private$discretization.algorithm.run)
    },
    get_table_name = function() {
      return (private$table.name)
    },    
    get_attributes = function() {
      return (private$attributes)
    },
    get_attribute_names = function() {
      return (private$attribute.names)
    },
    get_number_levels = function() {
      return (private$number.levels)
    },
    get_levels = function() {
      return (private$levels)
    },
    get_rows = function(rownames) {
      return (private$discretized.data.table[rownames, ])
    },
    get_number_instances = function() {
      return (private$number.instances)
    },
    get_number_attributes = function() {
      return (private$number.attributes)
    },
    get_data_instances = function() {
      return (private$data.instances)
    },
    get_data_instance_count = function(possible.level.combination) {
      total = 0

      if (total < private$number.instances) {
        possible.level.combination <- sort(possible.level.combination)
        
        base.node <- private$ad.tree$get_first_child_node_by_name(a.parent.node = private$ad.tree$get_root_node(), child.node.name = possible.level.combination[1])

        if (length(possible.level.combination) > 1) {
          if (!is.null(base.node) && !is.null(base.node[[1]])) {
            value = private$get_count(base.node, possible.level.combination, 2, private$attributes$unList())
            total <<- total + value
          } else {
            value = 0
          }
        } else {
          if (!is.null(base.node) && !is.null(base.node[[1]])) {
            value = base.node[[1]]$cou
          } else {
            value = 0
          }
        }   
      } else {
        value = 0
      }
      
      return (value)
    }
  ),
  private = list(
    discretized.table.name = NULL,
    table.name = NULL,
    timestamp = NULL,
    levels = NULL,
    number.levels = NULL,
    discretization.algorithm.run = NULL,
    attributes = NULL,
    attribute.names = NULL,
    number.attributes = NULL,
    data.instances = NULL,
    number.instances = NULL,
    ad.tree = NULL,
    
    build_ad_tree = function(a.discretized.data.table) {
      private$ad.tree <- Tree$new()
      root.node <- private$ad.tree$get_root_node()
      
      seq.attributes <- seq_len(length.out = private$attributes$length())

      apply(a.discretized.data.table, 2, function(data.instance) {
        lapply(seq.attributes, function(seq.attributes.index) {
          private$update_node_subtree(parent.current.node = root.node,
                                      data.instance,
                                      private$number.attributes, 
                                      seq.attributes.index, FALSE,
                                      private$attributes$unList())
        })
      })
    },
    
    update_node_subtree = function(parent.current.node, 
                                   data.instance.attribute.value.pairs
                                   , child.nodes.in.tree.length
                                   , index, parent.added,
                                   attributes) {
      
      data.instance.attribute.value.pair <- data.instance.attribute.value.pairs[[index]]

      attribute <- attributes[[index]]
      attribute.name <- attribute$get_attribute_name()

      a.child.name <- paste0(attribute.name, "-", data.instance.attribute.value.pair)
      
      if (parent.added) {
        a.child.node.name = attribute.name
        a.child.node.value = data.instance.attribute.value.pair
        a.child.attribute.position = attribute$get_attribute_position()
 
        node <- private$ad.tree$add_child_node(a.parent.node = parent.current.node, child.name = a.child.name, child.node.name = a.child.node.name, child.node.value = a.child.node.value, child.attribute.position = a.child.attribute.position)
      } else {
        node <- parent.current.node$children[[a.child.name]]
        
        if (is.null(node)) {
          a.child.node.name = attribute.name
          a.child.node.value = data.instance.attribute.value.pair
          a.child.attribute.position = attribute$get_attribute_position()
          
          node <- private$ad.tree$add_child_node(a.parent.node = parent.current.node, child.name = a.child.name, child.node.name = a.child.node.name, child.node.value = a.child.node.value, child.attribute.position = a.child.attribute.position)
          parent.added <- TRUE
        } else {
          node$cou <- node$cou + 1
          parent.added <- FALSE
        }
      }
      
      if (child.nodes.in.tree.length > index) {
        private$update_node_subtree(node,
                                    data.instance.attribute.value.pairs
                                    , child.nodes.in.tree.length
                                    , index + 1, parent.added,
                                    attributes)
      }
    },
    get_count = function(parent.nodes, possible.level.combination, index, node.names) {
      
      target.node <- possible.level.combination[[index]]
      
      target.node.name <- strsplit(target.node, "-")[[1]][[1]]
      
      target.attribute.position <- node.names[[target.node.name]]$get_attribute_position()
      
      child.nodes <- lapply(parent.nodes, function(parent.node) {
        
        level = target.attribute.position - node.names[[parent.node$node.name]]$get_attribute_position() + 1
        
        private$ad.tree$traverse(node = parent.node, level = level, nodes = nodes.at.level)
        
        filtered.nodes <- Filter(x = nodes.at.level$unList(), function(e) e$name == target.node)
        
        nodes.at.level$clear()
        
        filtered.nodes
      })
      
      child.nodes <- unlist(child.nodes)
      
      if (length(child.nodes) > 0 && length(child.nodes[[1]]) > 0) {
        if (index == length(possible.level.combination)) {
          return (Reduce('+', mapply(function(node) node$cou, child.nodes)))
        } else {
          return (private$get_count(child.nodes, possible.level.combination, (index + 1), node.names))
        }
      }
      
      return (0)
    }
  )
)


OrderingAlgorithmParams <- R6Class("OrderingAlgorithmParams")


OsAlgorithmParams <- R6Class(
  "OsAlgorithmParams",
  inherit = OrderingAlgorithmParams,
  public = list(
    initialize = function(a.max.number.parents = NULL) {
      private$max.number.parents <- a.max.number.parents
    },
    get_max_number_parents = function() {
      return (private$max.number.parents)
    }
  ),
  private = list(max.number.parents = NULL)
)


OrderingAlgorithmRun <- R6Class(
  "OrderingAlgorithmRun",
  public = list(
    initialize = function(a.discretized.data.table = NULL,
                          some.params = NULL,
                          a.processing.time = NULL) {
      private$discretized.data.table <- a.discretized.data.table
      private$params <- some.params
      private$processing.time <- a.processing.time
    },
    get_processing_time = function() {
      return (private$processing.time)
    },
    get_params = function() {
      return (private$params)
    }
  ),
  private = list(
    discretized.data.table = NULL,
    params = NULL,
    processing.time = 0.0
  )
)


OsAlgorithmRun <- R6Class(
  "OsAlgorithmRun",
  inherit = OrderingAlgorithmRun,
  public = list(
    initialize = function(a.discretized.data.table = NULL,
                          an.initial.nodes.order = NULL,
                          some.params = NULL,
                          a.processing.time = NULL) {
      super$initialize(
        a.discretized.data.table <- a.discretized.data.table,
        some.params <- some.params,
        a.processing.time <- a.processing.time
      )
      private$initial.order <- an.initial.nodes.order
    },
    get_initial_order = function() {
      return (private$initial.order)
    }
  ),
  private = list(
    initial.order = NULL
  )
)


NodesOrder <- R6Class(
  "NodesOrder",
  public = list(
    initialize = function(an.order.name = NULL,
                          an.order = NULL,
                          an.ordering.algorithm.run = NULL) {
      private$order.name <- an.order.name
      private$order <- an.order
      private$ordering.algorithm.run <- an.ordering.algorithm.run
    },
    get_order = function() {
      return (private$order)
    },
    get_ordering_algorithm_run = function() {
      return (private$ordering.algorithm.run)
    },
    get_order_name = function() {
      return (private$order.name)
    }
  ),
  private = list(
    order.name = NULL,
    order = NULL,
    ordering.algorithm.run = NULL
  )
)


K2AlgorithmParams <- R6Class(
  "K2AlgorithmParams",
  public = list(
    initialize = function(a.max.number.parents = NULL) {
      private$max.number.parents <- a.max.number.parents
    },
    get_max_number_parents = function() {
      return (private$max.number.parents)
    }
  ),
  private = list(
    max.number.parents = NULL
  )
)


K2AlgorithmRun <- R6Class(
  "K2AlgorithmRun",
  public = list(
    initialize = function(a.discretized.data.table = NULL,
                          a.nodes.order = NULL,
                          some.params = NULL,
                          a.processing.time = NULL) {
      private$discretized.data.table <- a.discretized.data.table
      private$nodes.order <- a.nodes.order
      private$params <- some.params
      private$processing.time <- a.processing.time
    },
    get_discretized_data_table = function() {
      return (private$discretized.data.table)
    },
    get_nodes_order = function() {
      return (private$nodes.order)
    },
    get_params = function() {
      return (private$params)
    },
    get_processing_time = function() {
      return (private$processing.time)
    }
  ),
  private = list(
    discretized.data.table = NULL,
    nodes.order = NULL,
    params = NULL,
    processing.time = NULL
  )
)


LearnedNetwork <- R6Class(
  "LearnedNetwork",
  inherit = Network,
  public = list(
    initialize = function(original.network, some.nodes.parents = NULL,
                          a.k2.algorithm.run = NULL) {
      a.network.name = NULL
      if (!is.null(a.k2.algorithm.run)) {
        a.network.name <- paste0(a.k2.algorithm.run$get_discretized_data_table()$get_table_name(), "_learned_network_", format(now(), "%Y_%m_%d_%H_%M_%S"))
      }
      
      super$initialize(a.network.name = a.network.name, some.nodes.parents = some.nodes.parents)
      private$k2.algorithm.run <- a.k2.algorithm.run
      
      learned.network <- Network$new(a.network.name = a.network.name, some.nodes.parents = private$nodes.parents)
      
      private$stats <- getLearnedNetworkStatistics(original.network, learned.network, private$k2.algorithm.run$get_discretized_data_table())
    },
    get_stats = function() {
      return (private$stats)
    },
    get_k2_algorithm_run = function() {
      return (private$k2.algorithm.run)
    }
  ),
  private = list(
    k2.algorithm.run = NULL,
    stats = NULL
  )
)


NetworkStats <- R6Class(
  "NetworkStats",
  public = list(
    initialize = function(true.positives = NULL,
                          false.positives = NULL,
                          false.negatives = NULL,
                          accuracy = NULL,
                          sensitivity = NULL,
                          bdeu.score = NULL,
                          k2.score = NULL,
                          shifted.hamming.distance = NULL) {
      private$true.positives <- true.positives
      private$false.positives <- false.positives
      private$false.negatives <- false.negatives
      private$accuracy <- accuracy
      private$sensitivity <- sensitivity
      private$bdeu.score <- bdeu.score
      private$k2.score <- k2.score
      private$shifted.hamming.distance <- shifted.hamming.distance
    },
    get_true_positives = function() {
      return (private$true.positives)
    },
    get_false_positives = function() {
      return (private$false.positives)
    },
    get_false_negatives = function() {
      return (private$false.negatives)
    },
    get_accuracy = function() {
      return (private$accuracy)
    },
    get_sensitivity = function() {
      return (private$sensitivity)
    },
    get_bdeu_score = function() {
      return (private$bdeu.score)
    },
    get_k2_score = function() {
      return (private$k2.score)
    },
    get_shifted_hamming_distance = function() {
      return (private$shifted.hamming.distance)
    }
  ),
  private = list(
    true.positives = NULL,
    false.positives = NULL,
    false.negatives = NULL,
    accuracy = NULL,
    sensitivity = NULL,
    bdeu.score = NULL,
    k2.score = NULL,
    shifted.hamming.distance = NULL
  )
)


Tree <- R6Class(
  "Tree",
  public = list(
    initialize = function() {
      private$root.node <- Node$new("tree.root")
    },
    get_root_node = function() {
      return (private$root.node)
    },
    add_child_node = function(a.parent.node, child.name, child.node.name, child.node.value, child.attribute.position) {
      child.node <- a.parent.node$AddChild(child.name, cou = 1, node.name = child.node.name, node.value = child.node.value, node.attribute.position = child.attribute.position)
      
      return (child.node)
    },
    get_first_child_node_by_name = function(a.parent.node, child.node.name) {
      return (a.parent.node$children[child.node.name])
    },
    traverse = function (node, level, nodes) {
      
      children <- node$children
      
      if (level == 2) {
        for (child in children) {
          nodes$add(child)
        }
      } else {
        for (child in children) {
          self$traverse(child, level = level - 1, nodes)  
        }
      }
    }
  ),
  private = list(
    root.node = NULL
  )
)


List <- R6Class(
  inherit = Vector,
  "List",
  public = list(
    initialize = function(some.elements = NULL, a.name = NULL) {
      return (super$initialize(a.mode = "list", some.elements, a.name))
    }
  )
)


Vector <- R6Class(
  "Vector", 
  public = list(
    initialize = function(a.mode, some.elements = NULL, a.name = NULL) {
      private$mode <- a.mode
      private$vector <- vector(mode = private$mode)
      private$size = 0
      private$counter = 0
      private$name = a.name
      
      if (!is.null(some.elements)) {
        
        if (is.list(some.elements) || is.vector(some.elements)) {
          some.elements <- as.list(some.elements)
          
          if (is.null(names <- names(some.elements))) {
            sapply(some.elements, function(some.element) {
              if (is.list(some.element) && length(some.element) > 0) {
                self$add(some.element[[1]])
              } else {
                self$add(some.element)
              }
            })
          } else {
            sapply(names, function(name) {
              some.element <- some.elements[[name]]
              if (is.list(some.element) && length(some.element) > 0) {
                self$add(some.element[[1]], name)
              } else {
                self$add(some.element, name)
              }
            })
          }
        } else {
          self$add(some.elements)
        }
      }
    },
    add = function(an.element = NULL, name = NULL) {
      if (private$size == 0) {
        private$size <- 1
        length(private$vector) <- 1
      }
      
      if (private$counter == private$size) {
        length(private$vector) <- private$size <- private$size * 2
      }
      
      private$counter <- private$counter + 1
      private$vector[[private$counter]] <- an.element
      
      if (!is.null(name)) {
        names(private$vector)[private$counter] <- name
      }
    },
    addAll = function(some.elements) {
      lapply(some.elements, function(some.element) {
        self$add(some.element)
      })
    },
    get = function(element.reference = NULL) {
      if (is.null(element.reference)) {
        if (private$counter > 0) {
          return (private$vector[1:private$counter])
        } else {
          return (private$vector)
        }
      } else {
        
        if (length(element.reference) == 1) {
          if ("List" %in% class(private$vector[[element.reference]])) {
            return (private$vector[[element.reference]])
          } else {
            return (List$new(private$vector[element.reference]))  
          }
        } else {
          return (List$new(private$vector[element.reference]))
        }    
      }
    },
    set = function(element.reference, value = NULL) {
      private$vector[[element.reference]] <- value
    },
    toString = function() {
      if (private$counter > 0) {
        return (toString(private$vector[1:private$counter]))
      }
      
      return ("NULL")
    },
    sapply = function(...) {
      if (private$counter > 0) {
        return (List$new(sapply(private$vector[1:private$counter], ...)))
      } 
      
      return (NULL)
    },
    lapply = function(..., names = NULL) {
      if (private$counter > 0) {
        result <- lapply(private$vector[1:private$counter], ...)
        
        if (!is.null(names)) {
          names(result) <- names
        }
        
        return (List$new(result))
      } 
      
      
      return (NULL)
    },
    get_pos = function(an.element) {
      return(which(private$vector == an.element))
    },
    length = function() {
      return (private$counter)
    },
    unList = function() {
      if (private$counter == 0) {
        return (list()) 
      } else {
        if (length(names(private$vector)) > 0) {
          return (private$vector[1:private$counter])
        } else {
          return (unlist(private$vector[1:private$counter]))
        }
      }
    },
    which.max = function() {
      return (which.max(private$vector[1:private$counter]))
    },
    swap = function(element.reference1, element.reference2) {
      element1 <- self$get(element.reference1)
      element2 <- self$get(element.reference2)
      
      temp <- private$vector[element.reference1]
      private$vector[element.reference1] <- private$vector[element.reference2]
      private$vector[element.reference2] <- temp
    },
    get_name = function() {
      return (private$name)
    },
    set_name = function(a.name) {
      private$name <- a.name
    }, 
    clear = function() {
      private$vector <- vector(mode = private$mode)
      private$size = 0
      private$counter = 0
    }
  ),
  private = list(
    vector = NULL,
    size = NULL,
    counter = NULL,
    name = NULL,
    mode = NULL
  )
)


ArgMaxResponse <- R6Class(
  "ArgMaxResponse", 
  public = list(
    initialize = function(a.value.list = NULL, a.value.max = NULL) {
      private$value.list <- a.value.list
      private$value.max <- a.value.max
    },
    get_value_list = function() {
      return (private$value.list)
    },
    get_value_max = function() {
      return (private$value.max)
    }
  ),
  private = list(
    value.list = NULL,
    value.max = NULL
  )
)


TabuListEntry <- R6Class(
  "TabuListEntry",
  public = list(
    initialize = function(a.nodes.order = NULL, a.gain = NULL, some.nodes.parents = NULL) {
      private$nodes.order <- a.nodes.order
      private$gain <- a.gain
      
      if (!is.null(some.nodes.parents)) {
        private$nodes.parents <- List$new(sapply(some.nodes.parents$unList(), function(x) {x$unList()}), "tabu.list.entry")
      }
    },
    get_nodes_order = function() {
      return (private$nodes.order)
    },
    get_nodes_parents = function() {
      return (private$nodes.parents)
    }
  ),
  private = list(
    nodes.order = NULL,
    gain = NULL,
    nodes.parents = NULL
  )
)


TabuList <- R6Class(
  "TabuList",
  inherit = List,
  public = list(
    initialize = function(a.tabu.list.entry = NULL) {
      super$initialize(a.name = "tabu.list")
      self$add(a.tabu.list.entry)
    },
    add = function(a.tabu.list.entry = NULL, name = NULL) {
      super$add(an.element = a.tabu.list.entry, name);
    },
    get_entry_by_nodes_order = function(nodes.order) {
      nodes.order.unlisted <- nodes.order$unList()
      nodes.order.unlisted.length <- nodes.order$length()
      
      contained <- sapply(private$vector[1:private$counter], function(list.entry) {
        sum(nodes.order.unlisted == list.entry$get_nodes_order()$unList()) == nodes.order.unlisted.length
      })
      
      if (sum(contained) > 0) {
        return (private$vector[contained == TRUE][[1]])
      } else {
        return (NULL)
      }
    }
  ),
  private = list(
    nodes.orders = NULL
  )
)


Attribute <- R6Class(
  "Attribute",
  public =list(
    initialize = function(an.attribute.name, an.attribute.position) {
      private$attribute.name <- an.attribute.name
      private$attribute.position <- an.attribute.position
    },
    get_attribute_name = function() {
      return (private$attribute.name)
    },
    get_attribute_position = function() {
      return (private$attribute.position)
    }
  ),
  private = list(
    attribute.name = NULL,
    attribute.position = NULL
  )
)
