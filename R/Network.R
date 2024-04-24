
#' R6 class representing Network
#'
#' @description
#' Creates a new network
#' @details
#' A network containing nodes and edges
#' @export
Network <- R6::R6Class("Network",
                       inherit = Node,
                       public = list(

                         #' @description Create a new network
                         #' @param id The id of the Network
                         #' @param style A Node_Style object
                         #' @param author Name of the creator of this network
                         #' @param description Description of this network
                         #' @param rankdir Direction of the graph; can be 'LR' (left-right) or 'TD' (top-down); default 'LR'
                         #' @param default Logical. If TRUE, then no data fields are initialized.
                         #' @return A Network object
                         initialize = function(id = "", style = NULL, author = "", description = "", rankdir = 'LR', default = FALSE){
                           if (!default){
                             super$initialize(id, style, author, description)
                             private$.structure = Network_Structure$new(rankdir)
                             private$.nodes <- Dictionary$new()
                           }
                         },

                         #' @description Get the network namespaced node id
                         #' @param node_id The id of the node
                         #' @return The namespaced id
                         ns = function(node_id){
                           return (private$.set_network_node_id(node_id))
                         },

                         #' @description Reset the network by clearing all the outputs of all the
                         #' unlocked nodes. Locked nodes are not reset.
                         #' @return network
                         reset = function(){
                           self$clear_outputs()
                           super$reset_status()
                           private$.interrupt <- FALSE
                           # reset node status
                           for (id in private$.nodes$keys){
                             private$.nodes$get(id)$reset_status()
                           }
                           invisible(self)
                         },

                         #' @description Clears the outputs of all unlocked nodes
                         clear_outputs = function(){
                           for (i in seq_along(private$.nodes$keys)){
                             private$.nodes$get(i)$clear_output()
                           }
                           invisible(self)
                         },

                         #' @description Create an edge between two nodes
                         #' @param representation An Edge representation. A character string defined as 'from_node -> to_node'.
                         #' @param socket Character. The input argument where the 'from' node is plugged into
                         #' @param ... Additional options that need to be pre-specified
                         #' @examples
                         #' make_data <- function(){ c(3,4,5,NA) }
                         #' u<-network_new() %>%
                         #'       node('data', style = node_style('Dataset'), fn = node_function(fn = 'make_data', lock = T)) %>%
                         #'       node('mean', style = node_style('Mean'), fn = node_function(fn = 'mean', args = 'x', na.rm = T))
                         add_edge = function(representation, socket, ...){
                           nodes <- Edge$get_nodes_from_representation(representation)

                           if (!private$.nodes$contains_key(nodes$from_node)){
                             # check if, after attaching namespace to the node id, the node exists in the network
                             ns_id <- private$.set_network_node_id(nodes$from_node)
                             if (!private$.nodes$contains_key(ns_id))
                               stop(paste("Node", nodes$from_node, "has not been created. You need to create the node before you create the link."))
                             else{
                               # this namespace-nodeid is in the network. Set node$from_node to ns_id
                               nodes$from_node <- ns_id
                             }
                           }
                           if (!private$.nodes$contains_key(nodes$to_node)){
                             ns_id <- private$.set_network_node_id(nodes$to_node)
                             if (!private$.nodes$contains_key(ns_id))
                               stop(paste("Node", nodes$to_node, "has not been created. You need to create the node before you create the link."))
                             else
                               nodes$to_node <- ns_id
                           }

                           if (missing(socket)){
                             socket <- private$.nodes$get(nodes$to_node)$fn$args[1]
                           }

                           edge <- Edge$new(from_node = nodes$from, to_node = nodes$to, socket = socket)
                           if (!edge$representation %in% private$.edge$keys){
                             private$.structure$edges$add_value_with_key(edge$representation, edge)
                             nodes <- edge$nodes

                             private$.nodes$get(nodes$to_node)$input_ids$add(nodes$from_node)
                             private$.nodes$get(nodes$from_node)$to_node$add(nodes$to_node)
                             options <- list(...)
                             n <- names(options)
                             for (i in seq_along(options)){
                               private$.nodes$get(nodes$to_node)$fn$set_option(n[i], options[[i]])
                             }
                             if (!private$.nodes$get(nodes$from_node)$to_node_arg$contains_key(nodes$to_node))
                              private$.nodes$get(nodes$from_node)$to_node_arg$add_value_with_key(nodes$to_node, socket)
                             else
                               private$.nodes$get(nodes$from_node)$to_node_arg$set_value_with_key(nodes$to_node, socket)
                           }
                           else{
                             warning(paste("The edge", edge$representation, "already exists in the network."))
                           }
                           invisible(self)
                         },
                         #' @description Remove an edge
                         #' @param representation Edge representation. A character string defined as 'from_node -> to_node'.
                         #' @param permanent Logical; if TRUE, then the edge is removed permanently. If false, the
                         #' edge between the two nodes can be rejoined using the function edge_restore
                         remove_edge = function(representation, permanent = TRUE){
                           if (permanent){
                             nodes <- private$.structure$edges$get(representation)$nodes
                             # set the input_ids of this 'from' node to NULL
                             input_ids <- private$.nodes$get(nodes$to_node)$input_ids
                             if (input_ids$size > 0){
                               for (n in input_ids$items){
                                 if (n == nodes$from_node){
                                   input_ids$remove(n)
                                   break
                                 }
                               }
                             }

                             # remove all "to" connections from this "from" node
                             private$.nodes$get(nodes$from_node)$to_node$remove(nodes$to_node)
                           }
                           private$.structure$remove_edge(representation, permanent = permanent)
                           invisible(self)
                         },

                         #' @description Lock all the nodes in the network
                         #' @param value Logical Set it to TRUE locks all the nodes
                         lock_all_nodes = function(value){
                           if (!logical(value))
                             stop("The value must be a logical.")
                           for (n in private$.nodes$keys){
                             private$.nodes$get(n)$set_lock(value)
                           }
                           invisible(self)
                         },
                         #' @description Add a node to the network
                         #' @param node A Node object
                         add_node = function(node){
                           # if the node's network_id is NULL, then set the node's network_id with this
                           # network_id. If the node's network_id is not NULL, then the node_id is already
                           # tagged with the e
                           if (is.null(node$network_id)){
                             node$network_id <- private$.id
                             node$id <- private$.set_network_node_id(node$id)
                           }
                           private$.nodes$add_value_with_key(node$id, node)
                           invisible(self)
                         },

                         #' @description Add a node to the network
                         #' @param ... Node objects
                         add_nodes = function(...){
                           nodes <- list(...)
                           if (is.list(nodes[[1]]))
                             nodes <- nodes[[1]]
                           for (n in nodes){
                             self$add_node(n)
                           }
                           invisible(self)
                         },
                         #' @description Remove a node from the network
                         #' @param node_id Character. The id of the node to be removed from the network.
                         remove_node = function(node_id){
                           # remove all the edges associated with this node
                           edges <- private$.structure$edges
                           for (key in edges$keys){
                             nodes <- unlist(edges$get(key)$nodes)
                             if (any(nodes == node_id)){
                               private$.structure$remove_edge(edges$get(key)$representation)
                             }
                           }

                           # remove all inputs_ids and to_node
                           # loop to node ids
                           for (n in private$.nodes$get(node_id)$to_node$items){
                             # loop to node input ids
                             for (j in seq_along(private$.nodes$get(n)$input_ids$items)){
                               if (private$.nodes$get(n)$input_ids$get(j) == node_id){
                                 private$.nodes$get(n)$input_ids$remove_at(j)
                               }
                             }
                           }
                           # loop from node
                           for (n in private$.nodes$get(node_id)$input_ids$items){
                             private$.nodes$get(n)$to_node$remove(node_id)
                             private$.nodes$get(n)$to_node_arg$remove(node_id)
                           }

                           private$.nodes$remove(node_id)
                           invisible(self)
                         },
                         #' @description Build the configuration of the network for display.
                         build = function(){
                           private$.structure$build(private$.nodes)
                           invisible(self)
                         },
                         #' @description Build the network graph that will be displayed
                         build_graph = function(){
                           private$.structure$build_graph(private$.nodes)
                           invisible(self)
                         },
                         #' @description A convenient method that sets the validity of the nodes
                         #' in the network. A node is valid if it has a function. Nodes that are valid
                         #' will not be locked, whereas nodes that are invalid will be locked. Locking
                         #' will also be performed as a side effect.
                         set_node_validity = function(){
                           for (i in 1:length(private$.nodes$keys)){
                             private$.nodes$get(i)$set_validity()
                           }
                           invisible(self)
                         },

                         #' Build a branch of the network using the parsed expression.
                         #' @description Build a branck of the network using the parsed expression.
                         #' @param parsed_expr The parsed expression
                         build_branch = function(parsed_expr){

                           # add nodes
                           for (i in seq(parsed_expr)){
                             if (!self$nodes$contains_key(self$ns(names(parsed_expr)[i]))){
                               f <- Function$new(fn = parsed_expr[[i]]$fn, ns = parsed_expr[[i]]$namespace, input_args = parsed_expr[[i]]$socket)
                               for (j in seq(parsed_expr[[i]]$options)){
                                 key <- names(parsed_expr[[i]]$options)[j]
                                 f$set_option(key, parsed_expr[[i]]$options[[key]])
                               }
                               self$add_node(Process_Node$new(names(parsed_expr)[i], fn = f))
                             } else{
                               # this node is already included in the network. Check if this input argument is also in there. If it is
                               # different, then likely we have another input argument to be included. So add this new input argument to the node.
                               # Otherwise, we exclude this node.
                               node <- self$nodes$get(self$ns(names(parsed_expr)[i]))
                               if (!is.null(node$fn$args)){
                                 if (!parsed_expr[[i]]$socket %in% node$fn$args)
                                   node$fn$add_arg(parsed_expr[[i]]$socket)
                               }
                               else
                                 warning(paste("The node id", names(parsed_expr)[i], "has already been added to the network. If you intend
                to use this same id because you are creating another branch from this node, then you
                do not need to do anything. Otherwise, please change id to a different name."))
                             }
                           }

                           # add edges
                           if (length(parsed_expr) > 1){
                             for (i in 1:(length(parsed_expr) - 1)){
                               e <- Edge$representation(from_node_id = names(parsed_expr)[i],
                                                        to_node_id = names(parsed_expr)[i+1])
                               self$add_edge(e, socket = parsed_expr[[i+1]]$socket)

                             }
                           }
                           invisible(self)
                         },

                         #' @description Run the network by executing all the node operations sequentially
                         #' @param node_id Character. If the node_id is missing, then all the nodes will be run. Otherwise,
                         #' only the node of the node_id will be run.
                         run = function(node_id = NA){
                           ids <- NA
                           if (!is.na(node_id)){
                             ids <- node_id
                           }
                           else{
                             self$reset()$
                               set_node_validity()$
                               build()$
                               build_graph()


                             ids <- private$.structure$node_sequence
                           }
                           self$set_status("running")
                           for (i in seq_along(ids)){
                             if (!self$status_running()){
                               break
                             }
                             id <- ids[i]
                             current_node <- private$.nodes$get(id)
                             if (!current_node$is_locked()){
                               current_node$set_status("running")
                               private$.structure$graph$set_node_color(id, current_node$style$status_color)
                               #  update_node_display(id)


                               input_nodes <- current_node$input_ids$items
                               outputs <- NULL

                               # get options
                               options <- current_node$fn$options_to_string()
                               if (!is.null(input_nodes)){
                                 # get the output of this 'from' node and use it as the input for this node
                                 # since there could be more than one incoming node, we have loop through them
                                 for (n in input_nodes){
                                   if (!is.null(private$.nodes$get(n)$output)){
                                     l <- list(private$.nodes$get(n)$output)
                                     names(l) <- private$.nodes$get(n)$to_node_arg$get(current_node$id)
                                     outputs <- append(outputs, l)

                                   }
                                 }

                               }

                               current_node$run(outputs, options)
                               private$.structure$graph$set_node_color(id, current_node$style$status_color)

                               if (current_node$status_fail()){
                                 # then we need to stop the run
                                 self$set_status("fail")
                                 break
                               }
                             }
                           }
                           self$set_status("success")

                           invisible(self)
                         },

                         #' @description Display the network graphically
                         plot = function(){
                           visNetworkShow(self$build()$build_graph(),
                                          private$.structure$rankdir)
                         },

                         #' @description summarize the network
                         #' @return the summary of the network object
                         summary = function(){
                           self$build()

                           ans <- list()
                           ans$nodes <- paste("Total number of nodes:", length(private$.nodes))
                           ans$edges <- paste("Total number of edges:", nrow(private$.structure$path))
                           ans$functions <- data.frame(
                             Node = sapply(private$.nodes, function(n) n$id),
                             Function = sapply(private$.nodes, function(n) n$fn$fn),
                             Status = sapply(private$.nodes, function(n) n$status)
                           )
                           ans$paths <- select(private$.structure$path, from_id, to_id)

                         },

                         #' @description Get all the locked nodes
                         #' @return A vector of locked node ids
                         get_locked_nodes = function(){
                           items <- NULL
                           for (n in names(private$.nodes)){
                             if (n$is_locked()){
                               items <- c(items, n$id)
                             }
                           }
                           return (items)
                         }
                       ),
                       active = list(
                         #' @field structure Return the network structure
                         structure = function(){
                           return (private$.structure)
                         },
                         #' @field nodes Get or set the nodes
                         nodes = function(node_list){
                           if (missing(node_list))
                            return (private$.nodes)
                           private$.nodes$items <- node_list
                         },
                         #' @field interrupt Whether the network run has been interrupted.
                         interrupt = function(value){
                           if (missing(value))
                             return (private$.interrupt)
                           if (!is.logical(value))
                             stop("The value is not a logical.")
                           private$.interrupt <- value
                           invisible(self)
                         }
                       ),
                       private = list(
                         # @field .nodes Stores a list of Node objects.
                         .nodes = NULL,
                         # @field .sources Stores a list of sources
                         .sources = NULL,
                         # @field .interrupt Whether the network is interrupted
                         .interrupt = FALSE,
                         # @field .structure Stores a Network_Structure object
                         .structure = NULL,
                         # @field .counter_id. Counter used to create new id for a new node
                         .counter_id = 1,

                         # @description .set_network_node_id Attach the network id to the node id
                         # @param node_id Character
                         # @return Character
                         .set_network_node_id = function(node_id){
                           return (paste(private$.id, node_id, sep = "-"))
                         },
                         # @description .get_network_node_id. Get the network id from the node id.
                         # @param node_id Character
                         # @return Character
                         .get_network_node_id = function(node_id){
                           return (strsplit(node_id, split = "-")[[1]][1])
                         },

                         # @description .new_id. Creates a new numeric ID
                         .new_id = function(){
                           private$.id <- as.character(private$.counter_id)
                           private$.counter_id <- private$.counter_id + 1
                         },

                         extract = function(){
                           values <- super$extract()

                           values <- append(values,
                             list(nodes = private$objExtract(private$.nodes),
                                  sources = private$.sources,
                                  interrupt = private$.interrupt,
                                  structure = private$objExtract(private$.structure),
                                  counter_id = private$.counter_id
                             )
                           )

                         },

                         restore = function(values){
                           super$restore(values)
                           private$.nodes <- private$objRestore(values$nodes)
                           private$.sources <- values$sources
                           private$.interrupt <- values$interrupt
                           private$.structure <- private$objRestore(values$structure)
                           private$.counter_id <- values$counter_id

                         }
                       )
)
#' static method
#'
#' is_network
#' @description Check if the object is a network class
#' @return A character
#' @export
is_Network <- function(obj){
  return (inherits(obj, "Network"))
}

network_new <- function(id, style = NULL, author = "", description = "", sources = NULL, rankdir = 'LR'){
  network <- Network$new(id = id,
                         style = style,
                         author = author,
                         description = description,
                         rankdir = rankdir)

  return (network)
}

network_add_nodes <- function(network, ...){
  nodes <- list(...)
  for (i in 1:length(nodes))
    network$add_node(nodes[[i]])
  return (network)
}

network_add_edges <- function(network, ...){
  edges <- list(...)
  for (i in 1:length(edges))
    network$add_edge(edges[[i]])
  return (network)
}
