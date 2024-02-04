#' R6 class representing a Netflow
#'
#' @description
#' Creates a Netflow object
#' @details
#' Netflow integrates and manages all the networks in the environment
#' @export
Netflow <- R6::R6Class("Netflow",
                       inherit = Network,
                       public = list(
                         #' initialize
                         #' @description Create a Netflow object
                         #' @param id The id of the Netflow
                         #' @param author The name of the creator of this network
                         #' @param description The description of this network
                         #' @param rankdir The direction of the graph; can be 'LR', 'TD'; default 'LR'
                         #' @return An Netflow object
                         initialize = function(id, author = "", description = "", rankdir = 'LR'){
                           super$initialize(id, author = author, description = description, rankdir = rankdir)
                         },
                         #' add_network
                         #' @description Add a network
                         #' @param network A Network object
                         add_network = function(network){
                           for (k in network$nodes$keys)
                             self$add_node(network$nodes$get(k))
                           for (k in network$structure$edges$keys)
                             private$.structure$edges$add_value_with_key(k, network$structure$edges$get(k))
                           private$.network_ids <- append(private$.network_ids, network$id)
                           invisible(self)
                         },
                         #' remove_network
                         #' @description Remove a network
                         #' @param network_id Character. The network id to be removed
                         remove_network = function(network_id){
                           for (k in private$.nodes$keys){
                             if (private$.nodes$get(k)$network_id == network_id)
                               super$remove_node(k)
                           }
                           invisible(self)
                         },
                         #' reset
                         #' @description Reset the networks by clearing all the outputs of all the
                         #' unlocked nodes. Locked nodes are not reset.
                         #' @param network_ids A vector of characters of network ids.
                         reset = function(network_ids){
                           if (missing(network_ids))
                             super$reset()
                           else{
                            for (k in private$.nodes$keys){
                              if (private$.nodes$get(k)$network_id %in% network_ids){
                                private$.nodes$get(k)$reset()
                              }
                            }
                           }
                           invisible(self)
                         },
                         #' clear_outputs
                         #' @description Helper function that clears the outputs of all unlocked networks
                         #' @param network_ids A vector of characters of network ids.
                         clear_outputs = function(network_ids){
                           if (missing(network_ids))
                             super$clear_outputs()
                           else{
                            for (k in private$.nodes$keys){
                              if (private$.nodes$get(k)$network_id %in% network_ids){
                                private$.nodes$get(k)$clear_output()
                              }
                            }
                           }
                         },

                         #' lock_networks
                         #' @description Lock all the nodes in the networks
                         #' @param network_ids A vector of characters of network ids
                         #' @param value Logical. TRUE to lock the networks
                         lock_networks = function(network_ids, value){
                           if (!logical(value))
                             stop("The value must be a logical.")
                           if (missing(network_ids))
                             network_ids <- private$.network_ids

                           for (k in private$.network$nodes$keys){
                             if (private$.nodes$get(k)$network_id  %in% network_ids)
                               private$.nodes$get(k)$set_lock(value)
                           }
                           invisible(self)
                         },
                         #' add_nodes
                         #' @description Add a series of nodes to a network
                         #' @param ... Node objects
                         #' @param network_id Character. The network id.
                         add_nodes = function(..., network_id){
                           if (missing(network_id))
                             network_id <- private$.id

                           nodes <- list(...)
                           if (is.list(nodes[[1]]))
                             nodes <- nodes[[1]]
                           for (n in nodes){
                             self$add_node(n, network_id)
                           }
                           invisible(self)
                         },
                         #' add_node
                         #' @description Add a node to the network
                         #' @param node A Node object
                         #' @param network_id Character. The network id
                         add_node = function(node, network_id){
                           if (!missing(network_id)){
                             if (is.null(node$network_id)){
                               node$network_id <- network_id
                               node$id <- private$.set_network_node_id(network_id, node$id)
                             }
                           }
                           private$.nodes$add_value_with_key(node$id, node)
                           invisible(self)
                         },
                         #' remove_node
                         #' @description Remove a node from the network
                         #' @param node_id Character. The id of the node to be removed from the network
                         remove_node = function(node_id){
                           super$remove_node(node_id)

                           invisible(self)
                         },
                         #' set_node_validity
                         #' @description A convenient function that sets the validity of the nodes
                         #' in the network. A node is valid if it has a function. Nodes that are valid
                         #' will not be locked, whereas nodes that are invalid will be locked. Locking
                         #' will also be performed as a side effect.
                         set_node_validity = function(){
                           super$set_node_validity()
                           invisible(self)
                         },
                         #' set_status
                         #' @description Get the current status of the network
                         #' @param value Character. The status descriptor. Can be "idle", "running",
                         #' "warning", "fail", "success"
                         set_status = function(value = c("idle", "running", "warning", "fail", "success")){
                           value <- match.arg(value)
                           private$.status <- value
                           invisible(self)
                         },

                         #' run
                         #' @description Run the network by executing all the node operations sequentially
                         #' @param node_id Character. If it is NA, then all the nodes will be run. Otherwise, the specified
                         #' node will be be run.
                         run = function(node_id = NA){
                           super$run(node_id)
                           invisible(self)
                         },

                         # FIXME
                         #' @description summarize the Netflow
                         #' @return the summary of the network object
                         summary = function(){
                           self$build()

                           ans <- list()
                           ans$nodes <- paste("Total number of networks:", length(private$.nodes))
                           ans$networks <- private$.network_ids
                           ans$paths <- private$.structure$node_sequence
                           return (ans)
                         },
                         #' get_locked_networks
                         #' @description Get all the locked networks
                         #' @return A Vector of locked network ids
                         get_locked_networks = function(){
                           items <- NULL
                           for (n in names(private$.networks)){
                             if (n$is_locked()){
                               items <- c(items, n$id)
                             }
                           }
                           return (items)
                         },
                         #' net_flow
                         #' @description Run the Netflow shiny gadget
                         net_flow = function(){
                           gadgetFlow(self)
                         },

                         #' @description Display the network graphically
                         print = function(){
                           visNetworkShow(self$structure$graph)
                         },
                         #' plot_node_update
                         #' @description Update the plot of the node in visnetwork
                         #' @param node_id Character. the node id.
                         #' @param network_id Character. The network id.
                         plot_node_update = function(node_id, network_id){
                           updateNodeStatus(private$.structure$graph$get_node(node_id), network_id)
                           invisible(self)
                         },
                         #' plot_network_update
                         #' @description Update a network in the Netflow.
                         #' @param network_id Character. The network id.
                         plot_network_update = function(network_id){
                           updateNetworkStatus(private$.structure$graph, network_id)
                           invisible(self)
                         },
                         #' server
                         #' @description the server
                         #' @param input the Shiny input
                         #' @param output the Shiny output
                         #' @param session the Shiny session
                         server = function(input, output, session){
                           shinyFlowServer(input, output, session, self)
                         },
                         #' call
                         #' @description call the module
                         #' @param input the Shiny input
                         #' @param output the Shiny output
                         #' @param session the Shiny session
                         call = function(input, output, session){
                           callModule(self$server, self$id)
                         }
                       ),
                       active = list(
                         #' @field network_ids Get a vector of network ids.
                         network_ids = function(value){
                           if (missing(value))
                             return (private$.network_ids)
                           stop("network_ids is read-only.")
                         }
                       ),
                       private = list(
                         # @field .network_ids
                         .network_ids = NULL,
                         # .set_network_node_id
                         # @description Create the network id - node id code
                         # @param network_id The network id
                         # @param node_id The node id
                         .set_network_node_id = function(network_id, node_id){
                           return (paste(network_id, node_id, sep = "-"))
                         },

                         extract = function(){
                           values <- super$extract()
                           values <- append(
                             list(network_ids = private$.network_ids)
                           )

                         },

                         restore = function(values){
                           super$restore(values)
                           private$.network_ids <- values$network_ids
                         }
                       )

)

#' is_network_group
#' @description Check if this object is a Netflow
#' @param obj An object
#' @return Logical
#' @export
is_Netflow <- function(obj){
  return (class(obj) == "Netflow")
}
