#' R6 class representing a network graph
#'
#' @description
#' A list that stores the information about the appearance of the nodes and edges
#' in the visnetwork
#'
#' @details
#' Specifies the appearance of the network
Network_Graph <- R6::R6Class("Network_Graph",
                             inherit = Serializable,
                             public = list(
                               #' initialize
                               #' @description Create a graph object
                               #' @param default Logical. It is not being used.
                               #' @return A Network_Graph object
                               initialize = function(default = FALSE){

                                 private$.node_table <- data.frame(id = NULL,
                                                                   label = NULL,
                                                                   color.background = NULL,
                                                                   color.border = NULL,
                                                                   shape = NULL,
                                                                   group = NULL,
                                                                   title = NULL
                                                                   )
                                 private$.edge_table <- data.frame(from = NULL,
                                                                   to = NULL,
                                                                   color = NULL,
                                                                   label = NULL,
                                                                   title = NULL)

                               },
                               #' get_node
                               #' @description Get the node's visnetwork description
                               #' @param node_id Character the id of the node
                               #' @return data frame
                               get_node = function(node_id){
                                 return (private$.node_table[private$.node_table$id == node_id, ])
                               },
                               #' set_node_color
                               #' @description Set the node's color
                               #' @param node_id Character the id of the node
                               #' @param color Character the color of the node
                               set_node_color = function(node_id, color){
                                 if (!node_id %in% private$.node_table$id){
                                   stop("No such node id exists in the network structure.")
                                 }
                                 if (!is.character(color))
                                   stop("The input 'color' must be a character.")
                                 private$.node_table$color.background[which(private$.node_table$id == node_id)] <- color
                                 invisible(self)
                               },
                               #' set_node_color
                               #' @description Set the node's shape
                               #' @param node_id Character the id of the node
                               #' @param shape Character the shape of the node
                               set_node_shape = function(node_id, shape){
                                 if (!node_id %in% private$.node_table$id){
                                   stop("No such node id exists in the network structure.")
                                 }
                                 if (!is.character(shape))
                                   stop("The input 'shape' must be a character.")
                                 private$.node_table$shape[which(private$.node_table$id == node_id)] <- shape
                               },
                               #' set_node_border
                               #' @description Set the node's border color
                               #' @param node_id Character the id of the node
                               #' @param border_color Character the color of the border
                               set_node_border = function(node_id, border_color){
                                 if (!node_id %in% private$.node_table$id){
                                   stop("No such node id exists in the network structure.")
                                 }
                                 if (!is.character(border_color))
                                   stop("The input 'border_color' must be a character.")
                                 private$.node_table$color.border[which(private$.node_table$id == node_id)] <- border_color
                               },
                               #' set_node_label
                               #' @description Set the node's label
                               #' @param node_id Character the id of the node
                               #' @param label Character the label of the node
                               set_node_label = function(node_id, label){
                                 if (!node_id %in% private$.node_table$id){
                                   stop("No such node id exists in the network structure.")
                                 }
                                 if (!is.character(label))
                                   stop("The input 'label' must be a character.")
                                 private$.node_table$label[which(private$.node_table$id == node_id)] <- label
                               },
                               #' is_empty
                               #' @description Check whether the node and edge tables are empty
                               #' @return Logical
                               is_empty = function(){
                                 return (nrow(private$.node_table) == 0 &&
                                           nrow(private$.edge_table) == 0)
                               },

                               #' build_graph
                               #' @description Create a node and edge table that describe the graph of the network
                               #' @param nodes A Dictionary of Nodes
                               #' @param node_level The levels where the nodes are placed in the visnetwork
                               #' @param edges A Dictionary of Edges
                               #' @param path The data frame that specifies the path
                               #' @importFrom dplyr mutate select %>%
                               #' @importFrom purrr map_chr map2_chr
                               build_graph = function(nodes, node_level, edges, path){
                                 private$.build_node_table(nodes, node_level)
                                 private$.build_edge_table(edges, path)
                                 invisible(self)
                               }
                             ),
                             active = list(
                               #' @field edge_table Get the edge table
                               edge_table = function(value){
                                 if (missing(value))
                                   return (private$.edge_table)
                                 stop("edge_table is read-only.")
                               },
                               #' @field node_table Get the node table
                               node_table = function(value){
                                 if (missing(value))
                                   return (private$.node_table)
                                 stop("node_table is read_only.")
                               }

                             ),
                             private = list(
                               # @field
                               .node_table = NULL,
                               # @field
                               .edge_table = NULL,
                               # .build_node_table
                               # @description Create a node table
                               # @param nodes a vector of Node objects
                               # @param node_level a vector contain numeric values representing the levels in visnetwork
                               # @importFrom purrr map
                               # @importFrom dplyr inner_join
                               .build_node_table = function(nodes, node_level){

                                 l <- purrr::map(node_level$id, function(n){
                                   nodes$get(n)$style$render(n, nodes$get(n)$network_id)
                                 })
                                 private$.node_table <- do.call('rbind', l) %>%
                                   dplyr::inner_join(node_level, by = "id")

                                 invisible(self)
                               },
                               #' .build_edge_table
                               # @description Create an edge table
                               # @param edges a vector of Edge objects
                               # @param path_id a table containing the links between the nodes
                               # @importFrom purrr map
                               # @importFrom dplyr select
                               .build_edge_table = function(edges, path_id){
                                 l <- purrr::map(path_id$representation, function(n){
                                   edges$get(n)$render()
                                 })

                                 private$.edge_table <- do.call('rbind', l) %>%
                                   cbind(dplyr::select(path_id, from = from_id, to = to_id))


                                 invisible(self)
                               },
                               extract = function(){
                                 values <-
                                   list(node_table = private$.node_table,
                                        edge_table = private$.edge_table
                                   )


                               },

                               restore = function(values){
                                 private$.node_table <- values$node_table
                                 private$.edge_table <- values$edge_table
                               }


                             )
)

#' @title is_Network_Graph
#' @description Check if the class of the object is Network_Graph
#' @param obj An object
#' @return Logical
#' @export
is_Network_Graph <- function(obj){
  return (inherits(obj, "Network_Graph"))
}
