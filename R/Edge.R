#' R6 Class representing a node
#'
#' @description
#' Creates an Edge object
#' @details
#' Specifies the connections between nodes
#' @export
Edge <- R6::R6Class("Edge",
                    inherit = Serializable,
                    public = list(

                      #' initialize
                      #' @description Create an Edge object
                      #' @param representation An edge representation, can be created using the
                      #' static method representation
                      #' @param from_node the id of the "from" node
                      #' @param to_node the id of the "to" node
                      #' @param socket Character the argument name of the "to" node function
                      #' @param default Logical. If TRUE, then no data fields are initialized.
                      #' @return An Edge object
                      initialize = function(representation, from_node, to_node, socket, default = FALSE){
                        if (!default){
                          if (missing(representation)){
                            if (!missing(from_node) && !missing(to_node)){
                              private$.representation <- self$create_representation(from_node, to_node)
                              private$.from_node <- from_node
                              private$.to_node <- to_node
                            } else{
                              stop("Missing either the 'from' node or 'to' node.")
                            }
                          } else{
                            nodes <- Edge$get_nodes_from_representation(representation)
                            private$.from_node <- nodes$from_node
                            private$.to_node <- nodes$to_node
                            private$.representation <- gsub(' ', '', representation)
                          }
                          private$.to_node_arg <- socket
                        }
                      },
                      #' create_representation
                      #' @description Create an edge representation
                      #' @param from_node the id of the "from" node
                      #' @param to_node the id of the "to" node
                      create_representation = function(from_node, to_node){
                        private$.representation <- paste0(from_node, "->", to_node)
                      },
                      #' is_connected
                      #' @description Check if the edge is connected
                      #' @return logical
                      is_connected = function(){
                        return (private$.connected)
                      },
                      #' render
                      #' @description Create a visnetwork data frame for rendering the edges
                      render = function(){
                        data.frame(
                          id = private$.representation,
                          color = private$.color,
                          label = if (!is.null(private$.to_node_arg)){
                            private$.to_node_arg
                          } else{
                            ""
                          },
                          stringsAsFactors = FALSE
                        )
                      },
                      #' set_connect
                      #' @description Set the edge connection
                      #' @param value logical
                      set_connect = function(value){
                        self$connected <- value
                        invisible(self)
                      }
                    ),
                    active = list(
                      #' @field connected Set the logical value for connected
                      connected = function(value){
                        if (missing(value))
                          stop("connected does not return any value.")
                        if (!is.logical(value))
                          stop("The value must be logical.")
                        private$.connected <- value
                        invisible(self)
                      },
                      #' @field representation Set the representation
                      representation = function(value){
                        if (missing(value))
                          return (private$.representation)
                        stop("representation is read only.")
                      },
                      #' @field from_node Get or set the id of the "from" node
                      from_node = function(node_id){
                        if (missing(node_id))
                          return (private$.from_node)
                        private$.from_node <- node_id
                        invisible(self)
                      },
                      #' @field to_node Get or set the id of the "to" node
                      to_node = function(node_id){
                        if (missing(node_id))
                          return (private$.to_node)
                        private$.to_node <- node_id
                        invisible(self)
                      },
                      #' @field to_node_arg Get or set the i"to" node argument
                      to_node_arg = function(value){
                        if (missing(value))
                          return (private$.to_node_arg)
                        private$.to_node_arg <- value
                        invisible(self)
                      },
                      #' @field nodes Get the "from" and "to" nodes
                      nodes = function(value){
                        if (missing(value))
                          return (list(from_node = private$.from_node, to_node = private$.to_node))
                        stop("nodes is read only.")
                      },
                      #' @field status_color Get or set the status color
                      status_color = function(status){
                        if (missing(status))
                          return (private$.color)
                        private$.color <- private$.status_color[[status]]
                        invisible(self)
                      }
                    ),
                    private = list(

                      # @field .connected Whether the edge is connected
                      .connected = TRUE,
                      # @field .representation A string representation - 'from_node -> to_node'
                      .representation = NULL,
                      # @field .from Stores the node id that connects to another node
                      .from_node = NULL,
                      # @field .to_node Stores the node id that receives the connection from the 'from' node
                      .to_node = NULL,
                      # @field .to_node_arg Stores the function argument that the edge points to
                      .to_node_arg = NULL,
                      # @field .color The color of the edge
                      .color = "black",
                      # @field .status_color A constant list. The stored status colors of the edge
                      .status_color = list(
                        "connected" = "black"
                      ),

                      extract = function(){
                        values <-
                          list(connected = private$.connected,
                               representation = private$.representation,
                               from_node = private$.from_node,
                               to_node = private$.to_node,
                               to_node_arg = private$.to_node_arg,
                               color = private$.color,
                               status_color = private$.status_color
                          )


                      },

                      restore = function(values){
                        private$.connected = values$connected
                        private$.from_node = values$from_node
                        private$.to_node = values$to_node
                        private$.to_node_arg = values$to_node_arg
                        private$.color = values$color
                        private$.status_color = values$status_color
                      }

                    )
)

#' static method
#' @title is_edge
#' @description Check if the object is an edge
#' @param obj an object
#' @return A logical
#' @export
is_Edge <- function(obj){
  return (inherits(obj, "Edge"))
}
#' get_nodes_with_edge
#'
Edge$get_nodes_from_representation <- function(representation){
  representation <- gsub(" ", "", representation)
  nodes <- strsplit(representation, split = "->")[[1]]
  return (list("from_node" = nodes[1], "to_node" = nodes[2]))
}

Edge$representation <- function(from_node_id, to_node_id){
  return (paste(from_node_id, to_node_id, sep = "->"))
}

