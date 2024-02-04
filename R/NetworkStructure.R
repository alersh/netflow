#' R6 class representing Network_Structure
#'
#' @description
#' Creates a Network_Structure object
#' @details
#' Specifies the structure of the network
Network_Structure <- R6::R6Class("Network_Structure",
                                 inherit = Serializable,
                                 public = list(
                                  #' @description Creates a Network_Structure object
                                  #' @param rankdir The direction of the visNetwork. Either "LR" (left-right) or "UD" (up-down). Default is LR.
                                  #' @param default Logical. If TRUE, then no data fields are initialized.
                                  #' @return A Network_Structure object
                                  initialize = function(rankdir = "LR", default = FALSE){
                                    if (!default){
                                      private$.rankdir <- rankdir
                                      private$.graph <- Network_Graph$new()
                                      private$.edges <- Dictionary$new()
                                      private$.node_sequence <- Set$new()
                                    }
                                  },


                                  #' restore edge
                                  #' @description Restore an edge that has been temporarily removed
                                  #' @param representation An edge representation
                                  restore_edge = function(representation){
                                    private$.edges$get(representation)$set_connect(TRUE)
                                    invisible(self)
                                  },

                                  #' remove_edge
                                  #' @description Remove an edge from the structure
                                  #' @param representation An edge representation
                                  #' @param permanent Logical. Whether the edge is removed permanently.
                                  remove_edge = function(representation, permanent = TRUE){
                                    if (permanent)
                                      private$.edges$remove(representation)
                                    else
                                      private$.edges$get(representation)$set_connect(FALSE)
                                    invisible(self)
                                  },

                                  #' @description Build the configuration of the network for display
                                  #' @param network_nodes A vector of Node objects.
                                  build = function(network_nodes){
                                    private$.build_path(network_nodes)

                                    # collect all loose nodes and put them in the first level
                                    solitary_nodes <- which(!network_nodes$keys %in% private$.node_sequence)

                                    if (length(solitary_nodes) > 0){
                                      solitary_node_names <- sapply(solitary_nodes, function(k){
                                        as.character(network_nodes$get(k)$id)
                                      })
                                      private$.node_sequence <- c(private$.node_sequence, solitary_node_names)
                                    }
                                    invisible(self)

                                  },
                                  #' build_graph
                                  #' @description Build the graph display
                                  #' @param network_nodes A vector of Node objects in the network
                                  build_graph = function(network_nodes){
                                    # build graph
                                    if (!network_nodes$is_empty() && !private$.edges$is_empty())
                                      private$.graph$build_graph(network_nodes,
                                                                 private$.level(network_nodes),
                                                                 private$.edges,
                                                                 private$.path)
                                    invisible(self)
                                  }
                                ),

                                active = list(
                                  #' @field rankdir Get or set the network direction. Either "LR" or "UD"
                                  rankdir = function(value  = c("LR", "UD")){
                                    if (missing(value))
                                      return (private$.rankdir)
                                    value <- match.arg(value)
                                    private$.rankdir <- value
                                    invisible(self)
                                  },
                                  #' @field node_sequence Get or set the node sequence
                                  node_sequence = function(value){
                                    if (missing(value))
                                      return (private$.node_sequence)
                                    private$.node_sequence <- value
                                    invisible(self)
                                  },
                                  #' @field graph Get the graph object
                                  graph = function(graph){
                                    if (missing(graph))
                                      return (private$.graph)
                                    stop("graph is read only.")
                                  },
                                  #' @field edges Get or set the Dictionary of edges
                                  edges = function(value){
                                    if (missing(value)){
                                      return (private$.edges)
                                    }
                                    private$.edges <- value
                                    invisible(self)
                                  },
                                  #' @field path Get the paths
                                  path = function(value){
                                    if (missing(value)){
                                      return (private$.path)
                                    }
                                    stop("You cannot set the path directly.")
                                    invisible(self)
                                  }

                                ),

                                private = list(
                                  # @field .rankdir The direction of the visNetwork
                                  .rankdir = NA,
                                  # @field .graph The Network_Graph object
                                  .graph = NULL,
                                  # @field .path The data frame representiing the path between nodes
                                  .path = NULL,
                                  # @field .node_sequence A vector of node ids that represent the order of the nodes in the network
                                  .node_sequence = NULL,
                                  # @field edges Stores a set of Edge objects
                                  .edges = NULL,

                                  # @description Determine all connections between the nodes
                                  # @param network_nodes A Dictionary of Node object in the network
                                  .build_path = function(network_nodes){
                                    n <- network_nodes$keys

                                    items <- lapply(private$.edges$items, function(e){
                                      if (e$is_connected())
                                        return (append(e$nodes, list(representation = e$representation)))
                                    })

                                    links <- data.frame(representation = rep(NA, length(items)),
                                                        from = rep(0, length(items)),
                                                        to = rep(0, length(items)),
                                                        from_id = rep(NA, length(items)),
                                                        to_id = rep(NA, length(items)))
                                    # enter position of each node
                                    if (length(items) > 0){
                                      for (i in seq_along(items)){
                                        t <- items[[i]]
                                        from_idx <- which(n == t[["from_node"]])
                                        to_idx <- which(n == t[["to_node"]])
                                        links[i, 'representation'] <- t[["representation"]]
                                        links[i, 'from'] <- from_idx
                                        links[i, 'to'] <- to_idx
                                        links[i, 'from_id'] <- n[from_idx]
                                        links[i, 'to_id'] <- n[to_idx]
                                      }
                                    }

                                    # sort by the 'to' column
                                    private$.path <- private$.build_path_sequence(links)
                                    private$.node_sequence <- unique(c(private$.path$from_id, private$.path$to_id))

                                  },
                                  # @description A function that builds the path sequence based upon the paths
                                  # @param links A data frame that specifies the "from" and "to" nodes
                                  # @return A data frame
                                  #' @importFrom dplyr arrange
                                  .build_path_sequence = function(links){
                                    flag <- 1
                                    while(flag > 0){
                                      flag <- 0
                                      maxvalue <- max(c(links$from, links$to))
                                      for (i in 1:nrow(links)){
                                        if (links$from[i] > links$to[i]){
                                          maxvalue <- maxvalue + 1
                                          links$to[i] <- maxvalue
                                          idx <- which(links$from_id == links$to_id[i])
                                          if (length(idx) > 0){
                                            links$from[idx] <- maxvalue
                                          }
                                          flag <- flag + 1
                                        }
                                      }
                                    }
                                    links <- arrange(links, to)
                                    return (links)
                                  },

                                  # @description Get the node order of the graph. Nodes will be run according to this ordering arrangement.
                                  # @param network_nodes A Dictionary of Node objects
                                  # @return A data frame of node levels
                                  #' @importFrom dplyr %>% select rename mutate filter bind_rows
                                  #' @importFrom purrr map_chr
                                  .level = function(network_nodes){
                                    nodes <- NULL
                                    levels <- NULL
                                    if (!is.null(private$.path) && nrow(private$.path) > 0){
                                      df <- private$.path %>%  # path must be in order of activation
                                        select(-from, -to) %>%
                                        rename(from = from_id, to = to_id)

                                      # first, we get all the branches
                                      tmp <- mutate(df, visited = rep(F, nrow(df))) # keep track of which edges have not been visited
                                      branches <- list() # keep track of all branches
                                      k <- 1
                                      i <- 1
                                      while (!all(tmp$visited)){
                                        if (!tmp$visited[i]){
                                          link <- c() # stores all the linked nodes on this branch
                                          t <- private$.edge_traverse(df, i, tmp, link)
                                          tmp <- t$tmp
                                          branches[[k]] <- t$link
                                          k <- k+1
                                        }
                                        i <- i+1
                                      }

                                      # attach all branches together through their common nodes
                                      nodes <- unique(c(df$from, df$to))
                                      levels <- data.frame(id = nodes, level = rep(NA, length(nodes)), stringsAsFactors = F)

                                      reference <- branches[[1]]
                                      visited <- rep(F, length(branches))
                                      levels[which(levels$id %in% reference), 'level'] <- seq(1, length(reference))
                                      visited[1] <- T

                                      for (i in seq_along(branches)){
                                        if(visited[i]){
                                          found <- NULL
                                          refIdx <- i
                                          reference <- branches[[refIdx]]
                                          # find other branches that contain the same nodes as this reference branch
                                          for (j in seq_along(branches)){
                                            if (j != refIdx && !visited[j]){
                                              if (any(reference %in% branches[[j]])){
                                                found <- c(found, j)
                                              }
                                            }
                                          }
                                          if (!is.null(found)){
                                            # for each node in this reference branch, get the other branch with the same node
                                            # and then set the levels for that branch
                                            for (k in found){
                                              idx <- which(branches[[k]] %in% reference)
                                              l <- filter(levels, id == branches[[k]][idx])$level
                                              s <- l - idx
                                              # set levels of all nodes in the branch based on this node level
                                              levels[which(levels$id %in% branches[[k]]), 'level'] <- seq(1, length(branches[[k]])) + s
                                              visited[k] <- T
                                            }
                                          }
                                        }
                                        else{
                                          # not visited yet, so find a visited branch that contains the same node
                                          # as this branch
                                          # if no such branch is found, then set levels to default
                                          found <- NULL
                                          for (j in seq_along(branches)){
                                            if (j != i && visited[j]){
                                              if (any(branches[[i]] %in% branches[[j]])){
                                                found <- c(found, j)
                                              }
                                            }
                                          }
                                          if (!is.null(found)){
                                            for (k in found){
                                              reference <- branches[[k]]
                                              idx <- which(branches[[i]] %in% reference)
                                              l <- filter(levels, id == branches[[i]][idx])$level
                                              s <- l - idx
                                              # set levels of all nodes in the branch based on this node level
                                              levels[which(levels$id %in% branches[[i]]), 'level'] <- seq(1, length(branches[[i]])) + s
                                              visited[i] <- T
                                            }
                                          }
                                          else{
                                            levels[which(levels$id %in% branches[[i]]), 'level'] <- seq(1, length(branches[[i]]))
                                          }
                                        }
                                        if (all(visited)){
                                          break
                                        }
                                      }
                                      # level to start at 1
                                      levels$level <- levels$level + (1 - min(levels$level))
                                    } # get all edges

                                    # collect all loose nodes and put them in the first level
                                    solitary_nodes <- which(!network_nodes$keys %in% nodes)

                                    if (length(solitary_nodes) > 0){
                                      solitary_node_names <- map_chr(solitary_nodes,
                                                                     function(k){network_nodes$get(k)$id})
                                      levels <- bind_rows(levels,
                                                          data.frame(id = solitary_node_names,
                                                                     level = rep(1, length(solitary_nodes)),
                                                                     stringsAsFactors = F))
                                    }
                                    return (levels)
                                  },

                                  # @description A recursive function that visits edges
                                  # @param df The data frame of edges, containg a from and to columns of nodes
                                  # @param i The index of the current node
                                  # @param tmp A temporary data frame that keeps track of all the visited and unvisited edges
                                  # @param link All the links associated with this ith node
                                  # @return A list containing the tmp and link objects
                                  #' @importFrom dplyr filter
                                  .edge_traverse = function(df, i, tmp, link){
                                    tmp$visited[i] <- T
                                    link <- c(link, df$from[i])

                                    to_node <- df$to[i]
                                    unvisited <- filter(tmp, !visited)
                                    if (!to_node %in% unvisited$from){
                                      link <- c(link, to_node)
                                    }
                                    else{
                                      # keep traversing
                                      idx <- which(!tmp$visited & tmp$from == to_node)
                                      # only take one if there is more than 1 paths

                                      t <- private$.edge_traverse(df, idx[1], tmp, link)
                                      tmp <- t$tmp
                                      link <- t$link
                                    }

                                    return (list(tmp = tmp, link = link))
                                  },

                                  extract = function(){
                                    values <-
                                      list(rankdir = private$.rankdir,
                                           graph = private$objExtract(private$.graph),
                                           path = private$.path,
                                           node_sequence = private$.node_sequence,
                                           output = private$.output,
                                           edges = private$objExtract(private$.edges)
                                      )


                                  },

                                  restore = function(values){
                                    private$.rankdir = values$rankdir
                                    private$.graph = private$objRestore(values$graph)
                                    private$.path = values$path
                                    private$.node_sequence = values$node_sequence
                                    private$.edges = private$objRestore(values$edges)

                                  }
                                )
)
#' @title is_Network_Structure
#' @description Check if the object is inherited from the Network_Structure
#' @param obj An object
#' @return Logical
#' @export
is_Network_Structure <- function(obj){
  return (inherits(obj, "Network_Structure"))
}
