#' R6 Class representing a node
#'
#' @description
#' A process node contains a function that processes the output from another node
#' @details
#' A Node with Function
#' @export
Process_Node <- R6::R6Class("Process_Node",
                            inherit = Node,
                            public = list(
                              #' @description Create a node
                              #' @param id The id name of this node
                              #' @param fn The function the node uses to process data
                              #' @param style A Node_Style object that specifies the appearance of the node
                              #' @param visualization The written code that creates custom visuals.
                              #' @param author Character. The name of the author.
                              #' @param description The description of this node
                              #' @param default Logical. If TRUE, then no data fields are initialized.
                              #' @return A Process_Node object
                              initialize = function(id = "", fn = NULL, style = NULL, visualization = NULL, author = "", description = "", default = FALSE){

                                if (!default){
                                  super$initialize(id, style, "process", author, description)
                                  private$.fn <- fn  # function of this node, including the arguments and options of this function
                                  private$.input_ids <- Set$new()
                                  private$.to_node <- Set$new()
                                  private$.to_node_arg <- Dictionary$new()
                                  # set custom visualization
                                  if (is.function(visualization))
                                    visualization <- paste(capture.output(visualization), collapse = "\n")

                                  private$.visualization <- visualization # visualization function
                                }
                              },

                              #' @description Run the function of this node
                              #' @param output_list The list of outputs associated with the nodes
                              #' @param argument The vector of arguments which the outputs go to
                              run = function(output_list, argument){
                                n <- names(output_list)
                                outputs <- ""
                                for (i in seq_along(output_list)){
                                  if (i == 1){
                                    outputs <- paste(n[i], deparse(substitute(output_list[[n[a]]], list(a = i))), sep = "=")
                                  } else{
                                    outputs <- paste(outputs, paste(n[i], deparse(substitute(output_list[[n[a]]], list(a = i))), sep = "="), sep = ",")
                                  }
                                }

                                if (!is.null(argument) && outputs != ""){
                                  argument <- paste(outputs, argument, sep = ",")
                                } else if (is.null(argument) && outputs != ""){
                                  argument <- outputs
                                }

                                expr <- private$.fn$make_function_string(argument)

                                self$output <- tryCatch({
                                  eval(expr)
                                }, error = function(e) e)

                                if (inherits(self$output, 'error')){
                                  self$set_status('fail')
                                  private$.errormsg <- self$output$message
                                }
                                else if (inherits(self$output, 'warning')){
                                  self$set_status('warning')
                                  private$.warningmsg <- self$output$message
                                }
                                else{
                                  # save output
                                  status <- tryCatch({
                                    saveOutput(self$output, self$id)
                                    self$set_output_filename()
                                    },
                                    error = function(e) e
                                  )
                                  if (inherits(status, "error")){
                                    self$set_status_fail()
                                    private$.errormsg <- status$message
                                  } else{
                                    self$set_status('success')
                                  }
                                }
                                return (self$status)
                              },
                              #' set_validity
                              #' @description Set the validity of the node. A node is considered valid
                              #' if it contains a function
                              set_validity = function(){
                                if (is.null(private$.fn))
                                  private$.style$invalidate()

                                invisible(self)
                              },
                              #' clear_output
                              #' @description Clear the output of this node
                              clear_output = function(){
                                if (!self$is_locked())
                                  self$output <- NULL
                                invisible(self)
                              },
                              #' reset
                              #' @description Reset the node's status and clear output
                              reset = function(){
                                self$reset_status()
                                self$clear_output()
                                invisible(self)
                              },
                              #' ui
                              #' @description Create the Shiny UI elements of the process node
                              #' @param ns The parent's namespace
                              ui = function(ns){
                                ns_node <- ns
                                nodeInfoUI(ns_node,
                                           nodeStatusUI(ns_node),
                                           nodeFunctionUI(ns_node),
                                           nodeOutputTableUI(ns_node),
                                           nodeVisualizationUI(ns_node),
                                           nodeDescriptionUI(ns_node))
                              },
                              #' server
                              #' @description Create the Shiny server
                              #' @param input The Shiny input
                              #' @param output The Shiny output
                              #' @param session The Shiny session
                              #' @param ns The parent's namespace
                              server = function(input, output, session, ns){

                                super$server(input, output, session, ns)
                                nodeFunctionServer(input, output, session, self, private$.ui_editable$fun)
                                nodeOutputTableServer(input, output, session, self)
                                nodeVisualizationServer(input, output, session, self)

                              },
                              #' set_ui_editable
                              #' @description Set which uis are editable. Only the status, function, and description
                              #' can be set
                              #' @param status Logical whether the status can be edited
                              #' @param fun Logical whether the function can be edited
                              #' @param description Logical whether the description can be edited
                              set_ui_editable = function(status = TRUE, fun = TRUE, description = TRUE){
                                super$set_ui_editable(status, description)
                                private$.ui_editable$fun <- fun
                                invisible(self)
                              },
                              #' set_output_filename
                              #' @description Store the name of the output file
                              set_output_filename = function(){
                                private$.outputFile <- self$id
                              }
                            ),
                            active = list(
                              #' output
                              #' @field output Get or set the output of this node
                              output = function(value){
                                if (missing(value))
                                  return (private$.output)

                                private$.output <- value
                                invisible (self)
                              },

                              #' input_ids
                              #' @field input_ids Get or set input_ids
                              input_ids = function(node_ids){
                                if (missing(node_ids))
                                  return (private$.input_ids)
                                if (!is_Set(node_ids))
                                  stop("node_ids must be a Set.")
                                private$.input_ids <- node_ids
                                invisible(self)
                              },
                              #' to_node
                              #' @field to_node Get or set to_node
                              to_node = function(node_ids){
                                if (missing(node_ids))
                                  return (private$.to_node)
                                if (!is_Set(node_ids))
                                  stop("node_ids must be a Set.")
                                private$.to_node <- node_ids
                                invisible(self)
                              },
                              #' to_node_arg
                              #' @field to_node_arg Get or set to_node_arg
                              to_node_arg = function(arg){
                                if (missing(arg))
                                  return (private$.to_node_arg)
                                if (!is.character(arg))
                                  stop("to_node_arg must be a character.")
                                private$.to_node_arg <- arg
                                invisible(self)
                              },

                              #' fn
                              #' @field fn Return the function object of this node
                              fn = function(fn){
                                if (missing(fn))
                                  return (private$.fn)
                                private$.fn <- fn
                                invisible(self)
                              },
                              #' visualization
                              #' @field visualization Get or set the custom visualization code
                              visualization = function(fn){
                                if (missing(fn))
                                  return (private$.visualization)

                                if (!is.function(fn))
                                  if (!is.character(fn))
                                    if (!is.null(fn))
                                      stop(paste0("For Node id ", id, ", the visualization object must be a function, a string, or NULL."))

                                if (is.function(fn))
                                  private$.visualization <- paste(capture.output(fn), collapse = "\n")
                                invisible(self)
                              }
                            ),
                            private = list(
                              # @field .fn A Function object
                              .fn = NULL,
                              # @field .input_ids A Set containing the ids of this node connecting to this node
                              .input_ids = NULL,
                              # @field .to_node A Set of forward connected nodes
                              .to_node = NULL,
                              # @field .to_node_arg A Dictionary, with key being the "to" node and value being the argument
                              .to_node_arg = NULL,
                              # @field .output An output data object
                              .output = NULL,
                              # @field .visualization It holds the code (characters) that creates the custom visualization
                              .visualization = NULL,
                              # @field .outputFile It stores the filename of the output
                              .outputFile = NULL,

                              extract = function(){
                                values <- super$extract()
                                values <- append(values,
                                  list(fn = private$objExtract(private$.fn),
                                       input_ids = private$objExtract(private$.input_ids),
                                       to_node = private$objExtract(private$.to_node),
                                       to_node_arg = private$objExtract(private$.to_node_arg),
                                       outputFile = private$.outputFile,
                                       visualization = private$.visualization
                                  )
                                )
                              },

                              restore = function(values){
                                super$restore(values)
                                private$.fn <- private$objRestore(values$fn)
                                private$.input_ids <- private$objRestore(values$input_ids)
                                private$.to_node <- private$objRestore(values$to_node)
                                private$.to_node_arg <- private$objRestore(values$to_node_arg)
                                private$.outputFile <- values$outputFile
                                private$.visualization <- values$visualization

                                private$.output <- readOutput(private$.outputFile)
                              }
                            )
)

#' is_node
#' @description Check if an object is a Process_Node
#' @param obj An object
#' @return logical
#' @export
is_Process_Node <- function(obj){
  return (inherits(obj, "Process_Node"))
}

