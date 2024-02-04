#' R6 Class representing a node
#'
#' @description
#' A node is the basic unit of a network.
#' @details
#' A node contains an ID, the name of the author, the description, and the display style
#' @export
Node <- R6::R6Class("Node",
                    inherit = Serializable,
                    public = list(

                      #' initialize
                      #' @description Create a node
                      #' @param id Character. The id name of this node
                      #' @param style A Node_Style object that specifies the appearance of the node
                      #' @param author Character. The name of the author.
                      #' @param description The description of this node.
                      #' @param default Logical. If TRUE, then no data fields are initialized.
                      #' @return the Node object
                      initialize = function(id, style = NULL,  author = "", description = "", default = FALSE){

                        if (!default){
                          if (missing(id))
                            stop("A node requires an id.")
                          private$.id <- id
                          self$set_ui_editable()
                          if (is.null(style)){
                            style <- Node_Style$new(label = private$.id)
                          }
                          private$.style <- style
                          private$.description <- description
                          private$.author <- author
                          self$set_ui_editable()
                        }
                      },

                      #' lock_to_string
                      #' @description Return 'lock' if the node is locked, 'unlock' otherwise
                      #' @param lock A logical
                      #' @return A Character string
                      lock_to_string = function(){
                        if (private$.lock){
                          return ("Locked")
                        }
                        return ("Not locked")
                      },

                      #' is_locked
                      #' @description Get the current state of the lock
                      #' @return A logical. TRUE if it is locked; FALSE otherwise
                      is_locked = function(){
                        return (private$.lock)
                      },
                      #' set_lock
                      #' @description Lock or unlock this node
                      #' @param value logical, whether to lock or unlock; default to TRUE
                      set_lock = function(value = TRUE){
                        self$lock <- value
                        invisible(self)
                      },
                      #' reset_status
                      #' @description Reset the status of the node to 'idle'. It will empty out
                      #' the error message and warning message.
                      reset_status = function(){
                        if (!self$is_locked()){
                          self$set_status("idle")
                          private$.errormsg <- ""
                          private$.warningmsg <- ""
                        }
                      },
                      #' set_status
                      #' @description Set the current status of the network
                      #' @param value Character. The status descriptor can be "idle", "running",
                      #' "warning", "fail", "success"
                      set_status = function(value = c("idle", "running", "warning", "fail", "success")){
                        value <- match.arg(value)
                        private$.status <- value
                        private$.style$set_status(value)
                        invisible(self)
                      },
                      #' status_fail
                      #' @description Check if the current status is a fail
                      #' @return A logical
                      status_fail = function(){
                        return (private$.status == "fail")
                      },
                      #' status_running
                      #' @description Check if the current status is running
                      #' @return A logical
                      status_running = function(){
                        return (private$.status == "running")
                      },
                      #' status_succeed
                      #' @description Check if the current status is a success
                      #' @return A logical
                      status_succeed = function(){
                        return (private$.status == "success")
                      },
                      #' status_idle
                      #' @description Check if the current status is in idle
                      #' @return A logical
                      status_idle = function(){
                        return (private$.status == "idle")
                      },
                      #' set_status_fail
                      #' @description Set the current status to fail
                      set_status_fail = function(){
                        private$.status == "fail"
                      },
                      #' set_status_running
                      #' @description Set the current status to running
                      set_status_running = function(){
                        private$.status = "running"
                      },
                      #' set_status_succeed
                      #' @description Set the current status to success
                      set_status_succeed = function(){
                        private$.status = "success"
                      },
                      #' set_status_idle
                      #' @description Set the current status to idle
                      set_status_idle = function(){
                        private$.status = "idle"
                      },

                      #' ui
                      #' @description UI
                      #' @importFrom shiny NS
                      #' @param ns The parent's namespace
                      ui = function(ns){
                        ns_node <- NS(ns(self$id))
                        nodeInfoUI(ns,
                                   nodeStatusUI(ns_node),
                                   nodeDescriptionUI(ns_node)
                                   )
                      },

                      #' @description The node server
                      #' @importFrom shiny showModal modalDialog
                      #' @param input The shiny input
                      #' @param output The shiny output
                      #' @param session The shiny session
                      #' @param ns The parent's namespace
                      server = function(input, output, session, ns){
                        node_ns <- session$ns

                        nodeStatusServer(input, output, session, self,  private$.ui_editable$status, ns)
                        nodeDescriptionServer(input, output, session, self, private$.ui_editable$description)

                        shiny::showModal(
                          shiny::modalDialog(title = self$id,
                                      size = 'l',
                                      self$ui(node_ns)
                          )
                        )
                      },
                      #' call
                      #' @description Call the module
                      #' @importFrom shiny callModule
                      #' @param input The shiny input
                      #' @param output The shiny output
                      #' @param session The shiny session
                      call = function(input, output, session){
                        shiny::callModule(self$server, self$id, session$ns)
                      },

                      #' @description Set which uis are editable
                      #' @param status Whether the "status" panel of the node is editable
                      #' @param description Whether the "description" panel of the node is editable
                      set_ui_editable = function(status = TRUE, description = TRUE){
                        private$.ui_editable$status <- status
                        private$.ui_editable$description <- description
                        invisible(self)
                      }
                    ),
                    active = list(
                      #' @field id Get or set the name that identifies this node
                      id = function(id){
                        if (missing(id))
                          return (private$.id)
                        private$.id <- id
                        invisible(self)
                      },

                      #' @field network_id Get or set the network_id this node belongs to
                      network_id = function(network_id){
                        if (missing(network_id))
                          return (private$.network_id)
                        private$.network_id <- network_id
                        private$.style$group <- network_id
                        invisible(self)
                      },
                      #' @field description Get or set the description of this node
                      description = function(description){
                        if (missing(description))
                          return (private$.description)
                        private$.description <- description
                        invisible(self)
                      },

                      #' @field author Get or set the name of the author
                      author = function(author){
                        if (missing(author))
                          return (private$.author)
                        private$.author <- author
                        invisible(self)
                      },
                      #' @field lock Lock or unlock this node
                      lock = function(value = TRUE){
                        if (missing(value))
                          return (private$.lock)
                        if (!is.logical(value))
                          stop("Argument must be a logical.")
                        private$.lock <- value
                        private$.style$lock <- value

                        invisible(self)
                      },
                      #' @field style Return the Node_Style object of this node
                      style = function(){
                        return (private$.style)
                      },

                      #' @field status Get the current status of the network
                      status = function(){
                        return (private$.status)

                      },
                      #' @field error_message Get the error message
                      error_message = function(){
                        return (private$.errormsg)
                      },

                      #' @field warning_message Get the warning message
                      warning_message = function(){
                        return (private$.warningmsg)

                      }
                    ),
                    private = list(
                      # @field .id The name that identifies this node
                      .id = NULL,
                      # @field .description The description of this node
                      .description = NULL,
                      # @field .network_id The network_id this node belongs to
                      .network_id = NULL,
                      # @field .author The name of the author
                      .author = NULL,
                      # @field .status The node status
                      .status = 'idle',
                      # @field .lock Logical. Whether the node is locked or not.
                      .lock = FALSE,
                      # @field .errormsg It holds the error message that occurs during the processing of this node
                      .errormsg = "",
                      # @field .warningmsg It holds the warning message that occurs  during the processing of this node
                      .warningmsg = "",
                      # @field .style It holds the Node_Style object
                      .style = NULL,
                      # @field .ui_editable  A list that sets whether the ui components are editable
                      .ui_editable = NULL,

                      extract = function(){
                        values <- list(id = private$.id,
                                       description = private$.description,
                                       network_id = private$.network_id,
                                       author = private$.author,
                                       status = private$.status,
                                       lock = private$.lock,
                                       errormsg = private$.errormsg,
                                       warningmsg = private$.warningmsg,
                                       style = private$.style,
                                       ui_editable = private$.ui_editable)

                      },

                      restore = function(values){
                        private$.id <- values$id
                        private$.description <- values$description
                        private$.network_id <- values$network_id
                        private$.author <- values$author
                        private$.status <- values$status
                        private$.lock <- values$lock
                        private$.errormsg <- values$errormsg
                        private$.warningmsg <- values$warningmsg
                        private$.style <- values$style
                        private$.ui_editable <- values$ui_editable

                      }
                    )
)

#'
#' is_node
#' @description Check if an object is a node
#' @return Logical
#' @export
is_Node <- function(obj){
  return (inherits(obj, Node$classname))
}




