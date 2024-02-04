#' Set
#'
#' @description
#' A Set stores a set of objects of the same type.
#' @details
#' A set is similar to a vector, except that all objects must be unique.
#' @export
Set <- R6::R6Class("Set",
                   inherit = Serializable,
                   public = list(
                     #' @description Creates a new Set
                     #' @param ... Objects
                     #' @param check_value_fn A function that checks whether the object satisfies certain specified conditions
                     #' @param default Logical. If TRUE, then no data fields are initialized.
                     #' @return A Set object
                     initialize = function(..., check_value_fn = NULL, default = FALSE){
                       if (!default){
                         s <- c(...)
                         if (length(s) > 0){
                          if (!is.null(check_value_fn)){
                           if (!is.function(check_value_fn))
                             stop("check_value must be a function.")
                           private$.check_value_fn <- check_value_fn
                          }
                         }
                         private$.items <- s
                       }
                     },
                     #' @description Add a new object into the Set.
                     #' @param value An Object
                     add = function(value){
                       if (value %in% private$.items)
                         stop("The value already exists in the set.")
                       if (!is.null(private$.check_value_fn))
                         private$.check_value_fn(value)

                       private$.items <- c(private$.items, value)
                       private$.size <- private$.size + 1

                       invisible(self)
                     },
                     #' @description Get an object based on the index
                     #' @param idx Integer. The index of the set.
                     get = function(idx){
                       if (!is.integer(idx))
                         stop("idx must be an integer.")
                       return (private$.items[idx])
                     },
                     #' @description Remove an object from the set.
                     #' @param value The object to be removed
                     remove = function(value){
                       idx <- grep(value, private$.items)
                       if (length(idx) > 0){
                        private$.items <- private$.items[-idx]
                        private$.size <- private$.size - 1
                       }
                       invisible(self)
                     },

                     #' @description Remove an object based upon the index
                     #' @param idx Integer. The index which the object is to be removed
                     remove_at = function(idx){
                       private$.items <- private$.items[-idx]
                       private$.size <- private$.size - 1
                       invisible(self)
                     },

                     #' @description Check if the set contains the object
                     #' @param value The object to be checked
                     #' @return Logical
                     contains = function(value){
                       flag <- FALSE
                       for (k in private$.items){
                         if (value == k){
                           flag <- TRUE
                           break
                         }
                       }
                       return (flag)
                     },

                     #' @description Clear all items in the set.
                     clear = function(){
                       private$.items <- NULL
                       private$.size <- 0
                       invisible(self)
                     },

                     #' @description Check if the set is empty.
                     #' @return Logical
                     is_empty = function(){
                       return (private$.size == 0)
                     }
                   ),
                   active = list(
                     #' @field size Setter for size
                     size = function(value){
                       if (!missing(value))
                         stop("Size is read-only.")
                       return (private$.size)
                     },
                     #' @field items Getter and Setter for the items in the set.
                     items = function(value){
                       if (missing(value)){
                         return (private$.items)
                       }
                       set <- unique(value)
                       if (length(set) < length(value)){
                         warning("Duplicated values are found. Only unique values will be stored.")
                       }

                       if (!is.null(private$.check_value_fn)){
                         for (v in set){
                           private$.check_value_fn(v)
                         }
                       }
                       private$.items <- set
                       private$.size <- length(set)
                       invisible(self)
                     }
                   ),
                   private = list(
                     # @field .items A vector of unique objects
                     .items = NULL,
                     # @field .size The current size of the set
                     .size = 0,
                     # @field .check_value_fn A function that performs the check on an incoming object
                     .check_value_fn = NULL,

                     extract = function(){
                       values <- list(items = private$.items,
                                      size = private$.size,
                                      check_value_fn = private$.check_value_fn
                       )

                     },

                     restore = function(values){
                       private$.items <- values$items
                       private$.size <- values$size
                       private$.check_value_fn <- values$check_value_fn
                     }
                   )
)

#' is_Set
#' @description Checks if the object belongs to the class Set
#' @param obj The object to be checked
#' @return Logical
is_Set <- function(obj){
  return (inherits(obj, "Set"))
}
