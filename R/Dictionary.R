#' Dictionary
#'
#' @description
#' A dictionary stores objects with keys.
#' @details
#' A dictionary is similar to a list, except that all keys must be unique.
#' @export
Dictionary <- R6::R6Class("Dictionary",
                          inherit = Serializable,
                          public = list(
                            #' @description Create a new dictionary. All keys must be unique.
                            #' @param ... key - value inputs
                            #' @param check_key_fn An optional function that provides a check of the key value
                            #' @param check_value_fn An optional function that provides a check of the input values
                            #' @param read_only Logical whether the dictionary is read only.
                            #' @param default Logical. If TRUE, then no data fields are initialized.
                            initialize = function(..., check_key_fn = NULL, check_value_fn = NULL, read_only = FALSE, default = FALSE){
                              if (!default){
                                d <- list(...)

                                if (length(d) > 0){
                                  if (is.list(d[[1]]))
                                    d <- d[[1]]

                                  if (!is.null(check_value_fn)){
                                    if (!is.function(check_value_fn))
                                      stop("check_value_fn must be a function.")
                                    private$.check_value_fn <- check_value_fn
                                  }
                                  if (!is.null(check_key_fn)){
                                    if (!is.function(check_key_fn))
                                      stop("check_key_fn must be a function.")
                                    private$.check_key_fn <- check_key_fn
                                  }

                                }
                                private$.read_only <- read_only
                                private$.items <- d
                              }
                            },
                            #' @description Get the value by the key. The key can be a character or an index number
                            #' @param key A character to access the value by the name, or a number to access the value by the index value
                            #' @return The value
                            get = function(key){
                              return (private$.items[[key]])
                            },
                            #' @description Remove the key-value in the dictionary.
                            #' @param key Character. The name of the key to be removed.
                            remove = function(key){
                              if (private$.read_only)
                                stop("The dictionary is read-only.")
                              if (!is.character(key))
                                stop("The key is not a character")
                              if (!self$contains_key(key))
                                stop("The key cannot be found in the dictionary.")
                              private$.items[[key]] <- NULL
                              private$.length <- private$.length - 1L
                              invisible(self)
                            },
                            #' @description Add a key-value in the dictionary
                            #' @param key Character. The name of the key.
                            #' @param value An object that is associated with the key
                            add_value_with_key = function(key, value){
                              if (private$.read_only)
                                stop("The dictionary is read-only.")
                              if (!is.character(key))
                                stop("The key is not a character.")
                              if (self$contains_key(key))
                                warning("The key already exists in the dictionary.")
                              if (!is.null(private$.check_value_fn))
                                private$.check_value_fn(value)
                              if (!is.null(private$.check_key_fn))
                                private$.check_key_fn(key)

                              private$.items[[key]] <- value
                              private$.length <- private$.length + 1L

                              invisible(self)
                            },
                            #' @description Set a value based on the key already in the dictionary.
                            #' @param key Character. The name of the key that is already in the dictionary.
                            #' @param value An object to be stored in the dictionary.
                            set_value_with_key = function(key, value){
                              if (private$.read_only){
                                stop("The dictionary is read-only.")
                              }
                              if (!is.character(key))
                                stop("The key is not a character.")
                              if (!self$contains_key(key))
                                stop("The key cannot be found in the dictionary.")
                              private$.items[[key]] <- value
                              invisible(self)
                            },
                            #' @description Remove all key-value items from the dictionary.
                            clear = function(){
                              private$.items <- list()
                              private$.length <- 0L
                              invisible(self)
                            },
                            #' @description Get all the keys containing a specific value
                            #' @param value An object
                            #' @return A vector
                            get_keys_with_value = function(value){
                              keys <- c()
                              n <- self$keys
                              for (k in seq_len(private$.length)){
                                if (private$.items[[k]] == value)
                                  keys <- c(keys, n[k])
                              }
                              return (keys)
                            },
                            #' @description Check if the dictionary is empty or not
                            #' @return Logical
                            is_empty = function(){
                              return (private$.length == 0L)
                            },
                            #' @description Check if the dictionary contains the key.
                            #' @param key Character. The name of the key.
                            #' @return Logical
                            contains_key = function(key){
                              if (!is.character(key))
                                stop("The key must be a character.")
                              return (key %in% self$keys)
                            }
                          ),
                          active = list(
                            #' @field items Get or set the entire dictionary. To enable value check, the check_value
                            #' function must have been set via either the initialization or active binding before setting
                            #' this list into the dictionary.
                            items = function(value){
                              if (missing(value))
                                return (private$.items)
                              if (!is.list(value))
                                stop("The value must be a list.")

                              if (is.null(names(value)) || any(names(value) == ""))
                                stop("One or more keys are missing.")

                              if (any(duplicated(names(value))))
                                stop("Duplicated key error.")

                              if (!is.null(private$.check_value_fn)){
                                for (v in value){
                                  private$.check_value_fn(v)
                                }
                              }
                              if (!is.null(private$.check_key_fn)){
                                for (k in names(value)){
                                  private$.check_key_fn(k)
                                }
                              }
                              private$.items <- value
                              private$.length <- length(value)
                              invisible(self)
                            },
                            #' @field check_value_fn Set the check value function
                            check_value_fn = function(fn){
                              if (missing(fn))
                                stop("check_value_fn is set only.")
                              if (!is.function(fn))
                                stop("check_value_fn requires a function with a single argument.")
                              if (length(formals(fn)) > 1L)
                                stop("Number of arguments must be 1.")
                              private$.check_value_fn <- fn
                              invisible(self)
                            },
                            #' @field check_key_fn Set the check key function
                            check_key_fn = function(fn){
                              if (missing(fn))
                                stop("check_key_fn is set only.")
                              if (!is.function(fn))
                                stop("check_key_fn requires a function with a single argument.")
                              if (length(formals(fn)) > 1L)
                                stop("Number of arguments must be 1.")
                              private$.check_key_fn <- fn
                              invisible(self)
                            },
                            #' @field length Get the length of the dictionary
                            length = function(value){
                              if (!missing(value))
                                stop("length is read-only.")
                              return (private$.length)
                            },
                            #'
                            #' @field keys Get all the keys in the dictionary
                            keys = function(value){
                              if (!missing(value))
                                stop("keys is read-only.")
                              return (names(private$.items))
                            },
                            #' @field values Get all the values in the dictionary
                            values = function(value){
                              if (!missing(value))
                                stop("values is read-only.")
                              v <- array(NA, dim = self$length)
                              for (i in seq_along(private$.items)){
                                v[i] <- private$.items[[i]]
                              }
                              return (v)
                            }


                          ),
                          private = list(
                            # @field .items A list values with names being the keys
                            .items = NULL,
                            # @field .length The length of the dictionary
                            .length = 0L,
                            # @field .check_value_fn The check value function
                            .check_value_fn = NULL,
                            # @field .check_key_fn The check key function
                            .check_key_fn = NULL,
                            # @field .read_only Logical
                            .read_only = FALSE,

                            extract = function(){
                              items_extract <- list()
                              obj_type <- list()
                              for (n in names(private$.items)){
                                if (inherits(private$.items[[n]], "R6")){
                                  items_extract[[n]] <- private$objExtract(private$.items[[n]])
                                  obj_type[[n]] <- "R6"
                                }
                                else{
                                  items_extract[[n]] <- private$.items[[n]]
                                  obj_type[[n]] <- typeof(private$.items[[n]])
                                }
                              }
                              values <- list(items = items_extract,
                                             items_type = obj_type,
                                             length = private$.length,
                                             check_value_fn = private$.check_value_fn,
                                             check_key_fn = private$.check_key_fn,
                                             read_only = private$.read_only
                              )

                            },

                            restore = function(values){
                              items_restore <- list()
                              for (n in names(values$items)){
                                if (values$items_type[[n]] == "R6")
                                  items_restore[[n]] <- private$objRestore(values$items[[n]])
                                else
                                  items_restore[[n]] <- values$items[[n]]
                              }
                              private$.items <- items_restore
                              private$.length <- values$length
                              private$.check_value_fn <- values$check_value_fn
                              private$.check_key_fn <- values$check_key_fn
                              private$.read_only <- values$read_only

                            }
                          )

)

#' is_Dictionary
#' @description Check if the object belongs to the class Dictionary.
#' @param obj The object to be checked
#' @return Logical
#' @export
is_Dictionary <- function(obj){
  return (inherits(obj, "Dictionary"))
}
