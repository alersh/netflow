#' R6 implementation of Function
#' @description
#' A Function class
#' @details
#' The Function object stores the function, the arguments, the parameter, and the methods
#' used to process the function.
#' @export
Function <- R6::R6Class("Function",
                        inherit = Serializable,
                        public = list(

                          #' @description Specify the node's function, arguments, options, and argument types. If options is NULL, node_function
                          #' will try to extract the options from this function given the input arguments.
                          #' @param fn Function of this node
                          #' @param ns Namespace of the function
                          #' @param input_args A vector. The argument(s) that receives the input(s).
                          #' @param data_types A list. Data types of arguments  (i.e. vector, array, matrix, data.frame, list, character, logical, formula, function).
                          #' The keys are the names of the arguments of the function and their values are the data types.
                          #' @param source The source files that are to be included
                          #' @param default Logical. If TRUE, then no data fields are initialized.
                          #' @return A Function object
                          initialize = function(fn = NULL, ns = NULL, input_args = NULL, data_types = NULL, source = NULL, default = FALSE){
                            if (!default){
                              if (!is.null(fn)){
                               private$.fn <- fn
                              }
                              if (!is.null(ns)){
                                private$.ns <- ns
                              }
                              private$.init_input_args(input_args)
                              # add optional arguments, excluding the input arguments, into options
                              if (is.null(private$.ns)){
                                private$.init_options(arguments(private$.fn, exclude = input_args))
                              } else{
                                private$.init_options(arguments(get(private$.fn, asNamespace(ns)), exclude = input_args))
                              }
                              private$.init_data_types(data_types)
                            }
                          },

                          #' @description Turn the function into a character string so that it can be evaluated by eval
                          #' @param s Character. The string of the arguments
                          #' @return The string of the function
                          make_function_string = function(s){
                            # check which argument is a character, and add quotes to it

                            if (!is.null(private$.ns)){
                              return (parse(text = paste0(private$.ns, "::`",  private$.fn, "`(", s, ")")))
                            }
                            return (parse(text = paste0("`", private$.fn, "`(", s, ")")))
                          },

                          #' @description Convert a list of options into a string. To be used in do.call.
                          #' @return A string
                          options_to_string = function(){
                            a <- self$options
                            types <- self$data_types
                            set_options <- NULL
                            for (o in names(a)){
                              if (!is.character(a[[o]]))
                                a[[o]] <- deparse(a[[o]])
                              if (is.null(set_options)){
                                if (o == '...'){
                                  set_options <- a[[o]]
                                }
                                else {
                                  if (!is.null(types)){
                                    if (!is.null(types[[o]])){
                                      if (types[[o]] == 'character'){
                                        a[[o]] <- gsub("'", "", a[[o]])
                                        a[[o]] <- paste0("'", a[[o]], "'")
                                      }
                                    }
                                  }
                                  set_options <- paste(o, a[[o]], sep = "=")
                                }

                              }
                              else{
                                if (o == '...'){
                                  set_options <- paste(set_options, a[[o]], sep = ",")
                                }
                                else {
                                  if (!is.null(types)){
                                    if (!is.null(types[[o]])){
                                      if (types[[o]] == 'character'){
                                        a[[o]] <- gsub("'", "", a[[o]])
                                        a[[o]] <- paste0("'", a[[o]], "'")
                                      }
                                    }
                                  }
                                  set_options <- paste(set_options,
                                                       paste(o, a[[o]], sep = "="), sep = ",")

                                }
                              }

                            }

                            return (set_options)
                          },

                          #' @description Set the value of an option
                          #' @param key Character. The name of the function argument already in the options
                          #' @param value An object that is acceptable to the argument as specified in key.
                          set_option = function(key, value){
                            if (!key %in% names(private$.options))
                              stop(paste("The option", key, "is not an optional argument for function", private$.fn))
                            private$.options[[key]] <- value
                            invisible(self)
                          },

                          #' @description Add a new input argument so that it can be used as a socket for an output from an
                          #' input node. This new input argument will automatically be removed from the function options.
                          #' @param key Character. The name of the function argument to be added.
                          add_arg = function(key){
                            if (!key %in% names(private$.options))
                              stop(paste("The option", key, "is not an argument for function", private$.fn))
                            private$.args <- c(private$.args, key)
                            private$.options[[key]] <- NULL

                            invisible(self)
                          },

                          #' @description Remove an input argument so that it can no longer be used as socket for an output
                          #' from an input node. The removed argument will be automatically added back to the  function options
                          #' @param key Character. The name of the input argument to be removed.
                          remove_arg = function(key){
                            idx <- grep(key, private$.args)
                            private$.args <- private$.args[-idx]
                            private$.options[[key]] <- formals(get(private$.fn, asNamespace(private$.ns)))[[key]]

                            invisible(self)
                          },

                          #' @description Set the data/object type of an argument.
                          #' @param key Character. The name of the argument.
                          #' @param data_type Character. The name of the data type.
                          set_data_type = function(key, data_type){
                            if (!key %in% names(private$.data_types)){
                              stop(paste("The argument", key, " is not an argument for function", private$.fn))
                            }
                            private$.data_types[[key]] <- data_type
                            invisible(self)
                          }
                        ),
                        active = list(
                          #' @field fn Getter and setter for fn
                          fn = function(fn){
                            if (missing(fn))
                              return (private$.fn)
                            private$.fn <- fn
                            invisible(self)
                          },
                          #' @field ns Getter and setter for ns.
                          ns = function(ns){
                            if (missing(ns))
                              return (private$.ns)
                            private$.ns <- ns
                            invisible(self)
                          },
                          #' @field args Getter and setter for the input arguments
                          args = function(arguments){
                            if (missing(arguments))
                              return (private$.args)
                            private$.args <- args
                            invisible(self)
                          },
                          #' @field options Getter and setter for the list of options and their parameters
                          options = function(arguments){
                            if (missing(arguments))
                              return (private$.options)
                            private$.options <- arguments
                            invisible(self)
                          },
                          #' @field data_types Getter and setter data types of the arguments
                          data_types = function(data_types){
                            if (missing(data_types))
                              return (private$.data_types)
                            stop('data_types is read-only.')
                          }

                        ),
                        private = list(
                            # @field .fn The function name
                            .fn = NULL,
                            # @field .args A vector of the argument names of the function
                            .args = NULL,
                            # @field .options A list, with the names being the argument names and the values being the parameters
                            .options = NULL,
                            # @field .ns The name of the namespace
                            .ns = NULL,
                            # @field .data_types A list with keys being the function arguments and the values being the data types
                            .data_types = NULL,

                            # @description Add input arguments
                            # @param arguments A vector of argument names
                            .init_input_args = function(arguments){
                              if (!is.null(arguments) && !is.vector(arguments))
                                stop("The arguments must be a vector.")

                              # check if the arguments are defined in this function
                              if (is.null(private$.fn))
                                stop("The function has not been defined.")

                              private$.args <- arguments
                              invisible()
                            },

                            # @description Add a list of options
                            # @param arguments A list of options with parameters
                            .init_options = function(arguments){
                              if (is.null(private$.fn))
                                stop("The function has not been defined.")

                              if (!is.list(arguments)){
                                if (!is.null(arguments))
                                  stop("The arguments must be a list.")
                              }
                              # check if the arguments are defined in this function
                              fn <- private$.fn
                              if (!is.null(private$.ns))
                                fn <- paste0(private$.ns, "::", fn)
                              if (!all(names(arguments) %in% names(formals(eval(parse(text = fn)))))){
                                stop(paste("Not all arguments are found in function", private$.fn))
                              }
                              # deparse argument if it is a character or a call
                              if (!is.null(arguments)){
                                for (i in seq_along(arguments)){
                                  if (is.character(arguments[[i]]) || is.call(arguments[[i]]))
                                    arguments[[i]] <- deparse(arguments[[i]])
                                }

                                private$.options <- arguments
                              } else{
                                private$.options <- NULL
                              }

                            },

                            # @description Define data types of the arguments
                            # @param data_types A list of arguments with their defined data types
                            .init_data_types = function(data_types){
                              np <- c(private$.args, names(private$.options))
                              n <- names(data_types)
                              for (i in seq_along(np)){
                                if (np[i] %in% n)
                                  private$.data_types[[np[i]]] <- data_types[[np[i]]]
                                else
                                  private$.data_types[[np[i]]] <- ""
                              }
                            },

                            extract = function(){
                              values <- list(
                                fn = private$.fn,
                                args = private$.args,
                                options = private$.options,
                                ns = private$.ns,
                                data_types = private$.data_types
                              )

                            },

                            restore = function(values){
                              private$.fn <- values$fn
                              private$.args <- values$args
                              private$.options <- values$options
                              private$.ns <- values$ns
                              private$.data_types <- values$data_types

                            }
                        )
)

#' is_Function
#' @description Check if the object belongs to the class Function
#' @param obj The object to be checked
#' @return Logical
#' @export
is_Function <- function(obj){
  return (inherits(obj, "Function"))
}

#' @title arguments
#' @description Extract arguments from the function.
#' @param fn The function from which the arguments are to be extracted
#' @param exclude Arguments to be removed from the extraction
#' @return A list of arguments of the function
#' @export
arguments <- function(fn, exclude = NULL){
    if (is.primitive(fn)){
      arguments <- as.list(formals(args(fn)))
    }
    else{
      arguments <- as.list(formals(fn))
    }
    for (n in exclude){
      arguments[[n]] <- NULL
    }

    return (arguments)
}

#' Parse a function call
#' @description Parse a function call.
#' @param s A string of function call.
#' @return A list that contains the namespace of the function, the function name,
#' the socket which the input is plugged into, and all the options that have been
#' specified in this function call.
#' @export
parse_function <- function(s){
  l = list(namespace = NULL, fn = NULL, socket = NULL, options = NULL)
  ns_fn = strsplit(s, split = "::")[[1]]
  fn_part <- NULL
  if (length(ns_fn) == 2) {
    l$namespace = ns_fn[1]
    fn_part = ns_fn[2]
  } else{
    fn_part <- ns_fn[1]
  }

  # extract the function and the argument parts. Since the argument inputs can contain (),
  # we cannot use strsplit to do this
  idx_first <- unlist(gregexpr("[(]", fn_part))[1] # if there are multiple (), then we choose the first (
  idx_last <- tail(unlist(gregexpr("[)]", fn_part)), 1)
  fn_s <- substring(fn_part, 1, idx_first - 1)
  # if () are next to each other, then the option part is ""
  fn_o <- ""
  if (idx_first != idx_last - 1) fn_o <- substring(fn_part, idx_first + 1, idx_last - 1)
  fn_options <- c(fn_s, fn_o)
  l$fn <- fn_options[1]

  # get the built-in arguments of the function
  if (!is.null(l$namespace))
    fn_args <- arguments(eval(parse(text = paste0(l$namespace, "::", l$fn))))
  else
    fn_args <- arguments(eval(parse(text = l$fn)))

  arg_list <- list()

  if (fn_options[2] != ""){
    out <- extract_input_item(fn_options[2], fn_args, arg_list)
    l$socket <- out$socket
    l$options <- out$arg_list
  }else{
    # Needs to deal with one argument, which could also be ...
    if (length(fn_args) == 1)
      l$socket <- names(fn_args)[1]
  }

  return (l)
}

#' Extract the items from the function call.
#' @description A helper function for the parse_function. Extract individual items
#' from the function call. The argument names
#' should be specified for their inputs, although even without the argument names this
#' function can determine which argument name an input item belongs by relying on the
#' order and location the input occurs. The first argument is considered the socket unless
#' a dot is used in another argument. That argument becomes the socket. A name in an input
#' that is followed by = can either be the argument name or part of the input. If the name is
#' not in the function call, then it is assumed to be part of the elipses ....
#' @param code_string The string that contains the function call
#' @param fn_args The arguments that are available for this function
#' @param arg_list The argument list that the extracted items are to be stored.
#' @return The arg_list
#' @export
extract_input_item <- function(code_string, fn_args, arg_list){
  s_length <- nchar(code_string)
  open_bracket <- 0
  item_count <- 1
  arg_name_key <- NULL
  value_start_idx <- NULL
  start_idx <- 1
  ellipses_list <- c()
  socket <- NULL
  found_dot <- FALSE

  for (idx in seq(s_length)){
    s <- substring(code_string, idx, idx)
    if (idx == s_length){
      # we've come to the end
      if (!is.null(arg_name_key)){
        # the name_key has a name, that means this input value is associated with a function argument
        val <- substring(code_string, value_start_idx, idx)

        # if the value is ., then this argument is the socket
        if (trimws(val) == "."){
          socket <- arg_name_key
          found_dot <- TRUE
        }
        else{
          arg_list[[arg_name_key]] <- val
        }
        arg_name_key <- NULL

      } else{
        # check if it is a dot
        # check which input argument it is in, and use fn_args to determine what argument this is.
        val <- substring(code_string, start_idx, idx)
        if (trimws(val) == ".")
          socket <- names(fn_args)[item_count]
        else
          # either the argument name is not a function argument, or there is no name argument.
          # We assume this belongs to ...
          ellipses_list <- append(ellipses_list, substring(code_string, start_idx, idx))
      }

    } else{
      if (s == "("){
        # continue until ")" is found; ignore "," in between
        open_bracket <- open_bracket + 1
      } else if (s == ")"){
        open_bracket <- open_bracket -1
      } else if (s == "="){
        if (open_bracket == 0){
          # then this must be the name argument. Check if it is in the function arguments
          # if it is, store it. Otherwise, it belongs to ...
          arg_name_key <- trimws(substring(code_string, start_idx, idx - 1))
          if (!arg_name_key %in% names(fn_args)){
            arg_name_key <- NULL
          }else{
            # the value must come after =. So we set the value_idx to idx + 1
            value_start_idx <- idx + 1
          }
        }
      } else if (s == ","){
        if (open_bracket == 0){
          # likely this comma separate input arguments
          if (!is.null(arg_name_key)){
            # the name_key has a name, that means this input value is associated with a function argument
            val <- substring(code_string, value_start_idx, idx - 1)

            # if the value is ., then this argument is the socket
            if (trimws(val) == "."){
              socket <- arg_name_key
              found_dot <- TRUE
            }
            else{
              arg_list[[arg_name_key]] <- val
            }
            arg_name_key <- NULL

          } else{
            # check if it is a dot
            # check which input argument it is in, and use fn_args to determine what argument this is.
            val <- substring(code_string, start_idx, idx - 1)
            if (trimws(val) == ".")
              socket <- names(fn_args)[item_count]
            else
              # either the argument name is not a function argument, or there is no name argument.
              # We assume this belongs to ...
              ellipses_list <- append(ellipses_list, val)
          }
          start_idx <- idx + 1
          value_start_idx <- NULL
          item_count <- item_count + 1
        }
      }
    }
  }
  if (!found_dot){
    # the first function argument must be the socket
    socket <- names(fn_args)[1]
  }

  if (length(ellipses_list) > 0)
    arg_list[["..."]] <- paste(ellipses_list, collapse = ",")
  return (list(arg_list = arg_list, socket = socket))
}

#' Parse a quoted expression
#' @description This function parsed an expression into individual function calls. Each
#' function call is further parsed to obtain the namespace of the function, the function,
#' the arguments and their inputs. Individual function calls must be connected by the pipe such as
#' the default %>%. The whole expression must be quoted i.e. quote(expr). The step_ids
#' must be specified for each function call and these ids will become their node ids.
#' @param quoted_expr A quoted expression
#' @param step_ids A vector of node ids corresponding to the function calls.
#' @param pipe The pipe that separates individual function calls. Default is %>%.
#' @return A list of parsed function items.
#' @export
parse_expr <- function(quoted_expr, step_ids, pipe = "%>%"){

  expr <- as.character(substitute(quoted_expr))
  expr <- paste(expr[-1], collapse = expr[1])

  steps <- list()
  i = 1

  b <- strsplit(gsub(" ", "", expr), split = pipe)[[1]]
  for (s in b){
    steps[[step_ids[i]]] <- parse_function(s)
    i = i+ 1
  }

  return (steps)
}
