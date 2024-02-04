#' R6 Class representing a Serializable class
#'
#' @description
#' A Serializable is the parent of a R6 inheritance tree.
#' @details
#' A Serializable contains methods that serialize the objects and restore the byte codes back into R objects.
#' @export
Serializable <- R6::R6Class("Serializable",
                            public = list(

                              #' serialize
                              #' @description Serialize this object. Only the data fields are serialized.
                              #' @return A raw object.
                              serialize = function(){
                                private$.serialize()
                              },
                              #' unserialize
                              #' @description Unserialize the raw data and restore the data fields.
                              #' @param s A raw serialized object
                              unserialize = function(s){
                                private$.unserialize(s)
                                invisible(self)
                              }
                            ),
                            private = list(

                              is_R6list = function(obj){
                                return (inherits(obj, "R6list"))
                              },

                              as_R6list = function(obj){
                                class(obj) <- append("R6list", class(obj))
                                return (obj)
                              },

                              # extract: An interface required to be implemented by the user
                              extract = function(){ stop("The 'extract' method must be implemented by the user.") },
                              # restore: An interface required to be implemented by the user
                              restore = function(values){ stop("The 'restore' method must be implemented by the user.") },

                              # objExtract: Extract the data from the R6 object obj
                              objExtract = function(obj){
                                o <- NULL
                                if (!is.null(obj) && inherits(obj, "R6")){
                                  o <- list(class = class(obj)[1],
                                            data = obj$serialize())
                                  o <- private$as_R6list(o)
                                }
                                return (o)
                              },
                              # objRestore: Restore the data fields from this R6list object
                              objRestore = function(l){
                                if (is.null(l)){
                                  return (l)
                                }
                                obj <- eval(parse(text = paste0(l$class, "$new(default = TRUE)")))
                                obj$unserialize(l$data)
                                return (obj)
                              },

                              .serialize = function(){

                                s <- serialize(private,
                                               connection = NULL,
                                               refhook = function(obj){
                                                 e <- obj$extract()
                                                 if (!is.raw(e)){
                                                   return (deparse(substitute(e)))
                                                 }
                                               })
                                return (s)

                              },

                              .unserialize = function(conn){
                                values <- unserialize(conn,
                                                      refhook = function(obj){
                                                        private$restore(eval(parse(text = obj)))
                                                      })
                                if (!is.null(values) && private$is_R6list(values)){
                                  obj <- private$objRestore(values)
                                }
                                return (obj)
                              }
                            )
)
