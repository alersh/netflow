#' @title fileIO
#' @description load or save network#'
#' @importFrom shiny observeEvent showModal removeModal modalDialog fluidRow column br
#' @importFrom shinyFiles parseSavePath parseFilePaths shinyFilesButton shinySaveButton shinyFileSave
#' shinyFileChoose
fileIO <- function(input, output, session, ns, network){
  observeEvent(input$CancelLoadSaveButton, {
    removeModal()
  })

  observe({
    shinyFileSave(input, "SaveFile", roots = c(wd = getwd()), filetypes = 'RDS', session = session)
    f <- parseSavePath(roots = c(wd = getwd()), input[['SaveFile']])

    if (nrow(f) > 0){
      writeR6(network, f$datapath)
    }
  })

  observeEvent(input[['LoadFile']], {

    f <- parseFilePaths(roots = c(wd = getwd()), input[['LoadFile']])
    if (nrow(f) > 0){
      network <- readR6(Netflow$new(), as.character(f$datapath))
      if (exists('network')){
          network$build()$build_graph()
      }
    }
  })

  observeEvent(input$NetworkMenuButton, {
    showModal(
      modalDialog(
        br(),
        fluidRow(
          column(6,
                 shinyFilesButton(ns('LoadFile'),
                                  label = 'Load Network',
                                  title = "Please select a .RDS file",
                                  multiple = F)
          ),
          column(6,
                 shinySaveButton(ns('SaveFile'),
                                 label = 'Save Network',
                                 title = "Save Network as ...",
                                 filetype = list(rds = "RDS"))
          )
        )
      )
    )
  })

  shinyFileSave(input, "SaveFile", roots = c(wd = getwd()), filetypes = 'RDS', session = session)
  shinyFileChoose(input, 'LoadFile', roots = c(wd = getwd()), filetypes = 'RDS', session = session)
}

fileIONet <- function(input, output, session, ns, network){
  observeEvent(input$CancelLoadSaveButton, {
    removeModal()
  })

  observeEvent(input[['SaveFile']], {
    f <- parseFilePaths(roots = c(wd = getwd()), input[['SaveFile']])
    if (!is.null(f)){
      writeR6(network, f$datapath)
    }
  })

  observeEvent(input[['LoadFile']], {

    f <- parseFilePaths(roots = c(wd = getwd()), input[['LoadFile']])
    if (nrow(f) > 0){
      network <- readR6(Network$new(), as.character(f$datapath))
      if (exists('network')){
        network$build()$build_graph()
      }
    }
  })

  observeEvent(input$NetworkMenuButton, {
    showModal(
      modalDialog(
        br(),
        fluidRow(
          column(6,
                 shinyFilesButton(ns('LoadFile'),
                                  label = 'Load Network',
                                  title = "Please select a .RDS file",
                                  multiple = F)
          ),
          column(6,
                 shinySaveButton(ns('SaveFile'),
                                 label = 'Save Network',
                                 title = "Save Network as ...",
                                 filetype = list(rds = "RDS"))
          )
        )
      )
    )
  })


  shinyFileSave(input, "SaveFile", roots = c(wd = getwd()), filetypes = 'RDS', session = session)
  shinyFileChoose(input, 'LoadFile', roots = c(wd = getwd()), filetypes = 'RDS', session = session)
}

#' Write R6 object into file
#' @description Write an R6 object into a RDS file.
#' @param obj The object to serialize.
#' @param filename The name of the file
#' @export
writeR6 <- function(obj, filename = ""){
  saveRDS(obj$serialize(), file = filename)
}

#' Load a file containing the R6 object
#' @description Load an R6 object from a RDS file.
#' @param filename The name of the file to read
#' @return The saved R6 object
#' @export
readR6 <- function(filename = "", obj){
  r <- readRDS(file = filename)
  obj$unserialize(r)
  return (obj)
}

#' Save the output data into a file.
#' @description Save the output created by a node into a file.
#' @param x The output data to be saved
#' @param filename The name of the file
#' @param repo The type of repository where the data will be saved.
#' @param type The type of file to be saved
#' @export
saveOutput <- function(x, filename = NULL, repo = c("local"), type = c("RDS", "csv")){
  if (repo == "local"){
    dir.create("data", showWarnings = FALSE)
    if (is.null(filename))
      stop("Cannot save output because of missing filename.")
    else{
      saveRDS(x, file.path("data", filename))
    }
  }
}

#' Read output data from a file.
#' @description  Load the output data from a file
#' @param filename The name of the file
#' @param repo The type of repository from which the data will be loaded
#' @return The output data
#' @export
readOutput <- function(filename = NULL, repo = c("local")){
  if (repo == "local"){
    if (is.null(filename))
      stop("Cannot read output file because of missing filename.")
    else
      return (readRDS(filename))
  }
}


