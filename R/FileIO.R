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

writeR6 <- function(obj, filename = ""){
  saveRDS(obj$serialize(), file = filename)
}

readR6 <- function(filename = "", obj){
  r <- readRDS(file = filename)
  obj$unserialize(r)
  return (obj)
}
