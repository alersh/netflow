# entry to running the network flow

#' @title model_flow
#' @description Opens the UI interface of the network
#' @param .network the network object
#' @return the network object
#' @export
model_flow <- function(.network = NULL){

  g <- gadgetFlow(.network)
  return (g)
}

#' @title gadgetFlow
#' @description Creates the UI interface of the network using Shiny gadget
#' @param .network the network object
#' @importFrom shiny callModule isolate observe observeEvent stopApp runGadget dialogViewer
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
gadgetFlow <- function(.network = NULL) {

  if (!is.null(.network$sources)){
    for (s in .network$sources){
      source(s)
    }
  }
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Netflow"),
    miniUI::miniContentPanel(
      shinyFlowUI(.network$id)
    )
  )

  server <- function(input, output, session) {
    #rv <- reactiveValues(network = .network)
    observe({
      isolate({
        callModule(module = shinyFlowServer, id = .network$id, network = .network)
      })
    })

    observeEvent(input$done, {
      stopApp()
    })

  }

  runGadget(ui, server, viewer = dialogViewer('Viewer', width = 1000, height = 1500))
}



