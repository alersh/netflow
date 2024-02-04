#' @title addNode
#' @description add node interactively
#' @importFrom shiny observeEvent showModal modalDialog textInput selectInput
#' uiOutput actionButton removeModal observe updateTextInput updateSelectInput
#' req validate need isTruthy
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param ns The namespace of the visNetwork
#' @param network The Network object
addNode <- function(input, output, session, ns, network){

  observeEvent(input$AddNodeButton, {
    if (input$NodeSelection == "Manual Specification" && input$AddNodeSelect == "Manual select")
      showModal(
        modalDialog(
        title = "Add a Node",
        selectInput(ns("NetworkID"), "Choose the Network for the New Node", choices = c(network$id, network$network_ids, "Other")),
        conditionalPanel(condition = 'input.NetworkID == "Other"',
                         textInput(ns("NewNetworkID"), "New Network ID", value = ""),
                         ns = ns),
        textInput(ns("NewNodeID"), "New Node ID", value = ""),
        textInput(ns("NewNodeLabel"), "Label", value = ""),
        #selectInput(ns("NewNodeType"), "Type of Node", choices = list("FileIO", "Processor", "Visualizer"), selected = "FileIO"),
        #selectInput(ns("NewNodeTypeFunction"), "New Node Function", choices  = list()),
        #conditionalPanel(condition = "input.NewNodeTypeFunction == 'None'",
        textInput(ns("NewNodeFunction"), "New Node Function", value = ""),
        selectInput(ns("NewNodeFunctionNS"), "Namespace of the Function", choices = list()),
        uiOutput(ns("NewNodeFunctionArguments")),
        selectInput(ns("NewNodeArguments"), "Function Arguments", choices = NULL, multiple = T),
        #                 ns = ns),
        footer = list(
          actionButton(ns("AddNodeProceedButton"), "Proceed"),
          actionButton(ns("AddNodeCancelButton"), "Cancel")
        )
      )
    )
  })

  observeEvent(input$AddNodeCancelButton, {
    removeModal()
  })


  observe({
    req(network, input$NewNodeType)
    updateSelectInput(session,
                      "NewNodeTypeFunction",
                      choices = append('None', getFunctionMenu(input$NewNodeType)))
  })

  observeEvent(input$NewNodeFunction, {
    req(input$NewNodeFunction)

    # get namespace of function
    w <- getAnywhere(input$NewNodeFunction)$where
    namespace <- NULL
    if (isTruthy(w)){
      idx <- grep('namespace', w)
      namespace <- vector("numeric", length = length(idx))
      l <- length(idx)
      tryCatch(
        for (i in 1:l){
          namespace[i] <- strsplit(w[idx[i]], split = ":")[[1]][2]
        },
        error = function(e) e
      )
    }
    updateSelectInput(session, "NewNodeFunctionNS", choices = namespace)
  })
  observeEvent(c(input$NewNodeFunction, input$NewNodeFunctionNS), {
    req(input$NewNodeFunction, input$NewNodeFunctionNS)
    # get arguments of function
    ags <- NULL

    if (isTruthy(input$NewNodeFunctionNS)){
      tryCatch(
        ags <- names(arguments(getFromNamespace(input$NewNodeFunction, input$NewNodeFunctionNS))),
        error = function(e) e
      )
    }
    ags <- c('None', ags)
    updateSelectInput(session, "NewNodeArguments", choices = ags, selected = ags[1])
  })


  observeEvent(input$AddNodeProceedButton, {
    req(input$NewNodeID)
    validate(
      need(!input$NewNodeID %in% network$nodes$keys, message = "This node ID has already been used. Please use a different ID name.")
    )
    a <- NULL
    if (input$NewNodeArguments != "None")
      a <- input$NewNodeArguments
    if (input$NetworkID == "Other"){
      req(input$NewNetworkID)
      newNetwork <- Network$new(input$NewNetworkID)$
        add_node(
          Process_Node$new(
            id = input$NewNodeID,
            style = Node_Style$new(label = input$NewNodeLabel),
            fn = Function$new(fn = input$NewNodeFunction,
                              input_args = a,
                              ns = input$NewNodeFunctionNS)
          )
        )
      network$add_network(newNetwork)
    } else{
      network$add_nodes(
        Process_Node$new(
          id = input$NewNodeID,
          style = Node_Style$new(label = input$NewNodeLabel),
          fn = Function$new(fn = input$NewNodeFunction,
                            input_args = a,
                            ns = input$NewNodeFunctionNS)
        ),
        network_id = input$NetworkID
      )

    }

    network$
      set_node_validity()$
      build()$
      build_graph()

    network$plot_network_update(ns)

    removeModal()
  })
}

