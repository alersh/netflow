#' shinyFlowUI
#' @description shiny rendering of a model flow module
#' @param id The network id name
#' @import shinyjs
#' @import shiny
#' @import visNetwork
#' @export
shinyFlowUI <- function(id){
  ns <- NS(id)

  fluidPage(
    useShinyjs(),
    fluidRow(
      hidden(
        shinyWidgets::dropdownButton(inputId = ns("FileMenuButton"),
                                     actionButton(ns("ProjectMenuButton"), "Project"),
                                     actionButton(ns("NetworkMenuButton"), "Network"),
                                     icon = icon("gear"),
                                     width = "300px"),
        br(),
        br()
      ),
      hidden(
        div(id = ns("Controls"),
          column(3,
            selectInput(ns("NodeSelection"),
                        label = "Process type",
                        choices = list("Manual Specification", "Package", "Read/Write", "Data Preprocessing", "Modelling", "Visualization"),
                        width = '200px'),
            conditionalPanel(condition = 'input.NodeSelection == "Package"',
                             selectInput(ns("PackageSelect"),
                                         label = "Choose a package",
                                         choices = list(),
                                         width = '200px'),
                             ns = ns)
            ),
          column(3,
            selectInput(ns("AddNodeSelect"),
                        label = "Select a node",
                        choices = list()),
            actionButton(ns("AddNodeButton"), "Add")
          ),
          column(9)
        )
      ),
      br(),
      br(),
      # Define layout, inputs, outputs
      uiOutput(ns("NodeMenuUI")),
      br(),
      br(),
      fluidRow(
        column(6,
               actionButton(ns("RunButton"), "Run", style = "color: white; background-color: green"),
               actionButton(ns("ResetButton"), "Reset", style = "color: grey; background-color: yellow"),
               hidden(
                 actionButton(ns("StopButton"), "Stop", style = "color: white; background-color: red")
               ),
               hidden(
                 actionButton(ns("LinkStopButton"), "Cancel Linking", style = "color: white; background-color: red")
               )
        ),
        column(3, offset = 3,
               actionButton(ns("NetworkInfoButton"), "i", width = '50px', style = "background-color: yellow")
               ),
        br(),
        visNetworkOutput(ns('NetworkStatus')),
        tags$script(paste0("Shiny.addCustomMessageHandler('", ns('resetValue'), "', function(variableName) {
                           Shiny.onInputChange(variableName, null);}); "))
      )

    )
  )
}

#' @title shinyFlowServer
#' @description The call module of shinyFlowUI. Use callModule to call this function
#' @param input the shiny input
#' @param output the shiny output
#' @param session the shiny session
#' @param series the series containing the network and the graph
#' @param network_Options the UI features that should be hidden from the user. These features are:
#' nodeAddDelete Add and delete a node in the interface
#' edgeAddDelete Add and delete an edge
#' fileLoadSave Load and save network
#' description Change network description
#' lockable Ability to lock individual locks
shinyFlowServer <- function(input, output, session, network,
                            network_options = list(lockable = TRUE, edgeAddDelete = TRUE,
                                                   fileMenu = TRUE, nodeAddDelete = TRUE, description = TRUE)){

  ns <- session$ns

  if (network_options$fileMenu){
    shinyjs::show("FileMenuButton")
    fileIO(input, output, session, ns, network)
    shinyjs::showElement("Controls")
  }

  if (network_options$nodeAddDelete){
    show("AddNodeButton")
    show("PackageSelect")
    show("NodeSelection")
    show("AddNodeSelection")
    addNode(input, output, session, ns, network)
  }

  if (network_options$edgeAddDelete) edgeModule(input, output, session, ns, network)

  networkDisplay(output, ns, network)

  observeEvent(input$NodeSelection, {
    if (input$NodeSelection == "Package"){
      packages <- rownames(installed.packages())
      updateSelectInput(session, "PackageSelect", choices = packages, selected = packages[1])
    }

    choices <- switch(input$NodeSelection,
                      "Manual Specification" = c("Manual select"),
                      "Package" = ls(asNamespace(packages[1])),
                      "Read/Write" = c("csv reader",
                                       "Excel reader",
                                       "db reader"),
                      "Data Preprocessing" = c("select",
                                               "filter",
                                               "group by",
                                               "summarise",
                                               "mutate"),
                      "Model" = c("linear regression"),
                      "Visualization" = "ggplot")
    updateSelectInput(session, "AddNodeSelect", choices = choices, selected = choices[1])
  })

  observeEvent(input$PackageSelect, {
    req(input$PackageSelect)
    updateSelectInput(session, "AddNodeSelect", choices = ls(asNamespace(input$PackageSelect)))
  })

  if (!is.null(network)){
    # build the network first
  network$
      set_node_validity()$
      build()$
      build_graph()
  }

  observeEvent(input$StopButton, {
    network$running <- FALSE
    hide("StopButton")
    show("ResetButton")
  })

  # network info
  observeEvent(input$NetworkInfoButton, {
    showModal(
      modalDialog(
        textInput(ns("NetworkAuthor"), "Author", value = network$author),
        textAreaInput(ns("NetworkDescription"), "About this Network", value = network$description),
        br(),
        actionButton(ns('NetworkDescriptionSaveButton'), "Save Author and Description"),
        br(),
        disable("NetworDescriptionSaveButton")
      )
    )
  })

  observeEvent(input$NetworkDescriptionSaveButton, {
    network$author <- input$NetworkAuthor
    network$description <- input$NetworkDescription
    disable('NetworkDescriptionSaveButton')
  })

  observeEvent(c(input$NetworkAuthor, input$NetworkDescription), {
    if (!network_options$description)
      disable("NetworkDescriptionSaveButton")
    else
      enable('NetworkDescriptionSaveButton')
  })

  observeEvent(input$ResetButton, {
    network$reset()$build_graph()$plot_network_update(ns)
  })
  observeEvent(input$RunButton, {

    req(network$nodes)
    disable('RunButton')
    hide("ResetButton")
    show("StopButton")

    runNetwork()

    network$set_status("success")
    enable("RunButton")
    show("ResetButton")
    hide("StopButton")
  })

  observeEvent(input$NodeHoldRunButton, {
    disable("RunButton")
    removeModal()
    network$nodes$get(input$NodeHold)$clear_output()
    runNetwork(input$NodeHold)
    network$set_status("idle")
    enable("RunButton")
    session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeHold"))
  })

  runNetwork <- function(node_id){

    if (!missing(node_id)){
      ids <- node_id
    }
    else{
      network$reset()$
        set_node_validity()$
        build()$
        build_graph()

      network$plot_network_update(ns)

      # Along the columns of the network matrix, starts from the left, with variables
      # along the rows being the inputs to the functions defined along the columns
      ids <- network$structure$node_sequence
    }
    network$set_status("running")
    for (i in seq_along(ids)){
      if (!network$status_running()){
        break
      }
      id <- ids[i]
      if (!network$nodes$get(id)$is_locked()){
        # set the ui of the running node
        current_node <- network$nodes$get(id)
        current_node$set_status("running")
        network$structure$graph$set_node_color(id, current_node$style$status_color)
        network$plot_node_update(id, ns)
        input_nodes <- current_node$input_ids$items
        outputs <- NULL

        # get options
        options <- current_node$fn$options_to_string()
        if (!is.null(input_nodes)){
          # get the output of this 'from' node and use it as the input for this node
          # since there could be more than one incoming node, we have to loop through them
          for (n in input_nodes){
            input_node <- network$nodes$get(n)
            if (!is.null(input_node$output)){
              l <- list(input_node$output)
              names(l) <- input_node$to_node_arg$get(current_node$id)
              outputs <- append(outputs, l)
            }
          }
        }
        # run this current node using all the outputs and the options (arguments) required.
        current_node$run(outputs, options)
        network$structure$graph$set_node_color(id, current_node$style$status_color)
        network$plot_node_update(id, ns)
        if (current_node$status_fail()){
          # then we need to stop the run
          network$set_status("fail")
        }
      }
    }
  }
  #
  observe({
    if (!network$nodes$is_empty() && !network$status_running()){
      enable("NetworkInfoButton")
      enable("AddNodeButton")
      enable("NetworkMenuButton")
    }
  })


  observeEvent(input$NodeInspectClick,{
    req(input$NodeInspectClick)

    network$nodes$get(input$NodeInspectClick)$call(input,output, session)
    session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeInspectClick"))
  })

  observeEvent(input$EdgeInspectClick, {
    req(input$EdgeInspectClick)
    showModal(
      modalDialog(title = input$EdgeInspectClick,
                  size = 'm',
                  edgeDeleteUI(ns),
                  footer = actionButton(ns("EdgeDismissButton"), "Dismiss")
      )
    )
  })

  observeEvent(input$DeleteEdgeButton, {
    edgeDeleteServer(input, network, ns)
  })

  observeEvent(input$EdgeDismissButton, {
    removeModal()
    session$sendCustomMessage(type = ns("resetValue"), message = ns("EdgeInspectClick"))
  })
}



# TODO
runParallelNetwork <- function(node_id){

  if (!missing(node_id)){
    ids <- node_id
  }
  else{
    network$reset()$
      set_node_validity()$
      build()$
      build_graph()

    network$plot_network_update(ns)

    # Along the columns of the network matrix, starts from the left, with variables
    # along the rows being the inputs to the functions defined along the columns
    ids <- network$structure$node_sequence
  }
  network$set_status("running")
  for (i in seq_along(ids)){
    if (!network$status_running()){
      break
    }
    id <- ids[i]
    if (!network$nodes$get(id)$is_locked()){
      # set the ui of the running node
      current_node <- network$nodes$get(id)
      current_node$set_status("running")
      network$structure$graph$set_node_color(id, current_node$style$status_color)
      network$plot_node_update(id, ns)
      input_nodes <- current_node$input_ids$items
      outputs <- NULL

      # get options
      options <- current_node$fn$options_to_string()
      if (!is.null(input_nodes)){
        # get the output of this 'from' node and use it as the input for this node
        # since there could be more than one incoming node, we have to loop through them
        for (n in input_nodes){
          input_node <- network$nodes$get(n)
          if (!is.null(input_node$output)){
            l <- list(input_node$output)
            names(l) <- input_node$to_node_arg$get(current_node$id)
            outputs <- append(outputs, l)
          }
        }
      }
      # run this current node using all the outputs and the options (arguments) required.
      current_node$run(outputs, options)
      network$structure$graph$set_node_color(id, current_node$style$status_color)
      network$plot_node_update(id, ns)
      if (current_node$status_fail()){
        # then we need to stop the run
        network$set_status("fail")
      }
    }
  }
}

