#' @title edgeModule
#' @importFrom shiny fluidRow column actionButton uiOutput renderUI req observe
#' observeEvent showModal modalDialog removeModal h4
#' @importFrom shinyjs show hide
#' @importFrom visNetwork visNetworkProxy visUpdateNodes
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param ns The namespace of the visNetwork
#' @param network The Network object
edgeModule <- function(input, output, session, ns, network){
  activateNode <- reactiveValues(click = FALSE,
                                 hold = FALSE)

  nodeHoldMenuUI <- function(ns) {
    fluidRow(
      tabsetPanel(
        tabPanel(title = "Mode",
                 column(12,
                        br(),
                        uiOutput(ns("NodeHoldRunUI")),
                        br(),
                        br(),
                        actionButton(ns("NodeHoldMenuEdgeButton"), "Link to")
                        )
                 ),
        tabPanel(title = "Delete",
                 tabPanel('Delete', value = ns("Delete"),
                          column(12,
                                 br(),
                                 actionButton(ns("DeleteNodeButton"), "Delete this Node")
                          )
                 )
        )
      )
    )
  }
  # TODO: fix remove_edge and remove_node when the node is connected to anything
  observeEvent(input$DeleteNodeButton, {
    if (!is.null(input$EdgeInspectClick)){
      network$remove_edge(input$EdgeInspectClick)
      visNetworkProxy(ns("NetworkStatus")) %>%
        visRemoveEdges(id = input$EdgeInspectClick)
    }
    else{
      network$remove_node(input$NodeHold)
      visNetworkProxy(ns("NetworkStatus")) %>%
        visRemoveNodes(id = input$NodeHold)
    }

    network$build()$build_graph()
    network$plot_network_update(ns)
    removeModal()
  })

  output$NodeHoldRunUI <- renderUI({
    req(input$NodeHold)
    if (!network$nodes$get(input$NodeHold)$is_locked())
      actionButton(ns("NodeHoldRunButton"), "Run this Node")
  })

  output$NodeMenuUI <- renderUI({
    req(input$NodeHold)
    showModal(
      modalDialog(title = input$NodeHold,
                  size = 's',
                  nodeHoldMenuUI(ns),
                  footer = actionButton(ns("NodeHoldDismissButton"), "Dismiss")
      )
    )
  })
  observeEvent(input$NodeHoldDismissButton, {
    removeModal()
    activateNode$hold <- FALSE
    session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeHold"))
  })
  observeEvent(input$NodeHoldMenuEdgeButton, {
    removeModal()
    activateNode$hold <- TRUE
    session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeClick"))
    show("LinkStopButton")

  })

  observeEvent(input$LinkStopButton, {
    activateNode$hold <- FALSE
    hide("LinkStopButton")
    session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeHold"))
    session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeClick"))
  })

  observeEvent(input$NodeClick,{
    req(input$NodeHold)
    if (activateNode$hold){
      if (length(input$NodeClick) > 0){
        if (input$NodeClick != input$NodeHold){
          # it's a valid node, so open another modaldialog to ask for the socket
          # first, check whether this node has already been connected,
          # then check whether the socket has been plugged in
          if (input$NodeHold %in% network$nodes$get(input$NodeClick)$input_ids$items){
            showModal(
              modalDialog(
                h4("The two nodes are already connected.")
              )
            )
            activateNode$hold <- FALSE
            hide("LinkStopButton")
            session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeHold"))
            session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeClick"))
          }
          else{
            input_ids <- network$nodes$get(input$NodeClick)$input_ids$items
            socketsUsed <- NULL
            if (length(input_ids) > 0){
              for (i in seq_along(input_ids)){
                # get all the sockets used
                socketsUsed <- c(socketsUsed, network$nodes$get(input_ids[i])$to_node_arg$get(input$NodeClick))
              }
            }
            # check for available sockets
            socketAvailable <- NULL
            if (is.null(socketsUsed)){
              socketAvailable <- network$nodes$get(input$NodeClick)$fn$args
            }
            else{
              w <- which(!network$nodes$get(input$NodeClick)$fn$args %in% socketsUsed)
              if (length(w) > 0)
                socketAvailable <- network$nodes$get(input$NodeClick)$fn$args[w]
            }
            if (!is.null(socketAvailable)){
              showModal(
                modalDialog(
                  selectInput(ns("EdgeChooseSocket"), "Choose an Available Socket", choices = socketAvailable),
                  footer = tagList(actionButton(ns("ChooseSocketButton"), "Accept"),
                                   actionButton(ns("CancelChooseSocketButton"), "Cancel"))
                )
              )
            }
            else{
              showModal(
                modalDialog(
                  h4("No socket is available. The node cannot be connected.")
                )
              )
              activateNode$hold <- FALSE
              hide("LinkStopButton")
              session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeHold"))
              session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeClick"))
            }
          }
        }
        else{
          activateNode$hold <- FALSE
          hide("LinkStopButton")
          session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeHold"))
          session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeClick"))
        }
      }

    }
  })
  observeEvent(input$ChooseSocketButton, {
    network$add_edge(Edge$representation(input$NodeHold, input$NodeClick), socket = input$EdgeChooseSocket)

    removeModal()
    network$build()$build_graph()$plot_network_update(ns)

    activateNode$hold <- FALSE
    hide("LinkStopButton")
    session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeHold"))
    session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeClick"))

  })
  observeEvent(input$CancelChooseSocketButton, {
    removeModal()
    activateNode$hold <- FALSE
    hide("LinkStopButton")
    session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeHold"))
    session$sendCustomMessage(type = ns("resetValue"), message = ns("NodeClick"))

  })
}

