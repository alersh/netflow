#' @title nodeStatusServer
#' @description The server side of the node status panel module
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param node A Node object
#' @param editable whether this panel is editable or not
#' @param ns The parent's namespace
nodeStatusServer <- function(input, output, session, node, editable = TRUE, ns){
  # status panel
  output$NodeStatusInfo <- shiny::renderText({
    paste("Status:   ", toupper(node$status))

  })

  output$NodeStatusMessage <- shiny::renderUI({
    if (node$error_message != "")
      p(paste("Error:   ", node$error_message), style = "color:red")
    else if (node$warning_message != "")
      p(paste("Warning:   ", node$warning_message), style = "color:red")
    else
      ""
  })

  observe({

    shiny::updateTextInput(session, "Label", value = node$style$label)
    shiny::updateCheckboxInput(session, "NodeLock", value = node$is_locked())

  })

  shiny::observeEvent(c(input$Label, input$NodeLock),{

    if (!editable)
      shinyjs::disable("NodeSaveStatusButton")
    else
      shinyjs::enable("NodeSaveStatusButton")
    if (!editable){
      shinyjs::hide("NodeLock")
    }
    else{
      shinyjs::show("NodeLock")
    }
  })

  shiny::observeEvent(input$NodeSaveStatusButton, {

    ds <- node$style
    ds$label <- input$Label
    node$lock <- input$NodeLock
    updateNodeStatus(node$style$render(node$id, node$network_id), ns)
    shinyjs::disable("NodeSaveStatusButton")
  })
}
#' @title nodeInputServer
#' @description The server side of the node input panel module
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param node A Node object
#' @param editable whether this panel is editable or not
nodeInputServer <- function(input, output, session, node, editable = TRUE){
  # Inputs panel
  shiny::observeEvent(input$NodeInfo, {
    shiny::req(input$NodeInspectClick)
    shiny::updateSelectInput(session, "InputIDs",
                      choices = node$input_ids$items)
  })

  shiny::observeEvent(input$InputIDs, {
    if ("inputs" %in% hideOptions)
      shinyjs::disable("NodeSaveInputsButton")
    else
      shinyjs::enable("NodeSaveInputsButton")

    if (!shiny::isTruthy(series$network$node[[input$InputIDs]]$to_args[[input$NodeInspectClick]]))
      shiny::updateSelectInput(session, 'Socket', choices = "")
    else
      shiny::updateSelectInput(session, 'Socket',
                        choices = series$network$node[[input$NodeInspectClick]]$fn$args,
                        selected = series$network$node[[input$InputIDs]]$to_args[[input$NodeInspectClick]])
  })

  edgeRemoveID <- shiny::reactiveValues(id = NULL)

  shiny::observeEvent(input$NodeSaveInputsButton, {
    series$network$node[[input$InputIDs]]$to_args[[input$NodeInspectClick]] <- input$Socket
    shinyjs::disable("NodeSaveInputsButton")
    if (input$RemoveInputButton > 0){
      for (e in edgeRemoveID$id){
        series$network <- edge_remove(series$network, edge = paste(e, "->", input$NodeInspectClick))
      }
    }
    series$network <- network_build(series$network)
    series$graph <- network_graph(series$network)
  })

  shiny::observeEvent(c(input$Socket, input$NodeDescription),{
    if (editable)
      shinyjs::disable("NodeSaveInputsButton")
    else
      shinyjs::enable("NodeSaveInputsButton")
  })
}
#' @title nodeFunctionServer
#' @description The server side of the node status function panel module#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param node A Node object
#' @param editable whether this panel is editable or not
nodeFunctionServer <- function(input, output, session, node, editable = TRUE){
  # Function panel
  ns <- session$ns
  output$FunctionText <- shiny::renderUI({
    shiny::h4(paste("Function:", node$fn$fn))
  })

  output$FunctionNSText <- shiny::renderUI({
    shiny::h4(paste("Namespace:", node$fn$ns))
  })

  output$FunctionSourceText <- shiny::renderUI({
    shiny::h4(paste("Source:", node$fn$source))
  })
  shiny::observeEvent(input$NodeSaveFunctionButton, {
    # loop through all option names and enter each parameter into set_options
    # we don't change the parameters in the node's function
    n <- names(node$fn$options)

    # TODO: For the option parameters that have been changed, save their previous values.
    # Then go through all the nodes downstream that are connected to this node
    # and change their current statuses to "pending" since all their outputs will
    # be changed. If the user revert the parameters back to their original values,
    # these values can be matched with the saved ones so that the values are matched,
    # then the nodes downstream will have their statuses reverted back to the current statuses.
    for (op in n){
      node$fn$set_option(op, input[[paste0(node$id, op)]])
    }

    shinyjs::disable("NodeSaveFunctionButton")
  })
  shiny::observe({
    n <- names(node$options)
    for (i in n){
      input[[paste0(node$id, i)]]
    }
    shinyjs::disable("FunctionText")
    shinyjs::disable("NodeSaveFunctionButton")

  })

  output$OutputUI <- shiny::renderUI({
    uis <- list()
    options <- node$fn$options
    option_names <- names(options)
    option_names2 <- names(node$options)
    for (op in option_names){
      value <- ""
      type <- ""
      if (op %in% option_names2){
        v <- node$options[[op]]
        if (!is.symbol(v)){
          value <- v
        }
      }
      else if (!is.symbol(options[[op]])){
        value <- options[[op]]

      }
      if (!is.null(type))
        type <- node$fn$data_types[[op]]
      uis[[op]] <- shiny::textInput(inputId = ns(paste0(node$id, op)),
                             label = op,
                             value = value,
                             width = 300,
                             placeholder = type)

    }
    for (op in option_names2){
      if (!op %in% option_names){
        if (!is.symbol(node$options[[op]])){
          value <- node$options[[op]]
        }
        if (!is.null(type))
          type <- node$fn$types[[op]]

        uis[[op]] <- shiny::textInput(inputId = ns(paste0(node$id, op)),
                               label = op,
                               value = value,
                               width = 300,
                               placeholder = type)
      }
    }

    shiny::tags$div(id = ns(paste0(node$id, "OptionsUI")),
             uis)
  })

  shiny::observe({
    purrr::map(names(node$fn$options),
        function(n) input[[paste0(node$id, n)]])
    if (!editable)
      shinyjs::disable("NodeSaveFunctionButton")
    else
      shinyjs::enable("NodeSaveFunctionButton")

  })
  output$FunctionHelpText <- shiny::renderPrint({

    h <- tryCatch({
      capture.output(
        tools:::Rd2txt(
          utils:::.getHelpFile(
            help(
              node$fn$fn,
              package = node$fn$ns
            )
          )
        )
      )
    },
    error = function(e) e
    )
    if (inherits(h, 'error')){
      cat("No help document is available.")
    }
    else{
      cat(h, sep = "\n")
    }
  })

  output$ArgumentText <- shiny::renderUI({

    h4("Sockets:", paste(node$fn$args, collapse = ', '))
  })

}
#' @title nodeOutputTableServer
#' @description The server side of the node output table panel module
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param node A Node object
nodeOutputTableServer <- function(input, output, session, node){
  # Output table panel

  output$OutputDataTable <- DT::renderDataTable({

    shiny::validate(
      shiny::need(input$OutputFormat == "Data Frame", message = F)
    )
    out <- node$output
    shiny::validate(
      shiny::need(!is.null(out), message = "No Output is Available.")
    )
    shiny::validate(
      shiny::need(is.data.frame(out), message = "Output is not a data frame.")
    )

    points <- input$NumPoints
    if (points == "all") points <- nrow(out)
    else if (points == "default") points <- 100
    if (points > nrow(out)) points <- nrow(out)

    out[1:points,]

  })
  output$OutputSummary <- shiny::renderPrint({

    shiny::validate(
      shiny::need(input$OutputFormat == "Summary", message = F)
    )
    out <- node$output
    shiny::validate(
      shiny::need(!is.null(out), message = "No Output is Available.")

    )
    summary(out)
  })

}
#' @title nodeVisualizationServer
#' @description The server side of the node visualization panel module
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param node A Node object
#' @param editable whether this panel is editable or not
nodeVisualizationServer <- function(input, output, session, node, editable = TRUE){
  # Output visualization panel

  shiny::observe({

    shinyjs::disable("VisualizationCustomButton")

    shiny::req(node$output)
    output <- node$output

    tryCatch({
      if (!is.data.frame(output))
        output <- as.data.frame(output)

      shiny::updateSelectInput(session, "PlotX", choices = c("", colnames(output)))
      shiny::updateSelectInput(session, "PlotY", choices = c("", colnames(output)))
      shiny::updateSelectInput(session, "PlotGroupBy", choices = c("None", colnames(output)))
    }, error = function(e){})
  })
  output$OutputGraph <- shiny::renderPlot({
    shiny::req(node$output)
    p <- NULL
    if (input$PlotMode == "Default"){
      shiny::validate(
        shiny::need(is.data.frame(node$output), message = "The plot requires a data frame.")
      )
      shiny::req(input$PlotX, input$PlotY)
      if (input$PlotGroupBy != "None"){
        p <- switch(input$PlotOptions,
                    'Scatter' = ggplot2::geom_point(ggplot2::aes_(rlang::sym(input$PlotX),
                                                                  rlang::sym(input$PlotY),
                                                color = rlang::sym(input$PlotGroupBy), fill = rlang::sym(input$PlotGroupBy))),
                    'Line' = ggplot2::geom_line(ggplot2::aes_(rlang::sym(input$PlotX),
                                                              rlang::sym(input$PlotY),
                                            color = rlang::sym(input$PlotGroupBy), fill = rlang::sym(input$PlotGroupBy))),
                    'Bar' = ggplot2::geom_bar(ggplot2::aes_(rlang::sym(input$PlotX),
                                                           rlang::sym(input$PlotY),
                                          color = rlang::sym(input$PlotGroupBy), fill = rlang::sym(input$PlotGroupBy)),
                                     stat = "identity")
        )
      }
      else{
        p <- switch(input$PlotOptions,
                    'Scatter' = ggplot2::geom_point(ggplot2::aes_(rlang::sym(input$PlotX),
                                                                  rlang::sym(input$PlotY))),
                    'Line' = ggplot2::geom_line(ggplot2::aes_(rlang::sym(input$PlotX),
                                                              rlang::sym(input$PlotY))),
                    'Bar' = ggplot2::geom_bar(ggplot2::aes_(rlang::sym(input$PlotX),
                                                            rlang::sym(input$PlotY)),
                                              stat = "identity")
        )
      }
      ggplot2::ggplot(data = node$output) + p
    }

  })

  output$OutputCustomGraph <- shiny::renderPlot({
    shiny::req(node$output)
    output <- node$output
    if (input$PlotMode == "Custom"){
      shiny::validate(
        shiny::need(isTruthy(node$visualization), message = "No custom visualization is available.")
      )

      eval(parse(text = node$visualization))(output)
    }
  })

  shiny::observe({
    shiny::req(input$PlotMode)
    if (input$PlotMode == "Custom"){
      shiny::showTab("CustomVisualization", "VisualizationCustom")
      shiny::showTab("CustomVisualization", "VisualizationCode")
      shiny::updateTextAreaInput(session, "VisualizationCustomCode", value = node$visualization)
    }
    else{
      shiny::hideTab("CustomVisualization", "VisualizationCustom")
      shiny::hideTab("CustomVisualization", "VisualizationCode")
    }
  })

  shiny::observeEvent(input$PlotMode, {
    if (input$PlotMode == "Custom"){
      shiny::showTab("CustomVisualization", "VisualizationCustom")
      shiny::showTab("CustomVisualization", "VisualizationCode")
      shiny::updateTextAreaInput(session, "VisualizationCustomCode", value = node$visualization)
    }
    else{
      shiny::hideTab("CustomVisualization", "VisualizationCustom")
      shiny::hideTab("CustomVisualization", "VisualizationCode")
    }
  })

  shiny::observeEvent(input$VisualizationCustomCode, {
    if (!editable)
      shinyjs::disable("VisualizationCustomButton")
    else
      shinyjs::enable("VisualizationCustomButton")
  })

  shiny::observeEvent(input$VisualizationCustomButton, {
    node$visualization <- input$VisualizationCustomCode
    shinyjs::disable("VisualizationCustomButton")
  })

}
#' @title nodeDescriptionServer
#' @description The server side of the node description panel module
#' @importFrom dplyr %>%
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param node A Node object
#' @param editable whether this panel is editable or not
nodeDescriptionServer <- function(input, output, session, node, editable = TRUE){
  # description panel
  rv <- shiny::reactiveValues(change = 0)
  shiny::observeEvent(input$NodeInfo,{

    shiny::updateTextAreaInput(session, "NodeDescription", value = node$description)
    shinyjs::disable("NodeSaveDescriptionButton")
  })
  shiny::observeEvent(input$NodeSaveDescriptionButton, {

    node$description <- input$NodeDescription
    rv$change <- rv$change + 1
    shinyjs::disable("NodeSaveDescriptionButton")
  })
  shiny::observeEvent(input$NodeDescription, {
    if (editable)
      shinyjs::disable("NodeSaveDescriptionButton")
    else
      shinyjs::enable("NodeSaveDescriptionButton")
  })
  return (rv$change)
}

nodeDeleteServer <- function(input, output, session, network){
  ns <- session$ns
  shiny::observeEvent(input$DeleteNodeButton, {
    shiny::showModal(
      shiny::modalDialog(
        shiny::h4("Are you sure you want to permanently delete this node?"),
        footer = shiny::tagList(
          shiny::actionButton(ns("DeleteNodeProceedButton"), "Proceed"),
          shiny::actionButton(ns("DeleteNodeCancelButton"), "Cancel")
        )
      )
    )
  })
  shiny::observeEvent(input$DeleteNodeProceedButton, {
    shiny::req(input$NodeInspectClick)

    ids <- network$structure$graph$edge_table %>%
      dplyr::filter(to == input$NodeInspectClick | from == input$NodeInspectClick) %>%
      .$id

    network$remove_node(input$NodeInspectClick)$
      build()$
      build_graph()

    visNetwork::visNetworkProxy(ns("NetworkStatus")) %>%
      visNetwork::visRemoveNodes(id = input$NodeInspectClick) %>%
      visNetwork::visRemoveEdges(id = ids)

    shiny::removeModal()
  })
  shiny::observeEvent(input$DeleteNodeCancelButton, {
    shiny::removeModal()
  })
}
#' @title edgeDeleteServer
#' @description The server side of the edge delete module
#' @param input Shiny input
#' @param network A Network object
#' @param ns The parent's namespace
edgeDeleteServer <- function(input, network, ns){
  shiny::observeEvent(input$DeleteEdgeButton, {
    network$remove_edge(input$EdgeInspectClick)

    network$build()$build_graph()

    if (!is.null(input$EdgeInspectClick)){
      visNetwork::visNetworkProxy(ns("NetworkStatus")) %>%
        visNetwork::visRemoveEdges(id = input$EdgeInspectClick)
    }
  })

}
