#' @title nodeInfoUI
#' @description The UI interface of the nodeInfo module
#' @importFrom shiny tabsetPanel tagList
#' @param ns The namespace of the visNetwork
#' @param ... shiny ui elements
nodeInfoUI <- function(ns, ...){
  fluidRow(
    tagList(
      tabsetPanel(id = ns('NodeInfo'),
                  ...)
    )
  )
}
#' @title nodeStatusUI
#' @description The UI interface of the node status panel
#' @importFrom shiny tabPanel column br h5 textInput checkboxInput
#' actionButton
#' @param ns The namespace of the visNetwork
nodeStatusUI <- function(ns){
  tabPanel('Status', value = "Status",
           column(6,
                  br(),
                  br(),
                  textOutput(ns("NodeStatusInfo")),
                  htmlOutput(ns("NodeStatusMessage")),
                  br(),
                  br(),
                  br(),
                  checkboxInput(ns('NodeLock'), "Lock", value = F)),
           column(6,
                  br(),
                  br(),
                  textInput(ns('Label'), 'Label'),
                  br(),
                  br(),
                  br(),
                  actionButton(ns("NodeSaveStatusButton"), "Save Changes")
           )

  )
}
#' @title nodeInputUI
#' @description The UI interface of the node input panel
#' @importFrom shiny tabPanel column br
#' actionButton selectInput
#' @param ns The namespace of the visNetwork
nodeInputUI <- function(ns){
  tabPanel('Inputs', value =  "Inputs",
           column(6,
                  br(),
                  selectInput(ns("InputIDs"), "Inputs", choices = list()),
                  actionButton(ns("RemoveInputButton"), "Remove this Input"),
                  br(),
                  br(),
                  br(),
                  selectInput(ns('Socket'), 'Socket', choices = list()), # arg which the 'from' node plugs in to
                  br()
           ),
           column(6,
                  br(),
                  br(),
                  br(),
                  actionButton(ns("NodeSaveInputsButton"), "Save Changes")
           )
  )
}
#' @title nodeFunctionUI
#' @description The UI interface of the node function panel
#' @importFrom shiny tabPanel column br h5
#' actionButton uiOutput verbatimTextOutput div
#' @param ns The namespace of the visNetwork
nodeFunctionUI <- function(ns){
  tabPanel('Function', value = "Function",
           column(12,
                  br(),
                  column(4,
                         br(),
                         br(),
                         uiOutput(ns('FunctionText')),
                         uiOutput(ns("ArgumentText")),
                         uiOutput(ns('FunctionNSText')),
                         uiOutput(ns("FunctionSourceText")),
                         br(),
                         br(),
                         h5("Options"),
                         uiOutput(ns("OutputUI")),
                         br(),
                         actionButton(ns("NodeSaveFunctionButton"), "Save Changes")
                  ),
                  column(8,
                         br(),
                         br(),
                         h5("Function Description"),
                         div(style="width: 560px;overflow-y:scroll; max-height: 800px;",
                             verbatimTextOutput(ns("FunctionHelpText"))
                         )
                  )
           )
  )
}
#' @title nodeIOutputTableUI
#' @description The UI interface of the node output table panel
#' @importFrom shiny tabPanel column br
#' selectInput verbatimTextOutput tableOutput
#' @importFrom DT dataTableOutput
#' @param ns The namespace of the visNetwork
nodeOutputTableUI <- function(ns){
  tabPanel('Output Table', value = "OutputTable",
           column(3,
                  br(),
                  selectInput(ns("OutputFormat"), "Format", choices = list("Summary", "Data Frame", "Array"),
                              width = 200)
           ),
           column(3,
                  br(),
                  conditionalPanel(condition = "input.OutputFormat != 'Summary'",
                    selectInput(ns("NumPoints"), "Number of Data Points", choices = list("default (100)" = "default", "all" = "all"), width = 200),
                    ns = ns)
                  ),
           column(6),
           column(12,
                  br(),
                  verbatimTextOutput(ns("OutputSummary")),
                  dataTableOutput(ns("OutputDataTable")),
                  tableOutput(ns('OutputTable'))
           )
  )
}
#' @title nodeVisualizationUI
#' @description The UI interface of the node visualization panel
#' @importFrom shiny fluidRow tabsetPanel tabPanel column br h5 textInput checkboxInput
#' actionButton selectInput conditionalPanel plotOutput
#' @param ns The namespace of the visNetwork
nodeVisualizationUI <- function(ns){
  tabPanel('Output Visualization', value = "OutputVisualization",
           br(),
           column(12,
                  selectInput(ns("PlotMode"), "Plot Mode", c("Default", "Custom")),
                  conditionalPanel(condition = "input.PlotMode == 'Default'",
                                   fluidRow(
                                     column(12,
                                            column(3,
                                                   selectInput(ns('PlotOptions'), "Plot Options", choices = list("Scatter", "Line", "Bar"), width = 200)
                                            ),
                                            column(3,
                                                   selectInput(ns("PlotX"), "x", choices = list())
                                            ),
                                            column(3,
                                                   selectInput(ns("PlotY"), "y", choices = list())
                                            ),
                                            column(3,
                                                   selectInput(ns("PlotGroupBy"), "Group by", choices = list())
                                            )
                                     ),
                                     column(12,
                                            plotOutput(ns("OutputGraph"))
                                     )
                                   ), ns = ns),
                  tabsetPanel(id = ns("CustomVisualization"),
                              tabPanel(title = "Visualization", value = "VisualizationCustom",
                                       plotOutput(ns("OutputCustomGraph"))
                              ),
                              tabPanel(title = "Code", value = "VisualizationCode",
                                       textAreaInput(ns("VisualizationCustomCode"), ""),
                                       actionButton(ns("VisualizationCustomButton"), "Apply")
                              )
                  )
           )
  )
}
#' @title nodeDescriptionUI
#' @description The UI interface of the node description panel
#' @importFrom shiny tabPanel column br
#' actionButton textAreaInput
#' @param ns The namespace of the visNetwork
nodeDescriptionUI <- function(ns){
  tabPanel('Description', value = "Description",
           column(12,
                  br(),
                  textAreaInput(ns('NodeDescription'), 'Description'),
                  actionButton(ns("NodeSaveDescriptionButton"), "Save Changes")
           )
  )
}
#' @title nodeDeleteUI
#' @description The UI interface of the node delete panel
#' @importFrom shiny tabPanel column br actionButton
#' @param ns The namespace of the visNetwork
nodeDeleteUI <- function(ns){
  tabPanel('Delete', value = ns("Delete"),
           column(12,
                  br(),
                  actionButton(ns("DeleteNodeButton"), "Click to Delete this Node From the Network")
           )
  )
}
#' @title edgeDeleteUI
#' @description The UI interface of the edge delete panel
#' @importFrom shiny tabPanel column br actionButton
#' @param ns The namespace of the visNetwork
edgeDeleteUI <- function(ns){
  tabPanel('Delete', value = ns("Delete"),
           column(12,
                  br(),
                  actionButton(ns("DeleteEdgeButton"), "Click to Delete this Edge From the Network")
           )
  )
}
