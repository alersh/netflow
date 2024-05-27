#' @title networkDisplay
#' @description visNetwork display
#' @importFrom visNetwork renderVisNetwork visEdges visOptions visPhysics
#' visEvents visInteraction
#' @importFrom shiny validate need
#' @importFrom dplyr filter %>%
#' @param output Shiny output
#' @param ns The namespace of the visNetwork
#' @param network The network object
networkDisplay <- function(output, ns, network){
  output$NetworkStatus <- renderVisNetwork({
    validate(
      need(!network$structure$graph$is_empty(), message = F)
    )
    visNetworkShow(network$structure$graph, network$structure$rankdir) %>%
      visOptions(selectedBy = "group") %>%
      visPhysics(enabled = F) %>%
      addFontAwesome() %>%
      visInteraction(selectConnectedEdges = F) %>%
      visEvents(doubleClick = paste0("function(data){
                            Shiny.onInputChange('", ns('NodeInspectClick') , "', data.nodes[0]);
                            Shiny.onInputChange('", ns('EdgeInspectClick'), "', data.edges[0]);
                            }"),
                hold = paste0("function(nodes){
                            Shiny.onInputChange('", ns('NodeHold') , "', nodes.nodes[0]);}"),
                click = paste0("function(nodes){
                            Shiny.onInputChange('", ns('NodeClick') , "', nodes.nodes[0]);}"))
  })
}
#' @title visNetworkShow
#' @description Show the visNetwork graph
#' @importFrom visNetwork visNetwork visEdges visHierarchicalLayout
#' visNetworkProxy
#' @importFrom shiny validate need
#' @param graph The Network_Graph object
#' @param dir The direction of the network
visNetworkShow <- function(graph, dir = "LR"){
  validate(
    need(!graph$is_empty(), message = "Graph is empty.")
  )

  visNetwork(graph$node_table,
             graph$edge_table,
             background = '#ECECE8') %>%
    visEdges(arrows = "to", length = 10, smooth = list(type = 'dynamic',
                                                       roundness = 1)) %>%
    visHierarchicalLayout(direction = dir, sortMethod = "directed")
}
#' @title updateNodeStatus
#' @description Update visNetwork node display
#' @importFrom visNetwork visNetworkProxy visUpdateNodes
#' @importFrom dplyr %>%
#' @param node_table The node table for rendering the visNetwork
#' @param ns The namespace of the visNetwork
updateNodeStatus <- function(node_table, ns){
  visNetworkProxy(ns("NetworkStatus")) %>%
    visUpdateNodes(node_table)
}
#' @title networkDisplay
#' @description visNetwork display
#' @importFrom visNetwork visUpdateEdges visNetworkProxy visUpdateNodes
#' @importFrom shiny validate need
#' @importFrom dplyr filter %>%
#' @param graph The Network_Graph object
#' @param ns The namespace of the visNetwork
updateNetworkStatus <- function(graph, ns){
  visNetworkProxy(ns("NetworkStatus")) %>%
    visUpdateNodes(graph$node_table) %>%
    visUpdateEdges(graph$edge_table)
}
