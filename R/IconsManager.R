#' Icons manager
#'
#' @description
#' Manages the icon table file
#'
#' @details
#' Contains functions for managing the icons used in this app

get_icon_type <- function(type){
  icon_type = list(database = "database",
                   process = "process",
                   lock = "lock",
                   invalid = "invalid",
                   crash = "crash")
  return (icon_type[[type]])
}

read_icons <- function(){
  print("Getting the font awesome icons ...")
  return (read.csv("img/fontawesome_icons.csv"))
}

get_icon_name <- function(type){
  return (icon_table$Icon_name[which(icon_table$Type == get_icon_type(type))])
}

get_icon_code <- function(type){
  return (icon_table$Code[which(icon_table$Type == get_icon_type(type))])
}

get_icon <- function(type){
  return (list(name = get_icon_name(type), code = get_icon_code(get_icon_type(type))))
}

icon_table <- read_icons()
