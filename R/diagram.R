#' @title Create workflow diagram
#' @description `make_diagram` creates a
#' @param report_file Step's list or Reportr object,
#' Default: knitr::current_input(dir = TRUE).
#' @param critical Critical steps,
#' Default: list(names = "lustering|uality", color = "Gold").
#' @param exclude Exclude these steps, Default: 'libraries'.
#' @param style Style has a pre-set of connections between steps,
#' Default: 'single-cell'.
#' @param extra_connections This further modifies connections, Default: NULL.
#' @param critical_connections Makes the connection red, Default: NULL.
#' @param central_node Step connected to the rest after it, Default: ''.
#' @param last_node Last step in the diagram, Default: ''.
#' @return Spec for a Graphviz diagram as text.
#' @details
#' At the moment is more difficult to get the right titles from a list or
#' an Reportr object
#' @examples
#' \dontrun{
#'  if(interactive()){
#'   DiagrammeR::grViz(make_diagram(list("Step1"= 1, "Step2" = 2), style = NULL))
#'  }
#' }
#' @seealso
#'  \code{\link[knitr]{current_input}}
#' @rdname make_diagram
#' @export
#' @importFrom knitr current_input
#' @importFrom stats setNames
make_diagram = function(
  report_file = knitr::current_input(dir = TRUE),
  critical = list(names = "lustering|uality", color = "Gold"),
  exclude = "libraries",
  style = "single-cell",
  extra_connections = NULL,
  critical_connections = NULL,
  central_node = "",
  last_node = ""
){
  if(is.null(report_file)) report_file =  "/home/ciro/scripts/workflow/report.rmd"
  is_tailored = FALSE
  if(isTRUE(style == "single-cell")){
    central_node = "lock"; last_node = ""
    extra_connections = c("cells$" = "etailed", "donors"= "etailed", "lustering" = "etailed", "lock" = "etailed")
    critical_connections = c("uality" = "etailed", "lustering" = "uality|oublet", "lock" = "lustering")
    is_tailored = TRUE
  }
  .get_connections = function(x, from, to, v){
    froms = names(grep(from, v, value = TRUE))
    tos = names(grep(to, v, value = TRUE))
    if(length(froms) > 0 && length(tos) > 0){
      new_cons = paste0(froms, " -> {", paste0(tos, collapse = " "), "}")
      tvar <- "->|\\{|\\}| " # if connection exists
      if(!grepl(gsub(tvar, "", new_cons), gsub(tvar, "", x))) x <- paste0(x, new_cons, "\n  ")
    }; return(x)
  }

  titles <- if(is.list(report_file)){
    names(report_file)
  }else if(class(report_file) == "Reportr"){
    sapply(report_file@steps, function(x){
      if(!is.null(x$title)) x$title else preset_titles[[x]]
    })
  }else{
    grep("\\$", system(paste("grep '^## '", report_file), intern = TRUE), value = TRUE)
  }
  titles <- grep(pattern = exclude, x = titles, invert = TRUE, value = TRUE)
  titles_checked <- grep("checkmark", titles)
  titles <- gsub(".*\\$ ", "", titles)
  titles_checked <- paste0(make.names(titles[titles_checked]), collapse = "|")
  if(titles_checked == "") titles_checked = "noneofthem123"
  names(titles) <- LETTERS[1:length(titles)]
  last_node_n = tail(grep(last_node, titles), 1)
  titles <- titles_trimmed <- titles[1:last_node_n]
  tmp = "Detailed analyses"
  if(is_tailored && !tmp %in% titles) titles <- c(titles, setNames(tmp, LETTERS[last_node_n+1]))
  central_node_n = tail(grep(central_node, titles), 1)
  if(isTRUE(central_node_n < last_node_n)){
    titles_extra <- rev(make.names(titles[(central_node_n+1):length(titles)]))
    if(central_node %in% names(extra_connections)){
      extra_connections[[central_node]] <- paste0(c(titles_extra, extra_connections[[central_node]]), collapse = "|")
    }else{ extra_connections <- c(extra_connections, setNames(paste0(titles_extra, , collapse = "|"), central_node)) }
    titles_trimmed <- titles[1:central_node_n]
  }

  my_diagram = paste0(
    "digraph {\n  ",
    "graph [layout = dot, rankdir = LR, overlap = 'false']\n  ",
    "node [shape = circle, fixedsize = true, height = 2, style = filled]\n  ",
    paste(sapply(names(titles), function(x){ # fill in the nodes
      paste0(
        x, " [label = '", titles[[x]], "'",
        ", fillcolor = ", ifelse(grepl(titles_checked, titles[[x]]), "DarkSeaGreen",
        ifelse(grepl(critical$names, titles[[x]]), critical$color, "Gainsboro")),
        "]"
      )
    }), collapse = "\n  "),
    "\n  # edge definitions with the node IDs\n  edge [color = Gainsboro]\n  ",
    paste0(names(titles_trimmed), collapse = " -> "), "\n  "
  )
  for(i in names(extra_connections)){ # create extra connections
    my_diagram <- .get_connections(
      my_diagram, from = i, to = extra_connections[[i]], v = titles
    )
  }
  my_diagram = paste0(my_diagram, "\n  edge [color = red]\n  ")
  for(i in names(critical_connections)){ # create critical connections
    my_diagram <- .get_connections(
      my_diagram, from = i, to = critical_connections[[i]], v = titles
    )
  }
  paste0(my_diagram, "\n  }")
}
