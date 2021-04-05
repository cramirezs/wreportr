#' An S4 class to represent your workflow.
#'
#' @slot steps A list of each step output in your workflow.
#' @slot project.name The tilte of your project.
#' @slot author Person creating the report.
#' @slot affiliation Institute, Department, or University, etc.
#' @slot email Author's email.
#' @slot address Server address, Default: 'file://'.
#' @slot path Path (in disk) equivalent of the address, Default: ''.
setClass(
  Class = "Reportr",
  slots = list(
    steps = "list",
    project.name = "character",
    author = "character",
    link = "character",
    affiliation = "character",
    email = "character",
    address = "character",
    path = "character",
    markdown = "list"
  )
)

#' @title Create Reportr object
#' @description `create_wsteps` creates the Reportr object and the necessary
#' structure for the package's functions.
#' @param steps List of paths (and other arguments embedded in each element
#' of the list).
#' @param project.name The name of your workflow/project, Default = 'My project'.
#' @param author Author, Default = Sys.info user.
#' @param affiliation Author, Default = 'Lab'.
#' @param email Author's email.
#' @param address Server address, Default: 'file://'.
#' @param path Path equivalent of the address, Default: ''.
#' @param markdown Markdown specs.
#' @return A Reportr object.
#' @examples
#' \dontrun{
#'  if(interactive()) create_wsteps(list())
#' }
#' @rdname create_wsteps
#' @export
create_wsteps <- function(
  steps,
  project.name = "My project",
  author = Sys.info()[["user"]],
  affiliation = "Lab",
  email = paste0(Sys.info()[["user"]], "@email.com"),
  address = "file://",
  path = "",
  markdown = list("")
) {
  UseMethod(generic = "create_wsteps", object = steps)
}
create_wsteps.default = function(
  steps,
  project.name = "My project",
  author = Sys.info()[["user"]],
  affiliation = "Lab",
  email = paste0(Sys.info()[["user"]], "@email.com"),
  address = "file://",
  path = "",
  markdown = list("")
){
  if(is.null(address)) address = "file://"
  if(is.null(path)) path = ""
  new(
    Class = "Reportr",
    steps = steps,
    project.name = stringr::str_to_title(project.name),
    author = stringr::str_to_title(author),
    affiliation = stringr::str_to_title(affiliation),
    email = email,
    address = address,
    path = path,
    markdown = markdown
  )
}

show <- function(object) {
  UseMethod(generic = "show")
}
#' @importFrom methods new slot slotNames
setMethod(
  f = "show",
  signature = "Reportr",
  definition = function(object) {
    cat("Reportr object \n")
    for(i in slotNames(object)){
      value = slot(object, i)
      value = if(class(value) != "list") value else length(value)
      cat(stringr::str_to_sentence(i), ": '", value, "'\n", sep = "")
    }
})

stack <- function(object, steps = list(), verbose = TRUE) {
  UseMethod(generic = "stack")
}
setMethod(
  f = "stack",
  signature = "Reportr",
  definition = function(object, steps, verbose = TRUE) {
    cat("Adding", length(steps), "step(s) to", length(object@steps), "step(s)\n")
    names0 = names(object@steps); names1 = names(steps)
    if(isTRUE(names0 %in% names1)){
      warning("Some names are repeated ", names0[names0 %in% names1])
    }
    object@steps = c(object@steps, as.list(steps))
    object
})
