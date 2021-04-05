#' Useful variables
#'
#' These variables will help us document each of the steps. They currently
#' tailored to single-cell analysis, but can be easily extended.
#'
#' @format Character lists.
#' @name wreportr-data
#' @examples
#' \dontrun{
#'  length(highlight_defaults)
#'  highlight_defaults[1:5]
#'
#'  length(preset_titles)
#'  preset_titles[1:5]
#'
#'  length(preset_description)
#'  preset_description[1:5]
#' }
NULL

#' @rdname wreportr-data
#' @format NULL
"highlight_defaults"

#' @rdname wreportr-data
#' @format NULL
"preset_titles"

#' @rdname wreportr-data
#' @format NULL
"preset_description"

utils::globalVariables(
  names = c("highlight_defaults", "preset_titles", "preset_description")
)
