
element_blank <- function() {
  ggplot2::element_blank()
}

element_line <- function(..., inherit.blank = TRUE) {
  ggplot2::element_line(..., inherit.blank = inherit.blank)
}

element_rect <- function(..., inherit.blank = TRUE) {
  ggplot2::element_rect(..., inherit.blank = inherit.blank)
}

element_text <- function(..., inherit.blank = TRUE) {
  ggplot2::element_text(..., inherit.blank = inherit.blank)
}

element_margin <- function(t = NULL, r = NULL, b = NULL, l = NULL, unit = "pt") {
  if (length(c(t, r, b, l)) != 4) return(NULL)
  else return(margin(t = t, r = r, b = b, l = l, unit = unit))
}

element_unit <- function(x = NULL, units = "points") {
  if (is.null(x)) return(NULL)
  else return(unit(x = x, units = units))
}

element_align <- function(number = NULL) {
  return(number)
}

element_title_position <- function(title_position = NULL) {
  return(title_position)
}

element_tag_position <- function(tag_position = NULL) {
  return(tag_position)
}

element_placement <- function(placement = NULL) {
  return(placement)
}

element_number <- function(number = NULL) {
  return(number)
}

element_direction <- function(direction = NULL) {
  return(direction)
}

element_position <- function(position = NULL) {
  return(position)
}

element_justification <- function(justification = NULL) {
  return(justification)
}

element_just <- function(just = NULL) {
  return(just)
}

element_logical <- function(logical = NULL) {
  return(logical)
}