# element函数 ----
element_blank <- function() {
  ggplot2::element_blank()
}

element_line <- function(..., inherit.blank = TRUE) {
  element <- ggplot2::element_line(..., inherit.blank = inherit.blank)
  arrow <- element$arrow
  if (inherits(arrow, "arrow")) {
    return(element)
  } else if (is.na(arrow)) {
    element["arrow"] <- list(FALSE)
  } else if (inherits(arrow, "logical") & !arrow) {
    element["arrow"] <- list(NULL)
  }  
  element
}

element_rect <- function(..., inherit.blank = TRUE) {
  ggplot2::element_rect(..., inherit.blank = inherit.blank)
}

element_text <- function(..., inherit.blank = TRUE) {
  ggplot2::element_text(..., inherit.blank = inherit.blank)
}

element_margin <- function(margin = NULL) {
  return(margin)
}

element_unit <- function(unit = NULL) {
  return(unit)
}

element_align <- function(align = NULL) {
  return(align)
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
