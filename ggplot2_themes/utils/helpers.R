.reactiveValues_to_theme <- function(x) {
  x <- reactiveValuesToList(x)
  for (ele in ELEMENTS) {
    if (inherits(x[[ele]], "function")){
      if (is.null(x[[ele]]())) {
        x[ele] <- list(NULL)
      } else {
        x[[ele]] <- x[[ele]]()
      }
    }
  }
  attr(x, "class") <- c("theme", "gg")
  attr(x, "complete") <- TRUE
  attr(x, "validate") <- TRUE
  return(x)
}

.get_attr_type <- function(x) {
  x <- x[1]
  if (is.null(x)) {
    return("NULL")
  } else if (is.na(x)) {
    return("NA") 
  } else if (inherits(x, "logical")) {
    if (x) {
      return("Value")
    } else {
      return("NA")
    }
  } else {
    return("Value")
  }
}
