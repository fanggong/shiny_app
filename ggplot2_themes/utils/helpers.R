.reactiveValues_to_theme <- function(x) {
  x <- reactiveValuesToList(x)
  for (ele in names(x)) {
    if (inherits(x[[ele]], "function")){
      x[[ele]] <- x[[ele]]()
    }
  }
  attr(x, "class") <- c("theme", "gg")
  attr(x, "complete") <- TRUE
  attr(x, "validate") <- TRUE
  return(x)
}
