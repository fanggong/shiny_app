

.format_data <- function(dat, type) {
  if (type == "numeric") {
    dat <- as.numeric(dat)
  } else if (type == "factor") {
    dat <- factor(dat, ordered = FALSE)
  } else if (type == "ordered") {
    dat <- ordered(dat)
  }
  dat
}


.format_statistic<- function(num, digits = 4) {
  format(round(num, digits), nsmall = digits)
}
