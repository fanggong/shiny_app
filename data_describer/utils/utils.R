
.convert_numeric <- function(vec, range, left, right) {
  fmt <- sprintf("vec %s %s & vec %s %s", 
                  ifelse(left, ">=", ">"),
                  range[1],
                  ifelse(right, "<=", "<"),
                  range[2])
  fmt <- eval(parse(text = fmt))
  vec <- ifelse(fmt, vec, NA)
  return(vec)
}

.convert_categorical <- function(vec, levels) {
  return(factor(vec, levels = levels, ordered = FALSE))
}

.convert_ordinal <- function(vec, levels) {
  return(factor(vec, levels = levels, ordered = TRUE))
}

.format_data <- function(dat, info) {
  data.table::setDF(dat)
  for (var in names(dat)) {
    idx <- which(info$var_name == var)
    if (info$var_type[idx] == "Numerical") {
      range <- info$range[idx]
      left <- str_detect(range, "\\[")
      right <- str_detect(range, "\\]")
      range <- str_match_all(range, "[0-9]+|[-]*Inf")[[1]][, 1]
      dat[var] <- .convert_numeric(dat[, var], range, left = FALSE, right = FALSE)
    } else if (info$var_type[idx] == "Categorical") {
      levels <- info$levels[idx]
      levels <- str_split(levels, ",")[[1]]
      dat[var] <- .convert_categorical(dat[, var], levels)
    } else if (info$var_type[idx] == "Ordinal") {
      levels <- info$levels[idx]
      levels <- str_split(levels, "<")[[1]]
      dat[var] <- .convert_ordinal(dat[, var], levels)
    }
  }
  return(dat)
}

.convert_range <- function(range) {
  left <- str_detect(range, "\\[")
  right <- str_detect(range, "\\]")
  range <- str_match_all(range, "[0-9]+|[-]*Inf")[[1]][, 1]
  sprintf("%s %s Value %s %s",
          range[1],
          ifelse(left, "≤", "<"),
          ifelse(right, "≤", "<"),
          range[2])
}

.proportion <- function(x, n) {
  x / n
}

.withSpinner <- function(ui_element) {
  shinycssloaders::withSpinner(
    ui_element, type = 5, size = 0.5, proxy.height = "50px", hide.ui = TRUE
  )
}

.spk_chr_box <- function(t) {
  sparkline::spk_chr(
    t, type = "box", width = 100, boxFillColor =  BOX_Fill_COLOR,
    medianColor = LINE_COLOR, boxLineColor = LINE_COLOR, lineColor = LINE_COLOR,
    outlierLineColor = LINE_COLOR, whiskerColor = LINE_COLOR
  )
}

.spk_chr_bar <- function(t) {
  t <- table(t, useNA = "ifany")
  value <- unname(t)
  level <- names(t)
  n_levels <- length(level)
  js <- sprintf(
    "{ offset: { %s } }",
    paste(paste0("'", (1:length(level))-1, "'"), paste0("'", level, "'"), sep = ":", collapse = ", ")
  )
  sparkline::spk_chr(
    value, type = "bar", width = 100, barColor = BAR_COLOR, chartRangeMin = 0,
    barWidth = pmin(10, 100/n_levels*0.8), barSpacing = pmin(2, 100/n_levels*0.2),
    tooltipFormat = "{{offset:offset}}: {{value}}",
    tooltipValueLookups = htmlwidgets::JS(js)
  )
}