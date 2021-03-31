library(dygraphs)
dygraph(dat) %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))
