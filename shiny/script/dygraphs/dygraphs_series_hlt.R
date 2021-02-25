library(dygraphs)
dygraph(index_history) %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))