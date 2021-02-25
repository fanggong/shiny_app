output$force_network <- renderForceNetwork({
  data(MisLinks)
  data(MisNodes)

  forceNetwork(
    Links = MisLinks, Nodes = MisNodes, Source = "source", Target = "target",
    Value = "value", NodeID = "name", Group = "group", opacity = 0.8
  )
})