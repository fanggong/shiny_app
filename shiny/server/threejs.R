output$threejs_main <- renderGlobe({
  data(flights)
  dest <- factor(sprintf("%.2f:%.2f",flights[,3], flights[,4]))
  freq <- sort(table(dest), decreasing=TRUE)
  frequent_destinations <- names(freq)[1:10]
  idx <- dest %in% frequent_destinations
  frequent_flights <- flights[idx, ]
  ll <- unique(frequent_flights[,3:4])
  globejs(
    lat=ll[, 1], long=ll[, 2], arcs=frequent_flights,bodycolor="#aaaaff", 
    arcsHeight=0.3, arcsLwd=2,arcsColor="#ffff00", arcsOpacity=0.15,
    atmosphere=TRUE, color="#00aaff", pointsize=0.5
  )
})