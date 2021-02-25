if (!require(networkD3)) install.packages("networkD3")

tabItem(
  tabName = "networkD3",
  forceNetworkOutput("force_network")
)