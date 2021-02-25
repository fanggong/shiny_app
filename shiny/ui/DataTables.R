if (!require(DT)) install.packages("DT")

tabItem(
  tabName = "DataTables",
  dataTableOutput("datatables_main")
)