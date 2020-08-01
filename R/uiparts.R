#### Menu Items #####
introMenu <- menuItem(
  "Introduction",
  tabName = "dashboard",
  icon = icon("info"),
  selected = T
)
dataSetmenu <- menuItem(
  "Data Summary",
  tabName = "dataset",
  icon = icon("table")
)
correlationmenu <- menuItem(
  "Correlation",
  tabName = "correlation",
  icon = icon("project-diagram")
)
chartMenu <- menuItem(
  "charts",
  tabName = "charts",
  icon = icon("chart-bar")
)

#### Body tabs ####
introTab <- tabItem(
  tabName = "dashboard",
  fluidRow(
    box(includeHTML("www/include.html"), width = 12, background = "light-blue"),
  ),
  fluidRow(
    box(dataTableOutput("dataTable"), width = 6, height = 4, title = "Explore the DataSet"),
    box(dataTableOutput("dataSummary"), width = 4,height = 4, title = "Summary")
  ),
  fluidRow(
    infoBoxOutput("femaleCount",width = 3),
    infoBoxOutput("maleCount",width = 3),
    infoBoxOutput("smokerCount",width = 3),
    infoBoxOutput("nonSmokerCount",width = 3)
  )
)
dataSetTab <- tabItem(
  tabName = "dataset",
  fluidRow(
    box(dataTableOutput("dataTable1"), width = 8, title = "The DataSet")
  ),
  fluidRow(
    box(dataTableOutput("dataSummary1"), width = 6, title = "Summary"),
    box(tableOutput("sexCount1"), width = 3, title = "Gender Summary"),
    box(tableOutput("smokerCount1"), width = 3, title = "Smoker Summary")
  )
)

corrTab <- tabItem(
  tabName = "correlation",
  fluidRow(
    box(plotOutput("corrPlot"), width = 6, title = "Corrlation graph"),
    box(tableOutput("corrCharges"), width = 2, title = "Corrlation values with charges")
  )
) 

chartTab <- tabItem(
  tabName = "charts",
  fluidRow(
    box(plotOutput("smokerComp"), width = 4, title = "Comparision of charges"),
    box(plotOutput("smokercharts"), width = 6, title = "Comparision of charges")
  ),
  fluidRow(
    box(plotOutput("scatter")),
    box(sliderInput("age", "Age:", min = min(csvData$age), max =  max(csvData$age),value = 18,),
        plotOutput("ageSmoker")
    )
  ),
  fluidRow(
    box( infoBoxOutput("chargeDiff") )
  )
)

  
