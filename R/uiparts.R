#### Menu Items #####
introMenu <- menuItem(
  "Introduction",
  tabName = "dashboard",
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
    tabBox(
      title = "",height = 400,
      tabPanel("DataSet", dataTableOutput("dataTable")) ,
      tabPanel("Data Summary",dataTableOutput("dataSummary") )
      
    ),
    fluidRow(
      column(width = 6,
          infoBoxOutput("femaleCount"),
          infoBoxOutput("maleCount"),
      ),
      column(width = 6,
             infoBoxOutput("smokerCount"),
             infoBoxOutput("nonSmokerCount",width = 4)
      ),
      column(width = 6,
        includeHTML("www/brief.html"),
        actionButton("Corrleation","Go to Corrleation",icon =  icon("project-diagram"))
      )
    ),
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

  
