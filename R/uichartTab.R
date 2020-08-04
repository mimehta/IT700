

chartTab <- tabItem(
  tabName = "charts",
  fluidRow(
    box(plotOutput("smokerComp"), width = 4, title = "Comparision of charges"),
    box(plotOutput("smokercharts"), width = 6, title = "Comparision of charges")
  ),
  fluidRow(
    box(plotOutput("scatter")),
    box(sliderInput("age", "Age:", 
          min = min(csvData$age), max =  max(csvData$age),value = 18,),
        plotOutput("ageSmoker")
    )
  ),
  fluidRow(
    box( infoBoxOutput("chargeDiff") )
  )
)

  
