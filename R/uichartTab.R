

chartTab <- tabItem(
  tabName = "charts",
  
  fluidRow(
    box(includeHTML("www/chartbrief.html"), width = 12, background = "light-blue"),
  ),
  fluidRow(
    box(title = "Distribution of charges",solidHeader = TRUE,
      column(checkboxGroupInput("smoBox","",inline = TRUE,
                         c("Smoker" = 2,"Non-smoker" = 1)
                         ) ,
      plotOutput("smoDistPlot"),width = 12),footer = "The charges of most of the non-smoker patients is less than 15K and peak is around 4K and most of the smoker patients charges are more than 15K with two peaks at 20K and 40K."
    ),
  ),
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

  
