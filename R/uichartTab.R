

chartTab <- tabItem(
  tabName = "charts",
  
  fluidRow(
    box(includeHTML("www/chartbrief.html"), width = 12, background = "light-blue"),
  ),
  fluidRow(
    box(title = "Distribution of charges", width = 12,collapsible = TRUE,
        fluidRow(
        column(width = 4,
          checkboxGroupInput("smoBox","",inline = TRUE,selected = c(2,1),
                         c("Smoker" = 2,"Non-smoker" = 1)
                         ) ,
          plotOutput("smoDistPlot")
        ),
        column( width = 3,
          plotOutput("smokerComp") 
        ), 
        column( width = 5, 
          plotOutput("smokercharts")
        ),
        ),
#        fluidRow(
          column(width = 4,
                 p("The charges of most of the non-smoker patients are less than 15K and peak is around 4K."),
                 p("The charges of most of the smoker patients are more than 15K with two peaks at 20K and 40K."),
          ),
          column(width = 3,
                 p("The average charges:"),
                 p("Smoker ", strong(smokerMean, "$ ","Non-Smoker ", strong(nonsmokerMean, "$"))),
          ),
          column(width = 5,
                 p("The Smoker Male pays ", strong(diffMale, "$") , " higher than non-smoker."),
                 p("The Smoker Female pays ", strong(diffFemale, "$") , " higher than non-smoker."),
          )
 #       )
    ),
    box(width = 12, title = "Comparision of charges",collapsible = TRUE,
        fluidRow(
          box(width = 12,
            p("dd")
          )
        )
    )
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

  
