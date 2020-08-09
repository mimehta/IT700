
corrTab <- tabItem(
  tabName = "correlation",
  fluidRow(
    box(includeHTML("www/corbrief.html"), width = 10, background = "light-blue")
  ),
  fluidRow(
    fluidRow(
      box("Correlation matrix",plotOutput("corrPlot"),solidHeader = TRUE,status = "primary" ),
      column(width = 6, background = "light-blue",
             box(includeHTML("www/corsummary.html"),background = "light-blue",width = 10),
      ),
      column(width = 6,
             valueBoxOutput("smokercorr",width = 3),
             valueBoxOutput("agecorr",width = 3),
             valueBoxOutput("bmiCorr",width = 3)
      ),
#      column(width = 6,
#             actionButton("smoker-chart","Go to Charts",icon =  icon("chart-area"))
#      )
    ),
  )
) 
