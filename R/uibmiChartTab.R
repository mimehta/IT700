bmiChartTab <- tabItem(
  tabName = "bmiChart",
  box(width = 12, title = "Comparision of charges with BMI",collapsible = TRUE,
      fluidRow(
        box(width = 12,
            p("Let's check the impact on the charges with the BMI of the patients.")
        ),
        column(width = 6,
               tabBox(width = 12,
                      tabPanel(title = "BMI", plotOutput("bmiscatter"), 
                               p("No significant pattern or difference of charges with respect to the BMI")
                      ),
                      tabPanel(title = "Smoker", plotOutput("bmismoscatter"), 
                               p("The smoker charges are significant higher than the non-smoker across BMI")
                      )
               ),
        ),
        column(width = 5,
               sliderInput("bmi", "BMI:",min = min(round(agesmokerData$bmi,digits = 0)), max =  48,value = 30,step = 1,
                  animate=animationOptions(
                    interval = 700,
                    playButton = tags$div(HTML('<i class="fa fa-play fa-2x" style = "color:#007ea7;"></i>')),
                    pauseButton = tags$div(HTML('<i class="fa fa-pause fa-2x" style = "color:#f27059;"></i>'))
                  )
                ),
               plotOutput("bmiSmoker"), 
        )
      )
  ) 
)
