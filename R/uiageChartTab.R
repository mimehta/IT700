ageChartTab <- tabItem(
  tabName = "ageChart",
  box(width = 12, title = "Comparision of charges with age",collapsible = TRUE,
      fluidRow(
        box(width = 12,
            p("Let's check the impact on the charges with the age of the patients.")
        ),
        column(width = 6,
               tabBox(width = 12,
                      tabPanel(title = "Gender", plotOutput("genscatter"), 
                               p("No significant pattern or difference of charges concerning gender.")
                      ),
                      tabPanel(title = "Smoker", plotOutput("smoscatter"), 
                               p("The smoker charges are significant higher than the non-smoker across age.")
                      )
               ),
               p("The medical charges increase slightly with age. The average charge for 18 years old is $",
                 strong(round(mean(agesmokerData[agesmokerData$age == 18,]$charges), digits = 0 ) ), 
                 " and the average charge for 60 years old is $",
                 strong(round(mean(agesmokerData[agesmokerData$age == 60,]$charges), digits = 0 ) ), 
                 ". The minimum medical charge also increase with the age irrespective of age. ",
                 "The chart reveals three groups of charge, low charges consist of the non-smokers, and high charges consist of the smokers.",
               )
        ),
        column(width = 5,
               sliderInput("age", "Age:",min = min(csvData$age), max =  max(csvData$age),value = 18,
                animate=animationOptions(
                  interval = 700,
                  playButton = tags$div(HTML('<i class="fa fa-play fa-2x" style = "color:#007ea7;"></i>')),
                  pauseButton = tags$div(HTML('<i class="fa fa-pause fa-2x" style = "color:#f27059;"></i>'))
                ) 
               ),
               plotOutput("ageSmoker"), 
               p("The chart shows the distribution of the charges for the group smoker and non-smoker with respect to the selected age.",
                 "Even at the very young age of 18, The charges of the non-smoker patients are much higher.",
                 "The average cost of the",  strong(textOutput('selectedAge',inline = TRUE)),
                 "years old smoker patients  is $",
                 strong(textOutput("chargeDiff",inline = TRUE)),
                 "more than the non-smoker patients."
               )
        )
      )
  ) 
)