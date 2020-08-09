

chartTab <- tabItem(
  tabName = "charts",
  
  fluidRow(
    box(includeHTML("www/chartbrief.html"), width = 12, background = "light-blue"),
  ),
  fluidRow(
    box(title = "Distribution of charges", width = 12,collapsible = TRUE,solidHeader = T,
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
    ),
    box(width = 12, title = "Comparision of charges",collapsible = TRUE,
      fluidRow(
        box(width = 12,
          p("Let's check the impact on the charges with the age of the patients.")
        ),
        column(width = 6,
          tabBox(width = 12,
            tabPanel(title = "Gender", plotOutput("genscatter"), 
              p("No significant pattern or difference of charges with respect to the gender.")
            ),
            tabPanel(title = "Smoker", plotOutput("smoscatter"), 
              p("The smoker charges are significant higher than the non-smoker across age.")
            )
          ),
          p("The medical charges increase slightly with age. The average charges for 18 years old is $",
            strong(round(mean(agesmokerData[agesmokerData$age == 18,]$charges), digits = 0 ) ), 
            " and average charges for 60 years old is $",
            strong(round(mean(agesmokerData[agesmokerData$age == 60,]$charges), digits = 0 ) ), 
            ". The minimum medical charges also increase with the age irrespective of age. ",
            "The chart reveals three groups of charges considering the treatment with low, medium, and high cost.",
          )
        ),
        column(width = 5,
          sliderInput("age", "Age:",min = min(csvData$age), max =  max(csvData$age),value = 18),
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
)
