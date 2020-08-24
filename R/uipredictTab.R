#library(intrval)

modelTab <- tabItem(
  tabName = "predicttab",
  
  fluidRow(
    box(includeHTML("www/predictbrief.html"), width = 12, background = "light-blue"),
  ),
  fluidRow(
    box(title = "Model Description", width = 12, collapsible = TRUE, collapsed = TRUE,
        fluidRow(
          column(width = 4,
            strong("Model Summary"),
            verbatimTextOutput("modelBrief"),
            p(strong('Coefficient Name:'), "The first column displays the name of the variable."),
            p(strong('Estimate:'), "These are the estimated values for the coefficients.",
              "For example, the estimated coefficient of age is ", strong(259.55), ".",
              " This means that an increase in 1 year of age will result in an increase of 295.55 in the value of charges."),
            p(strong("Standard Error:"), "These are the standard errors of the coefficients.",
              " It shows the average amount of the estimated coefficient of a parameter differs from the actual coefficient of a parameter." ),
            p("R-squared (R2) measures the proportion of variability, A high value of adjusted R-squared ",
              strong(textOutput('rmseText',inline = TRUE)), "shows that more than 74% of the variance in the data can be explained by the model." )
          ),
          column( width = 8,
            plotOutput("modelChart") ,
            p("The red line represent the result of the linear regression model."),
          ), 
        ),
    ),
  ),
  fluidRow(
    box(title = "Predict Medical Charges", width = 12,collapsible = TRUE,
      fluidRow(
        column( width = 6,
          checkboxInput("predsmoker", "Smoker", FALSE),
          sliderInput("predbmi", "BMI:",min = 24, max =  48,value = 30,step = 1,
            animate=animationOptions(
              interval = 700,
              playButton = tags$div(HTML('<i class="fa fa-play fa-2x" style = "color:#007ea7;"></i>')),
              pauseButton = tags$div(HTML('<i class="fa fa-pause fa-2x" style = "color:#f27059;"></i>'))
            ) 
          ),
          sliderInput("predage", "Age:",min = min(csvData$age), max =  max(csvData$age),value = 18,step = 1,
                animate=animationOptions(
                  interval = 700,
                  playButton = tags$div(HTML('<i class="fa fa-play fa-2x" style = "color:#007ea7;"></i>')),
                  pauseButton = tags$div(HTML('<i class="fa fa-pause fa-2x" style = "color:#f27059;"></i>'))
                ) 
          )
        ),
        column( width = 6,
          infoBoxOutput("predictCharges",width = 6),
          infoBoxOutput("predictError",width = 6),
        ),
        column(width = 6,
          p("The prediction of the medical charges for the person with the selected parameters. approx value is ", strong(textOutput("predCharge", inline = TRUE)), "$."),
        )
      )      
    )
  )

)
