
ui <- dashboardPage(
  dashboardHeader(title = "Medical charges",disable = F),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "dashboard", icon = icon("info"),selected = T ),
      menuItem("Data Summary", tabName = "dataset", icon = icon("table")),
      menuItem("Correlation", tabName = "correlation", icon = icon("project-diagram")),
      menuItem("charts", tabName = "charts", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      # Introduction tab content
      tabItem(tabName = "dashboard",
        fluidRow(
            box(includeHTML("www/include.html"),width = 12,background = "light-blue")
        )
      ),
      
      # Data Summary tab content
      tabItem(tabName = "dataset",
        fluidRow(
          box(dataTableOutput("dataTable"),width = 12, title = "The DataSet")
        ),
        fluidRow(
          box(dataTableOutput("dataSummary"),width = 6, title = "Summary"),
          box(tableOutput("sexCount"),width = 3, title = "Gender Summary"),
          box(tableOutput("smokerCount"),width = 3, title = "Smoker Summary")
        )
      ),
      
      # correlation tab content
      tabItem(tabName = "correlation",
         fluidRow(
          box(plotOutput( "corrPlot"), width= 6,title = "Corrlation graph"),
          box(tableOutput("corrCharges" ), width= 2,title = "Corrlation values with charges")
         )
      ) ,
      
      # charts tab content
      tabItem(tabName = "charts",
              fluidRow(
                box(plotOutput( "smokerComp"), width= 4,title = "Comparision of charges"),
                box(plotOutput("smokercharts" ), width= 6,title = "Comparision of charges")
              ),
              fluidRow(
                box(plotOutput("scatter")),
                box(  sliderInput("age", "Age:", min = min(csvData$age),max =  max(csvData$age), value = 18,),
                      plotOutput("ageSmoker")
                  )
              )
      )
    
      
    )
  )
)

server <- function(input, output, session) {
  
  output$corrCharges <- renderTable(sort( round(corrData[1:6,"charges"],digits = 2),decreasing = T ),
                                    rownames = T, colnames = F,bordered = T)
  output$corrPlot <- renderPlot(
    corrplot(corrData, method = "square", diag = F)
  )
  output$dataTable <- renderDataTable(csvData[,1:6],searchDelay = 1000,
                                      options = list(pageLength = 5,lengthChange=F,info = FALSE,searching = FALSE,lengthMenu=c(4,5)))
  output$dataSummary <- renderDataTable(summary(csvData[,c(1,3,7)])[c(1,3,4,6),],
                                        options = list(info = F, paging = F,searching = F, lengthMenu = F) )
  output$sexCount <- renderTable(sexCount,options = list(info = F, paging = F,searching = F, lengthMenu = F))
  output$smokerCount <- renderTable(smokerCount,options = list(info = F, paging = F,searching = F, lengthMenu = F))
  output$smokerComp <- renderPlot(
      expr = {
        boxplot(nonsmokerData$charges,smokerData$charges, 
          at = c(1,2), col = c("#226699","#FF5511"), 
          horizontal = F, notch = T, outline=FALSE,
          names  = c("nonsmoker", "smoker"), 
          xlab="charges")
      legend(0.5, 30000, c(   paste0("Mean:",smokerMean  ) ) , bg = "lightblue"  )
      legend(1.5, 50000, c(   paste0("Mean:",nonsmokerMean  ) ) , bg = "lightblue"  )
      }
      )
  output$smokercharts <- renderPlot(
  expr = {
      boxplot(charges ~ sex + smoker, sexsmokerData, horizontal = F, notch = T, 
          main="charges comparision for smoker- gender wise", at = c(1,2,3,4), 
          col = c("#226699","#FF5511"),outline=FALSE,names=c("","","","") )
      legend(1, 55000, c("female", "male"),  fill=c("#226699","#FF5511"))
      legend(1, 30000, c("non-smoker"),bg = "lightblue"  )
      legend(3.5, 5000, c("smoker"), bg = "lightblue")
    }
  )
  output$scatter <- renderPlot(
    {
      plot(agesmokerData$age, agesmokerData$charges,pch=16, col=c("#226699","#FF5511")[agesmokerData$smoker + 1])
      legend(17, 60000, c("non-smoker","smoker"),fill = c("#226699","#FF5511")  )
      
      abline(lm(charges ~ age, data = filter(agesmokerData,smoker ==0)),col ="#226699"  )
      abline(lm(charges ~ age, data = filter(agesmokerData,smoker ==1)) , col= "#FF5511")
    }
  )
  agesmok <- reactive(agesmokerData[agesmokerData$age == input$age & agesmokerData$smoker == 1,]$charges)
  agenonsmok <- reactive(agesmokerData[agesmokerData$age == input$age & agesmokerData$smoker == 0,]$charges)
  output$ageSmoker <- renderPlot(
    {
      beanplot(agenonsmok(),agesmok(), 
              at = c(1,2), col = list("#226699","#FF5511"),  what = c(1,1,1,0),side = "both",
              horizontal = F, notch = F, outline = F,axes = T,
              main="charges comparision", 
              names  = c("",""), 
              ylab="charges", xlab="nonsmoker:smoker")
      legend(0.5, 50000, c("non-smoker","smoker"),fill = c("#226699","#FF5511")  )
      
#      legend(1, 30000, paste0("Diff:",round(mean(agesmok()) - mean(agenonsmok()) , digits = 2)),bg = "#FF5511"  )
#      legend(0.8, 20000, paste0("mean:",round(mean(agenonsmok()) , digits = 2)),bg = "lightblue"  )
#      legend(1.6, 10000, paste0("mean:",round(mean(agesmok()) , digits = 2)),bg = "lightblue"  )
      
      }
  )
  
}

shinyApp(ui, server)