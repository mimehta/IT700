
ui <- dashboardPage(
  dashboardHeader(title = "Medical charges"),
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
                box(plotOutput( "smokerComp"), width= 6,title = "Comparision of charges"),
                box(plotOutput("smokercharts" ), width= 6,title = "Comparision of charges")
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
      expr = boxplot(nonsmokerData$charges,smokerData$charges, 
          at = c(1,2), col = c("blue","red"), 
          horizontal = T, notch = T, 
          names  = c("nonsmoker", "smoker"), 
          xlab="charges") 
      )
  output$smokercharts <- renderPlot(
  expr = {
      boxplot(charges ~ sex + smoker, sexsmokerData, horizontal = F, notch = T, 
          main="charges comparision for smoker- gender wise", at = c(1,2,3,4), 
          col = c("#226699","#FF5511"),outline=FALSE,names=c("","","","") )
      legend(1, 55000, c("female", "male"),  fill=c("#226699","#FF5511"))
      legend(1, 30000, c("non-smoker"),bg = "lightblue"  )
      legend(3.5, 5000, c("smoker"), bg = "#FFBB77")
    }
  )
  
}

shinyApp(ui, server)