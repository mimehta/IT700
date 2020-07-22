

ui <- fluidPage(
  titlePanel("Analysis of Insurance Data"),
  br(),
  includeHTML("www/include.html"),
  br(),
  fluidRow(
    column(4,  dataTableOutput("dataTable")),
    column(4,  dataTableOutput("dataSummary"))
  ),

  htmlTemplate("www/template.html"
        ,corrPlot = plotOutput( "corrPlot",width = 400, height = 300)
        ,corrCharges= tableOutput("corrCharges" )
  ),
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
}
shinyApp(ui, server) 