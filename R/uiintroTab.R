introTab <- tabItem(
  tabName = "dashboard",
  fluidRow(
    box(includeHTML("www/include.html"), width = 12, background = "light-blue"),
  ),
  fluidRow(
    tabBox(
      title = "",height = 400,
      tabPanel("DataSet", dataTableOutput("dataTable")) ,
      tabPanel("Data Summary",dataTableOutput("dataSummary") )
      
    ),
    fluidRow(
      column(width = 6,
             infoBoxOutput("femaleCount"),
             infoBoxOutput("maleCount"),
      ),
      column(width = 6,
             infoBoxOutput("smokerCount"),
             infoBoxOutput("nonSmokerCount",width = 4)
      ),
      column(width = 6, background = "light-blue",
             box(includeHTML("www/brief.html"),background = "light-blue",width = 10),
      ),
      column(width = 6,
             actionButton("Corrleation","Go to Corrleation",icon =  icon("project-diagram"))
      )
    ),
  )
)
