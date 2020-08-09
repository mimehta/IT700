
header <- dashboardHeader(title = "Medical charges", disable = F)

sidebar <- dashboardSidebar(sidebarMenu(id = "tabs",introMenu, correlationmenu, chartMenu))

body <- dashboardBody(tabItems(introTab, corrTab, chartTab,ageChartTab,bmiChartTab ) )

ui <- dashboardPage(header, sidebar, body , title = "Main Page")
