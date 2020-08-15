
header <- dashboardHeader(title = "Medical charges", disable = F)

sidebar <- dashboardSidebar(sidebarMenu(id = "tabs",introMenu, correlationmenu, chartMenu,predictmenu))

body <- dashboardBody(tabItems(introTab, corrTab, chartTab,ageChartTab,bmiChartTab,modelTab ) )

ui <- dashboardPage(header, sidebar, body , title = "Main Page")
