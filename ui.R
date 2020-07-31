
header <- dashboardHeader(title = "Medical charges", disable = F)

sidebar <- dashboardSidebar(sidebarMenu(introMenu, dataSetmenu, correlationmenu, chartMenu))

body <- dashboardBody(tabItems(introTab, dataSetTab, corrTab, chartTab))
ui <- dashboardPage(header, sidebar, body , title = "Main Page")
