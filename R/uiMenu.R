#### Menu Items #####
introMenu <- menuItem(
  "Introduction",
  tabName = "dashboard",
  icon = icon("table")
)

correlationmenu <- menuItem(
  "Correlation",
  tabName = "correlation",
  icon = icon("project-diagram")
)
chartMenu <- menuItem(
  "Charts",
  tabName = "charts",
  icon = icon("chart-bar"),startExpanded = TRUE,
  menuSubItem("smokerchart",tabName = "smokerChart"),
  menuSubItem("agechart",tabName = "ageChart"),
  menuSubItem("bmichart",tabName = "bmiChart")
)

predictmenu <- menuItem(
  "Prediction",
  tabName = "predicttab",
  icon = icon("crosshairs")
)