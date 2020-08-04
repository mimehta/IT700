introServer <- function(input, output, session) {

  output$corrCharges <-
    renderTable(
      sort(round(corrData[1:6, "charges"], digits = 2), decreasing = T),
      rownames = T,
      colnames = F,
      bordered = T
    )
  

  
}