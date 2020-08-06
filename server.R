server <- function(input, output, session) {
  observeEvent(session, {
    updateTabItems(session, "tabs", selected = "dashboard")
  })
  
  # add and remove tabs
  observeEvent(input$Corrleation, {
    updateTabItems(session, "tabs", selected = "correlation")
  })
  # add and remove tabs
  observeEvent(input$Charts, {
    updateTabItems(session, "tabs", selected = "charts")
  })

  output$dataTable <-
    renderDataTable(
      csvData[, 1:7],
      searchDelay = 1000,
      options = list(
        pageLength = 5,
        lengthChange = F,
        info = FALSE,
        searching = FALSE,
        lengthMenu = c(4, 5)
      )
    )
  
  output$dataSummary <- renderDataTable(
    summary(csvData[, c(1, 3, 7)])[c(1, 3, 4, 6),],
    options = list(
      info = F,
      paging = F,
      searching = F,
      lengthMenu = F
    ))
  
  output$corrPlot <-
    renderPlot(corrplot(corrData, method = "square", diag = F, tl.col = "black"))
  
  output$smokercorr <- renderValueBox(
    valueBox(
      paste0(round(corrData["smoker", "charges"]*100, digits = 0),"%"), "Smoker", 
      color = "maroon", width = 2,icon = icon("smoking"),
    )
  ) 
  output$agecorr <- renderValueBox(
    valueBox(
      paste0(round(corrData["age", "charges"]*100, digits = 0),"%"), "Age", 
      color = "teal", width = 2,icon = icon("birthday-cake"),
    )
  ) 
  output$bmiCorr <- renderValueBox(
    valueBox(
      paste0(round(corrData["bmi", "charges"]*100, digits = 0),"%"), "BMI", 
      color = "orange", width = 2,icon = icon("heartbeat"),
    )
  ) 
  

  output$femaleCount <- renderInfoBox(
    infoBox(
      "#Female", sexCount[1,2] , icon = icon("female"),
      color = "aqua", fill = TRUE,width = 3
    )
  ) 
  output$maleCount <- renderInfoBox(
    infoBox(
      "#Male", sexCount[2,2] , icon = icon("male"),
      color = "orange", fill = TRUE,width = 3
    )
  ) 
  output$smokerCount <- renderInfoBox(
    infoBox(
      "#Smoker", smokerCount[2,2] , icon = icon("smoking"),
      color = "teal", fill = TRUE,width = 3
    )
  ) 
  output$nonSmokerCount <- renderInfoBox(
    infoBox(
      "#non-smoker", smokerCount[1,2] , icon = icon("smoking-ban"),
      color = "maroon", fill = TRUE,width = 3
    )
  ) 
  output$sexCount <-
    renderTable(sexCount,
                options = list(
                  info = F,
                  paging = F,
                  searching = F,
                  lengthMenu = F
                ))
  output$smokerCount1 <-
    renderTable(smokerCount,
                options = list(
                  info = F,
                  paging = F,
                  searching = F,
                  lengthMenu = F
                ))
  output$smokerComp <- renderPlot(expr = {
    boxplot(
      nonsmokerData$charges,
      smokerData$charges,
      at = c(1, 2),
      col = c("#226699", "#FF5511"),
      horizontal = F,
      notch = T,
      outline = FALSE,
      names  = c("nonsmoker", "smoker"),
      xlab = "charges"
    )
    legend(0.5, 30000, c(paste0("Mean:", smokerMean)) , bg = "lightblue")
    legend(1.5, 50000, c(paste0("Mean:", nonsmokerMean)) , bg = "lightblue")
  })
  output$smokercharts <- renderPlot(expr = {
    boxplot(
      charges ~ sex + smoker,
      sexsmokerData,
      horizontal = F,
      notch = T,
      main = "charges comparision for smoker- gender wise",
      at = c(1, 2, 3, 4),
      col = c("#226699", "#FF5511"),
      outline = FALSE,
      names = c("", "", "", "")
    )
    legend(1, 55000, c("female", "male"),  fill = c("#226699", "#FF5511"))
    legend(1, 30000, c("non-smoker"), bg = "lightblue")
    legend(3.5, 5000, c("smoker"), bg = "lightblue")
  })
  output$scatter <- renderPlot({
    plot(
      agesmokerData$age,
      agesmokerData$charges,
      pch = 16,
      col = c("#226699", "#FF5511")[agesmokerData$smoker + 1]
    )
    legend(17,
           60000,
           c("non-smoker", "smoker"),
           fill = c("#226699", "#FF5511"))
    
    abline(lm(charges ~ age, data = filter(agesmokerData, smoker == 0)), col =
             "#226699")
    abline(lm(charges ~ age, data = filter(agesmokerData, smoker == 1)) , col = "#FF5511")
  })
  agesmok <-
    reactive(agesmokerData[agesmokerData$age == input$age &
                             agesmokerData$smoker == 1,]$charges)
  agenonsmok <-
    reactive(agesmokerData[agesmokerData$age == input$age &
                             agesmokerData$smoker == 0,]$charges)
  output$ageSmoker <- renderPlot({
    beanplot(
      agenonsmok(),
      agesmok(),
      at = c(1, 2),
      col = list("#226699", "#FF5511"),
      what = c(1, 1, 1, 0),
      side = "both",
      horizontal = F,
      notch = F, 
      outline = F,
      axes = T,
      main = "charges comparision",
      names  = c("", ""),
      ylab = "charges",
      xlab = "nonsmoker:smoker"
    )
    legend(0.5,
           50000,
           c("non-smoker", "smoker"),
           fill = c("#226699", "#FF5511"))
    
    #      legend(1, 30000, paste0("Diff:",round(mean(agesmok()) - mean(agenonsmok()) , digits = 2)),bg = "#FF5511"  )
    #      legend(0.8, 20000, paste0("mean:",round(mean(agenonsmok()) , digits = 2)),bg = "lightblue"  )
    #      legend(1.6, 10000, paste0("mean:",round(mean(agesmok()) , digits = 2)),bg = "lightblue"  )
    
  })
  
  output$chargeDiff <- renderInfoBox(
    infoBox(
      title = paste0("age#", input$age) , 
      value = round(mean(agesmok()) - mean(agenonsmok()),digits=2),color = "red",
        icon = icon("arrow-alt-circle-up")
    )
  )
  smoSel <-
    reactive(paste0("",input$smoBox))
  
  output$smoDistText <- renderText(smoSel())
  
  output$smoDistPlot <- renderPlot(
    {
      hist(csvData$charges, freq = FALSE,col = "black",density = 12, 
          main="", xlab = "Charges", ylim = c(0,0.00009))
      abline(v=mean(csvData$charges), lwd=3, lty=3)
      if( !is.null(input$smoBox)) {
        if( 1 %in% input$smoBox) {
          lines(density(csvData$charges[csvData$smoker=='N']),col = smokCol[1], lwd=3 )
          legend(30000, 0.000065, smokText[1], fill=smokCol[1],bty = "n")
        } 
        if( 2 %in% input$smoBox ) {
          lines(density(csvData$charges[csvData$smoker=='Y']),col = smokCol[2], lwd=3 )
          legend(30000, 0.000055, smokText[2], fill=smokCol[2],bty = "n")
        }
      }
      legend(30000, 0.00006, "charges", col = "black",density = 15,bty = "n")
      legend(11000, 0.00007, "Average",bty = "n")
    }
  )
  
}