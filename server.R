server <- function(input, output, session) {
  observeEvent(session, {
    updateTabItems(session, "tabs", selected = "dashboard")
  })
  
  # add and remove tabs
  observeEvent(input$Corrleation, {
    updateTabItems(session, "tabs", selected = "correlation")
  })
  # add and remove tabs
  observeEvent(input$smokerchart, {
    updateTabItems(session, "tabs", selected = "smokerChart")
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
      color = 'maroon', fill = TRUE,width = 3
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
      col = smokCol,
      horizontal = F,
      notch = T,
      outline = FALSE,
      names  = c("nonsmoker", "smoker"),
      ylab = "charges"
    )
  })
  
  output$smokercharts <- renderPlot(expr = {
    boxplot(
      charges ~ sex + smoker,
      sexsmokerData,
      horizontal = F,
      notch = T,
      at = c(1, 2, 3, 4),
      col = genderCol,
      outline = FALSE,xlab = "",
      names = c("", "", "", "")
    )
    rect(0, 0, 2.5, 70000, col = adjustcolor(smokCol[1],alpha = 0.1)  )
    rect(2.5, 0, 4.5, 70000,  col = adjustcolor( smokCol[2],alpha = 0.1))
    legend(1, 55000, c("female", "male"), bty = "n", fill = genderCol)
    legend(1, 30000, c("non-smoker"), bty = "n")
    legend(3, 10000, c("smoker"), bty = "n")
  })
  output$scatter <- renderPlot({
    plot(
      agesmokerData$age,
      agesmokerData$charges,
      pch = 16,
      col = smokCol[agesmokerData$smoker + 1]
    )
    legend(17, 60000, smokText,bty = "n", fill = smokCol)

    abline(lm(charges ~ age, data = agesmokerData), lwd =3, col = "black" )
    abline(lm(charges ~ age, data = filter(agesmokerData, smoker == 0)), lwd =3,col = smokCol[1])
    abline(lm(charges ~ age, data = filter(agesmokerData, smoker == 1)) ,lwd =3, col = smokCol[2])
  })
  agesmok <-
    reactive(agesmokerData[agesmokerData$age == input$age &
                             agesmokerData$smoker == 1,]$charges)
  agenonsmok <-
    reactive(agesmokerData[agesmokerData$age == input$age &
                             agesmokerData$smoker == 0,]$charges)
  output$selectedAge <- renderText(input$age)
  output$ageSmoker <- renderPlot({
    beanplot(
      agenonsmok(),
      agesmok(),
      at = c(1, 2),
      col = list("#52796f","maroon"),
      what = c(1, 1, 1, 0),
      side = "both",
      horizontal = F,
      notch = F, 
      outline = F,
      axes = T,
      names  = c("", ""),
      ylab = "charges",
      xlab = "nonsmoker:smoker"
    )
    legend(0.5,
           50000,
           c("non-smoker", "smoker"),
           fill = smokCol)
  })
  
  output$chargeDiff <- renderText(
      round(mean(agesmok()) - mean(agenonsmok()),digits=0)
  )
  smoSel <-
    reactive(paste0("",input$smoBox))
  
  output$smoDistText <- renderText(smoSel())
  
  output$smoDistPlot <- renderPlot(width = 370, height = 370,
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
  
  output$genscatter <- renderPlot(
    {
      plot(
        agesmokerData$age,
        agesmokerData$charges,
        pch = 16,xlab = "Age", ylab = "Charges",
        col = genderCol[agesmokerData$sex + 1]
      )
      legend(17, 60000, c("female", "male"),bty = "n", fill = genderCol)
      
      abline(lm(charges ~ age, data = agesmokerData), lwd = 2, col = "black" )
      abline(lm(charges ~ age, data = filter(agesmokerData, sex == 0)), lwd = 2,col = genderCol[1])
      abline(lm(charges ~ age, data = filter(agesmokerData, sex == 1)) ,lwd = 2, col = genderCol[2])
    }
  )
  output$smoscatter <- renderPlot(
    {
      plot(
        agesmokerData$age,
        agesmokerData$charges,
        pch = 16,xlab = "Age", ylab = "Charges",
        col = smokCol[agesmokerData$smoker + 1]
      )
      legend(17, 60000, c("non-smoker", "smoker"),bty = "n", fill = smokCol)
      
      abline(lm(charges ~ age, data = agesmokerData), lwd = 2, col = "black" )
      abline(lm(charges ~ age, data = filter(agesmokerData, smoker == 0)), lwd = 2,col = smokCol[1])
      abline(lm(charges ~ age, data = filter(agesmokerData, smoker == 1)) ,lwd = 2, col = smokCol[2])
    }
  )
  
  output$agescatter <- renderPlot(
    {
      plot(
        agesmokerData$age,
        agesmokerData$charges,
        pch = 16,xlab = "BMI", ylab = "Charges",
        col = genderCol[agesmokerData$sex + 1]
      )
      legend(17, 60000, c("female", "male"),bty = "n", fill = genderCol)
      
      abline(lm(charges ~ age, data = agesmokerData), lwd = 2, col = "black" )
      abline(lm(charges ~ age, data = filter(agesmokerData, sex == 0)), lwd = 2,col = genderCol[1])
      abline(lm(charges ~ age, data = filter(agesmokerData, sex == 1)) ,lwd = 2, col = genderCol[2])
    }
  )
  output$bmiscatter <- renderPlot(
    {
      plot(
        agesmokerData$bmi,
        agesmokerData$charges,
        pch = 16,xlab = "BMI", ylab = "Charges",
        col = genderCol[agesmokerData$sex + 1]
      )
      legend(17, 60000, c("female", "male"),bty = "n", fill = genderCol)
      
      abline(lm(charges ~ bmi, data = agesmokerData), lwd = 2, col = "black" )
      abline(lm(charges ~ bmi, data = filter(agesmokerData, sex == 0)), lwd = 2,col = genderCol[1])
      abline(lm(charges ~ bmi, data = filter(agesmokerData, sex == 1)) ,lwd = 2, col = genderCol[2])
    }
  )
  
  output$bmismoscatter <- renderPlot(
    {
      plot(
        agesmokerData$bmi,
        agesmokerData$charges,
        pch = 16,xlab = "BMI", ylab = "Charges",
        col = smokCol[agesmokerData$smoker + 1]
      )
      legend(17, 60000, c("non-smoker", "smoker"),bty = "n", fill = smokCol)
      
      abline(lm(charges ~ bmi, data = agesmokerData), lwd = 2, col = "black" )
      abline(lm(charges ~ bmi, data = filter(agesmokerData, smoker == 0)), lwd = 2,col = smokCol[1])
      abline(lm(charges ~ bmi, data = filter(agesmokerData, smoker == 1)) ,lwd = 2, col = smokCol[2])
    }
  )
  
  bmismok <-
    reactive(agesmokerData[round(agesmokerData$bmi,digits = 0) == input$bmi &
                             agesmokerData$smoker == 1,]$charges)
  bminonsmok <-
    reactive(agesmokerData[round(agesmokerData$bmi,digits = 0) == input$bmi &
                             agesmokerData$smoker == 0,]$charges)
  
  output$bmiSmoker <- renderPlot({
    beanplot(
      bminonsmok(),
      bmismok(),
      at = c(1, 2),
      col = list("#52796f","maroon"),
      what = c(1, 1, 1, 0),
      side = "both",
      horizontal = F,
      notch = F, 
      outline = F,
      axes = T,
      names  = c("", ""),
      ylab = "charges",
      xlab = "nonsmoker:smoker"
    )
    legend(0.5,
           50000,
           c("non-smoker", "smoker"),
           fill = smokCol)
  })

  output$modelBrief <- {
    renderPrint(
      summary(model)$coefficients[,1:2] )
  }

  output$modelChart <- {
    renderPlot(
      ggplot(agesmokerData, aes(x = prediction, y = charges)) + 
      geom_point(color = "#007ea7", alpha = 0.7) + 
      geom_abline(color = "red",size=2) +
      ggtitle("Prediction vs. Actual Charges")
    )
  }

  output$rmseText <- renderText(rmse)

  predDF <-
    reactive(
      data.frame(
        age = input$predage, 
        bmi = input$predbmi, 
        smoker = as.numeric(input$predsmoker)
      )
    )
  predresult <- reactive(
    predict(model, predDF(),se.fit = TRUE)
  )
  
  output$predictCharges <- renderInfoBox(
    infoBox(
      "Charges", paste0(round(predresult()$fit, digits = 0),"$") , icon = icon("crosshairs"),
      color = "teal", fill = TRUE,width = 5
    )
  ) 
  output$predictError<- renderInfoBox(
    infoBox(
      "Variance", paste0(round(predresult()$se.fit, digits = 0),"$") , icon = icon("exclamation"),
      color = "orange", fill = TRUE,width = 5
    )
  ) 
  
  output$predCharge <- renderText(round(predresult()$fit, digits = 0))
  
  
  
}