shinyServer(function(input, output) {
    
    # pairs chart render
    output$pairs <- renderPlot({
      ggpairs(data = ass1.data, 
              title = "Pairs of Variables Mapped by Operators",
              progress = FALSE,
              columns = input$pairs_varibles,
              cardinality_threshold = 350,
              mapping = aes(colour = ass1.data$Operator)) +
        theme(plot.title = element_text(size = 20, face = "bold"))
    })
    
    
    # boxplot output render
    output$boxplot <- renderPlot({
      num.ass1.data <- scale(num.ass1.data, center = input$standardise, scale = input$standardise)
      thing <- Boxplot(y=num.ass1.data, col = c("dark grey", "white"), horizontal = F,
                   outline = input$outliers, range = input$range, 
                   main = "Boxplots of Sensor Data",ylab = NA, use.cols = TRUE,
                   notch = FALSE, varwidth = FALSE, las=2)
      })
    
    
    # mosaic output render
    output$mosaic <- renderPlot({
      formula <- as.formula(paste("~",paste(input$VariablesA, collapse = " + ")))
      vcd::mosaic(formula, data = cat.ass1.data, main = "Catagorical Visualistion of Data", shade = TRUE, legend = TRUE)
    })
    
    
    # datatable render
    output$data_table <- DT::renderDataTable({
      DT::datatable(data = as.data.frame(ass1.data), options = list(scrollX = T))
    })
    
    
    # data table summary render
    output$SummaryA1 <- renderPrint({
      summary(ass1.data)
    })
    
    output$SummaryA2 <- renderPrint({
      summary(as.data.frame(ass1.data))
    })
    
    
    # rising value chart render
    output$rising_value_chart <- renderPlot({
      if (input$topmost){
        d = cont.ass1.data[, c(10, 4, 5, 14, 18, 23, 25, 28)]
        for (col in 1:ncol(d)) {
          d[,col] <- d[order(d[,col]),col]
        }
        d <- scale(x = d, center = TRUE, scale = TRUE)
        mypalette <- rainbow(ncol(d))
        matplot(x = seq(1, 100, length.out = nrow(d)), y = d, type = "l", xlab = "Percentile", 
                ylab = "Values", lty = 1, lwd = 1, col = mypalette, main = "Rising Value Chart With The Most Gaps")
        legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette,
               ncol = round(ncol(d)^0.3), xpd = T, cex = 0.8)
      } 
      
      else{
        d = cont.ass1.data
        for (col in 1:ncol(d)) {
          d[,col] <- d[order(d[,col]),col]
        }
        d <- scale(x = d, center = TRUE, scale = TRUE)
        mypalette <- rainbow(ncol(d))
        matplot(x = seq(1, 100, length.out = nrow(d)), y = d, type = "l", xlab = "Percentile", 
                ylab = "Values", lty = 1, lwd = 1, col = mypalette, main = "Rising Value Chart")
        legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette,
               ncol = round(ncol(d)^0.3), xpd = T, cex = 0.8)
      }
    })

    
    # correlation matrix render
    output$Corrgram <- renderPlot({
      corrgram(ass1.data, 
               order = input$group_method, 
               abs = input$abs, 
               text.panel = panel.txt,
               cor.method = input$corr_method,
               main = "Correlation of Sensor Data")
    })
    
    
    # matrix scatter render
    output$matrix_scatter <- renderPlot({
      if (input$group_by == "Operator"){
        pairs(num.ass1.data[, input$scatter_varibles], col = ass1.data$Operator, lower.panel = NULL,
              main = "Matrix Plot Grouped by Operator", cex = 1)
        par(xpd = NA)
        legend("bottomleft", fill = (unique(ass1.data$Operator)), legend = c(levels(ass1.data$Operator)))
      }
      
      else if(input$group_by == "Price"){
        pairs(num.ass1.data[, input$scatter_varibles], col = ass1.data$Price, lower.panel = NULL,
              main = "Matrix Plot Grouped by Price ", cex = 1)
        par(xpd = NA)
        legend("bottomleft", fill = (unique(ass1.data$Price)), legend = c(levels(ass1.data$Price)))
      }
      
      else if(input$group_by == "Speed"){
        pairs(num.ass1.data[, input$scatter_varibles], col = ass1.data$Speed, lower.panel = NULL,
              main = "Matrix Plot Grouped by Speed ", cex = 1)
        par(xpd = NA)
        legend("bottomleft", fill = (unique(ass1.data$Speed)), legend = c(levels(ass1.data$Speed)))
      }
      
      else if(input$group_by == "Priority"){
        pairs(num.ass1.data[, input$scatter_varibles], col = ass1.data$Priority, lower.panel = NULL,
              main = "Matrix Plot Grouped by Priority ", cex = 1)
        par(xpd = NA)
        legend("bottomleft", fill = (unique(ass1.data$Priority)), legend = c(levels(ass1.data$Priority)))
      }
      
      else if(input$group_by == "Duration"){
        pairs(num.ass1.data[, input$scatter_varibles], col = ass1.data$Duration, lower.panel = NULL,
              main = "Matrix Plot Grouped by Duration ", cex = 1)
        par(xpd = NA)
        legend("bottomleft", fill = (unique(ass1.data$Duration)), legend = c(levels(ass1.data$Duration)))
      }
      
      else if(input$group_by == "Temp"){
        pairs(num.ass1.data[, input$scatter_varibles], col = ass1.data$Temp, lower.panel = NULL,
              main = "Matrix Plot Grouped by Temperature ", cex = 1)
        par(xpd = NA)
        legend("bottomleft", fill = (unique(ass1.data$Temp)), legend = c(levels(ass1.data$Temp)))
      }
      
      else if(input$group_by == "State"){
        pairs(num.ass1.data[, input$scatter_varibles], col = ass1.data$State, lower.panel = NULL,
              main = "Matrix Plot Grouped by State ", cex = 1)
        par(xpd = NA)
        legend("bottomleft", fill = (unique(ass1.data$State)), legend = c(levels(ass1.data$State)))
      }
      
      else if(input$group_by == "Class"){
        pairs(num.ass1.data[, input$scatter_varibles], col = ass1.data$Class, lower.panel = NULL,
              main = "Matrix Plot Grouped by Class ", cex = 1)
        par(xpd = NA)
        legend("bottomleft", fill = (unique(ass1.data$Class)), legend = c(levels(ass1.data$Class)))
      }
      
      else if(input$group_by == "Surface"){
        pairs(num.ass1.data[, input$scatter_varibles], col = ass1.data$Surface, lower.panel = NULL,
              main = "Matrix Plot Grouped by Surface ", cex = 1)
        par(xpd = NA)
        legend("bottomleft", fill = (unique(ass1.data$Surface)), legend = c(levels(ass1.data$Surface)))
      }
    })
    
    
    # missing values render
    getCleanData1 <- reactive({
      data <- ass1.data
      vRatio <- apply(X = data, MARGIN = 2, FUN = pMiss)
      data[, vRatio < input$VarThreshold]
    })  
    
    getCleanData2 <- reactive({
      data <- getCleanData1()
      oRatio <- apply(X = data, MARGIN = 1, FUN = pMiss)
      data[oRatio < input$ObsThreshold, ]
    })  

    output$missed_values <- renderPlot({
      vis_dat(getCleanData2()) +
        ggtitle("Missing Value Chart") +
        theme(plot.title = element_text(size = 20, face = "bold"))
    })
    
    
    # categorical correlation render
    output$price_speed <- renderPlot({
      cat.ass1.data %>% 
        count(Price, Speed) %>%  
        ggplot(aes(Price, Speed)) +
        geom_tile(mapping = aes(fill = n))
    })
    
    output$price_duration <- renderPlot({
      cat.ass1.data %>% 
        count(Price, Duration) %>%  
        ggplot(aes(Price, Duration)) +
        geom_tile(mapping = aes(fill = n))
    })
    
    output$price_class <- renderPlot({
      cat.ass1.data %>% 
        count(Price, Class) %>%  
        ggplot(aes(Price, Class)) +
        geom_tile(mapping = aes(fill = n))
    })

    output$price_temp <- renderPlot({
      cat.ass1.data %>% 
        count(Price, Temp) %>%  
        ggplot(aes(Price, Temp)) +
        geom_tile(mapping = aes(fill = n))
    })
    
    output$Operator_speed <- renderPlot({
      cat.ass1.data %>% 
        count(Operator, Speed) %>%  
        ggplot(aes(Operator, Speed)) +
        geom_tile(mapping = aes(fill = n))
    })
    
    output$Operator_duration <- renderPlot({
      cat.ass1.data %>% 
        count(Operator, Duration) %>%  
        ggplot(aes(Operator, Duration)) +
        geom_tile(mapping = aes(fill = n))
    })
    
    output$Operator_class <- renderPlot({
      cat.ass1.data %>% 
        count(Operator, Class) %>%  
        ggplot(aes(Operator, Class)) +
        geom_tile(mapping = aes(fill = n))
    })
    
    output$Operator_temp <- renderPlot({
      cat.ass1.data %>% 
        count(Operator, Temp) %>%  
        ggplot(aes(Operator, Temp)) +
        geom_tile(mapping = aes(fill = n))
    })
    
    
    # Numerical Histogram render
    data1 <- reactive({
      input$bins
    })
    
    output$plot_hist <- renderPlot({
      par(mfrow=c(3,3))
      hist_1 <- num2.ass1.data[, input$hist_option1]
      bins <- seq(min(hist_1, na.rm = T), max(hist_1, na.rm = T), length.out = input$bins + 1)
      hist(hist_1, breaks = bins, main = paste("Histgram of", input$hist_option1), col = "grey")
      
      hist_2 <- num2.ass1.data[, input$hist_option2]
      bins <- seq(min(hist_2, na.rm = T), max(hist_2, na.rm = T), length.out = input$bins + 1)
      hist(hist_2, breaks = bins, main = paste("Histgram of", input$hist_option2), col = "grey")
      
      hist_3 <- num2.ass1.data[, input$hist_option3]
      bins <- seq(min(hist_3, na.rm = T), max(hist_3, na.rm = T), length.out = input$bins + 1)
      hist(hist_3, breaks = bins, main = paste("Histgram of", input$hist_option3), col = "grey")
      
      hist_4 <- num2.ass1.data[, input$hist_option4]
      bins <- seq(min(hist_4, na.rm = T), max(hist_4, na.rm = T), length.out = input$bins + 1)
      hist(hist_4, breaks = bins, main = paste("Histgram of", input$hist_option4), col = "black", border = 'white')
      
      hist_5 <- num2.ass1.data[, input$hist_option5]
      bins <- seq(min(hist_5, na.rm = T), max(hist_5, na.rm = T), length.out = input$bins + 1)
      hist(hist_5, breaks = bins, main = paste("Histgram of", input$hist_option5), col = "black", border = 'white')
      
      hist_6 <- num2.ass1.data[, input$hist_option6]
      bins <- seq(min(hist_6, na.rm = T), max(hist_6, na.rm = T), length.out = input$bins + 1)
      hist(hist_6, breaks = bins, main = paste("Histgram of", input$hist_option6), col = "black", border = 'white')
      
      hist_7 <- num2.ass1.data[, input$hist_option7]
      bins <- seq(min(hist_7, na.rm = T), max(hist_7, na.rm = T), length.out = input$bins + 1)
      hist(hist_7, breaks = bins, main = paste("Histgram of", input$hist_option7), col = "white")
      
      hist_8 <- num2.ass1.data[, input$hist_option8]
      bins <- seq(min(hist_8, na.rm = T), max(hist_8, na.rm = T), length.out = input$bins + 1)
      hist(hist_8, breaks = bins, main = paste("Histgram of", input$hist_option8), col = "white")
      
      hist_9 <- num2.ass1.data[, input$hist_option9]
      bins <- seq(min(hist_9, na.rm = T), max(hist_9, na.rm = T), length.out = input$bins + 1)
      hist(hist_9, breaks = bins, main = paste("Histgram of", input$hist_option9), col = "white")
    })
    
    # Catagorical Histogram render
    output$plot_cat_hist <- renderPlot({
      par(mfrow=c(4,3))
      barplot(prop.table(table(cat.ass1.data$Operator)), space = 0, main = "Operator", col = "grey")
      barplot(prop.table(table(cat.ass1.data$Price)), space = 0, main = "Price", col = "grey")
      barplot(prop.table(table(cat.ass1.data$Speed)), space = 0, main = "Speed", col = "grey")
      barplot(prop.table(table(cat.ass1.data$Priority)), space = 0, main = "Priority", col = "black", border = 'white')
      barplot(prop.table(table(cat.ass1.data$Duration)), space = 0, main = "Duration", col = "black", border = 'white')
      barplot(prop.table(table(cat.ass1.data$Temp)), space = 0, main = "Temp", col = "black", border = 'white')
      barplot(prop.table(table(cat.ass1.data$State)), space = 0, main = "State", col = "white")
      barplot(prop.table(table(cat.ass1.data$Class)), space = 0, main = "Class", col = "white")
      barplot(prop.table(table(cat.ass1.data$Surface)), space = 0, main = "Surface", col = "white")
      barplot(prop.table(table(cat.ass1.data$Location)), space = 0, main = "Location", col = "grey")
      barplot(prop.table(table(cat.ass1.data$Agreed)), space = 0, main = "Agreed", col = "grey")
      
    })
    
})

