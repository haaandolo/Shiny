shinyServer(function(input, output, session) {
  
  ########## Reactive Functions for different data ##########

  # Clean overall data without CODE
  getCleanData1 <- reactive({
    data <- covid.data.2
    vRatio <- apply(X = data, MARGIN = 2, FUN = pMiss)
    data[, vRatio < input$VarThreshold]
  })  
  
  getCleanData2 <- reactive({
    data <- getCleanData1()
    oRatio <- apply(X = data, MARGIN = 1, FUN = pMiss)
    data[oRatio < input$ObsThreshold, ]
  })
  
  # get clean data with CODE
  getCleanData3 <- reactive({
    data <- covid.data
    vRatio <- apply(X = data, MARGIN = 2, FUN = pMiss)
    data[, vRatio < input$VarThreshold]
  })  
  
  getCleanData4 <- reactive({
    data <- getCleanData3()
    oRatio <- apply(X = data, MARGIN = 1, FUN = pMiss)
    data[oRatio < input$ObsThreshold, ]
  })
  
  # test / train data
  getTrainData <- reactive({
    data <- getCleanData2()
    data.train <- data[data$OBS_TYPE == "Train", ]
  })
  
  getTestData <- reactive({
    data <- getCleanData2()
    data.test <- data[data$OBS_TYPE == "Test",]
  })
  
  # get numerical clean data
  getCleanDataNum1 <- reactive({
    data <- as.data.frame(covid.data[, c(3:11, 13:14)])
    vRatio <- apply(X = data, MARGIN = 2, FUN = pMiss)
    data[, vRatio < input$VarThreshold]
  })  
  
  getCleanDataNum2 <- reactive({
    data <- getCleanDataNum1()
    oRatio <- apply(X = data, MARGIN = 1, FUN = pMiss)
    data[oRatio < input$ObsThreshold, ]
  })
  
  # get catagotical clean data
  getCleanDataCat1 <- reactive({
    data <- as.data.frame(covid.data[, c(2, 12, 15)]) 
    vRatio <- apply(X = data, MARGIN = 2, FUN = pMiss)
    data[, vRatio < input$VarThreshold]
  })
  
  getCleanDataCat2 <- reactive({
    data <- getCleanDataCat1()
    oRatio <- apply(X = data, MARGIN = 1, FUN = pMiss)
    data[oRatio < input$ObsThreshold, ]
  })
  
  # get raw data or cleaned data
  getRawOrCleanData <- reactive({
    if (input$raw_data_missing == T || input$raw_data_pairs == T || 
        input$raw_data_corr == T || input$raw_data_matrix == T ||
        input$raw_data_rpart == T) {
      covid.data.2
    } else if (input$raw_data_missing == F || input$raw_data_pairs == F || 
               input$raw_data_corr == F || input$raw_data_matrix == F ||
               input$raw_data_rpart == F) {
      getCleanData2()
    }
  })
  
  getRawOrCleanData.num <- reactive({
    if (input$raw_data_boxplot == T || input$raw_data_rising == T ||
        input$raw_data_hist == T) {
      covid.data[, c(3:11, 13:14)]
    } else if (input$raw_data_boxplot == F || input$raw_data_rising == F ||
               input$raw_data_hist == F) {
      getCleanDataNum2()
    }
  })
  
  getRawOrCleanData.cat <- reactive({
    if (input$raw_data_mosaic == T) {
      covid.data.2[, c(2, 12, 15)]
    } else if (input$raw_data_mosaic == F) {
      getCleanDataCat2()
    }
  })

  ########## Render Plot Functions ##########
  
  # missing value
  output$missed_values <- renderPlot({
    covid.data = getCleanData2()
    vis_dat(covid.data) +
      ggtitle("Missing Value Chart") +
      theme(plot.title = element_text(size = 20, face = "bold"))
  })
  
  output$missed_values2 <- renderPlot({
    covid.data = getRawOrCleanData()
    naniar::gg_miss_upset(data = covid.data, nsets = input$n.set)
  })
  
  # box plot output render
  output$boxplot <- renderPlot({
    data.boxplot <- getRawOrCleanData.num()
    data.boxplot <- scale(data.boxplot, center = input$standardise, scale = input$standardise)
    thing <- Boxplot(y=data.boxplot, col = c("dark grey", "white"), horizontal = F,
                     outline = input$outliers, range = input$range, 
                     main = "Boxplots of Covid Data",ylab = NA, use.cols = TRUE,
                     notch = FALSE, varwidth = FALSE, las=2)
  })
  
  
  # pairs chart render
  output$pairs <- renderPlot({
    data <- getRawOrCleanData()
    ggpairs(data = data, 
            title = "Pairs of Covid Data Variables Grouped by Healthcare System",
            progress = FALSE,
            columns = input$pairs_varibles,
            cardinality_threshold = 1000,
            mapping = aes(colour = covid.data$HEALTHCARE_BASIS )) +
      theme(plot.title = element_text(size = 20, face = "bold"))
  })
  
  observe({updateSelectizeInput(session, inputId="pairs_varibles", label = NULL, choices = names(getRawOrCleanData()),
                                selected = c("POLITICS", "HEALTHCARE_COST", "POP_DENSITY"))})
  
  # mosaic output render
  output$mosaic <- renderPlot({
    formula <- as.formula(paste("~",paste(input$VariablesA, collapse = " + ")))
    vcd::mosaic(formula, data = getCleanDataCat2(), main = "Catagorical Visualistion of Covid Data", shade = TRUE, legend = TRUE)
  })
  
  observe({updateSelectizeInput(session, inputId="VariablesA", label = NULL, choices = names(getCleanDataCat2()),
                                selected = c("POLITICS", "HEALTHCARE_BASIS"))})
  
  
  # correlation matrix render
  output$Corrgram <- renderPlot({
    data <- getRawOrCleanData()
    corrgram(data, 
             order = input$group_method, 
             abs = input$abs, 
             text.panel = panel.txt,
             cor.method = input$corr_method,
             main = "Correlation of Covid Data")
  })
  
  # rising value chart render
  output$rising_value_chart <- renderPlot({
      d = getRawOrCleanData.num()
      for (col in 1:ncol(d)) {
        d[,col] <- d[order(d[,col]),col]
      }
      d <- scale(x = d, center = TRUE, scale = TRUE)
      mypalette <- rainbow(ncol(d))
      matplot(x = seq(1, 100, length.out = nrow(d)), y = d, type = "l", xlab = "Percentile", 
              ylab = "Values", lty = 1, lwd = 1, col = mypalette, main = "Rising Value Chart")
      legend(legend = colnames(d), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette,
             ncol = round(ncol(d)^0.3), xpd = T, cex = 0.8)
  })
  
  
  # matrix scatter render
  output$matrix_scatter <- renderPlot({
    matrix.data <- getRawOrCleanData()
    if (input$group_by == "POLITICS"){
      pairs(matrix.data[, input$scatter_varibles], col = matrix.data$POLITICS, lower.panel = NULL,
            main = "Matrix Plot Grouped by Politics", cex = 1)
      par(xpd = NA)
      legend("bottomleft", fill = (unique(matrix.data$POLITICS)), legend = c(levels(matrix.data$POLITICS)))
    }
    
    else if(input$group_by == "HEALTHCARE_BASIS"){
      pairs(matrix.data[, input$scatter_varibles], col = matrix.data$HEALTHCARE_BASIS, lower.panel = NULL,
            main = "Matrix Plot Grouped by Healthcare System ", cex = 1)
      par(xpd = NA)
      legend("bottomleft", fill = (unique(matrix.data$HEALTHCARE_BASIS)), legend = c(levels(matrix.data$HEALTHCARE_BASIS)))
    }
    
    else if(input$group_by == "OBS_TYPE"){
      pairs(matrix.data[, input$scatter_varibles], col = matrix.data$OBS_TYPE, lower.panel = NULL,
            main = "Matrix Plot Grouped by Train/Test Data ", cex = 1)
      par(xpd = NA)
      legend("bottomleft", fill = (unique(matrix.data$OBS_TYPE)), legend = c(levels(matrix.data$OBS_TYPE)))
    }
  })
  
  observe({updateSelectizeInput(session, inputId="scatter_varibles", label = NULL, choices = names(getRawOrCleanData()),
                             selected = c("GDP", "POP_DENSITY", "INFANT_MORT"))})
  
  # data table render
  output$data_table <- DT::renderDataTable({
    if (input$raw_data_table == T){
      covid_data <- covid.data
    } else if (input$raw_data_table == F) {
      covid_data <- getCleanData4()
    }
    DT::datatable(data = as.data.frame(covid_data), options = list(scrollX = T))
  })
  
  output$SummaryA1 <- renderPrint({
    if (input$raw_data_table == T){
      covid_data <- covid.data
    } else if (input$raw_data_table == F) {
      covid_data <- getCleanData4()
    }
    summary(covid_data)
  })
  
  output$SummaryA2 <- renderPrint({
    if (input$raw_data_table == T){
      covid_data <- covid.data
    } else if (input$raw_data_table == F) {
      covid_data <- getCleanData4()
    }
    summary(as.data.frame(covid_data))
  })
  
  # data table 2 render
  output$data_table_2 <- DT::renderDataTable({
    DT::datatable(data = as.data.frame(getCleanData4()), options = list(scrollX = T))
  })
  
  # Histogram
  output$plot_hist <- renderPlot({
    data <- getRawOrCleanData.num()
    hist_1 <- data[, input$hist_option1]
    bins <- seq(min(hist_1, na.rm = T), max(hist_1, na.rm = T), length.out = input$bins + 1)
    hist(hist_1, breaks = bins, main = paste("Histgram of", input$hist_option1), col = "darkgrey", border = 'white')
  })
  
  observe({updateSelectInput(session, inputId="hist_option1", label = NULL, choices = names(getRawOrCleanData.num()),
                                selected = c("HEALTHCARE_COST"))})
  
  # R.part missing value threshold
  output$r.part.tree <- renderPlot({
    covid.data.rpart <- getRawOrCleanData()
    covid.data.rpart$missingness <- apply(X = is.na(covid.data.rpart), MARGIN = 1, FUN = sum)
    tree <- caret::train(missingness ~ ., data = covid.data.rpart, method = "rpart", na.action = na.rpart)
    rpart.plot(tree$finalModel, 
               main = "Predicting the number of missing variables in an observation",
               roundint = TRUE, 
               clip.facs = TRUE)
  })
  
  # Testing data Boxplot with residual values shown
  output$box_plot_residuals <- renderPlot({
    coef <- input$coef_range
    covid.data.test <- getTestData()

    limits <- boxplot.stats(x = covid.data.test[[input$test_boxplot]], coef = coef)$stats
    covid.data.test$label <- ifelse(covid.data.test[[input$test_boxplot]] < limits[1] | covid.data.test[[input$test_boxplot]] > limits[5], rownames(covid.data), NA)
    
    ggplot(data = covid.data.test, mapping = aes(x = covid.data.test[[input$test_boxplot]], y = 1, label = label)) +
      geom_boxplot(coef = coef, outlier.colour = "red", fill = "light grey") +
      ggrepel::geom_text_repel(max.overlaps = 20) +
      labs(title = paste("Boxplots at IQR Multiplier of", coef, "(Testing Data)"), x = input$test_boxplot)+
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
      theme_classic()
  })
  
  observe({updateSelectizeInput(session, inputId="test_boxplot", label = NULL, choices = names(getCleanDataNum2()),
                                selected = c("GDP"))})
  
  
  # Training data Boxplot with residual values shown
  output$box_plot_residuals_train <- renderPlot({
    coef <- input$coef_range_train
    covid.data.train <- getTrainData()
    
    limits <- boxplot.stats(x = covid.data.train[[input$train_boxplot]], coef = coef)$stats
    covid.data.train$label <- ifelse(covid.data.train[[input$train_boxplot]] < limits[1] | covid.data.train[[input$train_boxplot]] > limits[5], rownames(covid.data), NA)
    
    ggplot(data = covid.data.train, mapping = aes(x = covid.data.train[[input$train_boxplot]], y = 1, label = label)) +
      geom_boxplot(coef = coef, outlier.colour = "red", fill = "light grey") +
      ggrepel::geom_text_repel(max.overlaps = 20) +
      labs(title = paste("Boxplots at IQR Multiplier of", coef, "(Training Data)"), x = input$train_boxplot)+
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
      theme_classic()
  })
  
  observe({updateSelectizeInput(session, inputId="train_boxplot", label = NULL, choices = names(getCleanDataNum2()),
                                selected = c("GDP"))})
  
  # Training and testing data Boxplot with residual values shown
  output$box_plot_residuals_train_test <- renderPlot({
    coef <- input$coef_range_train_test
    covid.data <- getCleanDataNum2()
    
    limits <- boxplot.stats(x = covid.data[[input$train_test_boxplot]], coef = coef)$stats
    covid.data$label <- ifelse(covid.data[[input$train_test_boxplot]] < limits[1] | covid.data[[input$train_test_boxplot]] > limits[5], rownames(covid.data), NA)
    
    ggplot(data = covid.data, mapping = aes(x = covid.data[[input$train_test_boxplot]], y = 1, label = label)) +
      geom_boxplot(coef = coef, outlier.colour = "red", fill = "light grey") +
      ggrepel::geom_text_repel(max.overlaps = 20) +
      labs(title = paste("Boxplots at IQR Multiplier of", coef, "(Training & Testing Data)"), x = input$train_test_boxplot)+
      theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
      theme_classic()
  })
  
  observe({updateSelectizeInput(session, inputId="train_test_boxplot", label = NULL, choices = names(getCleanDataNum2()),
                                selected = c("GDP"))})
  
  
  ########## Recipes ##########
  
  # the recipe itself
  imputationRecipe <- reactive({
    rec <- recipe(DEATH_RATE ~ ., getTrainData()) %>%
      update_role("OBS_TYPE", new_role = "split") 
    
    if (input$ImpMethod == "KNN") {
      rec <- step_impute_knn(rec, all_predictors(), neighbors = input$KNN_imp) %>%
        step_center(all_numeric(), -has_role("outcome")) %>%
        step_scale(all_numeric(), -has_role("outcome")) %>%
        step_dummy(all_predictors(), -all_numeric())
    } else if (input$ImpMethod == "Median") {
      rec <- step_impute_mode(rec) %>%
        step_center(all_numeric(), -has_role("outcome")) %>%
        step_scale(all_numeric(), -has_role("outcome")) %>%
        step_dummy(all_predictors(), -all_numeric())
    } else if (input$ImpMethod == "Partial Del") {
      rec <- step_naomit(rec, skip = T) %>%
        step_center(all_numeric(), -has_role("outcome")) %>%
        step_scale(all_numeric(), -has_role("outcome")) %>%
        step_dummy(all_predictors(), -all_numeric())
    }
    rec
  })  
  
  # training based on recipe
  get.imput.Model <- reactive({
    model <- caret::train(imputationRecipe(), 
                          data = getTrainData(), 
                          method = "glmnet")
    model
  })
  
  # predicting based on model
  model_predictions <- reactive({
    model_pred <- predict(get.imput.Model(), newdata = getTestData())
  })
  
  # output summary 
  output$imputation.summary <- renderPrint({
    print(get.imput.Model())
  })
  
  # actual vs prediction graph
  output$actual_pred_graph <- renderPlot({
    test_data <- getTestData()
    pred_resid_df <- data.frame(predictions = model_predictions(),
                                actual = test_data$DEATH_RATE,
                                residuals = test_data$DEATH_RATE - model_predictions(),
                                RMSE = caret::RMSE(model_predictions(), test_data$DEATH_RATE))
    
    rang <- range(c(pred_resid_df$actual, pred_resid_df$predictions))
    ggplot(data = pred_resid_df) +
      geom_point(aes(predictions, actual)) +
      geom_abline(slope = 1, col= "red") +
      theme_bw() +
      xlim(rang) +
      ylim(rang) +
      labs(title = paste("Death Rate Prediction vs Actual of Test Data, Using", 
                         input$ImpMethod, "Imputation", "(RMSE: ", round(pred_resid_df$RMSE, 2),")"),y = "Predicted", x = "Actual") +
      coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE)
  })
  
  output$residual_plot <- renderPlot({
    test_data <- getTestData()
    pred_resid_df <- data.frame(predictions = model_predictions(),
                                actual = test_data$DEATH_RATE,
                                residuals = test_data$DEATH_RATE - model_predictions())
    ggplot(pred_resid_df, aes(predictions, residuals)) +
      geom_point(col ="black") +
      geom_hline(aes(yintercept = 0, col = "red")) +
      xlab("Predicitons for Death Rate") +
      ylab("Residuals") +
      ggtitle("Residual Plot of Testing Data") +
      theme_bw() +
      theme(legend.position = "none")
  })

})

