UI <- fluidPage(theme = shinytheme("sandstone"),
                navbarPage("Eric Lee",
                          tabPanel(" ",
                                   navlistPanel(
                                     widths = c(2, 10),
                                     "Summary",
                                     tabPanel("How to use app",
                                              fluidRow(
                                                column(12,
                                                      sidebarPanel(
                                                        strong(h1("Fitting GLMNET Model")),
                                                        "This app is designed to be used sequentially. Please go thorugh each step.
                                                        In the first step you will be able to set your own missing variable and 
                                                        observation threshold. Everthing from step one onwards will adjust according
                                                        to the threshold set. In the exploratory data analyis section, you will be able
                                                        to see raw data should you choose to. In step three, a glmnet model will be fitted 
                                                        with the RMSE shown. If you are not satisfied with the RMSE, readjust the thresholds 
                                                        in step one. In the final step the outliers from each variable can be seen. These 
                                                        are labeled by there observation number so you can investigate it further in the 
                                                        data table provided"
                                                      )),
                                              )),
                                     
                                     "Step 1",
                                     tabPanel("Select missing value theshold",
                                              fluidRow(
                                                withSpinner(
                                                  plotOutput("missed_values", height = 600)
                                                )),
                                              
                                              fluidRow(
                                                sidebarPanel(width = 20,
                                                             sliderInput(inputId = "VarThreshold", label = "Threshold of variable missingness", 
                                                                         min = 1, max = 100, value = 50, post = "%"),
                                                             
                                                             sliderInput(inputId = "ObsThreshold", label = "Threshold of observations missingness", 
                                                                         min = 1, max = 100, value = 50, post = "%")
                                                             
                                                             ))
                                              ),
                                     
                                     "Step 2",
                                     tabPanel("Exploratory Data Analysis",
                                              tabsetPanel(
                                                tabPanel("Predicting Missing Values",
                                                         fluidRow(
                                                           column(2),
                                                           
                                                           column(8,
                                                                  align = "center",
                                                                  br(),
                                                                  br(),
                                                                  withSpinner(
                                                                    plotOutput("r.part.tree", height = 600)
                                                                  )),
                                                           
                                                           column(2)),
                                                         
                                                         fluidRow(
                                                           column(2),
                                                           
                                                           column(8,
                                                                  sidebarPanel(width = 12,
                                                                               strong("Interpreting the tree graph:"),
                                                                               "The percentage value is the proportion of observations considered for each
                                                                               node. The sum of lead nodes are 100%. The top value in each node is the aveage 
                                                                               number of mising observations for that node. Branches are formed to the left
                                                                               if decision is yes, otherwise the branch is formed to the right. For example,
                                                                               if show raw data is selected, when population density is less than 500, there is an 
                                                                               average of 1.1 missing variable per observation. Otherwise thes is 3.8",
                                                                               checkboxInput(inputId = "raw_data_rpart", label = "Show raw data", value = T))
                                                                  ),
                                                           
                                                           column(2)
                                                         )
                                                         ),
                                                
                                                tabPanel("Missing Value Chart",
                                                         fluidRow(
                                                           column(2,
                                                                  br(),
                                                                  br(),
                                                                  sidebarPanel(
                                                                    width=20,
                                                                    sliderInput(inputId = "n.set", label = "Select variable missingness", 
                                                                                min = 1, max = 15, step = 1, value = 6, post = "%"),
                                                                    checkboxInput(inputId = "raw_data_missing", label = "Show raw data", value = T),
                                                                    strong("Interpretation of missing values graph:"),
                                                                    "This graph shows sets of single or more grouped variables with the most missingness.
                                                                    For example, if show raw data is selected, the leftmost bar shows a single variable
                                                                    with the missing observations. The next bar show a group of variables that are missing
                                                                    together in many observations. Note that these variables may also be missing individually 
                                                                    or in another grouping in further observations")
                                                                  ),
                                                           
                                                           column(10, align = "center",
                                                                  br(),
                                                                  br(),
                                                                  withSpinner(
                                                                    plotOutput("missed_values2", height = 600)
                                                                  )),
                                                         )),
                                                
                                                tabPanel("Matrix Plot",
                                                         fluidRow(
                                                           column(2,
                                                                  br(),
                                                                  br(),
                                                                  sidebarPanel(
                                                                    width = 20,
                                                                    
                                                                    selectizeInput(inputId = "scatter_varibles", label = "Show variables",
                                                                                   choices = NA, 
                                                                                   multiple = TRUE, selected = NULL),
                                                                    
                                                                    selectInput("group_by",
                                                                                label = "Group by",
                                                                                choices = list("Politics" = "POLITICS", "Healthcare System" = "HEALTHCARE_BASIS", "Test/Train Data" = "OBS_TYPE"),
                                                                                selected = "HEALTHCARE_BASIS"),
                                                                    checkboxInput(inputId = "raw_data_matrix", label = "Show raw data", value = FALSE)
                                                                  )),
                                                           
                                                           column(10, align = "center",
                                                                  br(),
                                                                  br(),
                                                                  withSpinner(
                                                                    plotOutput("matrix_scatter", height = "700"),
                                                                  ))
                                                         )),
                                                
                                                tabPanel("Box Plot",
                                                         fluidRow(
                                                           column(2,
                                                                  br(),
                                                                  br(),
                                                                  sidebarPanel(
                                                                    width = 20,
                                                                    sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
                                                                    checkboxInput(inputId = "outliers", label = "Show outliers", value = FALSE),
                                                                    checkboxInput(inputId = "standardise", label = "Show standardized", value = T),
                                                                    checkboxInput(inputId = "raw_data_boxplot", label = "Show raw data", value = FALSE)
                                                                  )),
                                                           
                                                           column(10, align = "center",
                                                                  withSpinner(
                                                                    plotOutput("boxplot", height=700),
                                                                  ))
                                                         )),
                                                
                                                tabPanel("Correlation",
                                                         fluidRow(
                                                           column(2,
                                                                  br(),
                                                                  br(),
                                                                  sidebarPanel(
                                                                    width = 20,
                                                                    selectInput("corr_method",
                                                                                label = "Correlation method",
                                                                                choices = list("Pearson" = "pearson", "Kendall" = "kendall", "Spearman" = "spearman"),
                                                                                selected = "Pearson"),
                                                                    selectInput("group_method",
                                                                                label = "Grouping method",
                                                                                choices = list("None" = F, "OLO", "GW", "HC"),
                                                                                selected = "OLO"),
                                                                    checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = T),
                                                                    checkboxInput(inputId = "raw_data_corr", label = "Show raw data", value = FALSE)
                                                                  )),
                                                           
                                                           column(10, align = "center",
                                                                  br(),
                                                                  br(),
                                                                  withSpinner(
                                                                    plotOutput("Corrgram", height = 700)
                                                                  ))
                                                         )),
                                                
                                                tabPanel("Histogram",
                                                         tabPanel(strong("Numerical"),
                                                                  br(),
                                                                  fluidRow(
                                                                    column(2,
                                                                           sidebarPanel(
                                                                             width = 20,
                                                                             p(strong(h4("Select Variables"))),
                                                                             selectInput("hist_option1", label = "Grey", 
                                                                                         choices = NA,
                                                                                         selected = "POPULATION"),
                                                                             
                                                                             sliderInput("bins",
                                                                                         "Number of bins:",
                                                                                         min = 1,
                                                                                         max = 50,
                                                                                         value = 10),
                                                                             checkboxInput(inputId = "raw_data_hist", label = "Show raw data", value = FALSE)
                                                                           )),
                                                                    
                                                                    column(10, align = "center",
                                                                           withSpinner(plotOutput("plot_hist",
                                                                                                  height = 700))
                                                                    ))
                                                         )),
                                                
                                                tabPanel("Rising Value",
                                                         fluidRow(
                                                           column(12, align = "center",
                                                                  br(),
                                                                  br(),
                                                                  withSpinner(
                                                                    plotOutput("rising_value_chart", height = 700)
                                                                  )),
                                                           checkboxInput(inputId = "raw_data_rising", label = "Show raw data", value = FALSE)
                                                         )),
                                                
                                                tabPanel("Pairs Chart",
                                                         fluidRow(
                                                           br(),
                                                           br(),
                                                           column(2,
                                                                  sidebarPanel(
                                                                    width = 20,
                                                                    selectizeInput(inputId = "pairs_varibles", label = "Show variables",
                                                                                   choices = NA, 
                                                                                   multiple = TRUE, 
                                                                                   selected = c("POLITICS", "POLITICS", "AGE_MEDIAN", "INFANT_MORT")),
                                                                    checkboxInput(inputId = "raw_data_pairs", label = "Show raw data", value = T),
                                                                    "Note: If 'would you like to see the raw data' is not selected on the explantion page.
                                                                    This chart will not display values unless the threshold for missing variable and
                                                                    observations are above 58% in step one"
                                                                  )),
                                                           
                                                           column(10, align = "center",
                                                                  br(),
                                                                  br(),
                                                                  withSpinner(
                                                                    plotOutput("pairs", height = 700)
                                                                  ))
                                                         )),
                                                
                                                tabPanel("Mosaic Chart",
                                                         fluidRow(
                                                           column(2,
                                                                  br(),
                                                                  br(),
                                                                  sidebarPanel(width = 20,
                                                                               selectizeInput(inputId = "VariablesA", label = "Show variables",
                                                                                              choices = NA, 
                                                                                              multiple = TRUE, selected = NA),
                                                                               checkboxInput(inputId = "raw_data_mosaic", label = "Show raw data", value = FALSE)
                                                                  )),
                                                           
                                                           column(10, align = "center",
                                                                  br(),
                                                                  br(),
                                                                  withSpinner(
                                                                    plotOutput("mosaic",height = 700))
                                                           ))
                                                ),
                                                
                                                tabPanel("Data Table",
                                                         br(),
                                                         tabsetPanel(
                                                           tabPanel(strong("Table"),
                                                                    br(), 
                                                                    checkboxInput(inputId = "raw_data_table", label = "Show raw data", value = FALSE),
                                                                    withSpinner(
                                                                      dataTableOutput(outputId = "data_table",
                                                                                      height = 700))),
                                                           
                                                           tabPanel(strong("Summary"),
                                                                    verbatimTextOutput(outputId = "SummaryA1"),
                                                                    verbatimTextOutput(outputId = "SummaryA2")),
                                                         )),
                                              )),
                                     
                                     "Step 3",
                                     tabPanel("Select Imputation method",
                                              tabsetPanel(
                                                tabPanel("Actual vs Prediction",
                                                         br(),
                                                         fluidRow(
                                                           column(2,
                                                                  sidebarPanel(width = 20,
                                                                               selectInput(inputId = "ImpMethod", label = "Imputation method", 
                                                                                           choices = c("None", "KNN", "Partial Del","Median"), 
                                                                                           selected = "KNN"),
                                                                               sliderInput("KNN_imp", "Select KNN value", min = 1, max = 50, value = 5))
                                                                  
                                                           ),
                                                           
                                                           column(10,  align = "center",
                                                                  withSpinner(
                                                                    plotOutput("actual_pred_graph",
                                                                               height = 700)
                                                                  ))
                                                         )
                                                         
                                                ),
                                                
                                                tabPanel("Residual Plot",
                                                         br(),
                                                         align = "center",
                                                         withSpinner(
                                                           plotOutput("residual_plot",
                                                                      height = 700)
                                                         )),
                                                
                                                tabPanel("Summary",
                                                         br(),
                                                         withSpinner(
                                                           verbatimTextOutput(outputId = "imputation.summary")
                                                         )),
                                              )
                                              ),
                                     
                                     "Step 4",
                                     tabPanel("Outlier Detection",
                                              tabsetPanel(
                                                tabPanel("Test Data",
                                                         column(2,
                                                                br(),
                                                                sidebarPanel(
                                                                  width = 20,
                                                                  sliderInput(inputId = "coef_range", label = "Select IQR multiple", min = 0, max = 5, step = 0.1, value = 1.5),
                                                                  selectInput(inputId = "test_boxplot", label = "Select Variable", 
                                                                              choices = NA, 
                                                                              selected = NA)
                                                                )),
                                                         
                                                         withSpinner(
                                                           plotOutput("box_plot_residuals")
                                                         )),
                                                
                                                tabPanel("Training Data",
                                                         column(2,
                                                                br(),
                                                                sidebarPanel(
                                                                  width = 20,
                                                                  sliderInput(inputId = "coef_range_train", label = "Select IQR multiple", min = 0, max = 5, step = 0.1, value = 1.5),
                                                                  selectInput(inputId = "train_boxplot", label = "Select Variable", 
                                                                              choices = NA, 
                                                                              selected = NA)
                                                                )),
                                                         
                                                         withSpinner(
                                                           plotOutput("box_plot_residuals_train")
                                                         )),
                                                
                                                tabPanel("Training & Testing Data",
                                                         column(2,
                                                                br(),
                                                                sidebarPanel(
                                                                  width = 20,
                                                                  sliderInput(inputId = "coef_range_train_test", label = "Select IQR multiple", min = 0, max = 5, step = 0.1, value = 1.5),
                                                                  selectInput(inputId = "train_test_boxplot", label = "Select Variable", 
                                                                              choices = NA, 
                                                                              selected = NA)
                                                                )),
                                                         
                                                         withSpinner(
                                                           plotOutput("box_plot_residuals_train_test")
                                                         )),
                                                
                                                tabPanel("Data Table",
                                                         br(),
                                                         withSpinner(
                                                           dataTableOutput(outputId = "data_table_2",
                                                                           height = 700)
                                                         ))
                                              ))
                                     ))
                      

))


