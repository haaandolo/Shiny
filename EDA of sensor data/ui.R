UI <- fluidPage(
      navbarPage("Eric Lee",
        tabPanel("Matrix Plot",
                 fluidRow(
                   column(2,
                          sidebarPanel(
                            width = 20,
                            selectizeInput(inputId = "scatter_varibles", label = "Show variables",
                                           choices = list("Y", "Sensor 1" = "sensor1", "Sensor 2" = "sensor2", "Sensor 3" = "sensor3", "Sensor 4" = "sensor4", "Sensor 5" = "sensor5",
                                                          "Sensor 6" = "sensor6", "Sensor 7" = "sensor7", "Sensor 8" = "sensor8", "Sensor 9" = "sensor9", "Sensor 10" = "sensor10",
                                                          "Sensor 11" = "sensor12", "Sensor 13" = "sensor14", "Sensor 15" = "sensor16", "Sensor 17" = "sensor18", "Sensor 19" = "sensor20",
                                                          "Sensor 21" = "sensor21", "Sensor 22" = "sensor22", "Sensor 23" = "sensor23", "Sensor 24" = "sensor24", "Sensor 25" = "sensor25",
                                                          "Sensor 26" = "sensor26", "Sensor 27" = "sensor27", "Sensor 28" = "sensor28", "Sensor 29" = "sensor29", "Sensor 30" = "sensor30"), 
                                           multiple = TRUE, selected = list("Y", "sensor1", "sensor2", "sensor3", "sensor4", "sensor9", "sensor22", "sensor24", "sensor27")),
                            
                            selectInput("group_by",
                                        label = "Group by",
                                        choices = list("Operator", "Price", "Speed", "Priority", "Duration", "Temp", "State", "Class", "Surface"),
                                        selected = "Operator"),
                          )
                          ),
                   
                   column(10, align = "center",
                          withSpinner(
                            plotOutput("matrix_scatter", height = "700"),
                          )
                   )
                 )
        ),
                   
        
        tabPanel("Correlation",
                 tabsetPanel(
                   tabPanel(strong("Numerical"),
                            fluidRow(
                              column(2,
                                     br(),
                                     sidebarPanel(
                                       width = 20,
                                       # drop down for method input
                                       selectInput("corr_method",
                                                   label = "Correlation method",
                                                   choices = list("Pearson" = "pearson", "Kendall" = "kendall", "Spearman" = "spearman"),
                                                   selected = "Pearson"),
                                       
                                       # drop down for grouping method
                                       selectInput("group_method",
                                                   label = "Grouping method",
                                                   choices = list("None" = F, "OLO", "GW", "HC"),
                                                   selected = "OLO"),
                                       
                                       checkboxInput(inputId = "abs", label = "Uses absolute correlation", value = T)
                                     )
                              ),
                              
                              column(10, align = "center",
                                     withSpinner(
                                       plotOutput("Corrgram", height = 700)
                                     ))
                            )
                            ),
                   
                   tabPanel(strong("Catagorical"),
                            br(),
                            fluidRow(
                              column(3, align = "center",
                                     strong("Correlation of Price and Speed"),
                                     withSpinner(
                                       plotOutput("price_speed", height = 300))
                                     ),
                              
                              column(3, align = "center",
                                     strong("Correlation of Price and Duration"),
                                     withSpinner(
                                       plotOutput("price_duration", height = 300))),
                              
                              column(3, align = "center",
                                     strong("Correlation of Price and Class"),
                                     withSpinner(
                                       plotOutput("price_class", height = 300))),
                              
                              column(3, align = "center",
                                     strong("Correlation of Price and Temperature"),
                                     withSpinner(
                                       plotOutput("price_temp", height = 300 ))),
                            ),
                            
                            fluidRow(
                              column(3, align = "center",
                                     strong("Correlation of Operator and Speed"),
                                     withSpinner(
                                       plotOutput("Operator_speed", height = 300))
                              ),
                              
                              column(3, align = "center",
                                     strong("Correlation of Operator and Duration"),
                                     withSpinner(
                                       plotOutput("Operator_duration", height = 300))),
                              
                              column(3, align = "center",
                                     strong("Correlation of Operator and Class"),
                                     withSpinner(
                                       plotOutput("Operator_class", height = 300))),
                              
                              column(3, align = "center",
                                     strong("Correlation of Operator and Temperature"),
                                     withSpinner(
                                       plotOutput("Operator_temp", height = 300 ))),
                            )
                            )
                 ),

            ),
        
        tabPanel("Pairs",
                 fluidRow(
                   column(2,
                          sidebarPanel(
                            width = 20,
                            selectizeInput(inputId = "pairs_varibles", label = "Show variables",
                                           choices = names(ass1.data), multiple = TRUE, 
                                           selected = list("Operator", "Priority", "sensor1", "sensor2")),
                          )
                        ),
                   
                   column(10, align = "center",
                          withSpinner(
                            plotOutput("pairs", height = 700)
                          )
                          )

                 )
                 ),
        

        tabPanel("Missed Values",
                 fluidRow(
                   column(2,
                          sidebarPanel(
                            width=20,
                            sliderInput(inputId = "VarThreshold", label = "Threshold of variable missingness", 
                                        min = 1, max = 100, value = 50, post = "%"),
                            sliderInput(inputId = "ObsThreshold", label = "Threshold of observations missingness", 
                                        min = 1, max = 100, value = 50, post = "%")
                          )),
                   
                   column(10, align = "center",
                          withSpinner(
                            plotOutput("missed_values", height = 700)
                          ))
                 ),
        ),
        
        tabPanel("Boxplots",
                 fluidRow(
                   column(2,
                          sidebarPanel(
                            width = 20,
                            sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
                            checkboxInput(inputId = "outliers", label = "Show outliers", value = FALSE),
                            checkboxInput(inputId = "standardise", label = "Show standardized", value = FALSE)
                          )),
                   
                   column(10, align = "center",
                          withSpinner(
                            plotOutput("boxplot", height=700),
                          )
                          )
                 )),
        
        tabPanel("Mosaic",
                 fluidRow(
                   column(2,
                          sidebarPanel(width = 20,
                                       selectizeInput(inputId = "VariablesA", label = "Show variables",
                                                      choices = names(cat.ass1.data), multiple = TRUE, selected = list("Duration", "Operator", "State")),
                          )),
                   
                   column(10, align = "center",
                          withSpinner(
                            plotOutput("mosaic",height = 700))))),
        
        tabPanel("Rising Value",
                 fluidRow(
                   column(2,
                          sidebarPanel(width = 15,
                                       checkboxInput(inputId = "topmost", label = "Show the top most gaps", value = F))),
                   
                   column(10, align = "center",
                          plotOutput("rising_value_chart", height = 700))
                 ),
                 ),
        
        tabPanel("Histogram",
                 tabsetPanel(
                   tabPanel(strong("Numerical"),
                            br(),
                            fluidRow(
                              column(2,
                                     sidebarPanel(
                                       width = 20,
                                       p(strong(h4("Select Variables"))),
                                       selectInput("hist_option1", label = "Grey", choices = names(num2.ass1.data),selected = "sensor3"),
                                       
                                       selectInput("hist_option2", label = NULL, choices = names(num2.ass1.data),selected = "sensor4"),
                                       
                                       selectInput("hist_option3", label = NULL, choices = names(num2.ass1.data),selected = "sensor9"),
                                       
                                       selectInput("hist_option4", label = "Black", choices = names(num2.ass1.data),selected = "sensor13"),
                                       
                                       selectInput("hist_option5", label = NULL, choices = names(num2.ass1.data),selected = "sensor17"),
                                       
                                       selectInput("hist_option6", label = NULL, choices = names(num2.ass1.data),selected = "sensor22"),
                                       
                                       selectInput("hist_option7", label = "White", choices = names(num2.ass1.data),selected = "sensor24"),
                                       
                                       selectInput("hist_option8", label = NULL, choices = names(num2.ass1.data),selected = "Y"),
                                       
                                       selectInput("hist_option9", label = NULL, choices = names(num2.ass1.data),selected = "sensor1"),
                                       
                                       sliderInput("bins",
                                                   "Number of bins:",
                                                   min = 1,
                                                   max = 50,
                                                   value = 10)
                                     )
                              ),
                              
                              column(10, align = "center",
                                     withSpinner(plotOutput("plot_hist",
                                                            height = 700))
                              )
                            )
                            ),
                   
                   tabPanel(strong("Catagorical"),
                            fluidRow(
                              column(12, align = "center",
                                     withSpinner(plotOutput("plot_cat_hist",
                                                            height = 700))
                            )
                 )
                 ),
                 )
        ),
        
        tabPanel("Data Table",
                 tabsetPanel(
                   tabPanel(strong("Table"),
                            br(),
                            withSpinner(
                              dataTableOutput(outputId = "data_table",
                                              height = 700)
                            )
                   ),
                   
                   tabPanel(strong("Summary"),
                            verbatimTextOutput(outputId = "SummaryA1"),
                            verbatimTextOutput(outputId = "SummaryA2")),
                 )),
        )
      )
     
  


     



       




