### Required libraries 
library(shiny)
library(shinythemes)
library(ggplot2)
library(GGally)
library(shinycssloaders)
library(visdat)
library(dplyr)
library(car)
library(vcd)
library(DT)
library(corrgram)
library(seriation)
library(plotly)
library(naniar)
library(caret)
library(rpart)
library(rpart.plot)
library(recipes)
library(glmnet)
library(ggrepel)

### DATA
covid.data <- read.csv("covid_data.csv", header = T, na.strings = c("--", "NA", NA, -99), stringsAsFactors = T)

# Replacing numerical missing values
covid.data[covid.data == -99] <- NA

# Creating new levels for relevant factor variables
covid.data$POLITICS <- as.character(covid.data$POLITICS)
covid.data$POLITICS[is.na(covid.data$POLITICS)] <- "none"
covid.data$POLITICS <- as.factor(covid.data$POLITICS)
covid.data <- covid.data[order(covid.data$CODE),]
covid.data$CODE <- 1:nrow(covid.data)

# Fixing numerical values that are not really missing values
covid.data$HEALTHCARE_COST[is.na(covid.data$HEALTHCARE_COST)] <- 0

# covid data without CODE
covid.data.2 <- data.frame(POLITICS = covid.data$POLITICS,
                           POPULATION = covid.data$POPULATION,
                           AGE25_PROPTN = covid.data$AGE25_PROPTN,
                           AGE_MEDIAN = covid.data$AGE_MEDIAN,
                           AGE50_PROPTN = covid.data$AGE50_PROPTN,
                           POP_DENSITY = covid.data$POP_DENSITY,
                           GDP = covid.data$GDP,
                           INFANT_MORT = covid.data$INFANT_MORT,
                           DOCS = covid.data$DOCS,
                           VAX_RATE = covid.data$VAX_RATE,
                           HEALTHCARE_BASIS = covid.data$HEALTHCARE_BASIS,
                           HEALTHCARE_COST = covid.data$HEALTHCARE_COST,
                           DEATH_RATE = covid.data$DEATH_RATE,
                           OBS_TYPE = covid.data$OBS_TYPE)

# for missing values chart
pMiss <- function(x){ sum(is.na(x))/length(x)*100 }

# data for predicting missing values with r.part
covid.data.rpart <- covid.data
covid.data.rpart$missingness <- apply(X = is.na(covid.data.rpart), MARGIN = 1, FUN = sum)



thing <- c(T, F, T)
ok <- c(3, 4, 3)

ok[thing]
