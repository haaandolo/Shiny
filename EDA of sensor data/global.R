library(shiny)
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

# types of data
ass1.data <- read.csv("SensorData.csv", header = TRUE, stringsAsFactors = TRUE)
num.ass1.data <- as.matrix(ass1.data %>% select(!(ID:Surface)))
num2.ass1.data <- (ass1.data %>% select(!(ID:Surface)))
cat.ass1.data <- (ass1.data %>% select(ID:Surface))[, c(-3, -1)]
num.data.standardise <- scale(num.ass1.data, center=TRUE, scale=TRUE)
cont.ass1.data <- (ass1.data %>% select(!(ID:Surface)))

# for missing values chart
pMiss <- function(x){ sum(is.na(x))/length(x)*100 }

