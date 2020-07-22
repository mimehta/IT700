library(tidyverse)
library(corrplot)
library(shiny)
library(RColorBrewer)
library(xtable)

csvData <- read_csv("www/datasets_insurance.csv")
corrData <- cor(csvData)