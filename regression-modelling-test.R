
# Script for testing concepts related to regression modelling

setwd("C:/R-statistics-analysis")

library(ggplot2)
library(ggpattern)
library(dplyr)
library(deSolve)
library(reshape2)
library(patchwork)
library(tidyr)
library(kableExtra)
library(knitr)
library(lamW)
library(tibble)
library(flexplot)

##########################################################################
# Example: Wine data set from Kaggle
##########################################################################

wine <- read.csv("wine-dataset.csv")
names(wine)

hist(wine$Alcohol)
hist(wine$Flavanoids)
hist(wine$Color.intensity)dsdd
hist(wine$Magnesium)

# flexplot model of level of alcohol as function of flavanoids
flexplot(class ~ Color.intensity | Alcohol + Flavanoids, data = wine, method="lm")
flexplot(Color.intensity ~ Ash | Flavanoids + Proanthocyanins, data = wine, method="lm")
flexplot(Color.intensity ~ Alcohol | Flavanoids + Proanthocyanins, data = wine, method="lm")

color_model <- lm(Color.intensity ~ Alcohol + Flavanoids + Proanthocyanins, data = wine)
summary(color_model)
