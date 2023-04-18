library(ggplot2)
library(tidyverse)

setwd("~/Git projects/gall_thesis2/src/cleaning")

data <- read.csv("../../gen/output/data_with_awards.csv", sep = ";")

summary(lm(rating ~ NDC_scores*price_75 + secondary_NDC_scores + clean_grade + country + percentage + year, data))
