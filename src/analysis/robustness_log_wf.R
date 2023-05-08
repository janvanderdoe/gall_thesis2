library(ggplot2)
library(tidyverse)
library(quanteda)


setwd("~/Git projects/gall_thesis2/src/cleaning")

data <- read.csv("../../gen/output/log_wf_set_clean.csv", sep = ";")

#Normalize interacting variables
data$inv_NDC_scores <- 1 - data$NDC_scores
data$inv_secondary_NDC_scores <- 1 - data$secondary_NDC_scores
data$mc_NDC_scores <- data$inv_NDC_scores - mean(data$inv_NDC_scores, na.rm = T)

data$mc_price_75 <- data$price_75 - mean(data$price_75, na.rm = T)

summary(lm(rating ~ log_wf*mc_price_75 + average_sentence_length*mc_price_75 + intrinsic_cues + inv_secondary_NDC_scores +
             grade_clean +
             type +
             year +
             country +
             percentage +
             size +
             count, data))

cor(data$log_wf, data$familiarity)
