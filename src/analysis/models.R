library(ggplot2)
library(tidyverse)

setwd("~/Git projects/gall_thesis2/src/cleaning")

data <- read.csv("../../gen/output/data_with_awards.csv", sep = ";")

#Full original model (except grape variety)
summary(lm(rating ~ NDC_scores*price_75 + secondary_NDC_scores +
             clean_grade +
             type +
             producer +
             year +
             country +
             percentage +
             size +
             count +
             intrinsic_cues, data))

#Normalize interacting variables
data$inv_NDC_scores <- 1 - data$NDC_scores
data$inv_secondary_NDC_scores <- 1 - data$secondary_NDC_scores
data$mc_NDC_scores <- data$inv_NDC_scores - mean(data$inv_NDC_scores, na.rm = T)

data$mc_price_75 <- data$price_75 - mean(data$price_75, na.rm = T)

#Rerunning analysis
summary(lm(rating ~ mc_NDC_scores*mc_price_75 + intrinsic_cues + inv_secondary_NDC_scores +
             clean_grade +
             type +
             year +
             country +
             percentage +
             size +
             count, data))

#Subset low and high prices
data_low_high <- data %>% filter(price_75 > 14.65 | price_75 <= 5.98)

#Rerunning on low_hihg
summary(lm(rating ~ mc_NDC_scores*mc_price_75 + intrinsic_cues + inv_secondary_NDC_scores +
             grade_clean +
             type +
             year +
             country +
             percentage +
             size +
             count, data_low_high))
