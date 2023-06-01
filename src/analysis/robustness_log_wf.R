library(ggplot2)
library(tidyverse)
library(quanteda)


setwd("~/Git projects/gall_thesis2/src/cleaning")

data <- read.csv("../../gen/output/log_wf_set_clean.csv", sep = ";")

#Normalize interacting variables
data <- subset(data, select = -nan)

data$year_mc <- data$year - mean(data$year, na.rm = T)
data$percentage_mc <- data$percentage - mean(data$percentage, na.rm = T)
data$count_mc <- data$count - mean(data$count , na.rm = T)
data$grade_mc <- data$grade_clean - mean(data$grade_clean , na.rm = T) #Fix clean_grade
data$intrinsic_cues_mc <- data$intrinsic_cues - mean(data$intrinsic_cues , na.rm = T)
data$grade_mc <- data$clean_grade - mean(data$clean_grade , na.rm = T)
data$log_wf_mc <- data$log_wf - mean(data$log_wf, na.rm = T)

#Normalize interacting variables
data$inv_NDC_scores <- 1 - data$NDC_scores
data$inv_secondary_NDC_scores <- 1 - data$secondary_NDC_scores

data$mc_NDC_scores <- data$inv_NDC_scores - mean(data$inv_NDC_scores, na.rm = T)
data$mc_secondary_NDC_scores <- data$inv_secondary_NDC_scores - mean(data$inv_secondary_NDC_scores, na.rm = T)
data$mc_price_75 <- data$price_75 - mean(data$price_75, na.rm = T)

#log transform price
data$price_log <- log(data$price_75)
data$mc_log_price <- data$price_log - mean(data$price_log, na.rm = T)

#Grape variety
col_names <- paste(names(data[,30:138]),collapse = " + ")

sum_variety <- colSums(data[,30:138])
sum_variety[order(sum_variety, decreasing = TRUE)]

##Reducing number of columns
columns_to_include <- c('Chardonnay', 'Merlot')
data$others_variety <- apply(data[,30:138][, !(names(data[,30:138]) %in% c(columns_to_include, 'others'))], 1, function(x) as.integer(any(x == 1)))

#Producer
producers <- as.data.frame(model.matrix(~data$producer - 1))
colnames(producers) <- sub("data\\$producer", "", colnames(producers))
colnames(producers) <- gsub(" ", "_", colnames(producers))
sum_producer <- colSums(producers)
best_producers <- sum_producer[order(sum_producer, decreasing = T)]
best_producers

producers_to_include <- c('Viña_La_Rosa', 'Valdivieso', 'Les_Domaines_Paul_Mas', 'Felix_Solis', 'Farnese_Vini')
producers_included <- producers %>% dplyr::select(all_of(producers_to_include))
data <-cbind(data, producers_included)

data <- data %>% rename('Vina_La_Rosa' = 'Viña_La_Rosa')

summary(lm(rating ~ log_wf*mc_price_75 + average_sentence_length*mc_price_75 + intrinsic_cues + inv_secondary_NDC_scores +
             grade_clean +
             type +
             year +
             country +
             percentage +
             size +
             count, data))

model_log_wf <- lm(rating ~ log_wf_mc*mc_log_price + average_sentence_length*mc_log_price + mc_secondary_NDC_scores +
             grade_mc +
             type +
             year_mc +
             country +
             percentage_mc +
             count_mc +
             intrinsic_cues_mc +
             Vina_La_Rosa + Valdivieso + Les_Domaines_Paul_Mas + Felix_Solis + Farnese_Vini +
             Chardonnay + Merlot + Syrah + Sauvignon.blanc + Cabernet.sauvignon, data)
summary(model_log_wf)

cor(data$log_wf, data$familiarity)
