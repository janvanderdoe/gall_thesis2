library(ggplot2)
library(tidyverse)


setwd("~/Git projects/gall_thesis2/src/cleaning")

data_beer <- read.csv("../../gen/output/beer_wine_set_clean.csv", sep = ";")

data_beer$median_NDC_scores <- ifelse(data_beer$NDC_scores > median(data_beer$NDC_scores, na.rm = TRUE), "hard to process", "easy to process")
data_beer$inv_NDC_scores <- 1 - data_beer$NDC_scores
data_beer$inv_secondary_NDC_scores <- 1 - data_beer$secondary_NDC_scores
data_beer$mc_secondary_NDC_scores <- data_beer$inv_secondary_NDC_scores - mean(data_beer$inv_secondary_NDC_scores, na.rm = T)
data_beer$mc_NDC_scores <- data_beer$inv_NDC_scores - mean(data_beer$inv_NDC_scores, na.rm = T)
data_beer$mc_price_75 <- data_beer$price_75 - mean(data_beer$price_75, na.rm = T)


data_beer$percentage_mc <- data_beer$percentage - mean(data_beer$percentage, na.rm = T)
data_beer$count_mc <- data_beer$count - mean(data_beer$count , na.rm = T)

#graph
data_beer %>% filter(!is.na(category)) %>% group_by(median_NDC_scores, category) %>% summarize(
  mean.rating = mean(rating, na.rm = TRUE),
  sd.rating = sd(rating, na.rm = TRUE),
  n.rating = n()) %>% 
  mutate(se.rating = sd.rating / sqrt(n.rating),
         lower.ci.rating = mean.rating - qt(1 - (0.05 / 2), n.rating - 1) * se.rating,
         upper.ci.rating = mean.rating + qt(1 - (0.05 / 2), n.rating - 1) * se.rating) %>% 
  ggplot(aes(median_NDC_scores, mean.rating)) + geom_col(fill="gray", alpha=1) +
  facet_wrap(~category) +
  geom_errorbar( aes(x=median_NDC_scores, ymin=lower.ci.rating, ymax=upper.ci.rating), width=0.4, colour="black", alpha=0.8, size=0.8) +
  geom_text(aes(label = round(mean.rating, 3)), vjust = 6) +
  ylab("rating") +
  xlab("Median split of the Dale-Chall scores") +
  theme(axis.ticks.x=element_blank()) +
  coord_cartesian(ylim=c(4,5)) +
  ggtitle("Ratings for easy vs hard processing fluency for beer vs wine") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11,face="bold"),
    plot.title = element_text(hjust = 0.5, size=12,face="bold")
  )

top_levels <- levels(as.factor(data_beer$type))[order(table(as.factor(data_beer$type)), decreasing = TRUE)][1:5]
data_beer$type_top_5 <- factor(as.factor(data_beer$type), levels = top_levels)
data_beer$type_top_5 <- ifelse(is.na(data_beer$type_top_5), "Other", as.factor(data_beer$type_top_5))


top_levels <- levels(as.factor(data_beer$producer))[order(table(as.factor(data_beer$producer)), decreasing = TRUE)][1:5]
data_beer$producer_top_5 <- factor(as.factor(data_beer$producer), levels = top_levels)
data_beer$producer_top_5 <- ifelse(is.na(data_beer$producer_top_5), "Other", as.factor(data_beer$producer_top_5))

#model
model_beer_wine <- lm(rating ~ mc_NDC_scores*category + mc_secondary_NDC_scores +
             type_top_5 +
             producer_top_5 +
             percentage_mc +
             #size +
             count_mc, data_beer)

summary(model_beer_wine)
#t.test
t.test(rating ~ median_NDC_scores, data = data_beer %>% filter(category == "beer"))

#model with only beer to check if readability has a positive influende
summary(lm(rating ~ mc_NDC_scores + inv_secondary_NDC_scores +
             type +
             producer +
             percentage +
             count, data_beer %>% filter(category == "beer")))
