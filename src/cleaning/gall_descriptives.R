library(ggplot2)
library(tidyverse)

setwd("~/Git projects/gall_thesis2/src/cleaning")

data <- read.csv("../../gen/output/gall_full_data_setclean.csv", sep = ";")
#data_wine <- read.csv("../../gen/output/gall_clean.csv", sep = ";")

#ADDING VARIABLES
data$median_price <- ifelse(data$price_75 > median(data$price_75, na.rm = TRUE), "expensive", "cheap")
data$median_NDC_scores <- ifelse(data$NDC_scores > median(data$NDC_scores, na.rm = TRUE), "hard to process", "easy to process")

#SUMMARY STATISTICS
rel_cols <- c("price_75", "percentage", "year", "NDC_scores", "rating", "number_of_words", "average_char_length", "familiarity",
              "grade_clean", "rating_average", "cleaned_size", "count", "intrinsic_cues", "secondary_number_of_words", "secondary_familiarity") 

data_summary <- c("variable", "mean", "median", "sd", "min", "max")
for(col in rel_cols) {
  col <- c(col, mean(data[[col]], na.rm = TRUE), median(data[[col]], na.rm = TRUE), sd(data[[col]], na.rm = T), min(data[[col]], na.rm = TRUE), max(data[[col]], na.rm = TRUE))
  data_summary <- cbind(data_summary, col)
}
t(data_summary)

#Deciles price
quantile(data$price_75, probs = seq(.1, .9, by = .1), na.rm = T)
#Bar plot processing versus category
data %>% ggplot() +
  geom_bar(aes(median_NDC_scores, rating), position = "dodge", stat = "summary", fun.y = "mean") +
  facet_grid(~category) +
  coord_cartesian(ylim=c(4,5))

#Bar plot with error bars
data %>% filter(!is.na(median_price)) %>% group_by(median_NDC_scores, median_price) %>% summarize(
  mean.rating = mean(rating, na.rm = TRUE),
  sd.rating = sd(rating, na.rm = TRUE),
  n.rating = n()) %>% 
mutate(se.rating = sd.rating / sqrt(n.rating),
       lower.ci.rating = mean.rating - qt(1 - (0.05 / 2), n.rating - 1) * se.rating,
       upper.ci.rating = mean.rating + qt(1 - (0.05 / 2), n.rating - 1) * se.rating) %>% 
  ggplot(aes(median_NDC_scores, mean.rating)) + geom_col(fill="gray", alpha=1) +
  facet_wrap(~median_price) +
  geom_errorbar( aes(x=median_NDC_scores, ymin=lower.ci.rating, ymax=upper.ci.rating), width=0.4, colour="black", alpha=0.8, size=0.8) +
  geom_text(aes(label = round(mean.rating, 3)), vjust = 5) +
  ylab("rating") +
  xlab("Median split of the Dale-Chall scores") +
  theme(axis.ticks.x=element_blank()) +
  coord_cartesian(ylim=c(4,5)) +
  ggtitle("Ratings for easy vs hard processing fluency for cheap vs expensive wines") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11,face="bold"),
    plot.title = element_text(hjust = 0.5, size=12,face="bold")
  )

data %>% group_by(median_NDC_scores, category) %>% summarize(n())

#Histogram
data %>% ggplot(aes(NDC_scores, fill = category)) +
  geom_histogram(binwidth = 0.1) +
  scale_fill_grey()

#Histogram wines
data_wine %>% 
  ggplot(aes(producer)) +
  geom_histogram(stat = "count")

#Price distribution
data_wine %>% ggplot(aes(price)) +
  geom_histogram(bin_width = 10)

data %>% ggplot(aes(rating)) +
  geom_histogram()

varieties <- as.data.frame(colSums(data[27:136]))

names(varieties) <- "count"

varieties %>% arrange(desc(count)) %>% top_n(5) %>% ggplot(aes(count)) +
  geom_histogram()

data$quantiles <- cut(as.numeric(data$price_75), quantile(as.numeric(data$price_75), na.rm = TRUE))
levels(data$quantiles) <- c("Q1", "Q2", "Q3", "Q4")

#Bar plot with quantiles and processing
data %>% group_by(median_NDC_scores, quantiles) %>% summarize(
  mean.rating = mean(rating, na.rm = TRUE),
  sd.rating = sd(rating, na.rm = TRUE),
  n.rating = n()) %>% 
  mutate(se.rating = sd.rating / sqrt(n.rating),
         lower.ci.rating = mean.rating - qt(1 - (0.05 / 2), n.rating - 1) * se.rating,
         upper.ci.rating = mean.rating + qt(1 - (0.05 / 2), n.rating - 1) * se.rating) %>% 
  ggplot(aes(median_NDC_scores, mean.rating)) + geom_col(fill="gray", alpha=1) +
  facet_wrap(~quantiles) +
  geom_errorbar( aes(x=median_NDC_scores, ymin=lower.ci.rating, ymax=upper.ci.rating), width=0.4, colour="black", alpha=0.8, size=0.8) +
  geom_text(aes(label = round(mean.rating, 3)), vjust = 5) +
  ylab("rating") +
  theme(axis.ticks.x=element_blank()) +
  coord_cartesian(ylim=c(4,5))

#Readability graphs
data %>% filter(!is.na(median_price)) %>%  ggplot(aes(x=median_price, y=number_of_words)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  xlab("Price") +
  ylab("Number of words") +
  ggtitle("adsf")

data %>% filter(!is.na(median_price)) %>%  ggplot(aes(x=median_price, y=familiarity)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  xlab("Price") +
  ylab("Word unfamiliarity (% not in top 3000 Subtlex-NL)") +
  ggtitle("Unfamiliarity in product description of cheap vs expensive wine") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11,face="bold"),
    plot.title = element_text(hjust = 0.5, size=14,face="bold")
  )

#Word unfamiliarity
data %>% filter(!is.na(median_price)) %>% group_by(median_price) %>% summarize(
  mean.var = mean(average_sentence_length, na.rm = TRUE),
  sd.var = sd(average_sentence_length, na.rm = TRUE),
  n.var = n()) %>% 
  mutate(se.var = sd.var / sqrt(n.var),
         lower.ci.var = mean.var - qt(1 - (0.05 / 2), n.var - 1) * se.var,
         upper.ci.var = mean.var + qt(1 - (0.05 / 2), n.var - 1) * se.var) %>% 
  ggplot(aes(median_price, mean.var)) + geom_col(fill="gray", alpha=1) +
  geom_errorbar( aes(x=median_price, ymin=lower.ci.var, ymax=upper.ci.var), width=0.4, colour="black", alpha=0.8, size=0.8) +
  geom_text(aes(label = round(mean.var, 3)), vjust = 5) +
  xlab("Price") +
  ylab("Average number of words in sentence") +
  ggtitle("Average sentence length in description of cheap vs expensive wine") +
  coord_cartesian(ylim = c(10,14)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11,face="bold"),
    plot.title = element_text(hjust = 0.5, size=14,face="bold")
  )

#Word length
data %>% filter(!is.na(median_price)) %>% group_by(median_price) %>% summarize(
  mean.var = mean(familiarity, na.rm = TRUE),
  sd.var = sd(familiarity, na.rm = TRUE),
  n.var = n()) %>% 
  mutate(se.var = sd.var / sqrt(n.var),
         lower.ci.var = mean.var - qt(1 - (0.05 / 2), n.var - 1) * se.var,
         upper.ci.var = mean.var + qt(1 - (0.05 / 2), n.var - 1) * se.var) %>% 
  ggplot(aes(median_price, mean.var)) + geom_col(fill="gray", alpha=1) +
  geom_errorbar( aes(x=median_price, ymin=lower.ci.var, ymax=upper.ci.var), width=0.4, colour="black", alpha=0.8, size=0.8) +
  geom_text(aes(label = round(mean.var, 3)), vjust = 5) +
  xlab("Price") +
  ylab("Word unfamiliarity (% not in top 3000 Subtlex-NL)") +
  ggtitle("Unfamiliarity in product description of cheap vs expensive wine") +
  coord_cartesian(ylim = c(0.4,0.5)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11,face="bold"),
    plot.title = element_text(hjust = 0.5, size=14,face="bold")
  )

#bottom with top decile comparison
data$decile_price <- ntile(data$price_75, 10)

data %>% filter(decile_price == 1 | decile_price == 4) 
data %>% group_by(decile_price, median_NDC_scores) %>% summarize(
  mean.rating = mean(rating, na.rm = TRUE),
  sd.rating = sd(rating, na.rm = TRUE),
  n.rating = n()) %>% 
  mutate(se.rating = sd.rating / sqrt(n.rating),
         lower.ci.rating = mean.rating - qt(1 - (0.05 / 2), n.rating - 1) * se.rating,
         upper.ci.rating = mean.rating + qt(1 - (0.05 / 2), n.rating - 1) * se.rating) %>% 
  ggplot(aes(median_NDC_scores, mean.rating)) + geom_col(fill="gray", alpha=1) +
  facet_wrap(~decile_price) +
  geom_errorbar( aes(x=median_NDC_scores, ymin=lower.ci.rating, ymax=upper.ci.rating), width=0.4, colour="black", alpha=0.8, size=0.8) +
  geom_text(aes(label = round(mean.rating, 3)), vjust = 6) +
  ylab("rating") +
  xlab("Median split of the Dale-Chall scores") +
  theme(axis.ticks.x=element_blank()) +
  coord_cartesian(ylim=c(4,5)) +
  ggtitle("Ratings for easy vs hard processing fluency for lowest vs highest price deci vs wine") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11,face="bold"),
    plot.title = element_text(hjust = 0.5, size=12,face="bold")
  )

#Histogram of price
data %>% ggplot(aes(price_75)) +
  geom_histogram() +
  scale_x_log10() +
  xlab("Price per 75 CL") +
  ylab("Number of reviews") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11,face="bold"),
    plot.title = element_text(hjust = 0.5, size=12,face="bold")
  )

#Relationship percentage and review rating
data %>% ggplot(aes(percentage, rating)) +
  geom_point() +
  geom_smooth()

#T tests
t.test(data$rating ~ data$median_price)
t.test(data$number_of_words ~ data$median_price)
t.test(data$average_char_length ~ data$median_price)
t.test(data$familiarity ~ data$median_price)
t.test(rating ~ median_NDC_scores, data = data %>% filter(median_price == "cheap"))
t.test(data$NDC_scores~ data$median_price)

t.test(rating ~ zero_percentage, data %>% mutate(zero_percentage = ifelse(percentage == 0, "zero%", "non-zero%")))
