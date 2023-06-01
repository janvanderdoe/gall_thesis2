library(tidyverse)
library(reshape2)
library(ltm)
library(Hmisc)

setwd("~/Git projects/gall_thesis2/src/Pretest")

clean_data <- read.csv("../../gen/input/pretest_clean.csv", sep = ";")

clean_data <- clean_data %>% rename(order = Q.SP.DBN, eosp = Q.SP.NDB, wtp = Q.SP.DNB)

#Long to wide
data <- clean_data %>% spread(Question, Value)

readability <- dcast(clean_data, index + Gender + Age + Freq.purch + Freq.drink + unfamiliarity + number_words + Description + log_wf ~ Question, value.var = "Value")

cronbach.alpha(readability[,5:7])

readability$mean_readability <- rowMeans(readability[,c("Q1", "Q2", "Q3")])

readability %>% group_by(Description) %>% summarize(score = mean(mean_readability), mean(unfamiliarity), mean(number_words), mean(log_wf)) %>% arrange(score)
rcorr(as.matrix(readability %>% dplyr::select(mean_readability, number_words, unfamiliarity, log_wf)))

summary(lm(mean_readability ~ number_words + unfamiliarity + Age + Freq.purch + Freq.drink + Gender + log_wf,readability %>% filter(Description != 4)))

summary(lm(mean_readability ~ log_wf, readability))

clean_data <- clean_data %>% rename("Beer" = Q61, "Wine" = Q64, "Cheap_wine" = QID62, "Exp_wine" = Q63)

everyday_products <- melt(clean_data %>% dplyr::select(index, Beer, Wine, Cheap_wine, Exp_wine), id.vars = "index")

summary(aov(value ~ variable, everyday_products))
everyday_products %>% group_by(variable) %>% summarize(mean(value, na.rm = T))

#Comparison means beer versus wine
everyday_products %>% filter(!is.na(value)) %>% group_by(variable) %>% summarize(
  mean.rating = mean(value, na.rm = TRUE),
  sd.rating = sd(value, na.rm = TRUE),
  n.rating = n()) %>% 
  mutate(se.rating = sd.rating / sqrt(n.rating),
         lower.ci.rating = mean.rating - qt(1 - (0.05 / 2), n.rating - 1) * se.rating,
         upper.ci.rating = mean.rating + qt(1 - (0.05 / 2), n.rating - 1) * se.rating) %>% 
  ggplot(aes(variable, mean.rating)) + geom_col(fill="gray", alpha=1) +
  geom_errorbar( aes(x=variable, ymin=lower.ci.rating, ymax=upper.ci.rating), width=0.4, colour="black", alpha=0.8, size=0.8) +
  geom_text(aes(label = round(mean.rating, 3)), vjust = 5) +
  ylab("rating") +
  xlab("Median split of the Dale-Chall scores") +
  theme(axis.ticks.x=element_blank()) +
  coord_cartesian(ylim=c(1,5)) +
  ggtitle("Ratings for easy vs hard processing fluency for cheap vs expensive wines") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text=element_text(size=10),
    axis.title=element_text(size=11,face="bold"),
    plot.title = element_text(hjust = 0.5, size=12,face="bold")
  )

t.test(value ~ variable, everyday_products %>% filter(variable == "Beer" | variable == "Wine"))

wtp_eosp <- clean_data %>% dplyr::select(wtp, index, order, eosp, Age, Gender, Freq.purch, Freq.drink) %>% distinct()

#eo versus sp WTP
model_pretest <- lm(wtp ~ eosp + order + Gender + Age + Freq.purch + Freq.drink, wtp_eosp)

summary(model_pretest)

write.table(tidy(model_pretest), "../../gen/paper/model_pretest.csv", dec = ".", sep = ";") #export for table
