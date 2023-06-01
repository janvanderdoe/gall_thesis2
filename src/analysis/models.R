library(ggplot2)
library(tidyverse)
library(car) #used for vif
library(broom) #used as input
library(texreg) #comparing and saving models
library(writexl) #saving
library(caret) #boxcox
library(ggfortify) #autoplot
library(lmtest) #bp test
library(quanteda) #sentence length

setwd("~/Git projects/gall_thesis2/src/cleaning")

data <- read.csv("../../gen/output/data_with_awards.csv", sep = ";")

data <- subset(data, select = -nan)

#NDC scores calculation
data$number_of_sentences <- nsentence(data$main_description)
data$average_sentence_length <- data$number_of_words / data$number_of_sentences

data$inv_NDC_scores2 <- 1 - data$NDC_scores2

#Second description
data$secondary_NDC_scores2 <- 0.1579 * data$unfamiliarity_secondary * 100 + 0.0496 * data$secondary_number_of_words + 3.6365
data$inv_secondary_NDC_scores2 <- 1 - data$secondary_NDC_scores2

#Correlation
cor.test(data$familiarity, data$number_of_words)
cor.test(data$NDC_scores, data$average_sentence_length)

#Mean centering other variables
data$year_mc <- data$year - mean(data$year, na.rm = T)
data$percentage_mc <- data$percentage - mean(data$percentage, na.rm = T)
data$count_mc <- data$count - mean(data$count , na.rm = T)
data$grade_mc <- data$clean_grade - mean(data$clean_grade , na.rm = T)
data$intrinsic_cues_mc <- data$intrinsic_cues - mean(data$intrinsic_cues , na.rm = T)

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
data <-cbind(data, producers)

data <- data %>% rename('Vina_La_Rosa' = 'Viña_La_Rosa')

#MISSINGNESS
data_miss <- data %>% select(rating, mc_NDC_scores,mc_price_75, mc_secondary_NDC_scores,
                               grade_mc,
                               type,
                               year_mc,
                               country,
                               percentage_mc,
                               count_mc,
                               intrinsic_cues_mc)

data_miss <- data_miss[!complete.cases(data_miss),]

data$missing_grade <- ifelse(is.na(data$grade_mc), 1, 0)

model_missing_grade <- glm(missing_grade ~ year_mc + rating + count_mc + mc_price_75,
    family = binomial          # Distribution-family is binomial
    (link = "probit"),
    data = data)
summary(model_missing_grade)
write.table(tidy(model_missing_grade), "../../gen/paper/model_missing_grade.csv", dec = ".", sep = ";") #export for table

data$missing_year <- ifelse(is.na(data$year_mc), 1, 0)

summary(glm(missing_year ~ mc_NDC_scores + mc_price_75 + mc_secondary_NDC_scores +
              grade_mc +
              type +
              country +
              percentage_mc +
              count_mc +
              intrinsic_cues_mc +
              Vina_La_Rosa + Valdivieso + Les_Domaines_Paul_Mas + Felix_Solis + Farnese_Vini +
              Chardonnay + Merlot + Syrah + Sauvignon.blanc + Cabernet.sauvignon,
            family = binomial          # Distribution-family is binomial
            (link = "probit"),
            data = data))

data_complete <- data[complete.cases(data %>% select(rating, mc_NDC_scores,mc_price_75, mc_log_price, mc_secondary_NDC_scores,
                                            grade_mc,
                                            type,
                                            year_mc,
                                            country,
                                            percentage_mc,
                                            count_mc,
                                            intrinsic_cues_mc)),]

#Final model
final_model <- lm(rating ~ mc_NDC_scores*mc_price_75 + mc_secondary_NDC_scores +
             grade_mc +
             type +
             year_mc +
             country +
             percentage_mc +
             count_mc +
             intrinsic_cues_mc +
             Vina_La_Rosa + Valdivieso + Les_Domaines_Paul_Mas + Felix_Solis + Farnese_Vini +
             Chardonnay + Merlot + Syrah + Sauvignon.blanc + Cabernet.sauvignon, data = data)

summary(final_model)

#Final model with price log
final_model_log <- lm(rating ~ mc_NDC_scores*mc_log_price + mc_secondary_NDC_scores +
                    grade_mc +
                    type +
                    year_mc +
                    country +
                    percentage_mc +
                    count_mc +
                    intrinsic_cues_mc +
                    Vina_La_Rosa + Valdivieso + Les_Domaines_Paul_Mas + Felix_Solis + Farnese_Vini +
                    Chardonnay + Merlot + Syrah + Sauvignon.blanc + Cabernet.sauvignon, data = data)

summary(final_model_log)

#mean imputation
data$year_mc[is.na(data$year_mc)] <- 0
data$grade_mc[is.na(data$grade_mc)] <- 0
data$mc_log_price[is.na(data$mc_log_price)] <- 0

write.table(tidy(final_model_log), "../../gen/paper/full_model2.csv", dec = ".", sep = ";") #export for table
#Multicollinearity

write.csv2(round(as.data.frame(vif(
  lm(rating ~ mc_NDC_scores + mc_log_price + mc_secondary_NDC_scores +
       grade_mc +
       type +
       year_mc +
       country +
       percentage_mc +
       count_mc +
       intrinsic_cues_mc +
       Vina_La_Rosa + Valdivieso + Les_Domaines_Paul_Mas + Felix_Solis + Farnese_Vini +
       Chardonnay + Merlot + Syrah + Sauvignon.blanc + Cabernet.sauvignon, data)
)),3), "../../gen/paper/vif_output.csv") #no multicollinearity

#Heteroscedacitiy
bptest(final_model)

autoplot(final_model_log)

##Transforming y
trans_rating <- BoxCoxTrans(data_complete$rating+1e-10, na.rm = T) #lambda is 2

final_model_trans <- lm(rating^0.2 ~ mc_NDC_scores*mc_price_75 + mc_secondary_NDC_scores +
                    grade_mc +
                    type +
                    year_mc +
                    country +
                    percentage_mc +
                    count_mc +
                    intrinsic_cues_mc +
                    Vina_La_Rosa + Valdivieso + Les_Domaines_Paul_Mas + Felix_Solis + Farnese_Vini +
                    Chardonnay + Merlot + Syrah + Sauvignon.blanc + Cabernet.sauvignon, data_complete)

autoplot(final_model_trans)

## WLS
wt <- 1 / lm(abs(final_model_log$residuals) ~ final_model_log$fitted.values)$final_model_log^2

final_model_wt <- lm(rating ~ mc_NDC_scores*mc_log_price + mc_secondary_NDC_scores +
                    grade_mc +
                    type +
                    year_mc +
                    country +
                    percentage_mc +
                    count_mc +
                    intrinsic_cues_mc +
                    Vina_La_Rosa + Valdivieso + Les_Domaines_Paul_Mas + Felix_Solis + Farnese_Vini +
                    Chardonnay + Merlot + Syrah + Sauvignon.blanc + Cabernet.sauvignon, data, weights = wt)

#Autocorrelation
durbinWatsonTest(final_model)

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
summary(lm(rating ~ mc_NDC_scores*mc_price_75 + mc_secondary_NDC_scores +
             grade_mc +
             type +
             year_mc +
             country +
             percentage_mc +
             count_mc +
             intrinsic_cues_mc +
             Vina_La_Rosa + Valdivieso + Les_Domaines_Paul_Mas + Felix_Solis + Farnese_Vini +
             Chardonnay + Merlot + Syrah + Sauvignon.blanc + Cabernet.sauvignon, data_low_high))

# 15 producers and grape varieties
model_15_rc <- lm(rating ~ mc_NDC_scores*mc_price_75 + mc_secondary_NDC_scores +
                    grade_mc +
                    type +
                    year_mc +
                    country +
                    percentage_mc +
                    count_mc +
                    intrinsic_cues_mc +
                    Vina_La_Rosa + Valdivieso + Les_Domaines_Paul_Mas + Felix_Solis + Farnese_Vini + Bodega_Catena_Zapata + Torres + Cantina_di_Verona + Yealands + Alamos + Inycon + Piccini + San_silvestro + Vina_San_Rafael + Cantina_La_Vis +
                    Chardonnay + Merlot + Syrah + Sauvignon.blanc + Cabernet.sauvignon + Grenache + Tempranillo + Malbec + Pinot.grigio + Garnacha + Cinsault + Corvina + Rondinella + Mazuelo + Primitivo, data = data)

summary(model_15_rc)
data$date <- as.Date(data$date)
model_15_rc_log <- lm(rating ~ mc_NDC_scores*mc_log_price + mc_secondary_NDC_scores +
                    grade_mc +
                    type +
                    year_mc +
                    country +
                    percentage_mc +
                    count_mc +
                    intrinsic_cues_mc +
                    Vina_La_Rosa + Valdivieso + Les_Domaines_Paul_Mas + Felix_Solis + Farnese_Vini + Bodega_Catena_Zapata + Torres + Cantina_di_Verona + Yealands + Alamos + Inycon + Piccini + San_silvestro + Vina_San_Rafael + Cantina_La_Vis +
                    Chardonnay + Merlot + Syrah + Sauvignon.blanc + Cabernet.sauvignon + Grenache + Tempranillo + Malbec + Pinot.grigio + Garnacha + Cinsault + Corvina + Rondinella + Mazuelo + Primitivo, data = data)
summary(model_15_rc_log)
#Overview regressions comparison
screenreg(list(final_model, final_model_log, model_15_rc, model_15_rc_log), digits = 3)
screenreg(list(final_model, model_15_rc), digits = 3)
screenreg(list(final_model_log, model_15_rc_log), digits = 3)
screenreg(list(final_model_log, model_15_rc_log, model_beer_wine, model_log_wf), digits = 3)


#Export to word
wordreg(list(final_model_log, model_15_rc_log, model_beer_wine, model_log_wf), digits = 3, "../../gen/papermodels.doc", single.row = T)

wordreg(list(final_model_log), digits = 3, "../../gen/paper/models_all_obs.doc", single.row = T)

#Main models individually
write.table(tidy(model_15_rc_log), "../../gen/paper/model_15_rc_log.csv", dec = ".", sep = ";") #export for table
write.table(tidy(model_beer_wine), "../../gen/paper/model_beer_wine.csv", dec = ".", sep = ";") #export for table
write.table(tidy(model_log_wf), "../../gen/paper/model_log_wf.csv", dec = ".", sep = ";") #export for table