install.packages("lme4")
install.packages("tidyverse")
library(tidyverse)
library(lme4)
install.packages("dplyr")
library(dplyr)
library(ez)
library(multcomp)

sound_word_pre800_post500_n30 <- read.delim("sound_word_pre800_post500_n30.txt")

sound_word_pre800_post500_n30_exp <- sound_word_pre800_post500_n30 %>% 
  filter(status == "exp") %>%
  mutate(Look = ifelse(RIGHT_INTEREST_AREA_ID == "1" & RIGHT_IN_BLINK == "0" & RIGHT_IN_SACCADE == "0", "1", ifelse(RIGHT_INTEREST_AREA_ID == "2" & RIGHT_IN_BLINK == "0" & RIGHT_IN_SACCADE == "0", "2", ifelse(RIGHT_INTEREST_AREA_ID == "." & RIGHT_IN_BLINK == "0" & RIGHT_IN_SACCADE == "0", "3", "4"))))

str(sound_word_pre800_post500_n30)

sound_word_pre800_post500_n30_exp_1301 <- sound_word_pre800_post500_n30_exp[-seq(1, NROW(sound_word_pre800_post500_n30_exp), by = 1301),]

sound_word_pre800_post500_n30_exp_1301 <- sound_word_pre800_post500_n30_exp_1301 %>%
  mutate(Time = rep(seq(1:1300), times = 960))
#times je number of participants * number of exp trials, ovde 32 * 30 = 960
bini <- seq(0, 1300, by = 20)

#breaki <- seq(20,1300, by = 20)
#this is how it was
breaki <- seq(1,65, by = 1)
#this is how it should have been 

sound_word_pre800_post500_n30_exp_1301 <- sound_word_pre800_post500_n30_exp_1301 %>%
  mutate(Bins = cut(Time, bini, labels = breaki))

sound_word_pre800_post500_n30_exp_1301 <- sound_word_pre800_post500_n30_exp_1301 %>%
  separate(condition, into = c("language","freq"), sep = "_", remove = TRUE)

write.csv(sound_word_pre800_post500_n30_exp_1301, "sound_word_pre800_post500_n30_exp_1301.csv")

sound_word_pre800_post500_n30_exp_1301_joined <- sound_word_pre800_post500_n30_exp_1301 %>%
  left_join(Participant_overview_for_analysis1, by = "RECORDING_SESSION_LABEL")

sound_word_pre800_post500_n30_exp_1301_joined_lhq <- sound_word_pre800_post500_n30_exp_1301_joined %>%
  left_join(LHQ_trimmed_n30, by = "RECORDING_SESSION_LABEL")

write.csv(sound_word_pre800_post500_n30_exp_1301_joined_lhq, "sound_word_pre800_post500_n30_exp_1301_joined_lhq.csv")

sound_word_pre800_post500_n30_exp_1301_joined_lhq <- read.csv("~/CSPdata/Results/sound_word_pre800_post500_n30_exp_1301_joined_lhq.csv")
str(sound_word_pre800_post500_n30_exp_1301_joined_lhq)

loss_incor <- sound_word_pre800_post500_n30_exp_1301_joined_lhq %>%
  filter(ACCURACY == "incorrect") %>%
  group_by(RECORDING_SESSION_LABEL) %>%
  summarise(Counts = n()/1300)

write.csv(loss_incor, "loss_incorrect.csv")

sound_word_pre800_post500_n30_exp_1301_joined_lhq_filtered_cor <- sound_word_pre800_post500_n30_exp_1301_joined_lhq %>% 
  filter(ACCURACY == "correct") %>% 
  group_by(RECORDING_SESSION_LABEL, DELE.MELICET, language, freq, pair_num, target_freq, Bins, Look) %>%
  summarise(Counts = n()) %>%
  complete(Look, fill = list(Counts = 0))

gc()
#znaci nemoj da se zbunis ki luda sojka, znaci u tih 160 countova po binu imas 20 sekundi puta 8 triala od tog conditiona, tipa CS HT xD

sound_word_pre800_post500_n30_exp_1301_joined_lhq_filtered_cor_no_item <- sound_word_pre800_post500_n30_exp_1301_joined_lhq %>% 
  filter(ACCURACY == "correct") %>% 
  group_by(RECORDING_SESSION_LABEL, DELE.MELICET, language, freq, Bins, Look) %>%
  summarise(Counts = n()) %>%
  complete(Look, fill = list(Counts = 0))
# don't put in target_freq here

# SOUND_WORD_800pre_500post_exp_1301_agg_cor <- SOUND_WORD_800pre_500post_exp_1301_agg_cor %>% 
#    group_by(RECORDING_SESSION_LABEL, dominance, dele, language, pair_num, Bins) %>% 
#    mutate(Total = sum(Counts), Prop = Counts/Total)

sound_word_pre800_post500_n30_exp_1301_joined_lhq_filtered_cor_no_item <- sound_word_pre800_post500_n30_exp_1301_joined_lhq_filtered_cor_no_item %>%
  group_by(RECORDING_SESSION_LABEL, DELE.MELICET, language, freq, Bins) %>%
  mutate(Total = sum(Counts), Prop = Counts/Total)
# get actual frequency for this, last one which had it is _ordered



loss <- sound_word_pre800_post500_n30_exp_1301_joined_lhq_filtered_cor_no_item %>% 
  filter(Look == "4") %>% 
  group_by(RECORDING_SESSION_LABEL) %>% 
  summarize(Mean_Loss = mean(Prop))


sound_word_pre800_post500_n30_exp_1301_joined_lhq_filtered_cor_no_item %>% 
  filter(Prop <= 0.8 & Look == "1") %>%
  ggplot(data = ., aes(Bins, Prop, color = freq)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_smooth() +
  facet_wrap(~language)


sound_word_pre800_post500_n30_exp_1301_joined_lhq_filtered_cor_no_item_allLooks <- sound_word_pre800_post500_n30_exp_1301_joined_lhq_filtered_cor_no_item_allLooks %>%
  mutate(Freq_of_fixated_item = ifelse(Look == "1" & freq == "HT", "H", ifelse(Look == "1" & freq == "LT", "L", ifelse(Look == "2" & freq == "HT", "L", ifelse(Look == "2" & freq == "LT", "H", "x"))))) %>%
  filter(Freq_of_fixated_item %in% c("H","L"))
#ovde smo izbacili 4 and 3, samo 1 i 2 poglede na distractore i targete, ali ostaje u proporciji

#SOUND_WORD_800pre_500post_exp_1301_agg_cor_all_looks_no4$Bins <- as.integer(SOUND_WORD_800pre_500post_exp_1301_agg_cor_all_looks_no4$Bins)

sound_word_pre800_post500_n30_exp_1301_joined_lhq_filtered_cor_no_item_allLooks$Bins <- as.integer(sound_word_pre800_post500_n30_exp_1301_joined_lhq_filtered_cor_no_item_allLooks$Bins)


sound_word_pre800_post500_n30_exp_1301_joined_lhq_filtered_cor_no_item_allLooks_wReg <- sound_word_pre800_post500_n30_exp_1301_joined_lhq_filtered_cor_no_item_allLooks %>%
  mutate(Time = Bins*20) %>%
  mutate(Region = ifelse(Time >= 600 & Time <=1000, "200_pre_200_post_word", "other"))

# SOUND_WORD_800pre_500post_exp_1301_agg_no_item <- SOUND_WORD_800pre_500post_exp_1301_agg_no_item %>%
#   mutate(Time = Bins*20) %>%
#   mutate(Region = ifelse(Time >= 600 & Time <=800, "200 pre word", "other"))

target_sub_200pre_200post_word <- sound_word_pre800_post500_n30_exp_1301_joined_lhq_filtered_cor_no_item_allLooks_wReg %>% 
  filter(Region == "200_pre_200_post_word")

target_sub_200pre_200post_word_just1 <- target_sub_200pre_200post_word %>%
   filter(Look == 1)

target_sub_200pre_200post_word %>% 
  ggplot(data = ., aes(Bins, Prop, color = Freq_of_fixated_item)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_smooth() +
  facet_wrap(~language)

target_sub_200pre_200post_word_just1 %>% 
  ggplot(data = ., aes(Bins, Prop, color = freq)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_smooth() +
  facet_wrap(~language) + xlab("Time_scaled") + ylab("Proportion of looks")


library(nlme)

target_sub_200pre_200post_word$DELE.MELICET[is.na(target_sub_200pre_200post_word$DELE.MELICET)] <- 0.71
#mean of 25
#target_sub_cor_no_item_just1$dominance[is.na(target_sub_cor_no_item_just1$dominance)] <- 0.644261
#hocu da imputujem tu, recimo with mean
#target_sub_cor_200_no4$dele[is.na(target_sub_cor_200_no4$dele)] <- 28.08

#target_sub_cor_no_item_just1$dele[is.na(target_sub_cor_no_item_just1$dele)] <- 28.08

#target_sub_cor_no_item_just1$Bins <- as.integer(target_sub_cor_no_item_just1$Bins)

write.csv(target_sub_200pre_200post_word, "target_sub_200pre_200post_word.csv")


target_sub_200pre_200post_word <- read.csv("~/CS-aids-prediction/Data_Eyetracking/target_sub_200pre_200post_word.csv")

target_sub_200pre_200post_word_no30bin <- target_sub_200pre_200post_word %>%
  filter(Bins > 30)

target_model_200pre_200post_word <- lmer(Prop ~ language*DELE.MELICET*Bins*Freq_of_fixated_item + (1+language*Freq_of_fixated_item|RECORDING_SESSION_LABEL), data = target_sub_200pre_200post_word_poly)
summary(target_model_200pre_200post_word)


target_model_200pre_200post_word_just1_freq <- lmer(Prop ~ language*DELE.MELICET*Bins*freq + (1+language*Freq_of_fixated_item|RECORDING_SESSION_LABEL), data = target_sub_200pre_200post_word_just1)
summary(target_model_200pre_200post_word_just1_freq)
#ubaci pair_num kad filterujes gore
#(1+language*Freq_of_fixated_item|RECORDING_SESSION_LABEL)
#ovo RECORDING_SESSION_LABEL/pair_num znaci da je nesto nested in nesto, subject in therapist, jer svaki therapist vidi nekoliko odvojenih subjecta
# ali ovde ne moze, jer svaki participant vidi sve pair numove 

target_sub_200pre_200post_word_item <- read_csv("Data_Eyetracking/target_sub_200pre_200post_word_item.csv")

###ORTHOGONAL POLYNOMIALS TIME TRANSFORMATION


orthogonal_polynomials <- poly(sort(as.vector(unique(target_sub_200pre_200post_word_no30bin$Time))), 5)

time_codes <- data.frame(
  sort(as.vector(unique(target_sub_200pre_200post_word_no30bin$Time))),
  orthogonal_polynomials[, c(1:5)])

colnames(time_codes) <- c('Time','ot1','ot2','ot3','ot4','ot5')

target_sub_200pre_200post_word_no30bin_poly <- merge(target_sub_200pre_200post_word_no30bin, time_codes, by='Time')
#added orthogonal curvy polynomials

#mogla bih da plotujem ovde, mozda je trebalo od 1 da mi krece Time
target_sub_200pre_200post_word_no30bin_poly %>% 
  ggplot(data = ., aes(Time, ot3)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_smooth() +
  facet_wrap(~language)
#yeeeeeah sad je kako treba

target_sub_200pre_200post_word_no30bin_poly_centered <- target_sub_200pre_200post_word_no30bin_poly %>%
  mutate(Prop_cs = as.numeric(scale(Prop, center = TRUE, scale = TRUE)),
         DELE.MELICET_cs = as.numeric(scale(DELE.MELICET, center = TRUE, scale = TRUE)),
         Bins_cs = as.numeric(scale(Bins, center = TRUE, scale = TRUE)))

target_sub_200pre_200post_word_no30bin_poly_centered %>% 
  ggplot(data = ., aes(Bins_cs, Prop, color = Freq_of_fixated_item)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_smooth() +
  facet_wrap(~language) + xlab("Time_scaled") + ylab("Proportion of looks")

str(target_sub_200pre_200post_word_poly_centered)

write.csv(target_sub_200pre_200post_word_no30bin_poly_centered, "target_sub_200pre_200post_word_no30bin_poly_centered.csv")

target_sub_200pre_200post_word_poly_centered$DELE.MELICET_cs <- setNames(split(target_sub_200pre_200post_word_poly_centered$DELE.MELICET_cs, seq(nrow(target_sub_200pre_200post_word_poly_centered$DELE.MELICET_cs))), rownames(xy.df))
#was dis?

target_sub_200pre_200post_word_no30bin_poly_centered_catDom <- target_sub_200pre_200post_word_no30bin_poly_centered %>%
  mutate(dominance_cat = ifelse(DELE.MELICET>mean(DELE.MELICET), "SpDom", "EngDom"))

View(target_sub_200pre_200post_word_poly_centered)
str(target_sub_200pre_200post_word_poly_centered)

target_sub_200pre_200post_word_no30bin_poly_centered_catDom %>% ggplot(data = ., aes(Bins_cs, Prop, color = Freq_of_fixated_item)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_smooth() +
  facet_grid(cols = vars(language), rows = vars(dominance_cat)) + xlab("Time_scaled") + ylab("Proportion of looks")

write.csv(target_sub_200pre_200post_word_no30bin_poly_centered_catDom, "target_sub_200pre_200post_word_no30bin_poly_centered_catDom.csv")


#ITEM ORTHO POLY

target_sub_200pre_200post_word_item <- target_sub_200pre_200post_word_item %>%
  mutate(Time = Bins)

orthogonal_polynomials <- poly(sort(as.vector(unique(target_sub_200pre_200post_word_item$Time))), 5)

time_codes <- data.frame(
  sort(as.vector(unique(target_sub_200pre_200post_word_item$Time))),
  orthogonal_polynomials[, c(1:5)])

colnames(time_codes) <- c('Time','ot1','ot2','ot3','ot4','ot5')

target_sub_200pre_200post_word_item_poly <- merge(target_sub_200pre_200post_word_item, time_codes, by='Time')
#added orthogonal curvy polynomials

#mogla bih da plotujem ovde, mozda je trebalo od 1 da mi krece Time
target_sub_200pre_200post_word_item_poly %>% 
  ggplot(data = ., aes(Time, ot2)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_smooth() +
  facet_wrap(~language)
#yeeeeeah sad je kako treba

target_sub_200pre_200post_word_item_poly$DELE.MELICET[is.na(target_sub_200pre_200post_word_item_poly$DELE.MELICET)] <- 0.71



target_sub_200pre_200post_word_item_poly_centered <- target_sub_200pre_200post_word_item_poly %>%
  mutate(Prop_cs = as.numeric(scale(Prop, center = TRUE, scale = TRUE)),
         DELE.MELICET_cs = as.numeric(scale(DELE.MELICET, center = TRUE, scale = TRUE)),
         Bins_cs = as.numeric(scale(Bins, center = TRUE, scale = TRUE)))

target_sub_200pre_200post_word_item_poly_centered %>% 
  ggplot(data = ., aes(Bins_cs, Prop, color = Freq_of_fixated_item)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_smooth() +
  facet_wrap(~language) + xlab("Time_scaled") + ylab("Proportion of looks")

write.csv(target_sub_200pre_200post_word_item_poly_centered, "target_sub_200pre_200post_word_item_poly_centered.csv")

str(target_sub_200pre_200post_word_item_poly_centered)

#target_sub_200pre_200post_word_item_poly_centered$DELE.MELICET_cs <- setNames(split(target_sub_200pre_200post_word_poly_centered$DELE.MELICET_cs, seq(nrow(target_sub_200pre_200post_word_poly_centered$DELE.MELICET_cs))), rownames(xy.df))
#was dis?


target_sub_200pre_200post_word_item_poly_centered_catDom <- target_sub_200pre_200post_word_item_poly_centered %>%
  mutate(dominance_cat = ifelse(DELE.MELICET>mean(DELE.MELICET), "SpDom", "EngDom")) %>%
  dplyr::select(-DELE.MELICET_)

target_sub_200pre_200post_word_item_poly_centered_catDom %>% ggplot(data = ., aes(Bins_cs, Prop, color = Freq_of_fixated_item)) + stat_summary(fun.y = mean, geom = "point") + 
stat_smooth() +
facet_grid(cols = vars(language), rows = vars(dominance_cat)) + xlab("Time_scaled") + ylab("Proportion of looks")

write.csv(target_sub_200pre_200post_word_item_poly_centered_catDom, "target_sub_200pre_200post_word_item_poly_centered_catDom.csv")

#MODELS
target_sub_200pre_200post_word_no30bin_poly_centered_catDom_1only <- target_sub_200pre_200post_word_no30bin_poly_centered_catDom %>%
  filter(Look == 1)

model_target_sub_200pre_200post_word_poly <- lmer(Prop ~ language*DELE.MELICET*Freq_of_fixated_item*(ot1 + ot3+ ot2) + (1+language+Freq_of_fixated_item+ot1+ot2+ot3|RECORDING_SESSION_LABEL), target_sub_200pre_200post_word_poly_centered)
summary(model_target_sub_200pre_200post_word_poly)

#this one! poly 400ms 
model_target_sub_200pre_200post_word_no30bin_poly_centered_catDom <- lmer(Prop_cs ~ language*DELE.MELICET_cs*Freq_of_fixated_item*(ot1 + ot3+ ot2) + (1+language+Freq_of_fixated_item+ot1+ot2+ot3|RECORDING_SESSION_LABEL), target_sub_200pre_200post_word_no30bin_poly_centered_catDom)
summary(model_target_sub_200pre_200post_word_no30bin_poly_centered_catDom)
plot(model_target_sub_200pre_200post_word_no30bin_poly_centered_catDom)
qqnorm(resid(model_target_sub_200pre_200post_word_no30bin_poly_centered_catDom))
anova(model_target_sub_200pre_200post_word_no30bin_poly_centered_catDom)

#spanish -0.5 CS +0.5
target_sub_200pre_200post_word_no30bin_poly_centered_catDom_LangContrastCoding <- target_sub_200pre_200post_word_no30bin_poly_centered_catDom %>%
  mutate(language_cont = ifelse(language == 'S', -0.5, 0.5))
write.csv(target_sub_200pre_200post_word_no30bin_poly_centered_catDom_LangContrastCoding, "target_sub_200pre_200post_word_no30bin_poly_centered_catDom_LangContrastCoding.csv")

model_target_sub_200pre_200post_word_no30bin_poly_centered_catDom_LangContrastCoding <- lmer(Prop_cs ~ language_cont*DELE.MELICET_cs*Freq_of_fixated_item*(ot1 + ot3+ ot2) + (1+language_cont+Freq_of_fixated_item+ot1+ot2+ot3|RECORDING_SESSION_LABEL), target_sub_200pre_200post_word_no30bin_poly_centered_catDom_LangContrastCoding)
summary(model_target_sub_200pre_200post_word_no30bin_poly_centered_catDom_LangContrastCoding)

model_target_sub_200pre_200post_word_item_poly_centered_catDom_OP <- lmer(Prop_cs ~ language*DELE.MELICET_cs*Freq_of_fixated_item*(ot1 + ot3+ ot2) + (1+language+Freq_of_fixated_item+ot1+ot2+ot3|RECORDING_SESSION_LABEL)+(1+language+Freq_of_fixated_item+ot1+ot2+ot3|pair_num), target_sub_200pre_200post_word_item_poly_centered_catDom)
summary(model_target_sub_200pre_200post_word_item_poly_centered_catDom_OP)
plot(model_target_sub_200pre_200post_word_item_poly_centered_catDom_OP)
qqnorm(resid(model_target_sub_200pre_200post_word_item_poly_centered_catDom_OP))


model_target_sub_200pre_200post_word_poly_centered_1only <- lmer(Prop_cs ~ language*DELE.MELICET_cs*Freq_of_fixated_item*(ot1 + ot3+ ot2) + (1+language+Freq_of_fixated_item+ot1+ot2+ot3|RECORDING_SESSION_LABEL), target_sub_200pre_200post_word_poly_centered_1only)
summary(model_target_sub_200pre_200post_word_poly_centered_1only)

print(model_target_sub_200pre_200post_word_poly_prop_cs, correlation=TRUE)

model_target_sub_200pre_200post_word_cs <- lmer(Prop_cs ~ language*DELE.MELICET_cs*Freq_of_fixated_item*Bins  + (1+language*Freq_of_fixated_item|RECORDING_SESSION_LABEL), target_sub_200pre_200post_word_poly_centered)
summary(model_target_sub_200pre_200post_word_cs)

model_target_sub_200pre_200post_word_cs_bins_cs <- lmer(Prop_cs ~ language*DELE.MELICET_cs*Freq_of_fixated_item*Bins_cs  + (1+language*Freq_of_fixated_item|RECORDING_SESSION_LABEL), target_sub_200pre_200post_word_poly_centered)
summary(model_target_sub_200pre_200post_word_cs_bins_cs)

model_reg_target_sub_200pre_200post_word_no30bin_poly_centered_catDom_noBins <- lmer(Prop_cs ~ language*DELE.MELICET_cs*Freq_of_fixated_item  + (1+language*Freq_of_fixated_item|RECORDING_SESSION_LABEL), target_sub_200pre_200post_word_no30bin_poly_centered_catDom)
summary(model_reg_target_sub_200pre_200post_word_no30bin_poly_centered_catDom_noBins)

model_target_sub_200pre_200post_word_poly_centered_1only_noBins <- lmer(Prop_cs ~ language*DELE.MELICET_cs*Freq_of_fixated_item  + (1+language*Freq_of_fixated_item|RECORDING_SESSION_LABEL), target_sub_200pre_200post_word_poly_centered_1only)
summary(model_target_sub_200pre_200post_word_poly_centered_1only_noBins)


model_target_sub_200pre_200post_word_cs_bins_cs_prop_nonCs <- lmer(Prop ~ language*DELE.MELICET_cs*Freq_of_fixated_item*Bins_cs  + (1+language*Freq_of_fixated_item|RECORDING_SESSION_LABEL), target_sub_200pre_200post_word_poly_centered)
summary(model_target_sub_200pre_200post_word_cs_bins_cs_prop_nonCs)


model_target_sub_200pre_200post_word_poly_prop_cs
data.frame(target_sub_200pre_200post_word_poly_centered, Predicted=fitted(model_target_sub_200pre_200post_word_poly_prop_cs))->plotearly_poly_cs
str(plotearly_poly_cs)

data.frame(target_sub_200pre_200post_word_poly_centered, Predicted=fitted(model_target_sub_200pre_200post_word_cs))->plotearly_cs
plotearly_cs

data.frame(target_sub_200pre_200post_word_poly_centered_1only, Predicted=fitted(model_target_sub_200pre_200post_word_poly_centered_1only_noBins))->plotearly_cs_noBins_cs_only1
plotearly_cs_noBins_cs_only1


data.frame(target_sub_200pre_200post_word_poly_centered, Predicted=fitted(model_target_sub_200pre_200post_word_cs_bins_cs))->plotearly_cs_noBins_cs_only1

data.frame(target_sub_200pre_200post_word_poly_centered, Predicted=fitted(model_target_sub_200pre_200post_word_cs_bins_cs_prop_nonCs))->plotearly_cs_bins_cs_prop_nonCs
plotearly_cs_bins_cs_prop_nonCs

data.frame(target_sub_200pre_200post_word_poly_centered, Predicted=fitted(model_target_sub_200pre_200post_word_cs_noBins))->plotearly_noBins_prop_cs
plotearly_noBins_prop_cs


#number of observations, 4757, 30 participants x 4 conditions x 20 bins in 400ms x number of looks we looked at, 1 and 2
plot(model_target_sub_200pre_200post_word_cs_noBins)

anova(model_target_sub_200pre_200post_word_poly, model_target_sub_200pre_200post_word_cs)
anova(model_target_sub_200pre_200post_word_cs, model_target_sub_200pre_200post_word_cs_bins_cs)

model_target_sub_200pre_200post_word_poly_0 <- lmer(Prop ~ (ot1 + ot3+ ot2) + (1+language+Freq_of_fixated_item+ot1+ot2+ot3|RECORDING_SESSION_LABEL), target_sub_200pre_200post_word_poly)

summary(model_target_sub_200pre_200post_word_poly)
summary(model_target_sub_200pre_200post_word_poly_0)

anova(model_target_sub_200pre_200post_word_poly, model_target_sub_200pre_200post_word_poly_0)


write.csv(target_sub_200pre_200post_word_poly, "target_sub_200pre_200post_word_poly.csv")

data.frame(target_sub_200pre_200post_word, Predicted=fitted(target_model_200pre_200post_word))->plotearly1
plotearly


ggplot(plotearly_cs_noBins_cs_only1, aes(Bins_cs, Prop_cs, color=Freq_of_fixated_item))+facet_grid(.~language)+stat_summary(aes(y=Predicted, color=Freq_of_fixated_item), fun.y=mean, geom="line")+stat_summary(fun.data=mean_se, geom="pointrange")+theme_bw()+labs(x="-200 ms to word onset, +200 after word onset (normalized)", y = "Proportion of looks to items (normalized)")


anova(target_model_200pre_200post_word, model_target_sub_200pre_200post_word_poly)
