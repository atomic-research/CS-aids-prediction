library(tidyverse)


All_Panels_09202018_07102019_forpaper <- read.delim("~/CSPdata/All_Panels_09202018_07102019_forpaper.txt")

t.test(All_Panels_09202018_07102019_forpaper$Target_Freq_eng, All_Panels_09202018_07102019_forpaper$Target_Freq_sp, paired = TRUE)

t.test(All_Panels_09202018_07102019_forpaper_lohi_comparison$Target_Freq_eng_hi, All_Panels_09202018_07102019_forpaper_lohi_comparison$Target_Freq_eng_lo, paired = TRUE)

t.test(All_Panels_09202018_07102019_forpaper_lohi_comparison$Target_Freq_sp_hi, All_Panels_09202018_07102019_forpaper_lohi_comparison$Target_Freq_sp_lo, paired = TRUE)

All_Panels_09202018_07102019_forpaper_summ <- All_Panels_09202018_07102019_forpaper %>%
  summarize(Mean_eng = mean(Target_Freq_eng), SD_eng = sd(Target_Freq_eng), Mean_sp = mean(Target_Freq_sp), SD_sp = sd(Target_Freq_sp))

All_Panels_09202018_07102019_forpaper_summ_byFreq_eng <- All_Panels_09202018_07102019_forpaper %>%
  group_by(Condition_eng) %>%
  summarize(Mean = mean(Target_Freq_eng), SD = sd(Target_Freq_eng))

All_Panels_09202018_07102019_forpaper_summ_byFreq_sp <- All_Panels_09202018_07102019_forpaper %>%
  group_by(Condition_sp) %>%
  summarize(Mean = mean(Target_Freq_sp), SD = sd(Target_Freq_sp))

All_Panels_09202018_07102019_forpaper_freqDif <- All_Panels_09202018_07102019_forpaper %>%
  mutate(AbsDif = abs(Target_Freq_eng-Target_Freq_sp))  %>%
  mutate(Mean_absDif = mean(AbsDif), SD_absDif = sd(AbsDif))

round(0.5422216, 3)
