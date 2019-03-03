#library packages --------------------------------------------------------------------------

library(tidyverse)
library(foreign)
library(nnet)
library(reshape2)
library(RColorBrewer)
#data + data transformations----------------------------------------------------------------

#load data
datana <- read.csv("G:/My Drive/work/yorubaexp/BRG/MM02/MM02/MM02_tgrids.csv") 

#get rid of unnecessary columns
datana <- datana %>%
select(., - target_sylonsvoice) %>%
select(., - error) %>%
select(., - F0_error)  

#get rid of extreme outliers
datana <- datana %>%
  filter(., X != "53") %>%
  filter(., X != "57") %>%
  filter(., X != "68") %>%
  filter(., X != "108") %>%
  filter(., X != "116") %>%
  filter(., X != "219") %>%
  filter(., X != "242") %>%
  filter(., X != "243") %>%
  filter(., X != "246") %>%
  filter(., X != "255") 

#create avg spectilt column
datana <- datana %>%
  mutate(avg_spec = (specTilt_1 + specTilt_2 + specTilt_3 + specTilt_4) / 4) 

#avg hnr column
datana <- datana %>%
  mutate(avg_hnr = (hnr_1 + hnr_2 + hnr_3 + hnr_4) / 4) 



#avg f3 column
datana <- datana %>%
  mutate(avg_f3 = (f3_1 + f3_2 + f3_3 + f3_4) / 4) 

#avg f2 column
datana <- datana %>%
  mutate(avg_f2 = (f2_1 + f2_2 + f2_3 + f2_4) / 4)   

#avg f1 column
datana <- datana %>%
  mutate(avg_f1 = (f1_1 + f1_2 + f1_3 + f1_4) / 4) 

#avg f0 column
datana <- datana %>%
  mutate(avg_f0 = (f0_1 + f0_2 + f0_3 + f0_4) / 4) 

#avg shimmer column
datana <- datana %>%
  mutate(avg_shim = (shimmer_1 + shimmer_2 + shimmer_3 + shimmer_4) / 4) 

#avg jitter column
datana <- datana %>%
  mutate(avg_jitt = (jitter_1 + jitter_2 + jitter_3 + jitter_4) / 4) 

#avg amplitude column
datana <- datana %>%
  mutate(avg_amp = (amp_1 + amp_2 + amp_3 + amp_4) / 4)

#avg f1-f0 column
datana <- datana %>%
  mutate(avgf1minusf0 = (avg_f1 - avg_f0)) 

# specify L as reference level for modeling
datana$target_tone <- relevel(datasubset$target_tone, ref = "L")

#isolate tone categories    
Ltones <- datana %>%
  filter(target_tone == "L")

Mtones <- datana %>%
  filter(target_tone == "M")

Htones <- datana %>%
  filter(target_tone == "H")

#avg Ltone f0 first vs. second half
Ltones <- Ltones %>%
  mutate(avg_f0_half1 = (f0_1 + f0_2) / 2) 
Ltones <- Ltones %>%
  mutate(avg_f0_half2 = (f0_3 + f0_4) / 2) 

#restructure so portions of vowel are plottable by f0
dat.m <- melt(Ltones,id.vars = 'target_tone', measure.vars = c('f0_1','f0_2','f0_3','f0_4'))

#restructure so portions of vowel are plottable by hnr
dat.mhnr <- melt(Ltones,id.vars = 'target_tone', measure.vars = c('hnr_1','hnr_2','hnr_3','hnr_4'))

#same for spec
mspec <- melt(Ltones,id.vars = 'target_tone', measure.vars = c('specTilt_1','specTilt_2','specTilt_3','specTilt_4'))

#saem for f1minusf0
mf1f0 <- melt(Ltones,id.vars = 'target_tone', measure.vars = c('avgf1minusf0'))


#means, sd, various ------------------------------------------------------------------------

#mean + sd vowel durations
mean(Ltones$target_dur, na.rm = T)
sd(Ltones$f0_2, na.rm = T)

mean(Mtones$target_dur, na.rm = T)
sd(Mtones$f0_2, na.rm = T)

mean(Htones$target_dur, na.rm = T)
sd(Htones$f0_2, na.rm = T)

#minimum f0_2
min(Ltones$f0_2, na.rm = T)
min(Mtones$f0_2, na.rm = T)
min(Htones$f0_2, na.rm = T)

#median f0_2
median(Ltones$f0_2, na.rm = T)
median(Mtones$f0_2, na.rm = T)
median(Htones$f0_2, na.rm = T)

#avg Ltone f0 first vs. second half
mean(Ltones$avg_f0_half1, na.rm = T)
mean(Ltones$avg_f0_half2, na.rm = T)

#avg Ltone f0 slice 1 vs. slice 4
mean(Ltones$f0_1, na.rm = T)
mean(Ltones$f0_4, na.rm = T)

#avg + sd f0 per tone
mean(Ltones$avg_f0, na.rm = T)
sd(Ltones$avg_f0, na.rm = T)
mean(Mtones$avg_f0, na.rm = T)
sd(Mtones$avg_f0, na.rm = T)
mean(Htones$avg_f0, na.rm = T)
sd(Htones$avg_f0, na.rm = T)



#f1-f0
mean(Ltones$avgf1minusf0, na.rm = T)
sd(Ltones$avgf1minusf0, na.rm = T)
mean(Mtones$avgf1minusf0, na.rm = T)
sd(Mtones$avgf1minusf0, na.rm = T)
mean(Htones$avgf1minusf0, na.rm = T)
sd(Htones$avgf1minusf0, na.rm = T)


#avg + sd duration 
mean(Ltones$target_voweldur, na.rm = T)
sd(Ltones$target_voweldur, na.rm = T)
mean(Mtones$target_voweldur, na.rm = T)
sd(Mtones$target_voweldur, na.rm = T)
mean(Htones$target_voweldur, na.rm = T)
sd(Htones$target_voweldur, na.rm = T)

#avg + sd HNR 
mean(Ltones$avg_hnr, na.rm = T)
sd(Ltones$avg_hnr, na.rm = T)
mean(Mtones$avg_hnr, na.rm = T)
sd(Mtones$avg_hnr, na.rm = T)
mean(Htones$avg_hnr, na.rm = T)
sd(Htones$avg_hnr, na.rm = T)

#avg + sd spec 
mean(Ltones$avg_spec, na.rm = T)
sd(Ltones$avg_spec, na.rm = T)
mean(Mtones$avg_spec, na.rm = T)
sd(Mtones$avg_spec, na.rm = T)
mean(Htones$avg_spec, na.rm = T)
sd(Htones$avg_spec, na.rm = T)

#mean L hnr by slice
mean(Ltones$hnr_1, na.rm = T)
mean(Ltones$hnr_2, na.rm = T)
mean(Ltones$hnr_3, na.rm = T)
mean(Ltones$hnr_4, na.rm = T)

#mean M hnr by slice
mean(Mtones$hnr_1, na.rm = T)
mean(Mtones$hnr_2, na.rm = T)
mean(Mtones$hnr_3, na.rm = T)
mean(Mtones$hnr_4, na.rm = T)

#mean L spec by slice
mean(Ltones$specTilt_1, na.rm = T)
mean(Ltones$specTilt_2, na.rm = T)
mean(Ltones$specTilt_3, na.rm = T)
mean(Ltones$specTilt_4, na.rm = T)

#mean L f1f0 by slice
mean(Ltones$f1_1, na.rm = T) - mean(Ltones$f0_1, na.rm = T)
mean(Ltones$f1_2, na.rm = T) - mean(Ltones$f0_2, na.rm = T)
mean(Ltones$f1_3, na.rm = T) - mean(Ltones$f0_3, na.rm = T)
mean(Ltones$f1_4, na.rm = T) - mean(Ltones$f0_4, na.rm = T)

#mean H f1f0 by slice
mean(Htones$f1_1, na.rm = T) - mean(Htones$f0_1, na.rm = T)
mean(Htones$f1_2, na.rm = T) - mean(Htones$f0_2, na.rm = T)
mean(Htones$f1_3, na.rm = T) - mean(Htones$f0_3, na.rm = T)
mean(Htones$f1_4, na.rm = T) - mean(Htones$f0_4, na.rm = T)

#mean M f1f0 by slice
mean(Mtones$f1_1, na.rm = T) - mean(Mtones$f0_1, na.rm = T)
mean(Mtones$f1_2, na.rm = T) - mean(Mtones$f0_2, na.rm = T)
mean(Mtones$f1_3, na.rm = T) - mean(Mtones$f0_3, na.rm = T)
mean(Mtones$f1_4, na.rm = T) - mean(Mtones$f0_4, na.rm = T)

#mean L jitt by slice
mean(Ltones$jitter_1, na.rm = T)
mean(Ltones$jitter_2, na.rm = T)
mean(Ltones$jitter_3, na.rm = T)
mean(Ltones$jitter_4, na.rm = T)

#mean M jitt by slice
mean(Mtones$jitter_1, na.rm = T)
mean(Mtones$jitter_2, na.rm = T)
mean(Mtones$jitter_3, na.rm = T)
mean(Mtones$jitter_4, na.rm = T)

#mean H jitt by slice
mean(Htones$jitter_1, na.rm = T)
mean(Htones$jitter_2, na.rm = T)
mean(Htones$jitter_3, na.rm = T)
mean(Htones$jitter_4, na.rm = T)


#plots------------------------------------------------------------------------------------------

#f0 by tone, slice 1
ggplot(data = datana) +
geom_boxplot(mapping = aes(x = target_tone, y = f0_1, fill = target_tone), na.rm = TRUE) +
ylim (75,200)

#f0 by tone, slice 2
ggplot(data = datana) +
  geom_boxplot(mapping = aes(x = target_tone, y = f0_2, fill = target_tone), na.rm = TRUE) +
  ylim (75,200) 

#f0 by tone, slice 3
ggplot(data = datana) +
  geom_boxplot(mapping = aes(x = target_tone, y = f0_3, fill = target_tone), na.rm = TRUE) +
  ylim (75,200) 

#f0 by tone, slice 4
ggplot(data = datana) +
geom_boxplot(mapping = aes(x = target_tone, y = f0_4, fill = target_tone), na.rm = TRUE) +
ylim (75,200)

#SpecTilt by HNR slice 1
ggplot(data = datana) +
geom_point(mapping = aes(x = specTilt_1, y = hnr_1, color = target_tone), na.rm = TRUE) +
xlim(-20,20)

#SpecTilt by HNR slice 2
ggplot(data = datana) +
  geom_point(mapping = aes(x = specTilt_2, y = hnr_2, color = target_tone), na.rm = TRUE) +
  xlim(-20,20)

#SpecTilt by HNR slice 3
ggplot(data = datana) +
  geom_point(mapping = aes(x = specTilt_3, y = hnr_3, color = target_tone), na.rm = TRUE) +
  xlim(-20,20)

#SpecTilt by HNR slice 4
ggplot(data = datana) +
  geom_point(mapping = aes(x = specTilt_4, y = hnr_4, color = target_tone), na.rm = TRUE) +
  xlim(-20,20)

#annotate parts of plot
#annotate("rect", xmin = -11, xmax = 17, ymin = -14, ymax = 10.5, alpha = .2)

#plot avg spec + hnr
  ggplot(datana) +
  geom_point(mapping = aes(x = avg_spec, y = avg_hnr, color = target_tone, shape = target_tone), na.rm = TRUE) +
    labs(x = "Average spectral tilt", y = "Average HNR", key = "category") +
    theme(legend.title=element_blank()) +
    scale_color_brewer(palette="Set1")
  
  #spec - H2-H1. HNR - periodicity to noise ratio, signal to noise

#avg f0 plot
  ggplot(data = datana) +
    geom_boxplot(mapping = aes(x = target_tone, y = avg_f0, fill = target_tone), na.rm = TRUE) +
    ylim (75,200)
  
  #avg f1 plot
  ggplot(data = datana) +
    geom_boxplot(mapping = aes(x = target_tone, y = avg_f1, fill = target_tone), na.rm = TRUE) 
  
  #avg f2 plot
  ggplot(data = datana) +
    geom_boxplot(mapping = aes(x = target_tone, y = avg_f2, fill = target_tone), na.rm = TRUE) 
  
  
  #avg f3 plot
  ggplot(data = datana) +
    geom_boxplot(mapping = aes(x = target_tone, y = avg_f3, fill = target_tone), na.rm = TRUE) 
  
  #avg amp plot
  ggplot(datana) +
    geom_boxplot(aes(x = target_tone, y = avg_amp, fill = target_tone), na.rm = T)
  
  #shimmer jitter plot
  ggplot(datana) +
    geom_point(mapping = aes(x = shimmer_3, y = jitter_3, color = target_tone), na.rm = TRUE) 
  
  #f0 by sylons, slice 1
  ggplot(data = datana) +
    geom_boxplot(mapping = aes(x = target_sylons, y = f0_1, fill = target_tone), na.rm = TRUE) +
    ylim (75,200)
  
  #f1 by tone
  ggplot(data = datana) +
    geom_boxplot(mapping = aes(x = target_tone, y = avg_f1, fill = target_tone), na.rm = TRUE)
  
  #Ltone f0 slice
  ggplot(data = Ltones) +
    geom_boxplot(mapping = aes(x = target_tone, y = f0_1, fill = target_tone), na.rm = T, show.legend = F) +
    ylim (85,120) +
    labs(x = "L, slice 1") +
    labs(y = "f0")
  
  
  ggplot(data = Ltones) +
    geom_boxplot(mapping = aes(x = target_tone, y = f0_4, fill = target_tone), na.rm = TRUE, show.legend = F) +
    ylim (85,120) +
    labs(x = "L, slice 4") +
    labs(y = "f0")
  
  
  
  ggplot(dat.m) +
    geom_boxplot(aes(x = variable, y = value, fill = variable), na.rm = T, show.legend = F) +
    labs(y = "f0", x = "portion of vowel") +
    scale_fill_brewer(palette="Blues")
  
  ggplot(dat.mhnr) +
    geom_boxplot(aes(x = variable, y = value, fill = variable), na.rm = T, show.legend = F) +
    labs(y = "HNR", x = "portion of vowel") +
    scale_fill_brewer(palette="Blues")+
    ylim (-10,10)
  
  ggplot(mspec) +
    geom_boxplot(aes(x = variable, y = value, fill = variable), na.rm = T, show.legend = F) +
    labs(y = "spectral tilt", x = "portion of vowel") +
    scale_fill_brewer(palette="Blues")+
    ylim (-10,10)
  
  ggplot(mf1f0) +
    geom_boxplot(aes(x = variable, y = value, fill = variable), na.rm = T, show.legend = F) +
    labs(y = "spectral tilt", x = "portion of vowel") +
    scale_fill_brewer(palette="Blues")
  
#modeling -------------------------------------------------------------------------------

# subset of data for modeling
datasubset <- datana %>%
  select(., avg_hnr, avg_spec,target_voweldur,avgf1minusf0,target_tone,avg_shim,avg_jitt)

##multinomial regression 

# specify L as reference level for modeling
datana$target_tone <- relevel(datasubset$target_tone, ref = "L")
test <- multinom(target_tone2 ~ avg_hnr + avg_spec, data = datasubset)

summary(test)

#z scores
z <- summary(test)$coefficients/summary(test)$standard.errors
z

#2 tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


##lm

f0slicemod <- lm(avg_f0 ~ f0_1 + f0_2 + f0_3 + f0_4, data = Ltones)
summary(f0mod)


hnrslicemod <- lm(avg_hnr ~ hnr_1 + hnr_2 + hnr_3 +hnr_4, data = Ltones)
summary(hnrslicemod)

avghnrmod <- lm(avg_hnr ~ target_tone, data = datana)
summary(avghnrmod)

avgspecmod <- lm(avg_spec ~ target_tone, data = datana)
summary(avgspecmod)

specslicemod <- lm(avg_spec ~ specTilt_1 + specTilt_2 + specTilt_3 + specTilt_4, data = Ltones)
summary(specslicemod)

avgf1f0mod <- lm(avgf1minusf0 ~ target_tone, data = datana)
summary(avgf1f0mod)

avgdurmod <- lm(target_voweldur ~ target_tone, data = datana)
summary(avgdurmod)

avgjittmod <- lm(avg_jitt ~ target_tone, data = datana)
summary(avgjittmod)

#tonehnrintmod <- lm(avg_spec ~ target_tone * avg_hnr, data = datana)
#summary(tonehnrintmod)
#
#tonespecintmod <- lm(avg_hnr ~ target_tone * avg_spec, data = datana)
#summary(tonespecintmod)
#
# 
# nullmod <- lm(as.numeric(target_tone) ~ 1, data = datanafree)
# specmod <- lm(as.numeric(target_tone) ~  avg_spec, data = datanafree)
# #better than null 
# f1f0mod <- lm(as.numeric(target_tone) ~ avgf1minusf0, data = datanafree)
# #non-sig trend over null F(1) = 3.4 p = 0.06
# SHaddmod <- lm(as.numeric(target_tone) ~ avg_spec + avg_hnr, data = datanafree)
# #better than just spec F(1) = 97.74 p < .001
# SHintmod <- lm(as.numeric(target_tone) ~ avg_spec + avg_hnr + avg_spec:avg_hnr, data = datanafree)
# #SHint no better than spec + hnr, doesn't add much, F(1) = 1.59, p = .2
# hnrmod <- lm(as.numeric(target_tone) ~  avg_hnr, data = datanafree)
# #beetter than null
# SFaddmod <- lm(as.numeric(target_tone) ~  avg_spec + avgf1minusf0, data = datanafree)
# #better than just spec F(1) = 12.16 p < 0.001
# SFintmod <- lm(as.numeric(target_tone) ~  avg_spec * avgf1minusf0, data = datanafree)
# #better F(1) = F(1) p < 0.01 for interatcion of spec and f1-f0
# HFaddmod <- lm(as.numeric(target_tone) ~  avg_hnr + avgf1minusf0, data = datanafree)
# #better than just hnr F(1) = 16.9 p < 0.001
# HFintmod <- lm(as.numeric(target_tone) ~  avg_hnr * avgf1minusf0, data = datanafree)
# #not better - interaction of hnr and f1f0 not better F(1) = 2.4 p = .12
# SHFaddmod <- lm(as.numeric(target_tone) ~ avg_spec + avg_hnr + avgf1minusf0, data = datanafree)
# #not better - adding f1f0 to spec + hnr not better F(1) = 1.8 p = .12
# 
# fullmod <- lm(as.numeric(target_tone) ~ avg_spec * avg_hnr * avgf1minusf0, data = datanafree)
# 
# summary(fullmod)
# 0.15
# 
# anova(DSaddmod, DSintmod)
# 
# fullmodwjs <- lm(as.numeric(target_tone) ~ avg_spec * avg_hnr * avgf1minusf0 * avg_shim * avg_jitt, data = datanafree)
# summary(fullmodwjs)
# 
# 
# durmod <- lm(as.numeric(target_tone) ~ target_voweldur, data = datanafree)
# #better than null
# DSaddmod <- lm(as.numeric(target_tone) ~ target_voweldur + avg_spec, data = datanafree)
# #better than just duration F(1) = 133.3 p < .001
# DSintmod <- lm(as.numeric(target_tone) ~ target_voweldur * avg_spec, data = datanafree)
# #no better than just add dur and spec F(1) = .64 p = .42



#leaves you with ONE L TONE DATA POINT! BIG RED BUTTON!!!
# datasubset <- na.exclude(datasubset)






