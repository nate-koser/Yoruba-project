#library packages --------------------------------------------------------------------------

library(tidyverse)
library(foreign)
library(nnet)
library(reshape2)
library(RColorBrewer)
library(lme4)
library(lmerTest)
library(MuMIn)

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

#create avg columns 
datana <- datana %>%
    mutate(., avg_f3 = (f3_1 + f3_2 + f3_3 + f3_4) / 4,
              avg_f2 = (f2_1 + f2_2 + f2_3 + f2_4) / 4,
              avg_f1 = (f1_1 + f1_2 + f1_3 + f1_4) / 4,
              avg_spec = (specTilt_1 + specTilt_2 + specTilt_3 + specTilt_4) / 4,
              avg_hnr = (hnr_1 + hnr_2 + hnr_3 + hnr_4) / 4,
              avg_f0 = (f0_1 + f0_2 + f0_3 + f0_4) / 4,
              avg_jitt = (jitter_1 + jitter_2 + jitter_3 + jitter_4) / 4,
              avg_shim = (shimmer_1 + shimmer_2 + shimmer_3 + shimmer_4) / 4,
              avg_amp = (amp_1 + amp_2 + amp_3 + amp_4) / 4,
              avg_f1minusf0 = (avg_f1 - avg_f0))


# specify L as reference level for modeling
datana$target_tone <- relevel(datana$target_tone, ref = "L")

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
dat.m <- melt(Ltones,id.vars = 'block', measure.vars = c('f0_1','f0_2','f0_3','f0_4'))

#restructure so portions of vowel are plottable by hnr
dat.mhnr <- melt(Ltones,id.vars = 'target_tone', measure.vars = c('hnr_1','hnr_2','hnr_3','hnr_4'))

#same for spec
mspec <- melt(Ltones,id.vars = 'target_tone', measure.vars = c('specTilt_1','specTilt_2','specTilt_3','specTilt_4'))

#saem for f1minusf0
mf1f0 <- melt(Ltones,id.vars = 'target_tone', measure.vars = c('avg_f1minusf0'))

f0 ~ slice

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
mean(Ltones$avg_f1minusf0, na.rm = T)
sd(Ltones$avg_f1minusf0, na.rm = T)
mean(Mtones$avg_f1minusf0, na.rm = T)
sd(Mtones$avg_f1minusf0, na.rm = T)
mean(Htones$avg_f1minusf0, na.rm = T)
sd(Htones$avg_f1minusf0, na.rm = T)


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
  
  ggplot(datatry) +
    geom_point(aes(x = avg_f0, y = value, color = target_tone), na.rm = T, show.legend = F) +
    labs(y = "f0", x = "portion of vowel") #+
    #scale_color_brewer(palette="Blues")
  
  
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
  
  ggplot(dat.m, aes(x=variable, y=value, group=1, color = target_tone)) +
    stat_summary(geom="line")
  
#modeling -------------------------------------------------------------------------------

# subset of data for modeling
datasubset <- datana %>%
  select(., avg_hnr, avg_spec,target_voweldur,avg_f1minusf0,target_tone,avg_shim,avg_jitt)

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


#lmer

#duration
durmod <- lmer(target_voweldur ~ target_tone + (1|block) + (1|target_vowel), data = datana)
summary(durmod)
r.squaredGLMM(durmod)

#f0
f0mod <- lmer(avg_f0 ~ target_tone + (1|block) + (1|target_vowel), data = datana)
summary(f0mod)
r.squaredGLMM(f0mod)

#HNR
hnrmod <- lmer(avg_hnr ~ target_tone + (1|block) + (1|target_vowel), data = datana)
summary(hnrmod)
r.squaredGLMM(hnrmod)


#spectral tilt
specmod <- lmer(avg_spec ~ target_tone + (1|block) + (1|target_vowel), data = datana)
summary(specmod)
r.squaredGLMM(specmod)


#f1 minus f0 vowel
f1f0mod <- lmer(avg_f1minusf0 ~ target_tone + (1|target_vowel) , data = datana)
summary(f1f0mod)
r.squaredGLMM(f1f0mod)

#test model
testt <- lmer(value ~ variable + (1|block) , data = dat.m)
summary(testt)
r.squaredGLMM(testt)









