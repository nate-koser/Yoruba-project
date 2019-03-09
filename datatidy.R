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
  select(., - target_sylonsvoice,
            - error,
            - F0_error,
            - carrier_word,
            - target_start,
            - target_sylonsclass,
            - target_sylonsdur)

#get rid of extreme outliers
datatry <- datana %>%
  filter(., block != 1 | trial != 53,
            block != 1 | trial != 57,
            block != 2 | trial != 5,
            block != 2 | trial != 45,
            block != 2 | trial != 53,
            block != 4 | trial != 30,
            block != 4 | trial != 53,
            block != 4 | trial != 54,
            block != 4 | trial != 57,
            block != 5 | trial != 3)


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
