#load packages --------------------------------------------------------------------------

require(tidyverse)
require(foreign)
require(nnet)
require(reshape2)
require(RColorBrewer)
require(lme4)
require(lmerTest)
require(MuMIn)
require(Hmisc)
require(emmeans)

#CV ----------------------------------------------------------------------------------------
#data + data transformations----------------------------------------------------------------

#load data
datana <- read.csv("C:/Users/Nate/Documents/GitHub/Yoruba-project/MM02_tgrids.csv")

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
datana <- datana %>%
  filter(., block != 1 | trial != 53,  #tO L
            block != 1 | trial != 57,  #ti L
            block != 2 | trial != 5,   #la M
            block != 2 | trial != 45,  #ti L
            block != 2 | trial != 53,  #lO L
            block != 4 | trial != 30,  #ti L
            block != 4 | trial != 53,  #tE L
            block != 4 | trial != 54,  #nE H
            block != 4 | trial != 57,  #ni L
            block != 4 | trial != 25,  #no M
            block != 5 | trial != 3)   #nE H


#create columns
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
         avg_f1minusf0 = (avg_f1 - avg_f0),
         f1f01 = (f1_1 - f0_1),
         f1f02 = (f1_2 - f0_2),
         f1f03 = (f1_3 - f0_3),
         f1f04 = (f1_4 - f0_4))
datana$word <- paste(datana$target_word,datana$target_tone, sep = "_")


# specify L as reference level for modeling
datana$target_tone <- relevel(datana$target_tone, ref = "L")

#specify different slices as reference level, for example:
#dat.mf0$variable <- relevel(dat.mf0$variable, ref = "f0_1")

#isolate tone categories
Ltones <- datana %>%
  filter(target_tone == "L")

Mtones <- datana %>%
  filter(target_tone == "M")

Htones <- datana %>%
  filter(target_tone == "H")



#restructure so portions of vowel are modelable as ind. vars on DV f0, one frame per tone
dat.f0 <- melt(datana,id.vars = c('block','target_tone','target_sylons','word'), measure.vars = c('f0_1','f0_2','f0_3','f0_4'))

dat.lf0 <- melt(Ltones,id.vars = c('block','target_tone','target_sylons','word'), measure.vars = c('f0_1','f0_2','f0_3','f0_4'))

dat.mf0 <- melt(Mtones,id.vars = c('block','target_tone','target_sylons','word'), measure.vars = c('f0_1','f0_2','f0_3','f0_4'))

dat.hf0 <- melt(Htones,id.vars = c('block','target_tone','target_sylons','word'), measure.vars = c('f0_1','f0_2','f0_3','f0_4'))

#same for HNR
dat.lh <- melt(Ltones,id.vars = c('block','target_vowel','target_tone','word'), measure.vars = c('hnr_1','hnr_2','hnr_3','hnr_4'))

dat.mh <- melt(Mtones,id.vars = c('block','target_vowel','target_tone','word'), measure.vars = c('hnr_1','hnr_2','hnr_3','hnr_4'))

dat.hh <- melt(Htones,id.vars = c('block','target_vowel','target_tone','word'), measure.vars = c('hnr_1','hnr_2','hnr_3','hnr_4'))

#same for spec
dat.ls <- melt(Ltones,id.vars = c('block','target_tone','target_sylons','target_vowel','word'), measure.vars = c('specTilt_1','specTilt_2','specTilt_3','specTilt_4'))

dat.ms <- melt(Mtones,id.vars = c('block','target_tone','target_sylons','target_vowel','word'), measure.vars = c('specTilt_1','specTilt_2', 'specTilt_3','specTilt_4'))

dat.hs <- melt(Htones,id.vars = c('block','target_tone','target_sylons','target_vowel','word'), measure.vars = c('specTilt_1','specTilt_2', 'specTilt_3','specTilt_4'))

#same for f1minusf0
dat.l10 <- melt(Ltones,id.vars = 'block', measure.vars = c('f1f01', 'f1f02', 'f1f03', 'f1f04'))

dat.m10 <- melt(Mtones,id.vars = 'block', measure.vars = c('f1f01', 'f1f02', 'f1f03', 'f1f04'))

dat.h10 <- melt(Htones,id.vars = 'block', measure.vars = c('f1f01', 'f1f02', 'f1f03', 'f1f04'))


#rearrange data to plot avg f0 over time (probably a better way to do this?)
datf0plot <- melt(Ltones,id.vars = 'target_tone', measure.vars = c('f0_1','f0_2','f0_3','f0_4'))
datf0plotm <- melt(Mtones,id.vars = 'target_tone', measure.vars = c('f0_1','f0_2','f0_3','f0_4'))
datf0ploth <- melt(Htones,id.vars = 'target_tone', measure.vars = c('f0_1','f0_2','f0_3','f0_4'))

#sanity checks
#datasane <- datana %>%
  #filter(., target_vowel == "E")
  #view(datasane)

  #datasanee <- datana %>%
   # filter(., target_vowel == "e")
  #view(datasanee)

#CVCV --------------------------------------------------------------------------------------------

datacvcv <- read.csv("C:/Users/Nate/Documents/GitHub/Yoruba-project/cvcv_tgrids.csv")

#get rid of bad tokens
datacvcv <- datacvcv %>%
  filter(., subj != 'mz1' | block != 1 | trial != 51,
            subj != 'mz1' | block != 1 | trial != 65,
            subj != 'mz1' | block != 1 | trial != 67,
            subj != 'mz1' | block != 2 | trial != 54,
            subj != 'mz1' | block != 3 | trial != 4,
            subj != 'mz1' | block != 3 | trial != 74,
            subj != 'mz1' | block != 3 | trial != 75,
            subj != 'mz1' | block != 4 | trial != 73,
            subj != 'mz2' | block != 1 | trial != 21,
            subj != 'mz2' | block != 1 | trial != 22,
            subj != 'mz2' | block != 1 | trial != 9,
            subj != 'mz2' | block != 2 | trial != 5,
            subj != 'mz2' | block != 2 | trial != 16,
            subj != 'mz2' | block != 2 | trial != 17,
            subj != 'mz2' | block != 2 | trial != 49,
            subj != 'mz2' | block != 2 | trial != 70,
            subj != 'mz2' | block != 2 | trial != 81,
            subj != 'mz2' | block != 3 | trial != 39,
            subj != 'mz2' | block != 3 | trial != 41)

#create columns
datacvcv <- datacvcv %>%
  mutate(., avg_f1_v1 = (f1_1 + f1_2 + f1_3 + f1_4) / 4,
         avg_spec_v1 = (specTilt_1 + specTilt_2 + specTilt_3 + specTilt_4) / 4,
         avg_hnr_v1 = (hnr_1 + hnr_2 + hnr_3 + hnr_4) / 4,
         avg_f0_v1 = (f0_1 + f0_2 + f0_3 + f0_4) / 4,
         avg_jitt_v1 = (jitter_1 + jitter_2 + jitter_3 + jitter_4) / 4,
         avg_shim_v1 = (shimmer_1 + shimmer_2 + shimmer_3 + shimmer_4) / 4,
         avg_amp_v1 = (amp_1 + amp_2 + amp_3 + amp_4) / 4,
         avg_f1_v2 = (f1_1_v2 + f1_2_v2 + f1_3_v2 + f1_4_v2) / 4,
         avg_spec_v2 = (specTilt_1_v2 + specTilt_2_v2 + specTilt_3_v2 + specTilt_4_v2) / 4,
         avg_hnr_v2 = (hnr_1_v2 + hnr_2_v2 + hnr_3_v2 + hnr_4_v2) / 4,
         avg_f0_v2 = (f0_1_v2 + f0_2_v2 + f0_3_v2 + f0_4_v2) / 4,
         avg_jitt_v2 = (jitter_1_v2 + jitter_2_v2 + jitter_3_v2 + jitter_4_v2) / 4,
         avg_shim_v2 = (shimmer_1_v2 + shimmer_2_v2 + shimmer_3_v2 + shimmer_4_v2) / 4,
         avg_amp_v2 = (amp_1_v2 + amp_2_v2 + amp_3_v2 + amp_4_v2) / 4,
         avg_word_hnr = (avg_hnr_v1 + avg_hnr_v2) / 2,
         avg_word_spec = (avg_spec_v1 + avg_spec_v2) / 2,
         avg_word_f0 = (avg_f0_v1 + avg_f0_v2) / 2)

datacvcv$toneseq <- paste(datacvcv$tone1,datacvcv$tone2, sep = "")
datacvcv$toneseq <- as.factor(datacvcv$toneseq)
datacvcv$word <- paste(datacvcv$word,datacvcv$toneseq, sep = "_")

# specify L + LL as reference level for modeling
datacvcv$tone1 <- relevel(datacvcv$tone1, ref = "L")
datacvcv$tone2 <- relevel(datacvcv$tone2, ref = "L")
datacvcv$toneseq <- relevel(datacvcv$toneseq, ref = "LL")

#isolate subj
datas1 <- datacvcv %>%
  filter(subj == "mz1")
datas2 <- datacvcv %>%
  filter(subj == "mz2")

#filter out contours
datanocon <- datacvcv %>%
  filter(toneseq != "LH",
         toneseq != "HL")

#isolate tone categories
Ltones_v1 <- datacvcv %>%
  filter(tone1 == "L")
Mtones_v1 <- datacvcv %>%
  filter(tone1 == "M")
Htones_v1 <- datacvcv %>%
  filter(tone1 == "H")
Ltones_v2 <- datacvcv %>%
  filter(tone2 == "L")
Mtones_v2 <- datacvcv %>%
  filter(tone2 == "M")
Htones_v2 <- datacvcv %>%
  filter(tone2 == "H")

#tone categories and subjects
Ltones_v11 <- datacvcv %>%
  filter(tone1 == "L",  subj == "mz1")
Mtones_v11 <- datacvcv %>%
  filter(tone1 == "M", subj == "mz1")
Htones_v11 <- datacvcv %>%
  filter(tone1 == "H", subj == "mz1")
Ltones_v21 <- datacvcv %>%
  filter(tone2 == "L", subj == "mz1")
Mtones_v21 <- datacvcv %>%
  filter(tone2 == "M", subj == "mz1")
Htones_v21 <- datacvcv %>%
  filter(tone2 == "H", subj == "mz1")

Ltones_v12 <- datacvcv %>%
  filter(tone1 == "L",  subj == "mz2")
Mtones_v12 <- datacvcv %>%
  filter(tone1 == "M", subj == "mz2")
Htones_v12 <- datacvcv %>%
  filter(tone1 == "H", subj == "mz2")
Ltones_v22 <- datacvcv %>%
  filter(tone2 == "L", subj == "mz2")
Mtones_v22 <- datacvcv %>%
  filter(tone2 == "M", subj == "mz2")
Htones_v22 <- datacvcv %>%
  filter(tone2 == "H", subj == "mz2")


#restructure so portions of vowel are modelable as ind. vars on DV f0, one frame per tone
dat.f0_v1 <- melt(datacvcv,id.vars = c('block','tone1','vowel1','word', 'subj','toneseq'), measure.vars = c('f0_1','f0_2','f0_3','f0_4'))
dat.lf0_v1 <- melt(Ltones_v1,id.vars = c('block','tone1','vowel1','word', 'subj','toneseq'), measure.vars = c('f0_1','f0_2','f0_3','f0_4'))
dat.mf0_v1 <- melt(Mtones_v1,id.vars = c('block','tone1','vowel1','word', 'subj','toneseq'), measure.vars = c('f0_1','f0_2','f0_3','f0_4'))
dat.hf0_v1 <- melt(Htones_v1,id.vars = c('block','tone1','vowel1','word', 'subj','toneseq'), measure.vars = c('f0_1','f0_2','f0_3','f0_4'))
dat.f0_v2 <- melt(datacvcv,id.vars = c('block','tone2','vowel2','word', 'subj','toneseq'), measure.vars = c('f0_1_v2','f0_2_v2','f0_3_v2','f0_4_v2'))
dat.lf0_v2 <- melt(Ltones_v2,id.vars = c('block','tone2','vowel2','word', 'subj','toneseq'), measure.vars = c('f0_1_v2','f0_2_v2','f0_3_v2','f0_4_v2'))
dat.mf0_v2 <- melt(Mtones_v2,id.vars = c('block','tone2','vowel2','word', 'subj','toneseq'), measure.vars = c('f0_1_v2','f0_2_v2','f0_3_v2','f0_4_v2'))
dat.hf0_v2 <- melt(Htones_v2,id.vars = c('block','tone2','vowel2','word', 'subj','toneseq'), measure.vars = c('f0_1_v2','f0_2_v2','f0_3_v2','f0_4_v2'))

#same for HNR
dat.lh_v1 <- melt(Ltones_v1,id.vars = c('block','vowel1','tone1','word', 'subj','toneseq'), measure.vars = c('hnr_1','hnr_2','hnr_3','hnr_4'))
dat.mh_v1 <- melt(Mtones_v1,id.vars = c('block','vowel1','tone1','word', 'subj','toneseq'), measure.vars = c('hnr_1','hnr_2','hnr_3','hnr_4'))
dat.hh_v1 <- melt(Htones_v1,id.vars = c('block','vowel1','tone1','word', 'subj','toneseq'), measure.vars = c('hnr_1','hnr_2','hnr_3','hnr_4'))
dat.lh_v2 <- melt(Ltones_v2,id.vars = c('block','vowel2','tone2','word', 'subj','toneseq'), measure.vars = c('hnr_1_v2','hnr_2_v2','hnr_3_v2','hnr_4_v2'))
dat.mh_v2 <- melt(Mtones_v2,id.vars = c('block','vowel2','tone2','word', 'subj','toneseq'), measure.vars = c('hnr_1_v2','hnr_2_v2','hnr_3_v2','hnr_4_v2'))
dat.hh_v2 <- melt(Htones_v2,id.vars = c('block','vowel2','tone2','word', 'subj','toneseq'), measure.vars = c('hnr_1_v2','hnr_2_v2','hnr_3_v2','hnr_4_v2'))

#same for HNR s1
dat.lh_v11 <- melt(Ltones_v11,id.vars = c('block','vowel1','tone1','word', 'subj','toneseq'), measure.vars = c('hnr_1','hnr_2','hnr_3','hnr_4'))
dat.mh_v11 <- melt(Mtones_v11,id.vars = c('block','vowel1','tone1','word', 'subj','toneseq'), measure.vars = c('hnr_1','hnr_2','hnr_3','hnr_4'))
dat.hh_v11 <- melt(Htones_v11,id.vars = c('block','vowel1','tone1','word', 'subj','toneseq'), measure.vars = c('hnr_1','hnr_2','hnr_3','hnr_4'))
dat.lh_v21 <- melt(Ltones_v21,id.vars = c('block','vowel2','tone2','word', 'subj','toneseq'), measure.vars = c('hnr_1_v2','hnr_2_v2','hnr_3_v2','hnr_4_v2'))
dat.mh_v21 <- melt(Mtones_v21,id.vars = c('block','vowel2','tone2','word', 'subj','toneseq'), measure.vars = c('hnr_1_v2','hnr_2_v2','hnr_3_v2','hnr_4_v2'))
dat.hh_v21 <- melt(Htones_v21,id.vars = c('block','vowel2','tone2','word', 'subj','toneseq'), measure.vars = c('hnr_1_v2','hnr_2_v2','hnr_3_v2','hnr_4_v2'))

#same for HNR s2
dat.lh_v12 <- melt(Ltones_v12,id.vars = c('block','vowel1','tone1','word', 'subj','toneseq'), measure.vars = c('hnr_1','hnr_2','hnr_3','hnr_4'))
dat.mh_v12 <- melt(Mtones_v12,id.vars = c('block','vowel1','tone1','word', 'subj','toneseq'), measure.vars = c('hnr_1','hnr_2','hnr_3','hnr_4'))
dat.hh_v12 <- melt(Htones_v12,id.vars = c('block','vowel1','tone1','word', 'subj','toneseq'), measure.vars = c('hnr_1','hnr_2','hnr_3','hnr_4'))
dat.lh_v22 <- melt(Ltones_v22,id.vars = c('block','vowel2','tone2','word', 'subj','toneseq'), measure.vars = c('hnr_1_v2','hnr_2_v2','hnr_3_v2','hnr_4_v2'))
dat.mh_v22 <- melt(Mtones_v22,id.vars = c('block','vowel2','tone2','word', 'subj','toneseq'), measure.vars = c('hnr_1_v2','hnr_2_v2','hnr_3_v2','hnr_4_v2'))
dat.hh_v22 <- melt(Htones_v22,id.vars = c('block','vowel2','tone2','word', 'subj','toneseq'), measure.vars = c('hnr_1_v2','hnr_2_v2','hnr_3_v2','hnr_4_v2'))

#same for spec
dat.s_v1 <- melt(datacvcv,id.vars = c('block','tone1','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1','specTilt_2','specTilt_3','specTilt_4'))
dat.ls_v1 <- melt(Ltones_v1,id.vars = c('block','tone1','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1','specTilt_2','specTilt_3','specTilt_4'))
dat.ms_v1 <- melt(Mtones_v1,id.vars = c('block','tone1','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1','specTilt_2', 'specTilt_3','specTilt_4'))
dat.hs_v1 <- melt(Htones_v1,id.vars = c('block','tone1','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1','specTilt_2', 'specTilt_3','specTilt_4'))
dat.ls_v2 <- melt(Ltones_v2,id.vars = c('block','tone2','vowel2','word', 'subj','toneseq'), measure.vars = c('specTilt_1_v2','specTilt_2_v2','specTilt_3_v2','specTilt_4_v2'))
dat.ms_v2 <- melt(Mtones_v2,id.vars = c('block','tone2','vowel2','word', 'subj','toneseq'), measure.vars = c('specTilt_1_v2','specTilt_2_v2','specTilt_3_v2','specTilt_4_v2'))
dat.hs_v2 <- melt(Htones_v2,id.vars = c('block','tone2','vowel2','word', 'subj','toneseq'), measure.vars = c('specTilt_1_v2','specTilt_2_v2','specTilt_3_v2','specTilt_4_v2'))

#same for spec s1
dat.ls_v11 <- melt(Ltones_v11,id.vars = c('block','tone1','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1','specTilt_2','specTilt_3','specTilt_4'))
dat.ms_v11 <- melt(Mtones_v11,id.vars = c('block','tone1','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1','specTilt_2', 'specTilt_3','specTilt_4'))
dat.hs_v11 <- melt(Htones_v11,id.vars = c('block','tone1','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1','specTilt_2', 'specTilt_3','specTilt_4'))
dat.ls_v21 <- melt(Ltones_v21,id.vars = c('block','tone2','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1_v2','specTilt_2_v2','specTilt_3_v2','specTilt_4_v2'))
dat.ms_v21 <- melt(Mtones_v21,id.vars = c('block','tone2','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1_v2','specTilt_2_v2','specTilt_3_v2','specTilt_4_v2'))
dat.hs_v21 <- melt(Htones_v21,id.vars = c('block','tone2','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1_v2','specTilt_2_v2','specTilt_3_v2','specTilt_4_v2'))

#same for spec s2
dat.ls_v12 <- melt(Ltones_v12,id.vars = c('block','tone1','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1','specTilt_2','specTilt_3','specTilt_4'))
dat.ms_v12 <- melt(Mtones_v12,id.vars = c('block','tone1','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1','specTilt_2', 'specTilt_3','specTilt_4'))
dat.hs_v12 <- melt(Htones_v12,id.vars = c('block','tone1','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1','specTilt_2', 'specTilt_3','specTilt_4'))
dat.ls_v22 <- melt(Ltones_v22,id.vars = c('block','tone2','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1_v2','specTilt_2_v2','specTilt_3_v2','specTilt_4_v2'))
dat.ms_v22 <- melt(Mtones_v22,id.vars = c('block','tone2','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1_v2','specTilt_2_v2','specTilt_3_v2','specTilt_4_v2'))
dat.hs_v22 <- melt(Htones_v22,id.vars = c('block','tone2','vowel1','word', 'subj','toneseq'), measure.vars = c('specTilt_1_v2','specTilt_2_v2','specTilt_3_v2','specTilt_4_v2'))

#rearrange data to plot avg f0 over time
datf0plot_v1 <- melt(Ltones_v1,id.vars = c('subj','tone1','toneseq'), measure.vars = c('f0_1','f0_2','f0_3','f0_4'))
datf0plotm_v1 <- melt(Mtones_v1,id.vars = c('subj','tone1','toneseq'), measure.vars = c('f0_1','f0_2','f0_3','f0_4'))
datf0ploth_v1 <- melt(Htones_v1,id.vars = c('subj','tone1','toneseq'), measure.vars = c('f0_1','f0_2','f0_3','f0_4'))
datf0plot_v2 <- melt(Ltones_v2,id.vars = c('subj','tone2','toneseq'), measure.vars = c('f0_1_v2','f0_2_v2','f0_3_v2','f0_4_v2'))
datf0plotm_v2 <- melt(Mtones_v2,id.vars = c('subj','tone2','toneseq'), measure.vars = c('f0_1_v2','f0_2_v2','f0_3_v2','f0_4_v2'))
datf0ploth_v2 <- melt(Htones_v2,id.vars = c('subj','tone2','toneseq'), measure.vars = c('f0_1_v2','f0_2_v2','f0_3_v2','f0_4_v2'))


datf0plot_v2HL <- filter(datf0plot_v2, toneseq == "HL")
datf0ploth_v2LH <- filter(datf0ploth_v2, toneseq == "LH")

#create "syllable position" variable
dat.syll <- melt(datacvcv,id.vars = c('avg_word_hnr','avg_f0_v1','avg_f0_v2','avg_word_f0', 'block','tone1', 'tone2', 'word', 'subj','toneseq'), measure.vars = c('vowel1','vowel2'))

#frame for avg spec by tone
dat.spec <- gather(datacvcv, 'tone1', 'tone2', key = position, value = tone)
