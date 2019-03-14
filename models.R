source("datatidy.R")

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


#lmer--------------------------------------------------------------------------------

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

##slice models------------------------------------------------------------------------

#f0 slice L model
f0sliceL <- lmer(value ~ variable + (1|block) , data = dat.lf0)
summary(f0sliceL)
r.squaredGLMM(f0sliceL)

#f0 slice M model
f0sliceM <- lmer(value ~ variable + (1|block) , data = dat.mf0)
summary(f0sliceM)
r.squaredGLMM(f0sliceM)

#f0 slice H model
f0sliceH <- lmer(value ~ variable + (1|block) , data = dat.hf0)
summary(f0sliceH)
r.squaredGLMM(f0sliceH)

#hnr slice L model
hsliceL <- lmer(value ~ variable + (1|block) , data = dat.lh)
summary(hsliceL)
r.squaredGLMM(hsliceL)

#hnr slice M model
hsliceM <- lmer(value ~ variable + (1|block) , data = dat.mh)
summary(hsliceM)
r.squaredGLMM(hsliceM)

#hnr slice H model
hsliceH <- lmer(value ~ variable + (1|block) , data = dat.hh)
summary(hsliceH)
r.squaredGLMM(hsliceH)

#spec tilt slice L model
ssliceL <- lmer(value ~ variable + (1|block) , data = dat.ls)
summary(ssliceL)
r.squaredGLMM(ssliceL)

#spec tilt slice M model
ssliceM <- lmer(value ~ variable + (1|block) , data = dat.ms)
summary(ssliceM)
r.squaredGLMM(ssliceM)

#spec tilt slice H model
ssliceH <- lmer(value ~ variable + (1|block) , data = dat.hs)
summary(ssliceH)
r.squaredGLMM(ssliceH)

#f1-f0 slice L model
ffsliceL <- lmer(value ~ variable + (1|block) , data = dat.l10)
summary(ffsliceL)
r.squaredGLMM(ffsliceL)

#f1-f0 slice M model
ffsliceM <- lmer(value ~ variable + (1|block) , data = dat.m10)
summary(ffsliceM)
r.squaredGLMM(ffsliceM)

#f1-f0 slice H model
ffsliceH <- lmer(value ~ variable + (1|block) , data = dat.h10)
summary(ffsliceH)
r.squaredGLMM(ffsliceH)
