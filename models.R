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
