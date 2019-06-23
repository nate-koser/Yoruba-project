source("datatidy.R")

#modeling -------------------------------------------------------------------------------

#nested model comparisons ----------------------------------------------------------

#f0
nullf <- lmer(avg_f0 ~ 1 + (1|block) + (1|word), data = datana )
summary(nullf)

tonef <- lmer(avg_f0 ~ target_tone + (1|block) + (1|word), data = datana)
summary(tonef)

vowelf <- lmer(avg_f0 ~ target_vowel + (1|block) + (1|word), data = datana)
summary(vowelf)

onsf <- lmer(avg_f0 ~ target_sylons + (1|block) + (1|word), data = datana)
summary(onsf)

toneonsf <- lmer(avg_f0 ~ target_tone + target_sylons + (1|block) + (1|word), data = datana)
summary(toneonsf)


anova(tonef,toneonsf)

r.squaredGLMM(toneonsf)


#hnr
nullh <- lmer(avg_hnr ~ 1 + (1|block) + (1|word), data = datana )
summary(nullh)

toneh <- lmer(avg_hnr ~ target_tone  + (1|block) + (1|word), data = datana)
summary(toneh)

vowelh <- lmer(avg_hnr ~ target_vowel +  (1|block), data = datana)
summary(vowelh)

voweltoneh <- lmer(avg_hnr ~ target_tone + target_vowel + (1|block) + (1|word), data = datana)
summary(voweltoneh)

voweltoneonsh <- lmer(avg_hnr ~ target_tone + target_vowel + target_sylons + (1|block) + (1|word), data = datana)
summary(voweltoneonsh)


r.squaredGLMM(voweltoneh)

anova(nullh,toneh)

#spectilt
nulls <- lmer(avg_spec ~ 1 + (1|block) + (1|word), data = datana )
summary(nulls)

tones <- lmer(avg_spec ~ target_tone + (1|block) + (1|word), data = datana)
summary(tones)

vowels <- lmer(avg_spec ~ target_vowel + (1|block) + (1|word), data = datana)
summary(vowels)


voweltones <- lmer(avg_spec ~ target_tone + target_vowel + (1|block) + (1|word), data = datana)
summary(voweltones)

voweltoneonss <- lmer(avg_spec ~ target_tone + target_vowel + target_sylons + (1|block) + (1|word), data = datana)
summary(voweltoneonss)

anova(tones,voweltones)

r.squaredGLMM(tones)



#final models --------------------------------------------------------------------------------

#duration
durmod <- lmer(target_voweldur ~ target_tone + (block|word),  data = datana)
summary(durmod)
emmeans(durmod, list(pairwise ~ target_tone), adjust = "none")
r.squaredGLMM(durmod)

#f0
f0mod <- lmer(avg_f0 ~ target_tone +  target_sylons + (block|word), data = datana)
summary(f0mod)
r.squaredGLMM(f0mod)

#HNR
hnrmod <- lmer(avg_hnr ~ target_tone + (block|word), data = datana)
summary(hnrmod)
r.squaredGLMM(hnrmod)
emmeans(hnrmod, list(pairwise ~ target_tone), adjust = "none")

#spectral tilt
specmod <- lmer(avg_spec ~ target_tone +  (block|word), data = datana)
summary(specmod)
r.squaredGLMM(specmod)
emmeans(specmod, list(pairwise ~ target_tone), adjust = "none")



##slice models------------------------------------------------------------------------

#hnr slice L model
hsliceL <- lmer(value ~ variable + (block|word) , data = dat.lh)
summary(hsliceL)
r.squaredGLMM(hsliceL)
emmeans(hsliceL, list(pairwise ~ variable), adjust = "none")

#hnr slice M model
hsliceM <- lmer(value ~ variable + (block|word) , data = dat.mh)
summary(hsliceM)
r.squaredGLMM(hsliceM)
emmeans(hsliceM, list(pairwise ~ variable), adjust = "none")

#hnr slice H model
hsliceH <- lmer(value ~ variable + (block|word) , data = dat.hh)
summary(hsliceH)
r.squaredGLMM(hsliceH)
emmeans(hsliceH, list(pairwise ~ variable), adjust = "none")

#spec tilt slice L model
ssliceL <- lmer(value ~ variable + (block|word) , data = dat.ls)
summary(ssliceL)
r.squaredGLMM(ssliceL)
emmeans(ssliceL, list(pairwise ~ variable), adjust = "none")

#spec tilt slice M model
ssliceM <- lmer(value ~ variable + (block|word) , data = dat.ms)
summary(ssliceM)
r.squaredGLMM(ssliceM)
emmeans(ssliceM, list(pairwise ~ variable), adjust = "none")

#spec tilt slice H model
ssliceH <- lmer(value ~ variable  + (block|word) , data = dat.hs)
summary(ssliceH)
r.squaredGLMM(ssliceH)
emmeans(ssliceH, list(pairwise ~ variable), adjust = "none")


#CVCV------------------------------------------------------------------------------------------
#spec tilt-----------------------------------------------------------------

#by syllable
#syll and spec s1
specsyll1s1 <- lmer(avg_spec_v1 ~ tone1  +  (block|word)  , data = datas1)
summary(specsyll1s1)
emmeans(specsyll1s1, list(pairwise ~ tone1), adjust = "none")
r.squaredGLMM(specsyll1s1)

specsyll2s1 <- lmer(avg_spec_v2 ~ tone2  +  (block|word)  , data = datas1)
summary(specsyll2s1)
emmeans(specsyll2s1, list(pairwise ~ tone2), adjust = "none")
r.squaredGLMM(specsyll2s1)

#syll and spec s2
specsyll1s2 <- lmer(avg_spec_v1 ~ tone1 + (block|word), data = datas2)
summary(specsyll1s2)
emmeans(specsyll1s2, list(pairwise ~ tone1), adjust = "none")
r.squaredGLMM(specsyll1s2)

specsyll2s2 <- lmer(avg_spec_v2 ~ tone2 +  (block|word)  , data = datas2)
summary(specsyll2s2)
emmeans(specsyll2s2, list(pairwise ~ tone2), adjust = "none")
r.squaredGLMM(specsyll2s2)

#slice models
#spec syll 1  s1
lowspecs1s1 <- lmer(value ~ variable  +  (block|word), data = dat.ls_v11)
summary(lowspecs1s1)
emmeans(lowspecs1s1, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(lowspecs1s1)

midspecs1s1 <- lmer(value ~ variable  +  (block|word), data = dat.ms_v11)
summary(midspecs1s1)
emmeans(midspecs1s1, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(midspecs1s1)

highspecs1s1 <- lmer(value ~ variable  + (block|word), data = dat.hs_v11)
summary(highspecs1s1)
emmeans(highspecs1s1, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(highspecs1s1)

#spec syll 2 s1
lowspecs2s1 <- lmer(value ~ variable  + (block|word), data = dat.ls_v21)
summary(lowspecs2s1)
emmeans(lowspecs2s1, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(lowspecs2s1)

midspecs2s1 <- lmer(value ~ variable  + (block|word), data = dat.ms_v21)
summary(midspecs2s1)
emmeans(midspecs2s1, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(midspecs2s1)

highspecs2s1 <- lmer(value ~ variable  + (block|word), data = dat.hs_v21)
summary(highspecs2s1)
emmeans(highspecs2s1, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(highspecs2s1)


#spec syll 1  s2
lowspecs1s2 <- lmer(value ~ variable  + (block|word) , data = dat.ls_v12)
summary(lowspecs1s2)
emmeans(lowspecs1s2, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(lowspecs1s2)

midspecs1s2 <- lmer(value ~ variable  + (block|word) , data = dat.ms_v12)
summary(midspecs1s2)
emmeans(midspecs1s2, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(midspecs1s2)

highspecs1s2 <- lmer(value ~ variable  + (block|word) , data = dat.hs_v12)
summary(highspecs1s2)
emmeans(highspecs1s2, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(highspecs1s2)

#spec syll 2 s2
lowspecs2s2 <- lmer(value ~ variable  + (block|word) , data = dat.ls_v22)
summary(lowspecs2s2)
emmeans(lowspecs2s2, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(lowspecs2s2)

midspecs2s2 <- lmer(value ~ variable  + (block|word) , data = dat.ms_v22)
summary(midspecs2s2)
emmeans(midspecs2s2, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(midspecs2s2)

highspecs2s2 <- lmer(value ~ variable  + (block|word) , data = dat.hs_v22)
summary(highspecs2s2)
emmeans(highspecs2s2, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(highspecs2s2)

#sequence on spec tilt

seqspec1v1 <- lmer(avg_spec_v1 ~ toneseq +   (block|word), data = datas1 )
summary(seqspec1v1)
r.squaredGLMM(seqspec1v1)
emmeans(seqspec1v1, list(pairwise ~ toneseq), adjust = "none")

seqspec1v2 <- lmer(avg_spec_v2 ~ toneseq +   (block|word), data = datas1 )
summary(seqspec1v2)
r.squaredGLMM(seqspec1v2)
emmeans(seqspec1v2, list(pairwise ~ toneseq), adjust = "none")

seqspec2v1 <- lmer(avg_spec_v1 ~ toneseq +  (block|word) , data = datas2 )
summary(seqspec2v1)
r.squaredGLMM(seqspec2v1)
emmeans(seqspec2v1, list(pairwise ~ toneseq), adjust = "none")

seqspec2v2 <- lmer(avg_spec_v2 ~ toneseq +  (block|word) , data = datas2 )
summary(seqspec2v2)
r.squaredGLMM(seqspec2v2)
emmeans(seqspec2v2, list(pairwise ~ toneseq), adjust = "none")

#hnr---------------------------------------------------------------------

#by syllable
#syll 1
hnrsyll1 <- lmer(avg_hnr_v1 ~ tone1   + (1|subj) + (block|word)  , data = datacvcv)
summary(hnrsyll1)
emmeans(hnrsyll1, list(pairwise ~ tone1), adjust = "none")
r.squaredGLMM(hnrsyll1)

#syll 2
hnrsyll2 <- lmer(avg_hnr_v2 ~ tone2 +   (1|subj) + (block|word)  , data = datacvcv)
summary(hnrsyll2)
emmeans(hnrsyll2, list(pairwise ~ tone2), adjust = "none")
r.squaredGLMM(hnrsyll2)

#slice models
#hnr syll 1
lowhnrs1 <- lmer(value ~ variable  + (block|word)  + (1|subj), data = dat.lh_v1)
summary(lowhnrs1)
emmeans(lowhnrs1, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(lowhnrs1)

midhnrs1 <- lmer(value ~ variable  + (block|word)  + (1|subj), data = dat.mh_v1)
summary(midhnrs1)
emmeans(midhnrs1, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(midhnrs1)

highhnrs1 <- lmer(value ~ variable  + (block|word) + (1|subj), data = dat.hh_v1)
summary(highhnrs1)
emmeans(highhnrs1, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(highhnrs1)

#hnr syll 2
lowhnrs2 <- lmer(value ~ variable  + (block|word)  + (1|subj), data = dat.lh_v2)
summary(lowhnrs2)
emmeans(lowhnrs2, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(lowhnrs2)

midhnrs2 <- lmer(value ~ variable  + (block|word)  + (1|subj), data = dat.mh_v2)
summary(midhnrs2)
emmeans(midhnrs2, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(midhnrs2)

highhnrs2 <- lmer(value ~ variable  + (block|word) + (1|subj), data = dat.hh_v2)
summary(highhnrs2)
emmeans(highhnrs2, list(pairwise ~ variable), adjust = "none")
r.squaredGLMM(highhnrs2)


#sequence HNR
seqhnr1 <- lmer(avg_hnr_v1 ~ toneseq +  (1|subj) + (block|word), data = datacvcv )
summary(seqhnr1)
r.squaredGLMM(seqhnr1)
emmeans(seqhnr1, list(pairwise ~ toneseq), adjust = "none")

seqhnr2 <- lmer(avg_hnr_v2 ~ toneseq +   (1|subj) + (block|word) , data = datacvcv )
summary(seqhnr2)
r.squaredGLMM(seqhnr2)
emmeans(seqhnr2, list(pairwise ~ toneseq), adjust = "none")

#position on hnr
wordhnr <- lmer(avg_word_hnr ~ variable + value + (block|word) + (1|subj) , data = dat.syll)
summary(wordhnr)
r.squaredGLMM(wordhnr)
emmeans(highhnrs2, list(pairwise ~ variable), adjust = "none")


wordhnr <- lmer(avg_hnr_v1 ~ variable + toneseq +  (block|word) + (1|subj) , data = dat.lm)
summary(wordhnr)
r.squaredGLMM(wordhnr)
emmeans(highhnrs2, list(pairwise ~ variable), adjust = "none")

