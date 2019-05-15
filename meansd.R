source("datatidy.R")

#CV----------------------------------------------------------------------------------------
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

#CVCV--------------------------------------------------------------------------------------

#avg + sd f0 per tone by syllable
mean(Ltones_v1$avg_f0_v1, na.rm = T)
sd(Ltones_v1$avg_f0_v1, na.rm = T)
mean(Mtones_v1$avg_f0_v1, na.rm = T)
sd(Mtones_v1$avg_f0_v1, na.rm = T)
mean(Htones_v1$avg_f0_v1, na.rm = T)
sd(Htones_v1$avg_f0_v1, na.rm = T)
mean(Ltones_v2$avg_f0_v2, na.rm = T)
sd(Ltones_v2$avg_f0_v2, na.rm = T)
mean(Mtones_v2$avg_f0_v2, na.rm = T)
sd(Mtones_v2$avg_f0_v2, na.rm = T)
mean(Htones_v2$avg_f0_v2, na.rm = T)
sd(Htones_v2$avg_f0_v2, na.rm = T)

#avg + sd f0 by speaker
mean(Ltones_v11$avg_f0_v1, na.rm = T)
sd(Ltones_v11$avg_f0_v1, na.rm = T)
mean(Mtones_v11$avg_f0_v1, na.rm = T)
sd(Mtones_v11$avg_f0_v1, na.rm = T)
mean(Htones_v11$avg_f0_v1, na.rm = T)
sd(Htones_v11$avg_f0_v1, na.rm = T)
mean(Ltones_v21$avg_f0_v2, na.rm = T)
sd(Ltones_v21$avg_f0_v2, na.rm = T)
mean(Mtones_v21$avg_f0_v2, na.rm = T)
sd(Mtones_v21$avg_f0_v2, na.rm = T)
mean(Htones_v21$avg_f0_v2, na.rm = T)
sd(Htones_v21$avg_f0_v2, na.rm = T)

mean(Ltones_v12$avg_f0_v1, na.rm = T)
sd(Ltones_v12$avg_f0_v1, na.rm = T)
mean(Mtones_v12$avg_f0_v1, na.rm = T)
sd(Mtones_v12$avg_f0_v1, na.rm = T)
mean(Htones_v12$avg_f0_v1, na.rm = T)
sd(Htones_v12$avg_f0_v1, na.rm = T)
mean(Ltones_v22$avg_f0_v2, na.rm = T)
sd(Ltones_v22$avg_f0_v2, na.rm = T)
mean(Mtones_v22$avg_f0_v2, na.rm = T)
sd(Mtones_v22$avg_f0_v2, na.rm = T)
mean(Htones_v22$avg_f0_v2, na.rm = T)
sd(Htones_v22$avg_f0_v2, na.rm = T)



#avg + sd HNR
mean(Ltones_v1$avg_hnr_v1, na.rm = T)
sd(Ltones_v1$avg_hnr_v1, na.rm = T)
mean(Mtones_v1$avg_hnr_v1, na.rm = T)
sd(Mtones_v1$avg_hnr_v1, na.rm = T)
mean(Htones_v1$avg_hnr_v1, na.rm = T)
sd(Htones_v1$avg_hnr_v1, na.rm = T)
mean(Ltones_v2$avg_hnr_v2, na.rm = T)
sd(Ltones_v2$avg_hnr_v2, na.rm = T)
mean(Mtones_v2$avg_hnr_v2, na.rm = T)
sd(Mtones_v2$avg_hnr_v2, na.rm = T)
mean(Htones_v2$avg_hnr_v2, na.rm = T)
sd(Htones_v2$avg_hnr_v2, na.rm = T)

#avg + sd HNR by speaker
mean(Ltones_v11$avg_hnr_v1, na.rm = T)
sd(Ltones_v11$avg_hnr_v1, na.rm = T)
mean(Mtones_v11$avg_hnr_v1, na.rm = T)
sd(Mtones_v11$avg_hnr_v1, na.rm = T)
mean(Htones_v11$avg_hnr_v1, na.rm = T)
sd(Htones_v11$avg_hnr_v1, na.rm = T)
mean(Ltones_v21$avg_hnr_v2, na.rm = T)
sd(Ltones_v21$avg_hnr_v2, na.rm = T)
mean(Mtones_v21$avg_hnr_v2, na.rm = T)
sd(Mtones_v21$avg_hnr_v2, na.rm = T)
mean(Htones_v21$avg_hnr_v2, na.rm = T)
sd(Htones_v21$avg_hnr_v2, na.rm = T)

mean(Ltones_v12$avg_hnr_v1, na.rm = T)
sd(Ltones_v12$avg_hnr_v1, na.rm = T)
mean(Mtones_v12$avg_hnr_v1, na.rm = T)
sd(Mtones_v12$avg_hnr_v1, na.rm = T)
mean(Htones_v12$avg_hnr_v1, na.rm = T)
sd(Htones_v12$avg_hnr_v1, na.rm = T)
mean(Ltones_v22$avg_hnr_v2, na.rm = T)
sd(Ltones_v22$avg_hnr_v2, na.rm = T)
mean(Mtones_v22$avg_hnr_v2, na.rm = T)
sd(Mtones_v22$avg_hnr_v2, na.rm = T)
mean(Htones_v22$avg_hnr_v2, na.rm = T)
sd(Htones_v22$avg_hnr_v2, na.rm = T)


#avg + sd spec
mean(Ltones_v1$avg_spec_v1, na.rm = T)
sd(Ltones_v1$avg_spec_v1, na.rm = T)
mean(Mtones_v1$avg_spec_v1, na.rm = T)
sd(Mtones_v1$avg_spec_v1, na.rm = T)
mean(Htones_v1$avg_spec_v1, na.rm = T)
sd(Htones_v1$avg_spec_v1, na.rm = T)
mean(Ltones_v2$avg_spec_v2, na.rm = T)
sd(Ltones_v2$avg_spec_v2, na.rm = T)
mean(Mtones_v2$avg_spec_v2, na.rm = T)
sd(Mtones_v2$avg_spec_v2, na.rm = T)
mean(Htones_v2$avg_spec_v2, na.rm = T)
sd(Htones_v2$avg_spec_v2, na.rm = T)

#avg + sd spec by speaker
mean(Ltones_v11$avg_spec_v1, na.rm = T)
sd(Ltones_v11$avg_spec_v1, na.rm = T)
mean(Mtones_v11$avg_spec_v1, na.rm = T)
sd(Mtones_v11$avg_spec_v1, na.rm = T)
mean(Htones_v11$avg_spec_v1, na.rm = T)
sd(Htones_v11$avg_spec_v1, na.rm = T)
mean(Ltones_v21$avg_spec_v2, na.rm = T)
sd(Ltones_v21$avg_spec_v2, na.rm = T)
mean(Mtones_v21$avg_spec_v2, na.rm = T)
sd(Mtones_v21$avg_spec_v2, na.rm = T)
mean(Htones_v21$avg_spec_v2, na.rm = T)
sd(Htones_v21$avg_spec_v2, na.rm = T)

mean(Ltones_v12$avg_spec_v1, na.rm = T)
sd(Ltones_v12$avg_spec_v1, na.rm = T)
mean(Mtones_v12$avg_spec_v1, na.rm = T)
sd(Mtones_v12$avg_spec_v1, na.rm = T)
mean(Htones_v12$avg_spec_v1, na.rm = T)
sd(Htones_v12$avg_spec_v1, na.rm = T)
mean(Ltones_v22$avg_spec_v2, na.rm = T)
sd(Ltones_v22$avg_spec_v2, na.rm = T)
mean(Mtones_v22$avg_spec_v2, na.rm = T)
sd(Mtones_v22$avg_spec_v2, na.rm = T)
mean(Htones_v22$avg_spec_v2, na.rm = T)
sd(Htones_v22$avg_spec_v2, na.rm = T)



