source("datatidy.R")

#plots------------------------------------------------------------------------------------------

#plot avg spec + hnr
ggplot(datana, mapping = aes(x = avg_spec, y = avg_hnr, color = target_tone, shape = target_tone), na.rm = TRUE) +
  geom_point() +
  labs(x = "Average spectral tilt", y = "Average HNR", key = "category") +
  theme(legend.title=element_blank()) +
  stat_summary(data = datana, fun.data=mean_cl_normal) +
  geom_smooth(method ="lm", se = T, fullrange = F, level = .75)+
  scale_color_brewer(palette="Set1")

#plot avg spec + hnr v1
ggplot(datacvcv, mapping = aes(x = avg_spec_v1, y = avg_hnr_v1, color = tone1, shape = tone1)) +
  geom_point( na.rm = TRUE) +
  theme(legend.title=element_blank()) +
  stat_summary(fun.data=mean_cl_normal) +
  geom_smooth(method ="lm", se = T, fullrange = F, level = .95)+
  geom_point(mapping = aes(x = avg_spec_v2, y = avg_hnr_v2, color = tone2, shape = tone2), na.rm = TRUE) +
  theme(legend.title=element_blank()) +
  labs(x = "average spectral tilt", y = "average HNR", key = "category") +
  scale_color_brewer(palette="Set1")


#plot avg spec + hnr v2 slice 4
ggplot(datacvcv, mapping = aes(x = specTilt_4, y = hnr_4, color = tone1, shape = tone1)) +
  geom_point( na.rm = TRUE) +
  geom_point(mapping = aes(x = specTilt_4_v2, y = hnr_4_v2, color = tone2, shape = tone2), na.rm = TRUE, show.legend = FALSE) +
   labs(x = "Average spectral tilt", y = "Average HNR", key = "category") +
  theme(legend.title=element_blank()) +
  scale_color_brewer(palette="Set1")

#avg f0 boxplot
ggplot(data = datana) +
  geom_boxplot(mapping = aes(x = target_tone, y = avg_f0, fill = target_tone), na.rm = TRUE) +
  ylim (75,200)

#avg f0 boxplot
ggplot(data = datana) +
  geom_boxplot(mapping = aes(x = target_tone, y = avg_hnr, fill = target_tone), na.rm = TRUE) +
  scale_x_discrete(limits=c("L","M","H"))
 # ylim (75,200)




#avg amp plot
ggplot(datana) +
  geom_boxplot(aes(x = target_tone, y = avg_amp, fill = target_tone), na.rm = T)

#shimmer jitter plot
ggplot(datana) +
  geom_point(mapping = aes(x = shimmer_3, y = jitter_3, color = target_tone), na.rm = TRUE)


#boxplot f0 over "time" for L tone
ggplot(dat.lf0) +
  geom_boxplot(aes(x = variable, y = value, fill = variable), na.rm = T, show.legend = F) +
  labs(y = "f0", x = "portion of vowel") +
  scale_fill_brewer(palette="Blues")

#boxplot f0 by vowel
ggplot(datana) +
  geom_boxplot(aes(x = target_vowel, y = avg_f0, fill = target_vowel), na.rm = T, show.legend = F)

#boxplot hnr by vowel
ggplot(datana) +
  geom_boxplot(aes(x = target_vowel, y = avg_hnr, fill = target_vowel), na.rm = T, show.legend = F)

#boxplot spec by vowel
ggplot(datana) +
  geom_boxplot(aes(x = target_vowel, y = avg_spec, fill = target_vowel), na.rm = T, show.legend = F)


#line/point plot mean f0 over "time" i.e. f0 slice
ggplot(data = datf0plot, aes(x = variable, y=value, group = target_tone, color = target_tone)) +
  stat_summary( geom="line", fun.y = mean)+
  stat_summary(data = datf0plotm, geom="line", fun.y = mean)+
  stat_summary(data = datf0ploth, geom="line", fun.y = mean)+
  stat_summary(data = datf0plot, geom="point", fun.y = mean)+
  stat_summary(data = datf0plotm, geom="point", fun.y = mean)+
  stat_summary(data = datf0ploth, geom="point", fun.y = mean)+
  stat_summary(data = datf0plot, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) + #can also
  stat_summary(data = datf0plotm, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) +  #use errorbar
  stat_summary(data = datf0ploth, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) +  #with no se
  theme(axis.text.x=element_blank())+
  scale_color_brewer(palette="Set1") +
  theme(legend.title=element_blank())+
  labs(x = "time", y = "mean F0")

#line/point plot mean hnr over "time" i.e.  slice
ggplot(data = dathplot, aes(x = variable, y=value, group = target_tone, color = target_tone)) +
  stat_summary( geom="line", fun.y = mean)+
  stat_summary(data = dathplotm, geom="line", fun.y = mean)+
  stat_summary(data = dathploth, geom="line", fun.y = mean)+
  stat_summary(data = dathplot, geom="point", fun.y = mean)+
  stat_summary(data = dathplotm, geom="point", fun.y = mean)+
  stat_summary(data = dathploth, geom="point", fun.y = mean)+
  stat_summary(data = dathplot, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) + #can also
  stat_summary(data = dathplotm, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) +  #use errorbar
  stat_summary(data = dathploth, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) +  #with no se
  theme(axis.text.x=element_blank())+
  scale_color_brewer(palette="Set1") +
  theme(legend.title=element_blank())+
  labs(x = "time", y = "mean HNR") +
  ylim(0,25)

#line/point plot mean spectilt over "time" i.e.  slice
ggplot(data = datsplot, aes(x = variable, y=value, group = target_tone, color = target_tone)) +
  stat_summary( geom="line", fun.y = mean)+
  stat_summary(data = datsplotm, geom="line", fun.y = mean)+
  stat_summary(data = datsploth, geom="line", fun.y = mean)+
  stat_summary(data = datsplot, geom="point", fun.y = mean)+
  stat_summary(data = datsplotm, geom="point", fun.y = mean)+
  stat_summary(data = datsploth, geom="point", fun.y = mean)+
  stat_summary(data = datsplot, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) + #can also
  stat_summary(data = datsplotm, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) +  #use errorbar
  stat_summary(data = datsploth, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) +  #with no se
  theme(axis.text.x=element_blank())+
  scale_color_brewer(palette="Set1") +
  theme(legend.title=element_blank())+
  labs(x = "time", y = "mean spectral tilt") +
  ylim(-10,20)


#CVCV--------------------------------------------------------------------------------------------------

#f0----------------------------------------------------------------------------------------------------
#avg f0 by tone
ggplot(data = dat.spec) +
  geom_boxplot(mapping = aes(x = tone, y = avg_word_f0, fill = subj), na.rm = TRUE) +
  ylim (75,200)+
  scale_x_discrete(limits=c("L","M","H"))+
  theme(axis.title.x=element_blank())+
  labs(y = "mean word F0")+
  theme(legend.position = "none")

#avg f0 boxplot v1
ggplot(data = datacvcv) +
  geom_boxplot(mapping = aes(x = tone1, y = avg_f0_v1, fill = subj), na.rm = TRUE) +
  ylim (75,200)+
  scale_x_discrete(limits=c("L","M","H"))+
  labs(x = "syllable 1", y = "mean F0")+
  theme(legend.position="none")

#avg f0 boxplot v2
ggplot(data = datacvcv) +
  geom_boxplot(mapping = aes(x = tone2, y = avg_f0_v2, fill = subj), na.rm = TRUE) +
  ylim (75,200)+
  scale_x_discrete(limits=c("L","M","H"))+
  theme(axis.text.y=element_blank())+
  theme(legend.title=element_blank())+
  theme(axis.title.y=element_blank())+
  labs(x = "syllable 2")

#first syll f0 over time
ggplot(data = datf0plot_v1, aes(x = variable, y=value, group = toneseq, color = tone1)) +
  stat_summary( geom="line", fun.y = mean)+
  stat_summary(data = datf0plotm_v1, geom="line", fun.y = mean)+
  stat_summary(data = datf0ploth_v1, geom="line", fun.y = mean)+
  stat_summary(data = datf0plot_v1, geom="point", fun.y = mean)+
  stat_summary(data = datf0plotm_v1, geom="point", fun.y = mean)+
  stat_summary(data = datf0ploth_v1, geom="point", fun.y = mean)+
  stat_summary(data = datf0plot_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) + #can also
  stat_summary(data = datf0plotm_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) +  # , geom = "smooth", se = Tuse errorbar
  stat_summary(data = datf0ploth_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) +  #with no se
  theme(axis.text.x=element_blank())+
  scale_color_brewer(palette="Set1") +
  theme(legend.position="none")+
  labs(x = "time", y = "mean F0")+
  annotate("text", x = 4.2, y = 168, label = "HL")+
  annotate("text", x = 4.2, y = 164, label = "HH")+
  annotate("text", x = 4.2, y = 158, label = "HM")+
  annotate("text", x = 4.2, y = 150.7, label = "MH")+
  annotate("text", x = 4.2, y = 147, label = "ML")+
  annotate("text", x = 4.2, y = 143, label = "MM")+
  annotate("text", x = 4.2, y = 114, label = "LM")+
  annotate("text", x = 4.2, y = 107, label = "LL")+
  annotate("text", x = 4.2, y = 102, label = "LH")+
  annotate("text", x = 0.8, y = 105, label = "syllable 1")+
  ylim(90,190)

#second syll f0 over time
ggplot(data = datf0plot_v2, aes(x = variable, y=value, group = toneseq, color = tone2)) +
  stat_summary( geom="line", fun.y = mean)+
  stat_summary(data = datf0plotm_v2, geom="line", fun.y = mean)+
  stat_summary(data = datf0ploth_v2, geom="line", fun.y = mean)+
  stat_summary(data = datf0plot_v2, geom="point", fun.y = mean)+
  stat_summary(data = datf0plotm_v2, geom="point", fun.y = mean)+
  stat_summary(data = datf0ploth_v2, geom="point", fun.y = mean)+
  stat_summary(data = datf0plot_v2, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) + #can also
  stat_summary(data = datf0plotm_v2, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) +  #use errorbar
  stat_summary(data = datf0ploth_v2, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) +  #with no se
  #theme(axis.text.y=element_blank())+
  theme(axis.text.x=element_blank())+
  theme(axis.title.y = element_blank()) +
  scale_color_brewer(palette="Set1") +
  theme(legend.title=element_blank())+
  labs(x = "time")+
  annotate("text", x = 4.2, y = 165, label = "LH")+
  annotate("text", x = 4.2, y = 161, label = "HH")+
  annotate("text", x = 4.2, y = 157, label = "MH")+
  annotate("text", x = 4.2, y = 149, label = "MM")+
  annotate("text", x = 4.2, y = 145, label = "HM")+
  annotate("text", x = 4.2, y = 141, label = "LM")+
  annotate("text", x = 4.2, y = 111, label = "HL")+
  annotate("text", x = 4.2, y = 104, label = "ML")+
  annotate("text", x = 4.2, y = 99, label = "LL")+
  annotate("text", x = 0.8, y = 100, label = "syllable 2")+
  ylim(90,190)


#LH HL f0
ggplot(data = datf0plot_v2HL, aes(x = variable, y=value, group = toneseq)) +
  stat_summary( geom="line", fun.y = mean)+
  stat_summary(data = datf0ploth_v2LH, geom="line", fun.y = mean)+
  stat_summary(data = datf0plot_v2HL, geom="point", fun.y = mean)+
  stat_summary(data = datf0ploth_v2LH, geom="point", fun.y = mean)+
  stat_summary(data = datf0plot_v2ML, geom="point", fun.y = mean)+
  stat_summary(data = datf0ploth_v2LM, geom="point", fun.y = mean)+
  stat_summary(data = datf0plot_v2ML, geom="line", fun.y = mean)+
  stat_summary(data = datf0ploth_v2LM, geom="line", fun.y = mean)+
  stat_summary(data = datf0plot_v2HL, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) + #can also
  stat_summary(data = datf0ploth_v2LH, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) +
  stat_summary(data = datf0plot_v2ML, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) + #can also
  stat_summary(data = datf0ploth_v2LM, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) +#with no se
  facet_wrap(vars(toneseq), labeller = as_labeller(toneseq_names))+
  theme(axis.text.x=element_blank())+
  scale_color_brewer(palette="Set1") +
  theme(legend.title=element_blank())+
  labs(x = "time", y = "mean F0")

#f0 by vowel by speaker
#avg f0 boxplot s1 v1
ggplot(data = datas1) +
  geom_boxplot(mapping = aes(x = tone1, y = avg_f0_v1, fill = tone1), na.rm = TRUE) +
  ylim (75,200)
#avg f0 boxplot s1 v2
ggplot(data = datas1) +
  geom_boxplot(mapping = aes(x = tone2, y = avg_f0_v2, fill = tone2), na.rm = TRUE) +
  ylim (75,200)
#avg f0 boxplot s2 v1
ggplot(data = datas2) +
  geom_boxplot(mapping = aes(x = tone1, y = avg_f0_v1, fill = tone1), na.rm = TRUE) +
  ylim (75,200)
#avg f0 boxplot s2 v2
ggplot(data = datas2) +
  geom_boxplot(mapping = aes(x = tone2, y = avg_f0_v2, fill = tone2), na.rm = TRUE) +
  ylim (75,200)

#avg f0 no contour v1
ggplot(data = datanocon) +
  geom_boxplot(mapping = aes(x = tone1, y = avg_f0_v1, fill = tone1), na.rm = TRUE) +
  ylim (75,200)
#avg f0 no contour v2
ggplot(data = datanocon) +
  geom_boxplot(mapping = aes(x = tone2, y = avg_f0_v2, fill = tone2), na.rm = TRUE) +
  ylim (75,200)

#spectral tilt---------------------------------------------------------------------------------
#avg spectilt by tone
ggplot(data = dat.spec) +
  geom_boxplot(mapping = aes(x = tone, y = avg_word_spec, fill = subj), na.rm = TRUE) +
  ylim (-20,20)+
  scale_x_discrete(limits=c("L","M","H"))+
  theme(axis.title.x=element_blank())+
  labs(y = "mean word spectral tilt")

#avg spectilt boxplot v1
ggplot(data = datacvcv) +
  geom_boxplot(mapping = aes(x = tone1, y = avg_spec_v1, fill = subj), na.rm = TRUE) +
  ylim (-20,20)+
  scale_x_discrete(limits=c("L","M","H"))+
  labs(x = "syllable 1", y = "mean spectral tilt")+
  theme(legend.position="none")

#avg spectilt boxplot v2
ggplot(data = datacvcv) +
  geom_boxplot(mapping = aes(x = tone2, y = avg_spec_v2, fill = subj), na.rm = TRUE) +
  ylim (-20,20)+
  scale_x_discrete(limits=c("L","M","H"))+
  theme(axis.text.y=element_blank())+
  theme(axis.title.y=element_blank())+
  theme(legend.title=element_blank())+
  labs(x = "syllable 2")

#first syll spectilt over time
ggplot(data = dat.ls_v1, aes(x = variable, y=value, group = subj, shape = subj)) +
  stat_summary( geom="line", fun.y = mean)+
  ylim (-20,20)+
  stat_summary(data = dat.ms_v1, geom="line", fun.y = mean)+
  stat_summary(data = dat.hs_v1, geom="line", fun.y = mean)+
  stat_summary(data = dat.ls_v1, geom="point", fun.y = mean)+
  stat_summary(data = dat.ms_v1, geom="point", fun.y = mean)+
  stat_summary(data = dat.hs_v1, geom="point", fun.y = mean)+
  stat_summary(data = dat.ls_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) + #can also
  stat_summary(data = dat.ms_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) +  # , geom = "smooth", se = Tuse errorbar
  stat_summary(data = dat.hs_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) +  #with no se
  theme(axis.text.x=element_blank())+
  scale_color_brewer(palette="Set1") +
  theme(legend.position="none")+
  annotate("text", x = .8, y = -1.5, label = "H")+
  annotate("text", x = .8, y = -9, label = "L")+
  annotate("text", x = .8, y = -10.5, label = "M")+
  annotate("text", x = .8, y = 5.5, label = "L")+
  annotate("text", x = .8, y = 4, label = "M")+
  annotate("text", x = .8, y = 1.5, label = "H")+
  labs(x = "syllable 1", y = "mean spectral tilt")

#second syll spectilt over time
ggplot(data = dat.ls_v2, aes(x = variable, y=value, group = subj,  shape=subj)) +
  stat_summary( geom="line", fun.y = mean)+
  ylim (-20,20)+
  stat_summary(data = dat.ms_v2, geom="line", fun.y = mean)+
  stat_summary(data = dat.hs_v2, geom="line", fun.y = mean)+
  stat_summary(data = dat.ls_v2, geom="point", fun.y = mean)+
  stat_summary(data = dat.ms_v2, geom="point", fun.y = mean)+
  stat_summary(data = dat.hs_v2, geom="point", fun.y = mean)+
  stat_summary(data = dat.ls_v2, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) + #can also
  stat_summary(data = dat.ms_v2, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) +  # , geom = "smooth", se = Tuse errorbar
  stat_summary(data = dat.hs_v2, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) +  #with no se
  theme(axis.text.x=element_blank())+
  scale_color_brewer(palette="Set1") +
  #facet_wrap(vars(subj))+
  theme(axis.text.y=element_blank())+
  theme(axis.title.y=element_blank())+
  theme(legend.title=element_blank())+
  annotate("text", x = .8, y = -4, label = "H")+
  annotate("text", x = .8, y = -8, label = "L")+
  annotate("text", x = .8, y = -11, label = "M")+
  annotate("text", x = .8, y = 5.5, label = "M")+
  annotate("text", x = .8, y = 3.5, label = "H")+
  annotate("text", x = .8, y = 2.3, label = "L")+
  labs(x = "syllable 2")


#HNR---------------------------------------------------------------------------------
#avg HNR by tone
ggplot(data = dat.spec) +
  geom_boxplot(mapping = aes(x = tone, y = avg_word_hnr, fill = tone), na.rm = TRUE) +
  ylim (-20,30)+
  scale_x_discrete(limits=c("L","M","H"))+
  theme(axis.title.x=element_blank())+
  labs(y = "mean word HNR")+
  theme(legend.position = "none")

#avg HNR boxplot v1
ggplot(data = datacvcv) +
  geom_boxplot(mapping = aes(x = tone1, y = avg_hnr_v1, fill = tone1), na.rm = TRUE) +
  ylim (-20,30)+
  scale_x_discrete(limits=c("L","M","H"))+
  labs(x = "syllable 1", y = "mean HNR")+
  theme(legend.position="none")

#avg HNR boxplot v2
ggplot(data = datacvcv) +
  geom_boxplot(mapping = aes(x = tone2, y = avg_hnr_v2, fill = tone2), na.rm = TRUE) +
  ylim (-20,30)+
  scale_x_discrete(limits=c("L","M","H"))+
  theme(axis.text.y=element_blank())+
  theme(axis.title.y=element_blank())+
  theme(legend.title=element_blank())+
  labs(x = "syllable 2")

#first syll HNR over time
ggplot(data = dat.lh_v1, aes(x = variable, y=value, group = tone1, color = tone1, shape = tone1)) +
  stat_summary( geom="line", fun.y = mean)+
  ylim (-20,30)+
  stat_summary(data = dat.mh_v1, geom="line", fun.y = mean)+
  stat_summary(data = dat.hh_v1, geom="line", fun.y = mean)+
  stat_summary(data = dat.lh_v1, geom="point", fun.y = mean)+
  stat_summary(data = dat.mh_v1, geom="point", fun.y = mean)+
  stat_summary(data = dat.hh_v1, geom="point", fun.y = mean)+
  stat_summary(data = dat.lh_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) + #can also
  stat_summary(data = dat.mh_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) +  # , geom = "smooth", se = Tuse errorbar
  stat_summary(data = dat.hh_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) +  #with no se
  theme(axis.text.x=element_blank())+
  scale_color_brewer(palette="Set1") +
  theme(legend.position="none")+
  #facet_wrap(vars(subj))+
  labs(x = "syllable 1", y = "mean HNR")

#second syll HNR over time
ggplot(data = dat.lh_v2, aes(x = variable, y=value, group = tone2, color = tone2, shape=tone2)) +
  stat_summary( geom="line", fun.y = mean)+
  ylim (-20,20)+
  stat_summary(data = dat.mh_v2, geom="line", fun.y = mean)+
  stat_summary(data = dat.hh_v2, geom="line", fun.y = mean)+
  stat_summary(data = dat.lh_v2, geom="point", fun.y = mean)+
  stat_summary(data = dat.mh_v2, geom="point", fun.y = mean)+
  stat_summary(data = dat.hh_v2, geom="point", fun.y = mean)+
  stat_summary(data = dat.lh_v2, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) + #can also
  stat_summary(data = dat.mh_v2, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) +  # , geom = "smooth", se = Tuse errorbar
  stat_summary(data = dat.hh_v2, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) +  #with no se
  theme(axis.text.x=element_blank())+
  scale_color_brewer(palette="Set1") +
  #facet_wrap(vars(subj))+
  theme(axis.text.y=element_blank())+
  theme(axis.title.y=element_blank())+
  theme(legend.title=element_blank())+
  labs(x = "syllable 2", y = "mean HNR")


#first syll f0 over time
ggplot(data = dathplot_v1, aes(x = variable, y=value, group = toneseq, color = tone1)) +
  stat_summary( geom="line", fun.y = mean)+
  stat_summary(data = dathplotm_v1, geom="line", fun.y = mean)+
  stat_summary(data = dathploth_v1, geom="line", fun.y = mean)+
  stat_summary(data = dathplot_v1, geom="point", fun.y = mean)+
  stat_summary(data = dathplotm_v1, geom="point", fun.y = mean)+
  stat_summary(data = dathploth_v1, geom="point", fun.y = mean)+
  stat_summary(data = dathplot_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) + #can also
  stat_summary(data = dathplotm_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) +  # , geom = "smooth", se = Tuse errorbar
  stat_summary(data = dathploth_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95)) +  #with no se
  theme(axis.text.x=element_blank())+
  scale_color_brewer(palette="Set1") +
  theme(legend.position="none")+
  labs(x = "time", y = "mean F0")+
  annotate("text", x = 4.2, y = 168, label = "HL")+
  annotate("text", x = 4.2, y = 164, label = "HH")+
  annotate("text", x = 4.2, y = 158, label = "HM")+
  annotate("text", x = 4.2, y = 150.7, label = "MH")+
  annotate("text", x = 4.2, y = 147, label = "ML")+
  annotate("text", x = 4.2, y = 143, label = "MM")+
  annotate("text", x = 4.2, y = 114, label = "LM")+
  annotate("text", x = 4.2, y = 107, label = "LL")+
  annotate("text", x = 4.2, y = 102, label = "LH")+
  annotate("text", x = 0.8, y = 105, label = "syllable 1")
