source("datatidy.R")

#plots------------------------------------------------------------------------------------------

#plot avg spec + hnr
ggplot(datana) +
  geom_point(mapping = aes(x = avg_spec, y = avg_hnr, color = target_tone, shape = target_tone), na.rm = TRUE) +
  labs(x = "Average spectral tilt", y = "Average HNR", key = "category") +
  theme(legend.title=element_blank()) +
  scale_color_brewer(palette="Set1")

#plot avg spec + hnr v1
ggplot(datacvcv) +
  geom_point(mapping = aes(x = avg_spec_v1, y = avg_hnr_v1, color = tone1, shape = tone1), na.rm = TRUE) +
  labs(x = "Average spectral tilt", y = "Average HNR", key = "category") +
  theme(legend.title=element_blank()) +
  geom_point(mapping = aes(x = avg_spec_v2, y = avg_hnr_v2, color = tone2, shape = tone2), na.rm = TRUE) +
  labs(x = "Average spectral tilt", y = "Average HNR", key = "category") +
  theme(legend.title=element_blank()) +
  scale_color_brewer(palette="Set1")

#plot avg spec + hnr v2
ggplot(datacvcv) +
  geom_point(mapping = aes(x = avg_spec_v2, y = avg_hnr_v2, color = tone2, shape = tone2), na.rm = TRUE) +
  labs(x = "Average spectral tilt", y = "Average HNR", key = "category") +
  theme(legend.title=element_blank()) +
  scale_color_brewer(palette="Set1")

#avg f0 boxplot
ggplot(data = datana) +
  geom_boxplot(mapping = aes(x = target_tone, y = avg_f0, fill = target_tone), na.rm = TRUE) +
  ylim (75,200)

#avg f0 boxplot v1
ggplot(data = datacvcv) +
  geom_boxplot(mapping = aes(x = tone1, y = avg_f0_v1, fill = tone1), na.rm = TRUE) +
  ylim (75,200)


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


#line/point plot mean f0 over "time" i.e. f0 slice, first syllable
ggplot(data = datf0plot_v1, aes(x = variable, y=value, group = tone1, color = tone1)) +
  stat_summary( geom="line", fun.y = mean)+
  stat_summary(data = datf0plotm_v1, geom="line", fun.y = mean)+
  stat_summary(data = datf0ploth_v1, geom="line", fun.y = mean)+
  stat_summary(data = datf0plot_v1, geom="point", fun.y = mean)+
  stat_summary(data = datf0plotm_v1, geom="point", fun.y = mean)+
  stat_summary(data = datf0ploth_v1, geom="point", fun.y = mean)+
  stat_summary(data = datf0plot_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) + #can also
  stat_summary(data = datf0plotm_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) +  #use errorbar
  stat_summary(data = datf0ploth_v1, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) +  #with no se
  theme(axis.text.x=element_blank())+
  scale_color_brewer(palette="Set1") +
  theme(legend.title=element_blank())+
  labs(x = "time", y = "mean F0")

#line/point plot mean f0 over "time" i.e. f0 slice, second syllable
ggplot(data = datf0plot_v2, aes(x = variable, y=value, group = tone2, color = tone2)) +
  stat_summary( geom="line", fun.y = mean)+
  stat_summary(data = datf0plotm_v2, geom="line", fun.y = mean)+
  stat_summary(data = datf0ploth_v2, geom="line", fun.y = mean)+
  stat_summary(data = datf0plot_v2, geom="point", fun.y = mean)+
  stat_summary(data = datf0plotm_v2, geom="point", fun.y = mean)+
  stat_summary(data = datf0ploth_v2, geom="point", fun.y = mean)+
  stat_summary(data = datf0plot_v2, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) + #can also
  stat_summary(data = datf0plotm_v2, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) +  #use errorbar
  stat_summary(data = datf0ploth_v2, fun.data=mean_cl_normal, fun.args=list(conf.int=0.95), geom = "smooth", se = T) +  #with no se
  theme(axis.text.x=element_blank())+
  scale_color_brewer(palette="Set1") +
  theme(legend.title=element_blank())+
  labs(x = "time", y = "mean F0")




