source("datatidy.R")

#plots------------------------------------------------------------------------------------------

#plot avg spec + hnr
ggplot(datana) +
  geom_point(mapping = aes(x = avg_spec, y = avg_hnr, color = target_tone, shape = target_tone), na.rm = TRUE) +
  labs(x = "Average spectral tilt", y = "Average HNR", key = "category") +
  theme(legend.title=element_blank()) +
  scale_color_brewer(palette="Set1")


#avg f0 boxplot
ggplot(data = datana) +
  geom_boxplot(mapping = aes(x = target_tone, y = avg_f0, fill = target_tone), na.rm = TRUE) +
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

