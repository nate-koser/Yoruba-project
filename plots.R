source("datatidy.R")

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
