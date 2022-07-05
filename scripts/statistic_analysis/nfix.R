##Este script documenta el análsis estádistico para la 
#comparación de nanomoles de etileno (C2H4) por hectogramo (hg) 
#como producto de la reducción del acetileno por las cepas que 
#representan las diferentes huellas genéticas identificadas con el método
#ERIC-PCR que se aislaron de Lysiloma sp.

library(dplyr)
library(multcompView)
library(ggplot2)
library(ggpubr)

#DB
ethylene<-c(1559.138769,1222.032251,645.0858506,206.4038156,1027.794685,1035.811091,0,54.70588235,268.7980922,1209.6407,1925.384208,1632.896661,1908.614308,2474.348702,1414.664547,313.4499205,92.69085128,383.0890302,1319.446741,1109.494436,931.872814,1800.740223,1507.220986,1862.562798,1957.245514,1811.858824,1086.547536,1957.245514,1811.858824,1086.547536)
strains<-c("CCGB01","CCGB01","CCGB01",
           "CCGBR2","CCGBR2","CCGBR2",
           "CCGBR7","CCGBR7","CCGBR7",
           "CCGBR10","CCGBR10","CCGBR10",
           "CCGB12","CCGB12","CCGB12",
           "CCGBR13","CCGBR13","CCGBR13", 
           "CCGBR17", "CCGBR17", "CCGBR17",
           "CCGB20","CCGB20","CCGB20",
           "CCGBR22","CCGBR22","CCGBR22",
           "CCGBR26","CCGBR26","CCGBR26")
stranis<-as.factor(strains)
data_nfix<-data.frame("ethylene"=ethylene, "strains"=strains)

#ANOVA-oneway & Tuckey test

one.way <- aov(ethylene ~ strains, data = data_nfix)
summary(one.way)
TukeyHSD(one.way)
tukey<-TukeyHSD(one.way)

aov_residuals <- residuals(object = one.way )
shapiro.test(x = aov_residuals )#Prueba de normalidad de residuos
bartlett.test(aov_residuals, strains)#Prueba de homogeneidad de varianzas de residuos

#Graficando datos como puntos con errores estándar y  
#agrupados de acuerdo con la prueba de Tukey

mean.ethylene.data <- data_nfix %>%
  group_by(strains) %>%
  summarise(
    ethylene = mean(ethylene)
  )

mean.ethylene.data$group <- c("abc","a","ab","ab","c","abc","bc","ab","ab","c")

mean.ethylene.data

one.way.plot <- ggplot(data_nfix, aes(x = strains, y = ethylene, group=strains)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

one.way.plot <- one.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.ethylene.data, aes(x=strains, y=ethylene))

one.way.plot <- one.way.plot +
  geom_text(data=mean.ethylene.data, label=mean.ethylene.data$group, vjust = -7.7, size = 3.5)


one.way.plot <- one.way.plot +
  theme_classic2() +
  labs(
    x = "Strains",
    y = "Ethylene (nmol/hg)")
one.way.plot