rm(list=ls())

library(readr)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsci)
library(ggplot2)
library(hrbrthemes)

cutflower <- read.csv("cutflower2.csv", header = TRUE)
cutflower <- cutflower |> na.omit()
glimpse(cutflower)
head(cutflower)
cutflower <- cutflower |> mutate(Concentration = str_sub(treatment.method, 6, 8))
cutflower$Concentration <- as.numeric(cutflower$Concentration)
cutflower$Concentration <- format(cutflower$Concentration, nsmall = 1)
cutflower$Concentration <- factor(cutflower$Concentration, levels = c("0.0", "0.1", "0.3", "0.5"))
cutflower <- cutflower %>% filter(Group %in% c("SSV", "SSR", "HSV", "HSR"))
cutflower$Group <- factor(cutflower$Group, levels = c("SSV", "SSR", "HSV", "HSR"), order=T)
cutflower <- cutflower |> na.omit()

##AG  
dt_AG <- cutflower |> 
  filter(Cultivar == "AG")
str(dt_AG) 
  
AG_vase_life <- aov(Vase_life ~ Concentration * Group, data=dt_AG)
  summary(AG_vase_life)
  tukey_AG <- TukeyHSD(AG_vase_life)
  print(tukey_AG)
  library(multcompView)
  tukey.cld_AG <- multcompLetters4(AG_vase_life, tukey_AG)
  print(tukey.cld_AG)
  
dt_AG_vase_life <- dt_AG %>% group_by(Group, Concentration) %>%
    summarise(AG_mean = mean(Vase_life),
              AG_sd = sd(Vase_life)) %>% 
    arrange(desc(AG_mean)) %>% 
    ungroup()
  
dt_AG_vase_life
cld2_AG <- data.frame(letters = tukey.cld_AG$`Concentration:Group`$Letters)
dt_AG_vase_life$cld <- cld2_AG$letters
dt_AG_vase_life
  
cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")
  
AG <- ggplot(dt_AG_vase_life, aes(x=Concentration, y=AG_mean, color = Group, linetype = Group, shape = Group)) +
    geom_point(size=2) +
    geom_line(aes(group = Group)) +
    theme_bw() +
    labs(y="Average vase life (day)") +
    geom_errorbar(aes(ymin=AG_mean-AG_sd, ymax=AG_mean+AG_sd), width=0.1, alpha=0.5) +
    scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
    scale_linetype_manual(values = c("dashed", "twodash", "solid", "solid"), labels = c("SSV", "SSR", "HSV", "HSR")) +
    scale_shape_manual(values = c(24, 17, 21, 16), labels = c("SSV", "SSR", "HSV", "HSR")) +
    theme(legend.position = c(0.1, 0.8), 
          axis.title.x =  element_blank(), 
          legend.background = element_rect(fill="transparent"),
          legend.key.size = unit(0.5, 'cm')) + 
    ggtitle("Cultivar: AG") + ylim(4, 23)

AG  


##BP
dt_BP <- cutflower |> 
  filter(Cultivar == "BP")
str(dt_BP) 

BP_vase_life <- aov(Vase_life ~ Concentration * Group, data=dt_BP)
summary(BP_vase_life)
tukey_BP <- TukeyHSD(BP_vase_life)
print(tukey_BP)
library(multcompView)
tukey.cld_BP <- multcompLetters4(BP_vase_life, tukey_BP)
print(tukey.cld_BP)

dt_BP_vase_life <- dt_BP %>% group_by(Group, Concentration) %>%
  summarise(BP_mean = mean(Vase_life),
            BP_sd = sd(Vase_life)) %>% 
  arrange(desc(BP_mean)) %>% 
  ungroup()

dt_BP_vase_life
cld2_BP <- data.frame(letters = tukey.cld_BP$`Concentration:Group`$Letters)
dt_BP_vase_life$cld <- cld2_BP$letters
dt_BP_vase_life

cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

BP <- ggplot(dt_BP_vase_life, aes(x=Concentration, y=BP_mean, color = Group, linetype = Group, shape = Group)) +
  geom_point(size=2) +
  geom_line(aes(group = Group)) +
  theme_bw() +
  #labs(x = "Concentration (mM)", y="Average vase life (day)") +
  geom_errorbar(aes(ymin=BP_mean-BP_sd, ymax=BP_mean+BP_sd), width=0.1, alpha=0.5) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  scale_linetype_manual(values = c("dashed", "twodash", "solid", "solid"), labels = c("SSV", "SSR", "HSV", "HSR")) +
  scale_shape_manual(values = c(24, 17, 21, 16), labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(legend.position = "none", 
        axis.title.x =  element_blank(), 
        axis.title.y =  element_blank(), 
        legend.background = element_rect(fill="transparent"),
        legend.key.size = unit(0.5, 'cm')) + 
  ggtitle("Cultivar: BP") + ylim(4, 23)

BP


##CP
dt_CP <- cutflower |> 
  filter(Cultivar == "CP")
str(dt_CP) 

CP_vase_life <- aov(Vase_life ~ Concentration * Group, data=dt_CP)
summary(CP_vase_life)
tukey_CP <- TukeyHSD(CP_vase_life)
print(tukey_CP)
library(multcompView)
tukey.cld_CP <- multcompLetters4(CP_vase_life, tukey_CP)
print(tukey.cld_CP)

dt_CP_vase_life <- dt_CP %>% group_by(Group, Concentration) %>%
  summarise(CP_mean = mean(Vase_life),
            CP_sd = sd(Vase_life)) %>% 
  arrange(desc(CP_mean)) %>% 
  ungroup()

dt_CP_vase_life
cld2_CP <- data.frame(letters = tukey.cld_CP$`Concentration:Group`$Letters)
dt_CP_vase_life$cld <- cld2_CP$letters
dt_CP_vase_life

cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

CP <- ggplot(dt_CP_vase_life, aes(x=Concentration, y=CP_mean, color = Group, linetype = Group, shape = Group)) +
  geom_point(size=2) +
  geom_line(aes(group = Group)) +
  theme_bw() +
  labs(x = "Concentration (mM)", y="Average vase life (day)") +
  geom_errorbar(aes(ymin=CP_mean-CP_sd, ymax=CP_mean+CP_sd), width=0.1, alpha=0.5) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  scale_linetype_manual(values = c("dashed", "twodash", "solid", "solid"), labels = c("SSV", "SSR", "HSV", "HSR")) +
  scale_shape_manual(values = c(24, 17, 21, 16), labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(legend.position = "none", 
        legend.background = element_rect(fill="transparent"),
        legend.key.size = unit(0.5, 'cm')) + 
  ggtitle("Cultivar: CP") + ylim(4, 23)

CP


##KW
dt_KW <- cutflower |> 
  filter(Cultivar == "KW")
str(dt_KW) 

KW_vase_life <- aov(Vase_life ~ Concentration * Group, data=dt_KW)
summary(KW_vase_life)
tukey_KW <- TukeyHSD(KW_vase_life)
print(tukey_KW)
library(multcompView)
tukey.cld_KW <- multcompLetters4(KW_vase_life, tukey_KW)
print(tukey.cld_KW)

dt_KW_vase_life <- dt_KW %>% group_by(Group, Concentration) %>%
  summarise(KW_mean = mean(Vase_life),
            KW_sd = sd(Vase_life)) %>% 
  arrange(desc(KW_mean)) %>% 
  ungroup()

dt_KW_vase_life
cld2_KW <- data.frame(letters = tukey.cld_KW$`Concentration:Group`$Letters)
dt_KW_vase_life$cld <- cld2_KW$letters
dt_KW_vase_life

cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

KW <- ggplot(dt_KW_vase_life, aes(x=Concentration, y=KW_mean, color = Group, linetype = Group, shape = Group)) +
  geom_point(size=2) +
  geom_line(aes(group = Group)) +
  theme_bw() +
  labs(x = "Concentration (mM)") +
  geom_errorbar(aes(ymin=KW_mean-KW_sd, ymax=KW_mean+KW_sd), width=0.1, alpha=0.5) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  scale_linetype_manual(values = c("dashed", "twodash", "solid", "solid"), labels = c("SSV", "SSR", "HSV", "HSR")) +
  scale_shape_manual(values = c(24, 17, 21, 16), labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(legend.position = "none", 
        axis.title.y =  element_blank(), 
        legend.background = element_rect(fill="transparent"),
        legend.key.size = unit(0.5, 'cm')) + 
  ggtitle("Cultivar: KW") + ylim(4, 23)

KW


library(gridExtra)

grid.arrange(AG, BP, CP, KW, nrow=2)
