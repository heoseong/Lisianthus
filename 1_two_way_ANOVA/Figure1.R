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
head(cutflower)


#####vase_life treatment_group
head(cutflower)
aov_vase_life <- aov(Vase_life ~ Cultivar * Group, data=cutflower)
summary(aov_vase_life)
tukey <- TukeyHSD(aov_vase_life)
print(tukey)
library(multcompView)
tukey.cld <- multcompLetters4(aov_vase_life, tukey)
print(tukey.cld)

cutflo_vase_life <- cutflower %>% group_by(Group, Cultivar) %>%
  summarise(Vase_life_mean = mean(Vase_life),
            Vase_life_sd = sd(Vase_life)) %>% 
  arrange(desc(Vase_life_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`Cultivar:Group`$Letters)
cutflo_vase_life$cld <- cld2$letters
cutflo_vase_life

dd1 <- ggplot(cutflo_vase_life, aes(x=Group, y=Vase_life_mean, fill=Cultivar)) +
  geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin=Vase_life_mean-Vase_life_sd, ymax=Vase_life_mean+Vase_life_sd),
                width=0.3, colour="black", alpha=0.4, linewidth=0.4, position = position_dodge(0.9)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=Group, y=Vase_life_mean+Vase_life_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Treatment", y = "Average vase life (day)", fill = "Cultivar") +
  theme(legend.position = c(0.1, 0.85), 
        legend.background = element_rect(fill='transparent'),
        legend.key.size = unit(0.5, 'cm')) +
  scale_fill_discrete(labels = c("AG", "BP", "CP", "KW")) +
  scale_x_discrete(labels = c("SSV", "SSR", "HSV", "HSR"))

dd1



#####vase_life concentration
aov2_vase_life <- aov(Vase_life ~ Concentration * Group, data=cutflower)
summary(aov2_vase_life)
tukey2 <- TukeyHSD(aov2_vase_life)
print(tukey2)

tukey2.cld <- multcompLetters4(aov2_vase_life, tukey2)
print(tukey2.cld)

cutflo2_vase_life <- cutflower %>% group_by(Group, Concentration) %>%
  summarise(Vase_life2_mean = mean(Vase_life),
            Vase_life2_sd = sd(Vase_life)) %>% 
  arrange(desc(Vase_life2_mean)) %>% 
  ungroup()

cld22 <- data.frame(letters = tukey2.cld$`Concentration:Group`$Letters)
cutflo2_vase_life$cld <- cld22$letters
cutflo2_vase_life

cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

dd2<- ggplot(cutflo2_vase_life, aes(x=Concentration, y=Vase_life2_mean, color = Group, linetype = Group, shape = Group)) +
  geom_point(size=2) +
  geom_line(aes(group = Group)) +
  theme_bw() +
  labs(x = "Concentration (mM)", y="Average vase life (day)", fill = "Treatment") +
  geom_errorbar(aes(ymin=Vase_life2_mean-Vase_life2_sd, ymax=Vase_life2_mean+Vase_life2_sd), width=0.1, alpha=0.5) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  scale_linetype_manual(values = c("dashed", "twodash", "solid", "solid"), labels = c("SSV", "SSR", "HSV", "HSR")) +
  scale_shape_manual(values = c(24, 17, 21, 16), labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(legend.position = c(0.1, 0.85),
        legend.background = element_rect(fill="transparent"),
        legend.key.size = unit(0.5, 'cm'))

#  stat_compare_means(aes(group = Group), label = "p.signif")

dd2



#####vase_life Concentration_Cultivar
aov3_vl <- aov(Vase_life ~ Concentration * Cultivar, data=cutflower)
summary(aov3_vl)
tukey_3 <- TukeyHSD(aov3_vl)
print(tukey_3)

library(multcompView)
tukey.cld_3 <- multcompLetters4(aov3_vl, tukey_3)
print(tukey.cld_3)

cutflo3_vl <- cutflower %>% group_by(Cultivar, Concentration) %>%
  summarise(vl_mean = mean(Vase_life),
            vl_sd = sd(Vase_life)) %>% 
  arrange(desc(vl_mean)) %>% 
  ungroup()

cld3 <- data.frame(letters = tukey.cld_3$`Concentration:Cultivar`$Letters)
cutflo3_vl$cld <- cld3$letters
cutflo3_vl


cols <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")

dd3 <- ggplot(cutflo3_vl, aes(x=Concentration, y=vl_mean, color = Cultivar, linetype = Cultivar, shape = Cultivar)) +
  geom_point(size=2) +
  geom_line(aes(group = Cultivar)) +
  theme_bw() +
  labs(x= "Concentration (mM)", y="Average vase life (day)", fill = "Cultivar") +
  geom_errorbar(aes(ymin=vl_mean-vl_sd, ymax=vl_mean+vl_sd), width=0.1, alpha=0.5) +
  scale_color_manual(values = cols, labels = c("AG", "BP", "CP", "KW")) +
  scale_linetype_manual(values = c("dashed", "twodash", "solid", "solid"), labels = c("AG", "BP", "CP", "KW")) +
  scale_shape_manual(values = c(24, 17, 21, 16), labels = c("AG", "BP", "CP", "KW")) +
  theme(legend.position = c(0.1, 0.85),
        legend.background = element_rect(fill="transparent"),
        legend.key.size = unit(0.5, 'cm'))


dd3
library(gridExtra)
grid.arrange(dd1, dd2, dd3, nrow=1)

grid.arrange(dd1, dd2, nrow=1)
