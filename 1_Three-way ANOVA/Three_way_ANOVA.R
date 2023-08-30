rm(list=ls())
library(readr)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsci)
planting <- read.csv("planting.csv", header = TRUE)
glimpse(planting)
View(planting)
planting <- planting |> na.omit()
#planting <- planting %>% rename(length_li = length)
planting$Concentration <- format(planting$Concentration, nsmall = 1)
planting$Concentration <- factor(planting$Concentration, levels = c("0.0", "0.1", "0.3", "0.5"))
#planting <- planting %>% mutate(treat_cul = paste0(group, "_", cultivar))
#planting <- planting %>% mutate(one_group = str_sub(group, 1, 3))
#planting$treat.No. <- factor(planting$treat.No., levels = c(1, 2, 3, 4))
planting$Group <- factor(planting$Group)
#planting$substrate <- factor(planting$substrate)
#planting$treatment.method <- factor(planting$treatment.method)
#planting$treatment <- factor(planting$treatment)
planting$Cultivar <- factor(planting$Cultivar)
str(planting)
lian <- planting %>% filter(Group %in% c("SSV", "SSR", "HSV", "HSR"))
lian$Group <- factor(lian$Group, levels = c("SSV", "SSR", "HSV", "HSR"), order=T)
head(lian)

#######stem_volume
head(lian)
aov2_stem_volume <- aov(stem.volume ~ Concentration * Group * Cultivar, data=lian)
summary(aov2_stem_volume)
tukey <- TukeyHSD(aov2_stem_volume)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_stem_volume, tukey)
print(tukey.cld)

lian_stem_volume <- lian %>% group_by(Group, Cultivar, Concentration) %>%
  summarise(stem.volume_mean = mean(stem.volume),
            stem.volume_sd = sd(stem.volume)) %>% 
  arrange(desc(stem.volume_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`Concentration:Group:Cultivar`$Letters)
lian_stem_volume$cld <- cld2$letters
View(lian_stem_volume)
write.csv(lian_stem_volume, "lian_stem_volume.csv")

cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(lian_stem_volume, aes(x=Concentration, y=stem.volume_mean, color=Group)) +
  geom_point(size=2, aes(color=Group)) +
  geom_line(aes(group = Group, color=Group)) +
  geom_errorbar(aes(ymin=stem.volume_mean-stem.volume_sd, ymax=stem.volume_mean+stem.volume_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=Concentration, y=stem.volume_mean+stem.volume_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "Stem volume (mm)") + ylim(2, 7) +
  facet_grid(Group ~ Cultivar) +
  theme(legend.position = "none")


#######stem.nodes
head(lian)
aov2_stem.nodes <- aov(stem.nodes ~ Concentration * Group * Cultivar, data=lian)
summary(aov2_stem.nodes)
tukey <- TukeyHSD(aov2_stem.nodes)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_stem.nodes, tukey)
print(tukey.cld)

lian_stem.nodes <- lian %>% group_by(Group, Cultivar, Concentration) %>%
  summarise(stem.nodes_mean = mean(stem.nodes),
            stem.nodes_sd = sd(stem.nodes)) %>% 
  arrange(desc(stem.nodes_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`Concentration:Group:Cultivar`$Letters)
lian_stem.nodes$cld <- cld2$letters


cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(lian_stem.nodes, aes(x=Concentration, y=stem.nodes_mean, color=Group)) +
  geom_point(size=2, aes(color=Group)) +
  geom_line(aes(group = Group, color=Group)) +
  geom_errorbar(aes(ymin=stem.nodes_mean-stem.nodes_sd, ymax=stem.nodes_mean+stem.nodes_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=Concentration, y=stem.nodes_mean+stem.nodes_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "Stem nodes (no.)") + 
  facet_grid(Group ~ Cultivar) +
  theme(legend.position = "none")


#######length
head(lian)
aov2_length <- aov(length ~ Concentration * Group * Cultivar, data=lian)
summary(aov2_length)
tukey <- TukeyHSD(aov2_length)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_length, tukey)
print(tukey.cld)

lian_length <- lian %>% group_by(Group, Cultivar, Concentration) %>%
  summarise(length_mean = mean(length),
            length_sd = sd(length)) %>% 
  arrange(desc(length_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`Concentration:Group:Cultivar`$Letters)
lian_length$cld <- cld2$letters


cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(lian_length, aes(x=Concentration, y=length_mean, color=Group)) +
  geom_point(size=2, aes(color=Group)) +
  geom_line(aes(group = Group, color=Group)) +
  geom_errorbar(aes(ymin=length_mean-length_sd, ymax=length_mean+length_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=Concentration, y=length_mean+length_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "Stem length (mm)") + ylim(35, 75) + 
  facet_grid(Group ~ Cultivar) +
  theme(legend.position = "none")



#bush
head(lian)
aov2_bush <- aov(bush ~ Concentration * Group * Cultivar, data=lian)
summary(aov2_bush)
tukey <- TukeyHSD(aov2_bush)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_bush, tukey)
print(tukey.cld)

lian_bush <- lian %>% group_by(Group, Cultivar, Concentration) %>%
  summarise(bush_mean = mean(bush),
            bush_sd = sd(bush)) %>% 
  arrange(desc(bush_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`Concentration:Group:Cultivar`$Letters)
lian_bush$cld <- cld2$letters


cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(lian_bush, aes(x=Concentration, y=bush_mean, color=Group)) +
  geom_point(size=2, aes(color=Group)) +
  geom_line(aes(group = Group, color=Group)) +
  geom_errorbar(aes(ymin=bush_mean-bush_sd, ymax=bush_mean+bush_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=Concentration, y=bush_mean+bush_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "Stem bush (no.)") + ylim(2, 8) +
  facet_grid(Group ~ Cultivar) +
  theme(legend.position = "none")


###flowering.day
head(lian)
aov2_flowering.day <- aov(flowering.day ~ Concentration * Group * Cultivar, data=lian)
summary(aov2_flowering.day)
tukey <- TukeyHSD(aov2_flowering.day)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_flowering.day, tukey)
print(tukey.cld)

lian_flowering.day <- lian %>% group_by(Group, Cultivar, Concentration) %>%
  summarise(flowering.day_mean = mean(flowering.day),
            flowering.day_sd = sd(flowering.day)) %>% 
  arrange(desc(flowering.day_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`Concentration:Group:Cultivar`$Letters)
lian_flowering.day$cld <- cld2$letters


cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(lian_flowering.day, aes(x=Concentration, y=flowering.day_mean, color=Group)) +
  geom_point(size=2, aes(color=Group)) +
  geom_line(aes(group = Group, color=Group)) +
  geom_errorbar(aes(ymin=flowering.day_mean-flowering.day_sd, ymax=flowering.day_mean+flowering.day_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=Concentration, y=flowering.day_mean+flowering.day_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "Flowering day (day)") + ylim(55, 85) +
  facet_grid(Group ~ Cultivar) +
  theme(legend.position = "none")

###SPAD
head(lian)
aov2_SPAD <- aov(SPAD ~ Concentration * Group * Cultivar, data=lian)
summary(aov2_SPAD)
tukey <- TukeyHSD(aov2_SPAD)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_SPAD, tukey)
print(tukey.cld)

lian_SPAD <- lian %>% group_by(Group, Cultivar, Concentration) %>%
  summarise(SPAD_mean = mean(SPAD),
            SPAD_sd = sd(SPAD)) %>% 
  arrange(desc(SPAD_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`Concentration:Group:Cultivar`$Letters)
lian_SPAD$cld <- cld2$letters


cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(lian_SPAD, aes(x=Concentration, y=SPAD_mean, color=Group)) +
  geom_point(size=2, aes(color=Group)) +
  geom_line(aes(group = Group, color=Group)) +
  geom_errorbar(aes(ymin=SPAD_mean-SPAD_sd, ymax=SPAD_mean+SPAD_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=Concentration, y=SPAD_mean+SPAD_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "SPAD (value)") + ylim(40, 180) +
  facet_grid(Group ~ Cultivar) +
  theme(legend.position = "none")





###########cut flower
cutflower <- read.csv("cutflower.csv", header = TRUE)
cutflower <- cutflower |> na.omit()
head(cutflower)

cutflower$Group <- factor(cutflower$Group)
cutflower$Cultivar <- factor(cutflower$Cultivar)
cutflower$Petal_size <- as.numeric(cutflower$Petal_size)
cutflower <- cutflower |> na.omit()
sum(is.na(cutflower$Petal_size))
cutflower <- cutflower |> mutate(Concentration = str_sub(Treatment_method, 6, 8))
cutflower$Concentration <- as.numeric(cutflower$Concentration)
cutflower$Concentration <- format(cutflower$Concentration, nsmall = 1)
cutflower$Concentration <- factor(cutflower$Concentration, levels = c("0.0", "0.1", "0.3", "0.5"))
str(cutflower)

cutflo <- cutflower %>% filter(Group %in% c("SSV", "SSR", "HSV", "HSR"))
cutflo$Group <- factor(cutflo$Group, levels = c("SSV", "SSR", "HSV", "HSR"), order=T)
head(cutflo)

#####fresh_weight
head(cutflo)
aov2_Fresh_weight <- aov(Fresh_weight ~ Concentration * Group * Cultivar, data=cutflo)
summary(aov2_Fresh_weight)
tukey <- TukeyHSD(aov2_Fresh_weight)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_Fresh_weight, tukey)
print(tukey.cld)

cut_Fresh_weight <- cutflo %>% group_by(Group, Cultivar, Concentration) %>%
  summarise(Fresh_weight_mean = mean(Fresh_weight),
            Fresh_weight_sd = sd(Fresh_weight)) %>% 
  arrange(desc(Fresh_weight_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`Concentration:Group:Cultivar`$Letters)
cut_Fresh_weight$cld <- cld2$letters


cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(cut_Fresh_weight, aes(x=Concentration, y=Fresh_weight_mean, color=Group)) +
  geom_point(size=2, aes(color=Group)) +
  geom_line(aes(group = Group, color=Group)) +
  geom_errorbar(aes(ymin=Fresh_weight_mean-Fresh_weight_sd, ymax=Fresh_weight_mean+Fresh_weight_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=Concentration, y=Fresh_weight_mean+Fresh_weight_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "Fresh weight (mg)") + ylim(1, 5.5) +
  facet_grid(Group ~ Cultivar) +
  theme(legend.position = "none")


####Dry_weight
head(cutflo)
aov2_Dry_weight <- aov(Dry_weight ~ Concentration * Group * Cultivar, data=cutflo)
summary(aov2_Dry_weight)
tukey <- TukeyHSD(aov2_Dry_weight)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_Dry_weight, tukey)
print(tukey.cld)

cut_Dry_weight <- cutflo %>% group_by(Group, Cultivar, Concentration) %>%
  summarise(Dry_weight_mean = mean(Dry_weight),
            Dry_weight_sd = sd(Dry_weight)) %>% 
  arrange(desc(Dry_weight_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`Concentration:Group:Cultivar`$Letters)
cut_Dry_weight$cld <- cld2$letters


cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(cut_Dry_weight, aes(x=Concentration, y=Dry_weight_mean, color=Group)) +
  geom_point(size=2, aes(color=Group)) +
  geom_line(aes(group = Group, color=Group)) +
  geom_errorbar(aes(ymin=Dry_weight_mean-Dry_weight_sd, ymax=Dry_weight_mean+Dry_weight_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=Concentration, y=Dry_weight_mean+Dry_weight_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "Dry weight (mg)") + ylim(0, 4.5) +
  facet_grid(Group ~ Cultivar) +
  theme(legend.position = "none")



###weight_diff
head(cutflo)
aov2_Diff <- aov(Diff ~ Concentration * Group * Cultivar, data=cutflo)
summary(aov2_Diff)
tukey <- TukeyHSD(aov2_Diff)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_Diff, tukey)
print(tukey.cld)

cut_Diff <- cutflo %>% group_by(Group, Cultivar, Concentration) %>%
  summarise(Diff_mean = mean(Diff),
            Diff_sd = sd(Diff)) %>% 
  arrange(desc(Diff_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`Concentration:Group:Cultivar`$Letters)
cut_Diff$cld <- cld2$letters


cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(cut_Diff, aes(x=Concentration, y=Diff_mean, color=Group)) +
  geom_point(size=2, aes(color=Group)) +
  geom_line(aes(group = Group, color=Group)) +
  geom_errorbar(aes(ymin=Diff_mean-Diff_sd, ymax=Diff_mean+Diff_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=Concentration, y=Diff_mean+Diff_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "Weight difference (mg)") + ylim(0, 5)+
  facet_grid(Group ~ Cultivar) +
  theme(legend.position = "none")


####Vase life
head(cutflo)
aov2_Vase_life <- aov(Vase_life ~ Concentration * Group * Cultivar, data=cutflo)
summary(aov2_Vase_life)
tukey <- TukeyHSD(aov2_Vase_life)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_Vase_life, tukey)
print(tukey.cld)

cut_Vase_life <- cutflo %>% group_by(Group, Cultivar, Concentration) %>%
  summarise(Vase_life_mean = mean(Vase_life),
            Vase_life_sd = sd(Vase_life)) %>% 
  arrange(desc(Vase_life_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`Concentration:Group:Cultivar`$Letters)
cut_Vase_life$cld <- cld2$letters


cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(cut_Vase_life, aes(x=Concentration, y=Vase_life_mean, color=Group)) +
  geom_point(size=2, aes(color=Group)) +
  geom_line(aes(group = Group, color=Group)) +
  geom_errorbar(aes(ymin=Vase_life_mean-Vase_life_sd, ymax=Vase_life_mean+Vase_life_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=Concentration, y=Vase_life_mean+Vase_life_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "Vase life (day)") + ylim(0, 25) +
  facet_grid(Group ~ Cultivar) +
  theme(legend.position = "none")


####Petals
head(cutflo)
aov2_Petals <- aov(Petals ~ Concentration * Group * Cultivar, data=cutflo)
summary(aov2_Petals)
tukey <- TukeyHSD(aov2_Petals)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_Petals, tukey)
print(tukey.cld)

cut_Petals <- cutflo %>% group_by(Group, Cultivar, Concentration) %>%
  summarise(Petals_mean = mean(Petals),
            Petals_sd = sd(Petals)) %>% 
  arrange(desc(Petals_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`Concentration:Group:Cultivar`$Letters)
cut_Petals$cld <- cld2$letters


cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(cut_Petals, aes(x=Concentration, y=Petals_mean, color=Group)) +
  geom_point(size=2, aes(color=Group)) +
  geom_line(aes(group = Group, color=Group)) +
  geom_errorbar(aes(ymin=Petals_mean-Petals_sd, ymax=Petals_mean+Petals_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=Concentration, y=Petals_mean+Petals_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "Petals (no.)") + ylim(8, 20) +
  facet_grid(Group ~ Cultivar) +
  theme(legend.position = "none")


####petal_size
head(cutflo)
aov2_Petal_size <- aov(Petal_size ~ Concentration * Group * Cultivar, data=cutflo)
summary(aov2_Petal_size)
tukey <- TukeyHSD(aov2_Petal_size)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_Petal_size, tukey)
print(tukey.cld)

cut_Petal_size <- cutflo %>% group_by(Group, Cultivar, Concentration) %>%
  summarise(Petal_size_mean = mean(Petal_size),
            Petal_size_sd = sd(Petal_size)) %>% 
  arrange(desc(Petal_size_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`Concentration:Group:Cultivar`$Letters)
cut_Petal_size$cld <- cld2$letters


cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(cut_Petal_size, aes(x=Concentration, y=Petal_size_mean, color=Group)) +
  geom_point(size=2, aes(color=Group)) +
  geom_line(aes(group = Group, color=Group)) +
  geom_errorbar(aes(ymin=Petal_size_mean-Petal_size_sd, ymax=Petal_size_mean+Petal_size_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=Concentration, y=Petal_size_mean+Petal_size_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "Petal size (mm)") + ylim(40, 70) +
  facet_grid(Group ~ Cultivar) +
  theme(legend.position = "none")



#leaf analysis
planting <- read.csv("leaf_analysis_for_correlation.csv", header = TRUE)
glimpse(planting)
head(planting)
planting <- planting |> na.omit()
#planting <- planting %>% rename(length_li = length)
planting <- planting |> mutate(concentration = str_sub(treatment.method, 6, 8))
planting$concentration <- format(planting$concentration, nsmall = 1)
planting$concentration <- factor(planting$concentration, levels = c("0.0", "0.1", "0.3", "0.5"))
planting <- planting %>% mutate(treat_cul = paste0(group, "_", cultivar))
planting <- planting %>% mutate(one_group = str_sub(group, 1, 3))
planting$treat.No. <- factor(planting$treat.No., levels = c(1, 2, 3, 4))
planting$group <- factor(planting$group)
planting$substrate <- factor(planting$substrate)
planting$treatment.method <- factor(planting$treatment.method)
planting$treatment <- factor(planting$treatment)
planting$cultivar <- factor(planting$cultivar)
str(planting)
planting <- planting |> filter(group %in% c("SSV", "SSR", "HSV", "HSR"))
planting$group <- factor(planting$group, levels = c("SSV", "SSR", "HSV", "HSR"), order=T)


##nitrogen
head(planting)
aov2_T.N <- aov(T.N ~ concentration * group * cultivar, data=planting)
summary(aov2_T.N)
tukey <- TukeyHSD(aov2_T.N)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_T.N, tukey)
print(tukey.cld)

lian_T.N <- planting %>% group_by(group, cultivar, concentration) %>%
  summarise(T.N_mean = mean(T.N),
            T.N_sd = sd(T.N)) %>% 
  arrange(desc(T.N_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`concentration:group:cultivar`$Letters)
lian_T.N$cld <- cld2$letters

cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(lian_T.N, aes(x=concentration, y=T.N_mean, color=group)) +
  geom_point(size=2, aes(color=group)) +
  geom_line(aes(group = group, color=group)) +
  geom_errorbar(aes(ymin=T.N_mean-T.N_sd, ymax=T.N_mean+T.N_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=concentration, y=T.N_mean+T.N_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "Total nitrogen (%)") + ylim(0, 2) +
  facet_grid(group ~ cultivar) +
  theme(legend.position = "none")


###phosphorus
head(planting)
aov2_P.O. <- aov(P.O. ~ concentration * group * cultivar, data=planting)
summary(aov2_P.O.)
tukey <- TukeyHSD(aov2_P.O.)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_P.O., tukey)
print(tukey.cld)

lian_P.O. <- planting %>% group_by(group, cultivar, concentration) %>%
  summarise(P.O._mean = mean(P.O.),
            P.O._sd = sd(P.O.)) %>% 
  arrange(desc(P.O._mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`concentration:group:cultivar`$Letters)
lian_P.O.$cld <- cld2$letters

cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

y_expression <- expression(P[2]*O[5]~~"(%)")

ggplot(lian_P.O., aes(x=concentration, y=P.O._mean, color=group)) +
  geom_point(size=2, aes(color=group)) +
  geom_line(aes(group = group, color=group)) +
  geom_errorbar(aes(ymin=P.O._mean-P.O._sd, ymax=P.O._mean+P.O._sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=concentration, y=P.O._mean+P.O._sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = y_expression ) + ylim(0, 3) +
  facet_grid(group ~ cultivar) +
  theme(legend.position = "none")


####Potassium
head(planting)
aov2_K.O <- aov(K.O ~ concentration * group * cultivar, data=planting)
summary(aov2_K.O)
tukey <- TukeyHSD(aov2_K.O)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_K.O, tukey)
print(tukey.cld)

lian_K.O <- planting %>% group_by(group, cultivar, concentration) %>%
  summarise(K.O_mean = mean(K.O),
            K.O_sd = sd(K.O)) %>% 
  arrange(desc(K.O_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`concentration:group:cultivar`$Letters)
lian_K.O$cld <- cld2$letters

cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

y_expression <- expression(K[2]*O~~"(%)")

ggplot(lian_K.O, aes(x=concentration, y=K.O_mean, color=group)) +
  geom_point(size=2, aes(color=group)) +
  geom_line(aes(group = group, color=group)) +
  geom_errorbar(aes(ymin=K.O_mean-K.O_sd, ymax=K.O_mean+K.O_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=concentration, y=K.O_mean+K.O_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = y_expression) + ylim(1.5, 4) +
  facet_grid(group ~ cultivar) +
  theme(legend.position = "none")


###Calcium
head(planting)
aov2_CaO <- aov(CaO ~ concentration * group * cultivar, data=planting)
summary(aov2_CaO)
tukey <- TukeyHSD(aov2_CaO)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_CaO, tukey)
print(tukey.cld)

lian_CaO <- planting %>% group_by(group, cultivar, concentration) %>%
  summarise(CaO_mean = mean(CaO),
            CaO_sd = sd(CaO)) %>% 
  arrange(desc(CaO_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`concentration:group:cultivar`$Letters)
lian_CaO$cld <- cld2$letters

cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(lian_CaO, aes(x=concentration, y=CaO_mean, color=group)) +
  geom_point(size=2, aes(color=group)) +
  geom_line(aes(group = group, color=group)) +
  geom_errorbar(aes(ymin=CaO_mean-CaO_sd, ymax=CaO_mean+CaO_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=concentration, y=CaO_mean+CaO_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "CaO (%)") + ylim(0.1, 0.5) +
  facet_grid(group ~ cultivar) +
  theme(legend.position = "none")

###Magnesium
head(planting)
aov2_MgO <- aov(MgO ~ concentration * group * cultivar, data=planting)
summary(aov2_MgO)
tukey <- TukeyHSD(aov2_MgO)
print(tukey)

library(multcompView)
tukey.cld <- multcompLetters4(aov2_MgO, tukey)
print(tukey.cld)

lian_MgO <- planting %>% group_by(group, cultivar, concentration) %>%
  summarise(MgO_mean = mean(MgO),
            MgO_sd = sd(MgO)) %>% 
  arrange(desc(MgO_mean)) %>% 
  ungroup()

cld2 <- data.frame(letters = tukey.cld$`concentration:group:cultivar`$Letters)
lian_MgO$cld <- cld2$letters

cols <- c("coral1", "deeppink", "mediumpurple2", "mediumpurple4")

ggplot(lian_MgO, aes(x=concentration, y=MgO_mean, color=group)) +
  geom_point(size=2, aes(color=group)) +
  geom_line(aes(group = group, color=group)) +
  geom_errorbar(aes(ymin=MgO_mean-MgO_sd, ymax=MgO_mean+MgO_sd),
                width=0.3, colour="gray", alpha=0.9, size=0.4, position = position_dodge(0.9)) +
  scale_color_manual(values = cols, labels = c("SSV", "SSR", "HSV", "HSR")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_bw() +
  geom_text(aes(label = cld, x=concentration, y=MgO_mean+MgO_sd), 
            position = position_dodge(0.9), size=3,
            vjust=-1, hjust=0.5, colour = "gray25") +
  labs(x = "Concentration (mM)", y = "MgO (%)") + ylim(0.3, 1.1) +
  facet_grid(group ~ cultivar) +
  theme(legend.position = "none")
