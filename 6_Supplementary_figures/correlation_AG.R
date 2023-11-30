rm(list=ls())

library(tidyverse)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(gridExtra)

cutflower <- read.csv("Lab_C_h_calculated.csv", header = TRUE)
cutflower <- cutflower |> na.omit()
glimpse(cutflower)
str(cutflower)
cutflower$Cultivar <- as.factor(cutflower$Cultivar)
#cutflower$concentration <- as.factor(cutflower$concentration)
#cutflower$concentration <- factor(cutflower$concentration, levels = c("0", "0.1", "0.3", "0.5", order=T))
head(cutflower)

# correlation <- cutflower |> 
#   arrange(cultivar) |> 
#   select(cultivar, L_1, a_1, b_1, L_6, a_6, b_6, L_9, a_9, b_9, L_11, a_11, b_11, L_14, a_14, b_14)
# View(correlation)
col = c("green", "purple", "pink", "grey")

L_C_1 <- ggscatter(cutflower, x = "L_1", y = "C_1",    color = "Cultivar", palette = col,
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "C") + xlim(40, 90) + ylim(0, 40)

L_C_6 <- ggscatter(cutflower, x = "L_6", y = "C_6",    color = "Cultivar", palette = col,
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "C") + xlim(40, 90) + ylim(0, 40)



L_C_9 <- ggscatter(cutflower, x = "L_9", y = "C_9",    color = "Cultivar", palette = col,
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "C") + xlim(40, 90) + ylim(0, 40)


L_C_11 <- ggscatter(cutflower, x = "L_11", y = "C_11",    color = "Cultivar", palette = col,
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "C") + xlim(40, 90) + ylim(0, 40)

L_C_14 <- ggscatter(cutflower, x = "L_14", y = "C_14",    color = "Cultivar", palette = col,
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "C") + xlim(40, 90) + ylim(0, 40)

grid.arrange(L_C_1, L_C_6, L_C_9, L_C_11, L_C_14, ncol = 5)


####################


col = c("green", "purple", "pink", "grey")

V_h_14 <- ggscatter(cutflower, x = "h_14", y = "Vase_life",    color = "Cultivar", palette = col,
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "VL", y = "h") #+ xlim(40, 90) + ylim(0, 40)


grid.arrange(C_h_1, C_h_6, C_h_9, C_h_11, C_h_14, ncol = 5)


#############################






cor_AG <- correlation |> 
  filter(cultivar == "AG")
head(cor_AG)
str(cor_AG)
View(cor_AG)


# Scatter plot with correlation coefficient
#:::::::::::::::::::::::::::::::::::::::::::::::::
L_a_1 <- ggscatter(cor_AG, x = "L_1", y = "a_1",    #color = "concentration", palette = "jco",
                add = "reg.line",  # Add regressin line
                add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(55, 90) + ylim(-13, -2)


a_b_1 <- ggscatter(cor_AG, x = "a_1", y = "b_1",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b") +  xlim(-13, -2) + ylim(5, 25)


b_L_1 <- ggscatter(cor_AG, x = "b_1", y = "L_1",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_AG$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(5, 25) + ylim(55, 90)




#######Day 6
L_a_6 <- ggscatter(cor_AG, x = "L_6", y = "a_6",    #color = "concentration", palette = "jco",
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(55, 90) + ylim(-13, -2)



a_b_6 <- ggscatter(cor_AG, x = "a_6", y = "b_6",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b")  +  xlim(-13, -2) + ylim(5, 25)



b_L_6 <- ggscatter(cor_AG, x = "b_6", y = "L_6",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_AG$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(5, 25) + ylim(55, 90)





######Day 9
L_a_9 <- ggscatter(cor_AG, x = "L_9", y = "a_9",    #color = "concentration", palette = "jco",
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(55, 90) + ylim(-13, -2)



a_b_9 <- ggscatter(cor_AG, x = "a_9", y = "b_9",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b")  +  xlim(-13, -2) + ylim(5, 25)



b_L_9 <- ggscatter(cor_AG, x = "b_9", y = "L_9",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_AG$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(5, 25) + ylim(55, 90)




######Day 11
L_a_11 <- ggscatter(cor_AG, x = "L_11", y = "a_11",    #color = "concentration", palette = "jco",
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(55, 90) + ylim(-13, -2)



a_b_11 <- ggscatter(cor_AG, x = "a_11", y = "b_11",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b")  +  xlim(-13, -2) + ylim(5, 25)


b_L_11 <- ggscatter(cor_AG, x = "b_11", y = "L_11",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_AG$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(5, 25) + ylim(55, 90)





######Day 14
L_a_14 <- ggscatter(cor_AG, x = "L_14", y = "a_14",    #color = "concentration", palette = "jco",
                    add = "reg.line",  # Add regressin line
                    add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                    conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(55, 90) + ylim(-13, -2)



a_b_14 <- ggscatter(cor_AG, x = "a_14", y = "b_14",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b")  +  xlim(-13, -2) + ylim(5, 25)



b_L_14 <- ggscatter(cor_AG, x = "b_14", y = "L_14",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_AG$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(5, 25) + ylim(55, 90)



grid.arrange(L_a_1, L_a_6, L_a_9, L_a_11, L_a_14, 
             a_b_1, a_b_6, a_b_9, a_b_11, a_b_14,
             b_L_1, b_L_6, b_L_9, b_L_11, b_L_14, ncol = 5)







