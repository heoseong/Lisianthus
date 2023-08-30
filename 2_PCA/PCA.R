rm(list=ls())
library(readr)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsci)
planting <- read.csv("all_concat_modified.csv", header = TRUE)
str(planting)
head(planting)
planting$Group <- factor(planting$Group, levels = c("SSV", "SSR", "HSV", "HSR")) 
planting$Concentration <- as.numeric(planting$Concentration)
planting$Concentration <- format(planting$Concentration, nsmall = 1)
planting$Concentration <- factor(planting$Concentration, levels = c("0.0", "0.1", "0.3", "0.5"), order = T)
#planting$Cultivation_method <- factor(planting$Cultivation_method)
planting$Cultivar <- factor(planting$Cultivar)

pca_table <- planting |> select(-c(Group, Concentration, substrate, Cultivar, one_group, treat.No., samples, L, a, b))
head(pca_table)

library("FactoMineR")
library("factoextra")
pca_01 = PCA(pca_table, graph = TRUE) 

fviz_pca_biplot(pca_01, geom.ind = "point", col.ind = planting$Group, fill.ind = planting$Group,
                palette="Paired", habillage=planting$Group,
                addEllipses=TRUE, label="var", repel=TRUE, legend.title="Group", col.var = "black"
) +
  theme_bw() + theme(legend.position = c(0.1, 0.85), legend.background = element_rect(fill='transparent'))
#  labs(fill="treatment", color="Contrib", alpha="Contrib")


fviz_pca_biplot(pca_01, geom.ind = "point", col.ind = planting$Concentration, fill.ind = planting$Concentration,
                palette="Paired", habillage=planting$Concentration,
                addEllipses=TRUE, label="var", repel=TRUE, legend.title="Concentration", col.var = "black"
) +
  theme_bw() + theme(legend.position = c(0.15, 0.85), legend.background = element_rect(fill='transparent')) +
  scale_color_brewer(palette = "Set1")


fviz_pca_biplot(pca_01, geom.ind = "point", col.ind = planting$Cultivation_method, fill.ind = planting$Cultivation_method,
                palette="Paired", habillage=planting$Cultivation_method,
                addEllipses=TRUE, label="var", repel=TRUE, legend.title="Cultivation method", col.var = "black"
) +
  theme_bw() + theme(legend.position = c(0.19, 0.85), legend.background = element_rect(fill='transparent')) +
  scale_color_brewer(palette = "Paired")


fviz_pca_biplot(pca_01, geom.ind = "point", col.ind = planting$Cultivar, fill.ind = planting$Cultivar,
                palette="Paired", habillage=planting$Cultivar,
                addEllipses=TRUE, label="var", repel=TRUE, legend.title="Cultivar", col.var = "black"
) +
  theme_bw() + theme(legend.position = c(0.1, 0.85), legend.background = element_rect(fill='transparent')) +
  scale_color_brewer(palette = "Dark2")













