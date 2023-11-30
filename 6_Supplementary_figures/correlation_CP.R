cor_CP <- correlation |> 
  filter(cultivar == "CP")
head(cor_CP)
str(cor_CP)
View(cor_CP)
summary(cor_CP)

# Scatter plot with correlation coefficient
#:::::::::::::::::::::::::::::::::::::::::::::::::
L_a_1 <- ggscatter(cor_CP, x = "L_1", y = "a_1",    #color = "concentration", palette = "jco",
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(45, 85) + ylim(0, 20)


a_b_1 <- ggscatter(cor_CP, x = "a_1", y = "b_1",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b") + xlim(0, 20) + ylim(-5, 30)


b_L_1 <- ggscatter(cor_CP, x = "b_1", y = "L_1",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_CP$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(-5, 30) + ylim(45, 85)




#######Day 6
L_a_6 <- ggscatter(cor_CP, x = "L_6", y = "a_6",    #color = "concentration", palette = "jco",
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(45, 85) + ylim(0, 20)




a_b_6 <- ggscatter(cor_CP, x = "a_6", y = "b_6",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b")  + xlim(0, 20) + ylim(-5, 30)




b_L_6 <- ggscatter(cor_CP, x = "b_6", y = "L_6",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_CP$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(-5, 30) + ylim(45, 85)






######Day 9
L_a_9 <- ggscatter(cor_CP, x = "L_9", y = "a_9",    #color = "concentration", palette = "jco",
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(45, 85) + ylim(0, 20)




a_b_9 <- ggscatter(cor_CP, x = "a_9", y = "b_9",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b")  + xlim(0, 20) + ylim(-5, 30)




b_L_9 <- ggscatter(cor_CP, x = "b_9", y = "L_9",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_CP$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(-5, 30) + ylim(45, 85)





######Day 11
L_a_11 <- ggscatter(cor_CP, x = "L_11", y = "a_11",    #color = "concentration", palette = "jco",
                    add = "reg.line",  # Add regressin line
                    add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                    conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(45, 85) + ylim(0, 20)




a_b_11 <- ggscatter(cor_CP, x = "a_11", y = "b_11",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b") + xlim(0, 20) + ylim(-5, 30)



b_L_11 <- ggscatter(cor_CP, x = "b_11", y = "L_11",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_CP$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(-5, 30) + ylim(45, 85)






######Day 14
L_a_14 <- ggscatter(cor_CP, x = "L_14", y = "a_14",    #color = "concentration", palette = "jco",
                    add = "reg.line",  # Add regressin line
                    add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                    conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(45, 85) + ylim(0, 20)



a_b_14 <- ggscatter(cor_CP, x = "a_14", y = "b_14",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b") + xlim(0, 20) + ylim(-5, 30)




b_L_14 <- ggscatter(cor_CP, x = "b_14", y = "L_14",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_CP$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(-5, 30) + ylim(45, 85)




grid.arrange(L_a_1, L_a_6, L_a_9, L_a_11, L_a_14, 
             a_b_1, a_b_6, a_b_9, a_b_11, a_b_14,
             b_L_1, b_L_6, b_L_9, b_L_11, b_L_14, ncol = 5)

