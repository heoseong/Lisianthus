cor_KW <- correlation |> 
  filter(cultivar == "KW")
head(cor_KW)
str(cor_KW)
View(cor_KW)
summary(cor_KW)

# Scatter plot with correlation coefficient
#:::::::::::::::::::::::::::::::::::::::::::::::::
L_a_1 <- ggscatter(cor_KW, x = "L_1", y = "a_1",    #color = "concentration", palette = "jco",
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(55, 95) + ylim(-6, 4)


a_b_1 <- ggscatter(cor_KW, x = "a_1", y = "b_1",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b") + xlim(-6, 4) + ylim(0, 25)


b_L_1 <- ggscatter(cor_KW, x = "b_1", y = "L_1",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_KW$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(0, 25) + ylim(55, 95)




#######Day 6
L_a_6 <- ggscatter(cor_KW, x = "L_6", y = "a_6",    #color = "concentration", palette = "jco",
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(55, 95) + ylim(-6, 4)




a_b_6 <- ggscatter(cor_KW, x = "a_6", y = "b_6",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b") + xlim(-6, 4) + ylim(0, 25)




b_L_6 <- ggscatter(cor_KW, x = "b_6", y = "L_6",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_KW$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(0, 25) + ylim(55, 95)






######Day 9
L_a_9 <- ggscatter(cor_KW, x = "L_9", y = "a_9",    #color = "concentration", palette = "jco",
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(55, 95) + ylim(-6, 4)




a_b_9 <- ggscatter(cor_KW, x = "a_9", y = "b_9",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b")  + xlim(-6, 4) + ylim(0, 25)




b_L_9 <- ggscatter(cor_KW, x = "b_9", y = "L_9",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_KW$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(0, 25) + ylim(55, 95)





######Day 11
L_a_11 <- ggscatter(cor_KW, x = "L_11", y = "a_11",    #color = "concentration", palette = "jco",
                    add = "reg.line",  # Add regressin line
                    add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                    conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(55, 95) + ylim(-6, 4)




a_b_11 <- ggscatter(cor_KW, x = "a_11", y = "b_11",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b")  + xlim(-6, 4) + ylim(0, 25)



b_L_11 <- ggscatter(cor_KW, x = "b_11", y = "L_11",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_KW$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(0, 25) + ylim(55, 95)






######Day 14
L_a_14 <- ggscatter(cor_KW, x = "L_14", y = "a_14",    #color = "concentration", palette = "jco",
                    add = "reg.line",  # Add regressin line
                    add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                    conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(55, 95) + ylim(-6, 4)



a_b_14 <- ggscatter(cor_KW, x = "a_14", y = "b_14",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b")  + xlim(-6, 4) + ylim(0, 25)




b_L_14 <- ggscatter(cor_KW, x = "b_14", y = "L_14",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE
) + stat_cor(method = "pearson", label.y = min(cor_KW$L_1)*1.1) + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(0, 25) + ylim(55, 95)




grid.arrange(L_a_1, L_a_6, L_a_9, L_a_11, L_a_14, 
             a_b_1, a_b_6, a_b_9, a_b_11, a_b_14,
             b_L_1, b_L_6, b_L_9, b_L_11, b_L_14, ncol = 5)

