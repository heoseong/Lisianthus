cor_BP <- correlation |> 
  filter(cultivar == "BP")
head(cor_BP)
str(cor_BP)
View(cor_BP)
summary(cor_BP)
cor_BP[125, 12] <- 14.95
summary(cor_BP)
# Scatter plot with correlation coefficient
#:::::::::::::::::::::::::::::::::::::::::::::::::
L_a_1 <- ggscatter(cor_BP, x = "L_1", y = "a_1",    #color = "concentration", palette = "jco",
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson", label.y = min(cor_BP$a_1)) + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(40, 90) + ylim(-5, 25)


a_b_1 <- ggscatter(cor_BP, x = "a_1", y = "b_1",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b") + xlim(-5, 25) + ylim(-30, 25)


b_L_1 <- ggscatter(cor_BP, x = "b_1", y = "L_1",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(-30, 25) + ylim(40, 90)




#######Day 6
L_a_6 <- ggscatter(cor_BP, x = "L_6", y = "a_6",    #color = "concentration", palette = "jco",
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson", label.y = min(cor_BP$a_1)) + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(40, 90) + ylim(-5, 25)




a_b_6 <- ggscatter(cor_BP, x = "a_6", y = "b_6",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b")  + xlim(-5, 25) + ylim(-30, 25)




b_L_6 <- ggscatter(cor_BP, x = "b_6", y = "L_6",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(-30, 25) + ylim(40, 90)






######Day 9
L_a_9 <- ggscatter(cor_BP, x = "L_9", y = "a_9",    #color = "concentration", palette = "jco",
                   add = "reg.line",  # Add regressin line
                   add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                   conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson", label.y = min(cor_BP$a_1)) + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(40, 90) + ylim(-5, 25)




a_b_9 <- ggscatter(cor_BP, x = "a_9", y = "b_9",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b")  + xlim(-5, 25) + ylim(-30, 25)




b_L_9 <- ggscatter(cor_BP, x = "b_9", y = "L_9",
                   add = "reg.line",
                   add.params = list(color = "blue", fill = "lightgray"),
                   conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(-30, 25) + ylim(40, 90)





######Day 11
L_a_11 <- ggscatter(cor_BP, x = "L_11", y = "a_11",    #color = "concentration", palette = "jco",
                    add = "reg.line",  # Add regressin line
                    add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                    conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson", label.y = min(cor_BP$a_1)) + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(40, 90) + ylim(-5, 25)




a_b_11 <- ggscatter(cor_BP, x = "a_11", y = "b_11",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b")  + xlim(-5, 25) + ylim(-30, 25)



b_L_11 <- ggscatter(cor_BP, x = "b_11", y = "L_11",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(-30, 25) + ylim(40, 90)






######Day 14
L_a_14 <- ggscatter(cor_BP, x = "L_14", y = "a_14",    #color = "concentration", palette = "jco",
                    add = "reg.line",  # Add regressin line
                    add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                    conf.int = TRUE # Add confidence interval
) + stat_cor(method = "pearson", label.y = min(cor_BP$a_1)) + # label.x = 3, label.y = 3
  labs(x = "L", y = "a") + xlim(40, 90) + ylim(-5, 25)



a_b_14 <- ggscatter(cor_BP, x = "a_14", y = "b_14",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "a", y = "b")  + xlim(-5, 25) + ylim(-30, 25)




b_L_14 <- ggscatter(cor_BP, x = "b_14", y = "L_14",
                    add = "reg.line",
                    add.params = list(color = "blue", fill = "lightgray"),
                    conf.int = TRUE
) + stat_cor(method = "pearson") + # label.x = 3, label.y = 3
  labs(x = "b", y = "L") + xlim(-30, 25) + ylim(40, 90)




grid.arrange(L_a_1, L_a_6, L_a_9, L_a_11, L_a_14, 
             a_b_1, a_b_6, a_b_9, a_b_11, a_b_14,
             b_L_1, b_L_6, b_L_9, b_L_11, b_L_14, ncol = 5)

