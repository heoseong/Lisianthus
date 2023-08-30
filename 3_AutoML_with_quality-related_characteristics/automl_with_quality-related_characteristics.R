rm(list=ls())

# # The following two commands remove any previously installed H2O packages for R.
# if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
# if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
# 
# # Next, we download packages that H2O depends on.
# pkgs <- c("RCurl","jsonlite")
# for (pkg in pkgs) {
#   if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
# }
# 
# # Now we download, install and initialize the H2O package for R.
# install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))
# 
# # Finally, let's load H2O and start up an H2O cluster
# library(h2o)
# h2o.init()


library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidymodels) 
library(h2o) 
library(knitr)
library(tidyr)
library(curl)
library(lares)
h2o.init()
lisianthus <- h2o.importFile(path = "all_united_20230413.csv")
h2o.dim(lisianthus)
h2o.ls()
h2o.describe(lisianthus)
h2o.hist(lisianthus$SPAD)


#Partition training, validation and testing splits
lisianthus_split <- h2o.splitFrame(data = lisianthus, ratios = 0.8, seed=1)
lisianthus_train <- lisianthus_split[[1]]
#lisianthus_validate <- lisianthus_split[[2]]
lisianthus_test <- lisianthus_split[[2]]
dim(lisianthus_train)

#Specify feature and target variables
target <- "Vase_life"
features <- setdiff(names(lisianthus_train), target)

automl_algorithms <- c(
  "GLM",
  "GBM",
  "DRF", 
  "XGBoost", 
  "DeepLearning",
  "StackedEnsemble"
)

lisianthus_automl <- h2o.automl(
  y = target,
  x = features,
  training_frame = lisianthus_train,

  nfolds = 5,
  include_algos = automl_algorithms,
  project_name = "lisianthus_vase_life",
  max_runtime_secs = 120,
  seed = 20230322
)

exa <- h2o.explain(lisianthus_automl, lisianthus_test)
exa


#Evaluate the leaderboard
lisianthus_automl_leaderboard <- h2o.get_leaderboard(object=lisianthus_automl)
head(lisianthus_automl_leaderboard, 10)
#leaderboard_tbl %>% head() %>% kable()

#Select the most accurate model
lisianthus_automl_best_model <- lisianthus_automl@leader
model_names <- lisianthus_automl@leaderboard$model_id
model_names[1]
top_model <- h2o.getModel("StackedEnsemble_BestOfFamily_4_AutoML_1_20230725_175500")

# top_model@model$model_summary %>% 
#   pivot_longer(cols = everything(),
#                names_to = "Parameter", values_to = "Value") %>% 
#   kable(align = 'c')

#Report model parameters
lisianthus_automl_best_model@model$model_summary

#Evaluate accuracy
report_regression_metrics <- function(metrics){
  metrics <- list(
    RMSE = h2o.rmse(metrics), 
    MAE = h2o.mae(metrics), 
    R2 = h2o.r2(metrics)
  )
  return(metrics)
}



#Evaluate accuracy on validation split
lisianthus_automl_validation_metrics <- h2o.performance(
  model = lisianthus_automl_best_model,
  valid = T
)

report_regression_metrics(
  metrics = lisianthus_automl_validation_metrics
)

#Evaluate accuracy using cross validation
lisianthus_automl_cv_metrics <- h2o.performance(
  model = lisianthus_automl_best_model,
  xval = T
)

report_regression_metrics(
  metrics = lisianthus_automl_cv_metrics
)

#Evaluate accuracy using testing splits
lisianthus_automl_test_metrics <- h2o.performance(
  model = lisianthus_automl_best_model,
  newdata = lisianthus_test
)

report_regression_metrics(
  metrics = lisianthus_automl_test_metrics
)


lisianthus_automl_train_metrics <- h2o.performance(
  model = lisianthus_automl_best_model,
  newdata = lisianthus_train
)

report_regression_metrics(
  metrics = lisianthus_automl_train_metrics
)



#Visualize predictions using residual analysis plot
h2o.residual_analysis_plot(
  model = lisianthus_automl_best_model,
  newdata = lisianthus_test
)


#Obtain predicted values
lisianthus_automl_test_predictions <- h2o.predict(
  object = lisianthus_automl_best_model,
  newdata = lisianthus_test
)

head(lisianthus_automl_test_predictions, 10)

mplot_lineal(tag = )

#Evaluate important features, logistic regression model
h2o.varimp_plot(
  model = lisianthus_automl_best_model,
  num_of_features = 10
)


h2o_predictions <- h2o.predict(top_model, newdata = lisianthus_test) %>%
  as_tibble() %>% bind_cols(as_tibble(lisianthus_test))


h2o_cf <- h2o_predictions %>% 
  count(Vase_life, pred= predict) %>% 
  mutate(label = "h2o", .before = 1)

kable(h2o_cf)


test_precict <- cbind(as.data.frame(lisianthus_test$Vase_life),
      as.data.frame(h2o_predictions$predict))

test_precict
str(test_precict)
library(ggplot2)

ggplot(data=real_frame, aes(x=Vase_life, y=h2o_predictions$predict)) +
  geom_point(data=real_test, color=real_test$Cultivar) +
  geom_smooth(method=lm, color="red", size= 0.5, fill="#69b3a2", se=TRUE) +
  theme_bw()


###그래프 변경 수정
real_frame <- as_tibble(test_precict)
head(real_frame)
real_test <- as_tibble(lisianthus_test)
Cultivar <- real_test$Cultivar
real_frame <- cbind(real_frame, Cultivar)
  
head(real_frame)
tail(real_frame)
tail(real_test$Vase_life)



ggplot(data=real_frame, aes(x=Vase_life, y=h2o_predictions$predict, color=Cultivar, fill = Cultivar)) +
  geom_abline(slope = 1, colour = "grey", lty = "dashed", size=0.5) +
  geom_point(size=5, color = "gray", shape = 21) +
  theme_bw() +
  labs(x="True values (day)", y="Predicted values (day)") +
  theme(legend.position = c(0.1, 0.85),
        legend.background = element_rect(fill='transparent'),
        legend.key.size = unit(0.5, 'cm')) +
  annotate(geom="text", x=12, y=7, label = "StackedEnsemble_AllModels_3_AutoML_1", hjust=0) +
  annotate(geom="text", x=12, y=6.5, label = "RMSE: 2.3309", hjust=0) +
  annotate(geom="text", x=12, y=6, label = "MAE: 1.7683", hjust=0) +  
  annotate(geom="text", x=12, y=5.5, label = expression(R^2: ~ 0.6367), hjust=0)


h2o.shutdown(prompt=FALSE)