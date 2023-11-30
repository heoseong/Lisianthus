
###install.packages("tidyverse")
###install.packages("h2o")
library(tidyverse)
library(h2o)

### this is the data with Lab and SPAD
df = read.csv("Lab_new_longer.csv") 
head(df)
glimpse(df)
### encode factor
df$day = as.factor(df$day)
df$cult = as.factor(df$cult)
df$group = as.factor(df$group)
df$concentration = as.factor(df$concentration)
### keep necessary columns only
df = df[ , c("day", "cult", "concentration", "a", "b", "L", "C", "h") ]


# Split the data frame into Train and Test dataset
## 75% of the sample size
smp_size <- floor(0.75 * nrow(df))

## set the seed to make your partition reproducible
set.seed(5)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train_df <- df[train_ind, ]
test_df <- df[-train_ind, ]

# initialize the h2o
h2o.init() 


train_df_h2o<-as.h2o(train_df)
test_df_h2o<-as.h2o(test_df)
# Identify predictors and response
y <- "day"
x <- setdiff(names(train_df_h2o), y)
# Number of CV folds (to generate level-one data for stacking)
nfolds <- 5
# 1. Generate a 3-model ensemble (GBM + RF + Logistic)
# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train_df_h2o,
                  nfolds = nfolds,
                  keep_cross_validation_predictions = TRUE,
                  seed = 5)
# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train_df_h2o,
                          nfolds = nfolds,
                          keep_cross_validation_predictions = TRUE,
                          seed = 5)
# Train & Cross-validate a LR
my_lr <- h2o.glm(x = x,
                 y = y,
                 training_frame = train_df_h2o,
                 #family = c("binomial"),
                 family = c("multinomial"),
                 nfolds = nfolds,
                 keep_cross_validation_predictions = TRUE,
                 seed = 5)
# Train a stacked random forest ensemble using the GBM, RF and LR above
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                metalearner_algorithm="drf",
                                training_frame = train_df_h2o,
                                base_models = list(my_gbm, my_rf, my_lr))
# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = test_df_h2o)
# Compare to base learner performance on the test set
perf_gbm_test <- h2o.performance(my_gbm, newdata = test_df_h2o)
perf_rf_test <- h2o.performance(my_rf, newdata = test_df_h2o)
perf_lr_test <- h2o.performance(my_lr, newdata = test_df_h2o)
baselearner_best_auc_test <- max(h2o.auc(perf_gbm_test), h2o.auc(perf_rf_test), h2o.auc(perf_lr_test))
ensemble_auc_test <- h2o.auc(perf)
print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))
