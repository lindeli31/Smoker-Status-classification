rm(list=ls())

data <- read.csv('train_dataset.csv')
library(tidyverse)
library(dplyr)
data <- as.tibble(data)
glimpse(data)


# train e test ------------------------------------------------------------

library(dplyr)
target <- "smoking"
data[[target]] <- factor(data[[target]], levels = c(1, 0), labels = c("yes", "no"))

set.seed(123) # Per riproducibilità
data <- data %>%
  mutate(split = ifelse(runif(n()) < 0.7, "train", "test"))

train <- data %>% filter(split == "train")
test <- data %>% filter(split == "test")

train <- train %>% select(-split)
test <- test %>% select(-split)



#per le variabili LDL,trigliceride,AST,ALT sembrano esserci
#dei valori estremi, differenti da tutti gli altri del dataset,
#vale la pena vedere se eliminandoli cambiano le stime
#per i metodi proposti

library(caret)
library(rpart)


target <- 'smoking'

train[[target]] <- as.factor(train[[target]])
X <- train[, setdiff(names(train), target)]  
# Converti X in data.frame (non tibble) per evitare warning di row names in caret
X <- as.data.frame(X)
y <- factor(train[[target]], levels = c("yes", "no"))
target <- "smoking"
cp_values <- seq(0.0001, 0.005, by = 0.00005)
cp_grid   <- expand.grid(cp = cp_values)
set.seed(123)
# for Sens & Spec (needs classProbs + twoClassSummary)
cv_ctrl_sum <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)
# for Accuracy (defaultSummary)
cv_ctrl_acc <- trainControl(
  method          = "cv",
  number          = 5,
  savePredictions = "final"
)

# Tune for Sensitivity --------------------------------------------------
cv_sens <- train(
  x         = X, y         = y,
  method    = "rpart",
  metric    = "Sens",
  trControl = cv_ctrl_sum,
  tuneGrid  = cp_grid
)

# Tune for Specificity --------------------------------------------------
cv_spec <- train(
  x         = X, y         = y,
  method    = "rpart",
  metric    = "Spec",
  trControl = cv_ctrl_sum,
  tuneGrid  = cp_grid
)

# Tune for Accuracy -----------------------------------------------------
cv_acc <- train(
  x         = X, y         = y,
  method    = "rpart",
  metric    = "Accuracy",
  trControl = cv_ctrl_acc,
  tuneGrid  = cp_grid
)

# Merge results ---------------------------------------------------------
res <- merge(cv_sens$results[, c("cp","Sens")],
             cv_spec$results[, c("cp","Spec")],
             by = "cp")
res <- merge(res,
             cv_acc$results[, c("cp","Accuracy")],
             by = "cp")

# Plot all three metrics with explicit colors -------------------------------
cols <- c("red", "blue", "darkgreen")  # pick three distinct colors

matplot(res$cp,
        res[, c("Sens","Spec","Accuracy")],
        type = "l",        # lines
        lty  = 1,          # solid lines
        col  = cols,       # set the colors
        xlab = "cp",
        ylab = "Metric value",)

legend("bottomleft",
       legend = c("Sensitivity", "Specificity", "Accuracy"),
       col    = cols,    # use the same colors
       lty    = 1,
       bty    = "n")
best_cp <- cv_sens$bestTune$cp
abline(v = best_cp, lty = 2, lwd = 2)

# Final model by Sensitivity --------------------------------------------
best_cp <- cv_sens$bestTune$cp
cat("Best cp by Sensitivity:", best_cp, "\n")

final_model <- rpart(
  smoking ~ .,
  data    = train,
  control = rpart.control(cp = best_cp)
)

# Test‐set evaluation ---------------------------------------------------
X_test <- test[, setdiff(names(test), target)]
preds  <- predict(final_model, newdata = X_test, type = "class")
cm     <- confusionMatrix(preds, test[[target]], positive = "yes")
print(cm)




library(pROC)

# Ottenere le probabilità predette
probs <- predict(final_model, newdata = test, type = "prob")

# Calcolare la curva ROC
roc_obj <- roc(response = test[[target]], predictor = probs[,"yes"], levels = c("no", "yes"), direction = "<")

# Tracciare la curva ROC
plot(roc_obj, col = "blue", lwd = 2, main = "Curva ROC")

# Calcolare l'AUC
auc_value <- auc(roc_obj)
print(auc_value)

library(randomForest)


set.seed(123)  # Per garantire la riproducibilità
rf_model <- randomForest(
  smoking ~ .,
  data = train,
  ntree = 1000,
  importance = TRUE
)
rf_probs <- predict(rf_model, newdata = test, type = "prob")[, "yes"]
rf_preds <- ifelse(rf_probs >= 0.5, "yes", "no")
rf_preds <- factor(rf_preds, levels = c("yes", "no"))
conf_matrix <- confusionMatrix(rf_preds, test$smoking, positive = "yes")
print(conf_matrix)

#in confronto all'albero normale:
confusionMatrix(preds, test[[target]])




#migliorato decisamente, anche per quanto riguarda la sensibilità che è quella 
#che ci interessa di più(fumatori identificati/fumatori)

roc_obj <- roc(response = test$smoking, predictor = rf_probs, levels = c("yes", "no"))
plot(roc_obj, col = "blue", lwd = 2, main = "Curva ROC - Random Forest")
auc_value <- auc(roc_obj)
cat("AUC:", auc_value, "\n")

