
rm(list=ls())

#----------------------------------------------------------
# Importing libraries 
#----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(MASS)
library(pROC)
library(caret)
library(tidymodels)

#----------------------------------------------------------
# selection of predictors
#----------------------------------------------------------
data <- read.csv("train_dataset.csv")
#eliminating variables that inherently violate normality
data<- data %>% dplyr::select(-c(hearing.left., hearing.right., 
                               dental.caries, Urine.protein, eyesight.left., 
                               eyesight.right.))

#factorizing the response variable
data <- data %>% mutate(smoking=as.factor(smoking))

# Summary of datasets
# data: original data 
# train_org: original training set (unstandardized) from split on data
# train_org_std: standardized original training set 
# test_org: original test set (unstandardized) from split on data
# test_org_std: standardized original test set
# train: training set derived from splitting train_org into (train, validation)
# validation: validation set derived from splitting train_org into (train, validation)

# X_train, X_validation (each with 3 versions - see below): continuous variables only
#                                used for model training
# X_train_clean (also with 3 versions): continuous variables with some outliers removed,
#                          not ultimately used due to delivery time constraints
# train_def: final training set with standardized variables and 
#            selected features for final model training

# test_def: final test set with standardized variables and final selected features
#            for model evaluation


#----------------------------------------------------------
# Definition of target variable and continuous variables  
#----------------------------------------------------------

target = "smoking"
continuous_var<- data %>% dplyr::select(-all_of(target)) %>% names()


#----------------------------------------------------------
# Split data 
#----------------------------------------------------------

set.seed(123) 
data <- data %>%
  mutate(split = ifelse(runif(n()) < 0.7, "train", "test"))

train_org <- data %>% filter(split == "train") %>% dplyr::select(-c(split))
train_org_std<- train_org %>%  mutate(across(all_of(continuous_var),~scale(.)[,1]))
#split on the original train to get the validation set 
set.seed(123)
train<- training(initial_split(train_org))
validation<- testing(initial_split(train_org))


test_org <- data %>% filter(split=="test") %>% dplyr::select(-c(split))

# We standardized the training, test, and validation sets for implementing LDA and QDA to avoid the influence of variables' 
# natural scales on discrimination. Standardization yields interpretable discriminant coefficients.
train_std<- train %>% mutate(across(all_of(continuous_var), ~scale(.)[,1]))
validation_std<- validation %>% mutate(across(all_of(continuous_var),~scale(.)[,1]))
test_org_std<- test_org %>% mutate( across(all_of(continuous_var),~scale(.)[,1]))


#----------------------------------------------------------------------
# DISCRIMINANT ANALYSIS 
#----------------------------------------------------------------------

# -------------------------------------------------------------------------
# Three versions of datasets 
# -------------------------------------------------------------------------
#X_train: the initial dataset with variables that inherently violate normality removed.  
#X_train2: for each group of highly correlated variables, one variable was removed.  
#X_train3: by examining the model coefficients (model$scaling), we removed variables
#         with the smallest coefficients (which therefore have less impact).
X_train<- train_std
X_validation<-validation_std

X_train2 <- X_train %>% dplyr::select(-c("ALT", "relaxation", "waist.cm.", "Cholesterol"))
X_validation2 <- X_validation %>% dplyr::select(-c("ALT", "relaxation", "waist.cm.", "Cholesterol"))

X_train3<- X_train2 %>% dplyr::select(-c( 
  "age", "systolic", "serum.creatinine", 
  "AST", "LDL", "HDL", "fasting.blood.sugar"))
X_validation3<- X_validation2 %>% dplyr::select(-c( 
  "age", "systolic", "serum.creatinine",
  "AST", "LDL", "HDL", "fasting.blood.sugar"
))
#----------------------------------------------------------
# ---------------------------------------------------------
# LDA
# ---------------------------------------------------------
#----------------------------------------------------------

# ---------------------------------------------------------
# 1 Metric Function lDA (train, valid, nome)
# ---------------------------------------------------------
fit_lda <- function(train_df, valid_df, label) {
  
  mod  <- lda(smoking ~ ., data = train_df)
  pred <- predict(mod, valid_df)
  
  cm   <- caret::confusionMatrix(pred$class, valid_df$smoking, positive = "1")
  roc_ <- roc(valid_df$smoking, pred$posterior[, 2])
  
  list(model = mod,
       metrics = data.frame(model = label,
                            sens  = round(cm$byClass["Sensitivity"], 3),
                            spec  = round(cm$byClass["Specificity"], 3),
                            auc   = round(auc(roc_), 4)))
}

# ----------------------------------------------------------
# 2.  Fit LDA
# ----------------------------------------------------------
res1_lda <- fit_lda(X_train, X_validation, "LDA_full")
res2_lda <- fit_lda(X_train2, X_validation2, "LDA_drop4")
res3_lda <- fit_lda(X_train3, X_validation3, "LDA_drop11")
#i modelli sono 
lda1 <- res1_lda$model
lda2<- res2_lda$model
lda3 <- res3_lda$model
lda_summary <- bind_rows(res1_lda$metrics,
                         res2_lda$metrics,
                         res3_lda$metrics) %>%
  arrange(desc(sens))

print(lda_summary)

# -------------------------------------------------------------------------
# 3.  ROC plot comparison
# -------------------------------------------------------------------------
plot(roc(X_validation$smoking,
         predict(lda1, X_validation)$posterior[, 2]),
     col = "red", legacy.axes = TRUE,
     main = "ROC – LDA varianti", print.auc = FALSE)

lines(roc(X_validation2$smoking,
          predict(lda2, X_validation2)$posterior[, 2]),
      col = "blue")

lines(roc(X_validation3$smoking,
          predict(lda3, X_validation3)$posterior[, 2]),
      col = "darkgreen")

legend("bottomright",
       legend = c("full", "drop4", "drop11"),
       col = c("red", "blue", "darkgreen"), lwd = 2)


#COMMENT: 
#As our goal is sensitibity we might choose the model with all predictors

#-----------------------------------------------------------------
#-----------------------------------------------------------------
# QDA
#-----------------------------------------------------------------
#-----------------------------------------------------------------


# ----------------------------------------------------------------
# 1.  Metric Function QDA
# ----------------------------------------------------------------
fit_qda <- function(train_df, valid_df, label = "model") {
  
  mod <- qda(smoking ~ ., data = train_df)
  
  pred <- predict(mod, valid_df)
  
  # Confusion matrix con soglia standard 0.5
  cm <- caret::confusionMatrix(pred$class, valid_df$smoking,
                               positive = "1")
  
  sens   <- cm$byClass["Sensitivity"]
  spec   <- cm$byClass["Specificity"]
  
  rocobj <- roc(valid_df$smoking, pred$posterior[, 2])
  auc    <- auc(rocobj)
  
  list(model = mod,
       metrics = data.frame(model = label,
                            sens  = round(sens, 3),
                            spec  = round(spec, 3),
                            auc   = round(auc, 3)))
}


# -------------------------------------------------------------
# 2  Fit e Risults
# -------------------------------------------------------------
res1_qda <- fit_qda(X_train, X_train, "QDA_full")
res2_qda <- fit_qda(X_train2, X_train2, "QDA_drop4")
res3_qda <- fit_qda(X_train3, X_train3, "QDA_drop11")
#i modelli sono
qda1 <-res1_qda$model
qda2 <- res2_qda$model
qda3 <- res3_qda$model
qda_summary <- bind_rows(res1_qda$metrics,
                         res2_qda$metrics,
                         res3_qda$metrics) %>%
  arrange(desc(sens))

print(qda_summary)

# --------------------------------------------------------
# 3.  ROC plot comparison
# --------------------------------------------------------
plot(roc(X_validation$smoking, predict(qda1, X_validation)$posterior[,2]),
     col = "red",  legacy.axes = TRUE,
     main = "ROC – QDA varianti", print.auc = FALSE)
lines(roc(X_validation2$smoking, predict(qda2, X_validation2)$posterior[,2]),
      col = "blue")
lines(roc(X_validation3$smoking, predict(qda3, X_validation3)$posterior[,2]),
      col = "darkgreen")
legend("bottomright", legend = c("full", "drop4", "drop11"),
       col = c("red","blue","darkgreen"), lwd = 2)


#-----------------------------------------------------------
# MATRICI DI CONFUSIONE A CONFRONTO
#-----------------------------------------------------------


qda_summary
lda_summary
#qda1 vs lda1
caret::confusionMatrix( predict(qda1, X_validation)$class, X_validation$smoking,
                        positive = "1")$table

caret::confusionMatrix(predict(lda1, X_validation)$class, X_validation$smoking, 
                       positive = "1")$table

#We observe that QDA penalize more the group with higher variance
#which in our case is the class of smokers
-log(det(cov(X_train %>% filter(smoking == "1") %>% dplyr::select(-c(smoking)))))
-log(det(cov(X_train %>% filter(smoking == "0") %>% dplyr::select(-c(smoking)))))



# -------------------------------------------------------------------------
lda_summary
qda_summary



caret::confusionMatrix( predict(qda1, X_validation)$class, X_validation$smoking,
                        positive = "1")$table
caret::confusionMatrix(predict(lda1, X_validation)$class, X_validation$smoking, 
                       positive = "1")$table

lda1$scaling
lda2$scaling
lda3$scaling

caret::confusionMatrix(predict(lda1, X_validation)$class, X_validation$smoking, 
                       positive = "1")$table

caret::confusionMatrix(predict(qda3, X_validation3)$class, X_validation3$smoking, 
                       positive = "1")$table
# -------------------------------------------------------------------------
# Final Fit on the original training set  with alla predictors
# -------------------------------------------------------------------------

train_def<- train_org_std 
test_def<- test_org_std 
lda_def  <- lda(smoking~., data = train_def)
pred_def<- predict(lda_def, test_def)
confusionMatrix(pred_def$class, test_def$smoking, positive = "1")

