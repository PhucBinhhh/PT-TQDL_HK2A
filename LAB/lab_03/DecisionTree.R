if(!require(tidyverse)) install.packages("tidyverse")
  library(tidyverse)

if(!require(rpart))install.packages("rpart")
  library(rpart)  
# Xây dựng Decision Tree (CART)
if(!require(rpart.plot))install.packages("rpart.plot")

  library(rpart.plot)  
# Visualize cây đẹp hơn
if(!require(caret))install.packages("caret")
  library(caret)


options(timeout = 600)
# Tuning + CV (nhất quán Bài 17)
if(!require(tidymodels))install.packages("tidymodels")
  library(tidymodels) 
# Modern workflow
if(!require(pROC))install.packages("pROC")
  library(pROC)

if(!require(vip))install.packages("vip")
  library(vip)



titanic <- read.csv("C:\\Users\\Admin\\Downloads\\titanic.csv", stringsAsFactors = TRUE)

titanic_clean <- titanic %>%
  
  select(survived, pclass, sex, age, sibsp, parch, fare) %>%
  mutate(survived = factor(survived, levels = c(0, 1),
                           labels = c("No", "Yes"))) %>%
  drop_na()



set.seed(42)
train_idx  <- createDataPartition(titanic_clean$survived, p = 0.8, list = FALSE)
tit_train  <- titanic_clean[train_idx, ]
tit_test   <- titanic_clean[-train_idx, ]

cat("Train:", nrow(tit_train), "| Test:", nrow(tit_test), "\n")
prop.table(table(tit_train$survived))

#Cây mặc định

set.seed(42)
tree_default <- rpart(
  survived ~ .,
  data   = tit_train,
  method = "class"    # "class" cho phân loại, "anova" cho hồi quy
)

# Visualize
rpart.plot(
  tree_default,
  type    = 4,         # Kiểu vẽ (0–5, type=4 hiển thị rõ nhất)
  extra   = 104,       # Hiển thị xác suất và % quan sát
  under   = TRUE,
  fallen.leaves = TRUE,
  main    = "Decision Tree — Titanic (Mặc định)"
)

# Xem cấu trúc dạng text
print(tree_default)

# Xem bảng cp (complexity parameter)
printcp(tree_default)
plotcp(tree_default)   # Vẽ cross-validation error theo cp


#Đánh giá mô hình


# Dự đoán nhãn
pred_class <- predict(tree_default, newdata = tit_test, type = "class")

# Dự đoán xác suất (để tính AUC)
pred_prob <- predict(tree_default, newdata = tit_test, type = "prob")[, "Yes"]

# Confusion matrix
cm <- confusionMatrix(pred_class, tit_test$survived, positive = "Yes")
print(cm)

# AUC
roc_tree <- roc(tit_test$survived, pred_prob,
                levels = c("No", "Yes"), quiet = TRUE)
cat("AUC (Decision Tree mặc định):", round(auc(roc_tree), 4), "\n")

#Ảnh hưởng của độ sâu cây (Depth)

# Huấn luyện cây ở nhiều độ sâu khác nhau
depths   <- 1:10
acc_train <- numeric(length(depths))
acc_test  <- numeric(length(depths))

for (i in seq_along(depths)) {
  tree_d <- rpart(survived ~ ., data = tit_train, method = "class",
                  control = rpart.control(maxdepth = depths[i], cp = 0))
  
  acc_train[i] <- mean(predict(tree_d, tit_train, type = "class") == tit_train$survived)
  acc_test[i]  <- mean(predict(tree_d, tit_test,  type = "class") == tit_test$survived)
}

# Visualize bias-variance tradeoff
data.frame(depth = depths, Train = acc_train, Test = acc_test) %>%
  pivot_longer(-depth, names_to = "Set", values_to = "Accuracy") %>%
  ggplot(aes(x = depth, y = Accuracy, color = Set, linetype = Set)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Train" = "steelblue", "Test" = "tomato")) +
  labs(title  = "Bias–Variance Tradeoff — Decision Tree (Titanic)",
       subtitle = "Train Accuracy vs Test Accuracy theo độ sâu cây",
       x = "Độ sâu cây (maxdepth)", y = "Accuracy") +
  theme_minimal()

#
#Depth tăng → Train Accuracy tăng liên tục (cây "học thuộc").
#Test Accuracy tăng rồi giảm — điểm giảm là dấu hiệu overfitting bắt đầu.
#Depth tối ưu là nơi Test Accuracy đạt cao nhất.
#
#
#




#Pruning với Cross-Validation — Chọn cp tối ưu 

#Cách 1: Dùng rpart trực tiếp


# Xây cây đầy đủ (cp = 0)
set.seed(42)
tree_full <- rpart(survived ~ ., data = tit_train, method = "class",
                   control = rpart.control(cp = 0, minsplit = 2))

# Bảng cp và CV error
cp_table <- as.data.frame(tree_full$cptable)
print(cp_table)

# Vẽ CV error theo cp
plotcp(tree_full, main = "Cross-Validation Error theo Complexity Parameter (cp)")

# Tìm cp tối ưu: cp có xerror nhỏ nhất
best_cp <- cp_table$CP[which.min(cp_table$xerror)]
cat("cp tối ưu:", round(best_cp, 6), "\n")

# Cắt tỉa cây
tree_pruned <- rpart::prune(tree_full, cp = best_cp)

rpart.plot(tree_pruned, type = 4, extra = 104, fallen.leaves = TRUE,
           main = paste("Cây sau Pruning (cp =", round(best_cp, 4), ")"))

# So sánh kết quả
pred_pruned <- predict(tree_pruned, tit_test, type = "class")
acc_full    <- mean(predict(tree_full,   tit_test, type="class") == tit_test$survived)
acc_pruned  <- mean(pred_pruned == tit_test$survived)

cat(sprintf("Accuracy — Cây đầy đủ:  %.4f\n", acc_full))
cat(sprintf("Accuracy — Sau pruning: %.4f\n", acc_pruned))


#Cách 2: Dùng caret (nhất quán Bài 17)

ctrl <- trainControl(
  method          = "cv",
  number          = 10,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Grid search cp
cp_grid <- expand.grid(cp = c(0, 0.001, 0.005, 0.01, 0.02, 0.05, 0.1))

set.seed(42)
tree_caret <- train(
  survived ~ .,
  data      = tit_train,
  method    = "rpart",
  trControl = ctrl,
  tuneGrid  = cp_grid,
  metric    = "ROC"
)

# Kết quả theo từng cp
print(tree_caret$results[, c("cp", "ROC", "Sens", "Spec")])
plot(tree_caret, main = "Chọn cp tối ưu qua 10-Fold CV (caret)")

cat("cp tối ưu (caret):", tree_caret$bestTune$cp, "\n")
cat("AUC tốt nhất:     ", round(max(tree_caret$results$ROC), 4), "\n")

#Cách 3: Dùng tidymodels


# Recipe
tit_recipe <- recipe(survived ~ ., data = tit_train) %>%
  step_dummy(all_nominal_predictors())

# Model spec với cost_complexity (= cp trong rpart)
tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth      = tune(),
  min_n           = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

# Workflow
tree_wf <- workflow() %>%
  add_recipe(tit_recipe) %>%
  add_model(tree_spec)

# Folds
set.seed(42)
tit_folds <- vfold_cv(tit_train, v = 10, strata = survived)

# Tune
tree_grid <- grid_regular(
  cost_complexity(range = c(-4, -1)),
  tree_depth(range = c(2, 10)),
  min_n(range = c(2, 20)),
  levels = 4
)

set.seed(42)
tree_tune <- tune_grid(
  tree_wf,
  resamples = tit_folds,
  grid      = tree_grid,
  metrics   = metric_set(roc_auc, accuracy)
)

# Best params
best_tree <- select_best(tree_tune, metric = "roc_auc")
cat("Tham số tốt nhất:\n")
print(best_tree)

# Visualize tuning
autoplot(tree_tune) +
  labs(title = "Tuning Decision Tree — Titanic (tidymodels)")

# Final fit
final_tree_fit <- last_fit(
  finalize_workflow(tree_wf, best_tree),
  split = initial_split(titanic_clean, prop = 0.8, strata = survived)
)

collect_metrics(final_tree_fit)


#Visualize cây tốt nhất và Feature Importance


# Vẽ cây sau khi pruning (dùng kết quả từ caret)
best_tree_model <- tree_caret$finalModel
rpart.plot(
  best_tree_model,
  type          = 4,
  extra         = 104,
  fallen.leaves = TRUE,
  box.palette   = "RdYlGn",
  shadow.col    = "gray",
  main          = "Decision Tree tối ưu — Titanic"
)

# Feature Importance
importance_df <- data.frame(
  variable   = names(best_tree_model$variable.importance),
  importance = best_tree_model$variable.importance
) %>%
  arrange(desc(importance))

ggplot(importance_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance — Decision Tree (Titanic)",
       x = NULL, y = "Importance (Gini Decrease)") +
  theme_minimal()



#Regression Tree — Wines Quality
#Dự đoán điểm chất lượng rượu (quality — biến liên tục).

wines <- read.csv("C:\\Users\\Admin\\Downloads\\wines.csv", stringsAsFactors = TRUE)

# Dùng các biến hóa học để dự đoán quality
wines_reg <- wines %>% select(-type)

set.seed(42)
w_split <- initial_split(wines_reg, prop = 0.8)
w_train <- training(w_split)

w_test  <- testing(w_split)

# Regression Tree (method = "anova")
tree_reg <- rpart(
  quality ~ .,
  data    = w_train,
  method  = "anova",
  control = rpart.control(cp = 0.01)
)

rpart.plot(
  tree_reg,
  type          = 4,
  extra         = 101,    # Hiển thị số quan sát tại mỗi nút
  fallen.leaves = TRUE,
  main          = "Regression Tree — Wine Quality"
)

# Đánh giá
pred_reg <- predict(tree_reg, newdata = w_test)

rmse_tree <- sqrt(mean((w_test$quality - pred_reg)^2))
mae_tree  <- mean(abs(w_test$quality - pred_reg))
r2_tree   <- 1 - sum((w_test$quality - pred_reg)^2) /
  sum((w_test$quality - mean(w_test$quality))^2)

cat(sprintf("RMSE: %.4f\n", rmse_tree))
cat(sprintf("MAE:  %.4f\n", mae_tree))
cat(sprintf("R²:   %.4f\n", r2_tree))

# So sánh với Linear Regression
lm_wine <- lm(quality ~ ., data = w_train)
pred_lm  <- predict(lm_wine, newdata = w_test)

rmse_lm <- sqrt(mean((w_test$quality - pred_lm)^2))
r2_lm   <- 1 - sum((w_test$quality - pred_lm)^2) /
  sum((w_test$quality - mean(w_test$quality))^2)

cat(sprintf("\nSo sánh:\n"))
cat(sprintf("  Regression Tree — RMSE: %.4f | R²: %.4f\n", rmse_tree, r2_tree))
cat(sprintf("  Linear Regression—RMSE: %.4f | R²: %.4f\n", rmse_lm,   r2_lm))
#Pruning cho Regression Tree:
  
  # Cây đầy đủ rồi tìm cp tối ưu
  set.seed(42)
tree_reg_full <- rpart(quality ~ ., data = w_train, method = "anova",
                       control = rpart.control(cp = 0))

plotcp(tree_reg_full, main = "CV Error theo cp — Wine Quality")

best_cp_reg <- tree_reg_full$cptable[which.min(tree_reg_full$cptable[,"xerror"]), "CP"]
tree_reg_pruned <- prune(tree_reg_full, cp = best_cp_reg)

pred_pruned_reg <- predict(tree_reg_pruned, newdata = w_test)
rmse_pruned     <- sqrt(mean((w_test$quality - pred_pruned_reg)^2))

cat(sprintf("RMSE — Cây đầy đủ:  %.4f\n", sqrt(mean((w_test$quality - predict(tree_reg_full, w_test))^2))))
cat(sprintf("RMSE — Sau pruning: %.4f\n", rmse_pruned))
#6.6. So sánh toàn diện: Decision Tree vs Logistic Regression
#German Credit — Classification
german <- read.csv("german1.csv")
german$target <- factor(german$target, levels = c(1, 2),
                        labels = c("Good", "Bad"))

set.seed(42)
g_split <- initial_split(german, prop = 0.8, strata = target)
g_train <- training(g_split)
g_test  <- testing(g_split)

ctrl_compare <- trainControl(
  method = "cv", number = 10,
  classProbs = TRUE, summaryFunction = twoClassSummary
)

# Logistic Regression
set.seed(42)
m_lr <- train(target ~ ., data = g_train, method = "glm",
              family = "binomial", trControl = ctrl_compare, metric = "ROC")

# Decision Tree (tuning cp)
set.seed(42)
m_tree <- train(target ~ ., data = g_train, method = "rpart",
                trControl = ctrl_compare, metric = "ROC",
                tuneGrid = expand.grid(cp = c(0, 0.001, 0.005, 0.01, 0.02, 0.05)))

# Tổng hợp kết quả CV
results <- resamples(list(
  LogisticRegression = m_lr,
  DecisionTree       = m_tree
))

summary(results, metric = "ROC")
dotplot(results, metric = "ROC",
        main = "So sánh AUC (10-Fold CV) — German Credit")

# Đánh giá trên test set
get_auc <- function(model, test, truth) {
  p   <- predict(model, newdata = test, type = "prob")[, "Bad"]
  roc_obj <- roc(test[[truth]], p, levels = c("Good", "Bad"), quiet = TRUE)
  round(auc(roc_obj), 4)
}

cat("AUC trên TEST SET:\n")
cat("  Logistic Regression:", get_auc(m_lr,   g_test, "target"), "\n")
cat("  Decision Tree:      ", get_auc(m_tree, g_test, "target"), "\n")
#Wines — Nhận diện ranh giới phi tuyến
# Dự đoán type (red/white) — ranh giới trong không gian hóa học
wines_clf <- wines %>% select(-quality)
wines_clf$type <- factor(wines_clf$type)

set.seed(42)
wc_split <- initial_split(wines_clf, prop = 0.8, strata = type)
wc_train <- training(wc_split)
wc_test  <- testing(wc_split)

ctrl_w <- trainControl(method = "cv", number = 5,
                       classProbs = TRUE, summaryFunction = twoClassSummary)

set.seed(42)
m_lr_w <- train(type ~ ., data = wc_train, method = "glm",
                family = "binomial", trControl = ctrl_w, metric = "ROC")

set.seed(42)
m_tree_w <- train(type ~ ., data = wc_train, method = "rpart",
                  trControl = ctrl_w, metric = "ROC",
                  tuneGrid = expand.grid(cp = c(0, 0.001, 0.005, 0.01)))

cat("\n=== Wines — Phân loại red/white ===\n")
cat("Logistic Regression AUC (CV):", round(m_lr_w$results$ROC[which.max(m_lr_w$results$ROC)], 4), "\n")
cat("Decision Tree AUC (CV):      ", round(max(m_tree_w$results$ROC), 4), "\n")

#Medical Care — Cây lớn và biến hỗn hợp
medical <- read.csv("medical_care.csv", stringsAsFactors = TRUE)

medical_clean <- medical %>%
  select(UCURNINS, UMARSTAT, USATMED, REGION, FHOSP, FDENT, FEMER,
         FDOCT, UIMMSTAT, UAGE, U_FTPT, U_WKSLY, UBRACE, GENDER, UEDUC3) %>%
  drop_na() %>%
  mutate(UCURNINS = factor(UCURNINS))

set.seed(42)
med_split <- initial_split(medical_clean, prop = 0.8, strata = UCURNINS)
med_train <- training(med_split)
med_test  <- testing(med_split)

# Decision Tree với cp được tuned
ctrl_med <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE, summaryFunction = twoClassSummary
)

set.seed(42)
tree_med <- train(
  UCURNINS ~ .,
  data      = med_train,
  method    = "rpart",
  trControl = ctrl_med,
  metric    = "ROC",
  tuneGrid  = expand.grid(cp = c(0, 0.0001, 0.0005, 0.001, 0.005, 0.01))
)

cat("cp tối ưu (Medical):", tree_med$bestTune$cp, "\n")
cat("AUC (CV):           ", round(max(tree_med$results$ROC), 4), "\n")

# Test set
pred_med  <- predict(tree_med, newdata = med_test, type = "prob")[, "Yes"]
roc_med   <- roc(med_test$UCURNINS, pred_med, levels = c("No","Yes"), quiet = TRUE)
cat("AUC (Test set):     ", round(auc(roc_med), 4), "\n")

# So sánh với Logistic Regression (từ Bài 17)
lr_med <- glm(UCURNINS ~ UMARSTAT + USATMED + REGION + FHOSP + FDENT +
                FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT + U_WKSLY +
                UBRACE + GENDER + UEDUC3,
              data = med_train, family = binomial())

pred_lr_med <- predict(lr_med, newdata = med_test, type = "response")
roc_lr_med  <- roc(med_test$UCURNINS, pred_lr_med, levels = c("No","Yes"), quiet = TRUE)

cat("\n--- So sánh trên Test Set (Medical) ---\n")
cat("Logistic Regression AUC:", round(auc(roc_lr_med), 4), "\n")
cat("Decision Tree AUC:      ", round(auc(roc_med),    4), "\n")

# Vẽ cây tốt nhất (thường nhỏ gọn sau pruning)
rpart.plot(tree_med$finalModel, type = 4, extra = 104, fallen.leaves = TRUE,
           main = "Decision Tree tối ưu — Medical Care",
           box.palette = "RdBu")
#6.8. Tổng hợp so sánh tất cả dataset
# Tạo bảng tổng kết đẹp
summary_results <- tibble(
  Dataset    = c("Titanic", "German Credit", "Wines (type)", "Medical Care"),
  Model      = c("Decision Tree", "Decision Tree", "Decision Tree", "Decision Tree"),
  AUC_CV     = c(
    round(max(tree_caret$results$ROC), 4),
    round(max(m_tree$results$ROC),     4),
    round(max(m_tree_w$results$ROC),   4),
    round(max(tree_med$results$ROC),   4)
  ),
  AUC_LR_CV  = c(
    round(max(m_lr$results$ROC),   4),   # Từ Bài 17 hoặc tính lại
    round(max(m_lr$results$ROC),   4),
    round(max(m_lr_w$results$ROC), 4),
    NA
  )
)

print(summary_results)























































































































































