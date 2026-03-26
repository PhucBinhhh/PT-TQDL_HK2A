options(timeout = 600)
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(randomForest)) install.packages("randomForest")
if(!require(ranger)) install.packages("ranger")
if(!require(tidymodels)) install.packages("tidymodels")
if(!require(vip)) install.packages("vip")

library(tidyverse)
library(randomForest)  # Package cổ điển, dễ học
library(ranger)        # Nhanh hơn, dùng cho dữ liệu lớn
library(caret)         # Unified interface (nhất quán Bài 17)
library(tidymodels)    # Modern workflow
library(vip)           # Visualize feature importance
library(pROC)



#8.1. Bagging từ đầu — Hiểu rõ cơ chế
#Trước khi dùng package, ta tự cài đặt Bagging để hiểu bên trong hoạt động như thế nào:
  
  library(rpart)

titanic <- read.csv("C:/Users/Admin/Desktop/TANPHAT/hocotruong/Năm ba 2025-2026/HK2_A/Phantichvatrucquandulieu/LAB/lab_03/titanic.csv", stringsAsFactors = TRUE)
titanic_clean <- titanic %>%
  select(survived, pclass, sex, age, sibsp, parch, fare) %>%
  mutate(survived = factor(survived, levels = c(0,1),
                           labels = c("No","Yes"))) %>%
  drop_na()

set.seed(42)
n <- nrow(titanic_clean)
B <- 200  # Số cây

# Ma trận lưu dự đoán: mỗi cột là một cây
bag_preds <- matrix(NA, nrow = n, ncol = B)

for (b in 1:B) {
  # Bước 1: Bootstrap sample (lấy mẫu CÓ hoàn lại)
  boot_idx  <- sample(1:n, size = n, replace = TRUE)
  boot_data <- titanic_clean[boot_idx, ]
  
  # Bước 2: Huấn luyện Decision Tree trên bootstrap sample
  tree_b <- rpart(survived ~ ., data = boot_data, method = "class")
  
  # Bước 3: Dự đoán trên toàn bộ dữ liệu gốc
  bag_preds[, b] <- as.integer(
    predict(tree_b, newdata = titanic_clean, type = "class") == "Yes"
  )
}

# Bước 4: Majority vote — nếu > 50% cây nói "Yes" → dự đoán "Yes"
vote_pct  <- rowMeans(bag_preds)
bag_final <- factor(ifelse(vote_pct >= 0.5, "Yes", "No"))

# So sánh với cây đơn lẻ
tree_single <- rpart(survived ~ ., data = titanic_clean, method = "class")
pred_single <- predict(tree_single, type = "class")

acc_single  <- mean(pred_single == titanic_clean$survived)
acc_bagging <- mean(bag_final   == titanic_clean$survived)

cat(sprintf("Accuracy — Cây đơn lẻ:    %.4f\n", acc_single))
cat(sprintf("Accuracy — Bagging (%d cây): %.4f\n", B, acc_bagging))
#Quan sát sự hội tụ theo số cây:
  
  # Theo dõi accuracy của Bagging khi tăng dần số cây
  acc_by_ntree <- numeric(B)

for (b in 1:B) {
  vote_b         <- rowMeans(bag_preds[, 1:b, drop = FALSE])
  pred_b         <- factor(ifelse(vote_b >= 0.5, "Yes", "No"))
  acc_by_ntree[b] <- mean(pred_b == titanic_clean$survived)
}

data.frame(ntree = 1:B, accuracy = acc_by_ntree) %>%
  ggplot(aes(x = ntree, y = accuracy)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = acc_single, color = "tomato",
             linetype = "dashed", linewidth = 1) +
  annotate("text", x = 150, y = acc_single - 0.005,
           label = "Cây đơn lẻ", color = "tomato") +
  labs(title  = "Accuracy của Bagging theo số cây — Titanic",
       x = "Số cây (B)", y = "Accuracy") +
  theme_minimal()


#8.2. Random Forest với randomForest — German Credit
german <- read.csv("german1.csv")

german$target <- factor(german$target, levels = c(1, 2),
                        labels = c("Good", "Bad"))

# Chia train/test
set.seed(42)
train_idx <- createDataPartition(german$target, p = 0.8, list = FALSE)
g_train   <- german[train_idx, ]
g_test    <- german[-train_idx, ]

cat("Train:", nrow(g_train), "| Test:", nrow(g_test), "\n")
prop.table(table(g_train$target))
#Huấn luyện mô hình
p <- ncol(g_train) - 1  # Số biến đầu vào

set.seed(42)
rf_model <- randomForest(
  target     ~ .,
  data       = g_train,
  ntree      = 500,
  mtry       = floor(sqrt(p)),  # ≈ sqrt(p) cho phân loại
  importance = TRUE,            # Bật để tính feature importance
  keep.forest = TRUE
)

# Xem tóm tắt mô hình
print(rf_model)


# Đánh giá trên tập test
# Dự đoán nhãn
pred_class <- predict(rf_model, newdata = g_test, type = "class")

# Dự đoán xác suất (để tính AUC)
pred_prob  <- predict(rf_model, newdata = g_test, type = "prob")[, "Bad"]

# Confusion Matrix
cm <- confusionMatrix(pred_class, g_test$target, positive = "Bad")
print(cm)

# AUC
roc_rf <- roc(g_test$target, pred_prob,
              levels = c("Good", "Bad"), quiet = TRUE)
cat("\nAUC (Random Forest):", round(auc(roc_rf), 4), "\n")

# So sánh với Decision Tree từ Bài 18
tree_dt <- rpart(target ~ ., data = g_train, method = "class")
pred_dt  <- predict(tree_dt, newdata = g_test, type = "prob")[, "Bad"]
roc_dt   <- roc(g_test$target, pred_dt,
                levels = c("Good", "Bad"), quiet = TRUE)

cat("AUC (Decision Tree):", round(auc(roc_dt), 4), "\n")
cat("Cải thiện:          ", round(auc(roc_rf) - auc(roc_dt), 4), "\n")



#8.3. OOB Error — Theo dõi sự hội tụ
# err.rate: ma trận lưu OOB error theo từng cây
oob_df <- data.frame(
  ntree    = 1:500,
  OOB      = rf_model$err.rate[, "OOB"],
  Good     = rf_model$err.rate[, "Good"],
  Bad      = rf_model$err.rate[, "Bad"]
)

oob_df %>%
  pivot_longer(-ntree, names_to = "Type", values_to = "Error") %>%
  ggplot(aes(x = ntree, y = Error, color = Type)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("OOB"  = "black",
                                "Good" = "steelblue",
                                "Bad"  = "tomato")) +
  labs(title   = "OOB Error theo số cây — German Credit",
       subtitle = "OOB Error hội tụ sau ~100–150 cây",
       x = "Số cây (ntree)", y = "Error Rate",
       color = "Loại lỗi") +
  theme_minimal()

# Tìm ntree tối thiểu mà OOB error đã ổn định
# (định nghĩa: chênh lệch OOB error < 0.001 trong 50 cây liên tiếp)
oob_stable <- which(abs(diff(oob_df$OOB, lag = 50)) < 0.001)[1] + 50
cat("OOB error ổn định sau khoảng:", oob_stable, "cây\n")

#8.4. Feature Importance — Biến nào quan trọng?
  # --- MDI: Mean Decrease Gini ---
  importance_mdi <- as.data.frame(importance(rf_model, type = 2)) %>%
  rownames_to_column("variable") %>%
  rename(MDI = MeanDecreaseGini) %>%
  arrange(desc(MDI))

# --- MDA: Mean Decrease Accuracy (Permutation) ---
importance_mda <- as.data.frame(importance(rf_model, type = 1)) %>%
  rownames_to_column("variable") %>%
  rename(MDA = MeanDecreaseAccuracy) %>%
  arrange(desc(MDA))

# Vẽ song song để so sánh
p1 <- ggplot(importance_mdi %>% head(15),
             aes(x = reorder(variable, MDI), y = MDI)) +
  geom_col(fill = "steelblue") + coord_flip() +
  labs(title = "MDI (Mean Decrease Gini)",
       x = NULL, y = "Importance") +
  theme_minimal()

p2 <- ggplot(importance_mda %>% head(15),
             aes(x = reorder(variable, MDA), y = MDA)) +
  geom_col(fill = "tomato") + coord_flip() +
  labs(title = "MDA (Permutation Importance)",
       x = NULL, y = "Importance") +
  theme_minimal()

library(patchwork)
p1 + p2 +
  plot_annotation(title = "Feature Importance — German Credit Random Forest")
#So sánh thứ hạng giữa MDI và MDA:
  
  # Tạo bảng so sánh thứ hạng
  rank_compare <- importance_mdi %>%
  mutate(rank_MDI = row_number()) %>%
  left_join(
    importance_mda %>% mutate(rank_MDA = row_number()),
    by = "variable"
  ) %>%
  select(variable, rank_MDI, rank_MDA) %>%
  mutate(rank_diff = abs(rank_MDI - rank_MDA)) %>%
  arrange(rank_MDI)

print(head(rank_compare, 10))
cat("\nBiến có thứ hạng chênh lệch nhiều nhất:\n")
print(rank_compare %>% arrange(desc(rank_diff)) %>% head(5))

#8.5. Tuning mtry với Cross-Validation
#Cách 1: caret (nhất quán Bài 17)
ctrl_rf <- trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Thử nhiều giá trị mtry
tune_grid <- expand.grid(mtry = c(2, 3, 5, 7, 10, 15, 20))

set.seed(42)
rf_caret <- train(
  target ~ .,
  data      = g_train,
  method    = "rf",
  trControl = ctrl_rf,
  tuneGrid  = tune_grid,
  metric    = "ROC",
  ntree     = 300
)

# Kết quả theo từng mtry
print(rf_caret$results[, c("mtry", "ROC", "Sens", "Spec")])
plot(rf_caret, main = "Chọn mtry tối ưu — 5-Fold CV (caret)")

cat("mtry tối ưu:", rf_caret$bestTune$mtry, "\n")
cat("AUC tốt nhất:", round(max(rf_caret$results$ROC), 4), "\n")
#Cách 2: tidymodels (hiện đại)
# Chuẩn bị dữ liệu
set.seed(42)
g_split <- initial_split(german, prop = 0.8, strata = target)
g_train_tm <- training(g_split)
g_test_tm  <- testing(g_split)
g_folds    <- vfold_cv(g_train_tm, v = 5, strata = target)

# Recipe
rf_recipe <- recipe(target ~ ., data = g_train_tm) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# Model spec với mtry và min_n cần tune
rf_spec <- rand_forest(
  mtry  = tune(),
  trees = 300,
  min_n = tune()
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# Workflow
rf_wf <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_spec)

# Grid tham số
rf_grid <- grid_regular(
  mtry(range  = c(2, 15)),
  min_n(range = c(2, 20)),
  levels = 4
)

# Tune
set.seed(42)
rf_tune <- tune_grid(
  rf_wf,
  resamples = g_folds,
  grid      = rf_grid,
  metrics   = metric_set(roc_auc, accuracy)
)

# Xem top kết quả
collect_metrics(rf_tune) %>%
  filter(.metric == "roc_auc") %>%
  arrange(desc(mean)) %>%
  head(8) %>%
  print()

# Vẽ kết quả tuning
autoplot(rf_tune) +
  labs(title = "Tuning mtry và min_n — Random Forest (tidymodels)")

# Tham số tốt nhất
best_rf <- select_best(rf_tune, metric = "roc_auc")
cat("Tham số tốt nhất:\n")
print(best_rf)

# Fit mô hình cuối cùng
final_rf_wf  <- finalize_workflow(rf_wf, best_rf)
final_rf_fit <- last_fit(final_rf_wf, g_split)

# Kết quả
cat("\n--- Kết quả trên Test Set ---\n")
collect_metrics(final_rf_fit) %>% print()

# Confusion matrix
collect_predictions(final_rf_fit) %>%
  conf_mat(truth = target, estimate = .pred_class) %>%
  autoplot(type = "heatmap") +
  labs(title = "Confusion Matrix — Random Forest (German Credit)")

# ROC Curve
collect_predictions(final_rf_fit) %>%
  roc_curve(truth = target, .pred_Bad) %>%
  autoplot() +
  labs(title = "ROC Curve — Random Forest (German Credit)")
#8.6. Wines — Regression Forest
#Random Forest cũng áp dụng được cho hồi quy (dự đoán biến liên tục).

wines <- read.csv("wines.csv", stringsAsFactors = TRUE)

# Dự đoán quality (biến liên tục)
wines_reg <- wines %>% select(-type)

set.seed(42)
wr_split <- initial_split(wines_reg, prop = 0.8)
wr_train <- training(wr_split)
wr_test  <- testing(wr_split)

# Regression Random Forest (method = "anova" → randomForest tự nhận biết)
set.seed(42)
rf_reg <- randomForest(
  quality    ~ .,
  data       = wr_train,
  ntree      = 300,
  mtry       = floor(ncol(wr_train) / 3),  # p/3 cho hồi quy
  importance = TRUE
)

print(rf_reg)

# Đánh giá
pred_rf_reg <- predict(rf_reg, newdata = wr_test)

rmse_rf <- sqrt(mean((wr_test$quality - pred_rf_reg)^2))
mae_rf  <- mean(abs(wr_test$quality - pred_rf_reg))
r2_rf   <- 1 - sum((wr_test$quality - pred_rf_reg)^2) /
  sum((wr_test$quality - mean(wr_test$quality))^2)

# So sánh với Regression Tree (Bài 18) và Linear Regression
lm_wine   <- lm(quality ~ ., data = wr_train)
pred_lm   <- predict(lm_wine, newdata = wr_test)
rmse_lm   <- sqrt(mean((wr_test$quality - pred_lm)^2))
r2_lm     <- 1 - sum((wr_test$quality - pred_lm)^2) /
  sum((wr_test$quality - mean(wr_test$quality))^2)

tree_reg  <- rpart(quality ~ ., data = wr_train, method = "anova")
pred_tree <- predict(tree_reg, newdata = wr_test)
rmse_tree <- sqrt(mean((wr_test$quality - pred_tree)^2))
r2_tree   <- 1 - sum((wr_test$quality - pred_tree)^2) /
  sum((wr_test$quality - mean(wr_test$quality))^2)

# Bảng so sánh
tibble(
  Model   = c("Linear Regression", "Decision Tree", "Random Forest"),
  RMSE    = round(c(rmse_lm, rmse_tree, rmse_rf), 4),
  R2      = round(c(r2_lm,   r2_tree,   r2_rf),   4)
) %>%
  arrange(RMSE) %>%
  print()
#Visualize predicted vs actual:
  
  data.frame(
    actual    = wr_test$quality,
    predicted = pred_rf_reg
  ) %>%
  ggplot(aes(x = actual, y = predicted)) +
  geom_jitter(alpha = 0.3, color = "steelblue", width = 0.1) +
  geom_abline(slope = 1, intercept = 0, color = "red",
              linetype = "dashed", linewidth = 1) +
  labs(title    = "Predicted vs Actual — Random Forest Regression (Wines)",
       subtitle  = paste0("RMSE = ", round(rmse_rf, 4),
                          " | R² = ", round(r2_rf, 4)),
       x = "Actual Quality", y = "Predicted Quality") +
  theme_minimal()
#8.7. Titanic — So sánh toàn diện với caret
titanic_clean <- titanic %>%
  select(survived, pclass, sex, age, sibsp, parch, fare) %>%
  mutate(survived = factor(survived, levels = c(0,1),
                           labels = c("No","Yes"))) %>%
  drop_na()

set.seed(42)
tit_split <- initial_split(titanic_clean, prop = 0.8, strata = survived)
tit_train <- training(tit_split)
tit_test  <- testing(tit_split)

ctrl_comp <- trainControl(
  method = "cv", number = 5,
  classProbs = TRUE, summaryFunction = twoClassSummary
)

set.seed(42)
# Decision Tree (Bài 18)
m_tree <- train(survived ~ ., data = tit_train, method = "rpart",
                trControl = ctrl_comp, metric = "ROC",
                tuneGrid = expand.grid(cp = c(0, 0.001, 0.01, 0.05)))

# Bagging (treebag = Bagging với Decision Tree)
m_bag  <- train(survived ~ ., data = tit_train, method = "treebag",
                trControl = ctrl_comp, metric = "ROC")

# Random Forest
m_rf   <- train(survived ~ ., data = tit_train, method = "rf",
                trControl = ctrl_comp, metric = "ROC",
                tuneGrid = expand.grid(mtry = c(2, 3, 4)),
                ntree = 300)

# So sánh
results_tit <- resamples(list(
  DecisionTree = m_tree,
  Bagging      = m_bag,
  RandomForest = m_rf
))

summary(results_tit, metric = "ROC")

dotplot(results_tit, metric = "ROC",
        main = "So sánh AUC (5-Fold CV) — Titanic")

# Đánh giá trên test set
get_auc_test <- function(model, test, truth_col, pos_level) {
  p   <- predict(model, newdata = test, type = "prob")[, pos_level]
  lvl <- levels(test[[truth_col]])
  round(auc(roc(test[[truth_col]], p, levels = lvl, quiet = TRUE)), 4)
}

cat("\n--- AUC trên Test Set ---\n")
cat("Decision Tree:", get_auc_test(m_tree, tit_test, "survived", "Yes"), "\n")
cat("Bagging:      ", get_auc_test(m_bag,  tit_test, "survived", "Yes"), "\n")
cat("Random Forest:", get_auc_test(m_rf,   tit_test, "survived", "Yes"), "\n")
#8.8. Medical Care — Random Forest với ranger (dữ liệu lớn)
#Với dữ liệu lớn (35,000+ quan sát), dùng ranger thay vì randomForest vì tốc độ nhanh hơn đáng kể và hỗ trợ xử lý song song.

library(ranger)

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

# ranger: nhanh hơn randomForest, hỗ trợ parallel
set.seed(42)
rf_ranger <- ranger(
  UCURNINS    ~ .,
  data        = med_train,
  num.trees   = 300,
  mtry        = 4,
  importance  = "impurity",       # MDI
  probability = TRUE,             # Trả về xác suất thay vì nhãn
  num.threads = parallel::detectCores() - 1,  # Dùng đa nhân
  verbose     = FALSE
)

cat("OOB Prediction Error:", round(rf_ranger$prediction.error, 4), "\n")

# Dự đoán trên test set
pred_ranger <- predict(rf_ranger, data = med_test)$predictions[, "Yes"]
roc_med     <- roc(med_test$UCURNINS, pred_ranger,
                   levels = c("No","Yes"), quiet = TRUE)
cat("AUC (Random Forest — Medical):", round(auc(roc_med), 4), "\n")

# So sánh với Logistic Regression (Bài 17)
lr_med  <- glm(UCURNINS ~ UMARSTAT + USATMED + REGION + FHOSP + FDENT +
                 FEMER + FDOCT + UIMMSTAT + UAGE + U_FTPT + U_WKSLY +
                 UBRACE + GENDER + UEDUC3,
               data = med_train, family = binomial())
pred_lr <- predict(lr_med, newdata = med_test, type = "response")
roc_lr  <- roc(med_test$UCURNINS, pred_lr,
               levels = c("No","Yes"), quiet = TRUE)

# So sánh với Decision Tree (Bài 18)
tree_med <- rpart(UCURNINS ~ ., data = med_train, method = "class")
pred_dt  <- predict(tree_med, newdata = med_test, type = "prob")[, "Yes"]
roc_dt   <- roc(med_test$UCURNINS, pred_dt,
                levels = c("No","Yes"), quiet = TRUE)

cat("\n--- So sánh AUC trên Test Set (Medical Care) ---\n")
cat("Logistic Regression (Bài 17):", round(auc(roc_lr),  4), "\n")
cat("Decision Tree (Bài 18):      ", round(auc(roc_dt),  4), "\n")
cat("Random Forest (Bài 19):      ", round(auc(roc_med), 4), "\n")

# Feature Importance từ ranger
imp_ranger <- data.frame(
  variable   = names(rf_ranger$variable.importance),
  importance = rf_ranger$variable.importance
) %>%
  arrange(desc(importance))

ggplot(imp_ranger, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance (MDI) — Random Forest (Medical Care)",
       x = NULL, y = "Mean Decrease Impurity") +
  theme_minimal()
#8.9. So sánh randomForest vs ranger
# Benchmark tốc độ trên Medical Care
cat("--- Benchmark tốc độ ---\n")

# randomForest (single thread)
t1 <- system.time(
  randomForest(UCURNINS ~ ., data = med_train,
               ntree = 100, importance = FALSE)
)

# ranger (multi thread)
t2 <- system.time(
  ranger(UCURNINS ~ ., data = med_train,
         num.trees = 100, num.threads = parallel::detectCores() - 1,
         verbose = FALSE)
)

cat(sprintf("randomForest: %.1f giây\n", t1["elapsed"]))
cat(sprintf("ranger:       %.1f giây\n", t2["elapsed"]))
cat(sprintf("ranger nhanh hơn: %.1fx\n", t1["elapsed"] / t2["elapsed"]))





















































































