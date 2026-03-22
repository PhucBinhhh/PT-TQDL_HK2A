# --- 1. CÀI ĐẶT VÀ TẢI THƯ VIỆN ---
packages <- c("caret", "pROC", "mice", "corrplot", "ggplot2")
for (p in packages) {
  if(!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}g

if(!require(caret)) install.packages("caret")
if(!require(pROC)) install.packages("pROC")
if(!require(mice)) install.packages("mice")
if(!require(corrplot)) install.packages("corrplot")
if(!require(ggplot2)) install.packages("ggplot2")

# --- 2. TẢI DỮ LIỆU ---
# Lưu ý: Bạn kiểm tra lại đường dẫn file csv của mình
water <- read.csv("C:/Users/Admin/Desktop/TANPHAT/hocotruong/Năm ba 2025-2026/HK2_A/Phantichvatrucquandulieu/LAB/lab_03/water_potability.csv")

# Chuyển Potability sang Factor ngay từ đầu với nhãn rõ ràng
water$Potability <- factor(water$Potability, levels = c(0, 1), labels = c("No", "Yes"))
#lệch 


# --- 3. XỬ LÝ DỮ LIỆU THIẾU (NA) ---
# Điền NA cho Trihalomethanes trước bằng Median
water$Trihalomethanes[is.na(water$Trihalomethanes)] <- median(water$Trihalomethanes, na.rm = TRUE)

# Điền NA cho các cột còn lại bằng MICE
imp_water <- mice(water, m = 2, method = 'pmm', maxit = 5, seed = 123)
water_clean <- complete(imp_water)

# --- 4. XỬ LÝ OUTLIERS ---
remove_outliers <- function(df, column) {
  Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  df <- df[df[[column]] >= lower & df[[column]] <= upper, ]
  return(df)
}

for (i in 1:9) {
  col_name <- colnames(water_clean)[i]
  water_clean <- remove_outliers(water_clean, col_name)
}

# --- 5. CHUẨN HÓA (SCALING) ---
# Scale 9 cột đầu tiên
water_clean[, 1:9] <- scale(water_clean[, 1:9])

# --- 6. PHÂN CHIA DỮ LIỆU ---
set.seed(42)
train_indices <- createDataPartition(water_clean$Potability, p = 0.7, list = FALSE)
train_data <- water_clean[train_indices, ]
test_data  <- water_clean[-train_indices, ]

# --- 7. HUẤN LUYỆN MÔ HÌNH LOGISTIC ---
model <- glm(Potability ~ poly(ph, 2) + poly(Hardness, 2) + poly(Solids, 2) + 
                   poly(Chloramines, 2) + poly(Sulfate, 2) + poly(Conductivity, 2) + 
                   poly(Organic_carbon, 2) + poly(Trihalomethanes, 2) + poly(Turbidity, 2), 
                 data = train_balanced, 
                 family = binomial)

# --- 8. DỰ ĐOÁN VÀ TÌM NGƯỠNG TỐI ƯU ---
probabilities <- predict(model, newdata = test_data, type = "response")

# Tạo đối tượng ROC
roc_obj <- roc(test_data$Potability, probabilities, quiet = TRUE)

# Tìm ngưỡng tốt nhất (Best Threshold)
best_coords <- coords(roc_obj, "best", ret = "threshold", transpose = TRUE)
best_threshold <- as.numeric(best_coords[1])



# Phân loại theo ngưỡng tối ưu
predicted_classes <- ifelse(probabilities > best_threshold, "Yes", "No")
predicted_classes <- factor(predicted_classes, levels = c("No", "Yes"))

# --- 9. ĐÁNH GIÁ VÀ TRỰC QUAN HÓA ---

# A. Confusion Matrix & F1-Score
conf_matrix <- confusionMatrix(predicted_classes, test_data$Potability, mode = "everything")
print(conf_matrix)

# B. Vẽ Confusion Matrix (Heatmap)
plt_cm <- as.data.frame(conf_matrix$table)
print(
  ggplot(plt_cm, aes(Prediction, Reference, fill = Freq)) +
    geom_tile() + 
    geom_text(aes(label = Freq), color = "white", size = 8) +
    scale_fill_gradient(low = "#3498db", high = "#e74c3c") +
    labs(title = "Confusion Matrix - Water Potability", x = "Dự đoán", y = "Thực tế") +
    theme_minimal()
)

# C. Vẽ đường cong ROC
plot(roc_obj, main = paste("ROC Curve - AUC:", round(auc(roc_obj), 4)), 
     col = "darkblue", lwd = 3, print.thres = best_threshold)
abline(a = 0, b = 1, lty = 2, col = "gray")

# D. Xuất các chỉ số quan trọng
cat("\n--- CÁC CHỈ SỐ ĐÁNH GIÁ ---\n")
cat("Accuracy:", round(conf_matrix$overall["Accuracy"], 4), "\n")
cat("F1-Score:", round(conf_matrix$byClass["F1"], 4), "\n")
cat("Best Threshold used:", round(best_threshold, 4), "\n")












# --- 1. CÂN BẰNG DỮ LIỆU (UNDERSAMPLING) ---
set.seed(42)

train_data <- water_clean 

# Chia tách dữ liệu theo nhãn
data_yes <- train_data[train_data$Potability == "Yes", ]
data_no  <- train_data[train_data$Potability == "No", ]

# Lấy ngẫu nhiên số lượng mẫu "No" bằng với số lượng mẫu "Yes"
n_yes <- nrow(data_yes)
data_no_downsampled <- data_no[sample(nrow(data_no), n_yes), ]

# Gộp lại thành tập Train mới cân bằng 50/50
train_balanced <- rbind(data_yes, data_no_downsampled)

cat("Phân bổ nhãn sau khi Undersampling:\n")
print(table(train_balanced$Potability))

names(train_data)

#[1] "ph"              "Hardness"        "Solids"          "Chloramines"    
#[5] "Sulfate"         "Conductivity"    "Organic_carbon"  "Trihalomethanes"
#[9] "Turbidity"       "Potability"    

# --- 2. LỌC BIẾN (FEATURE SELECTION) ---
# Bước A: Chạy mô hình với tất cả các biến trên tập dữ liệu đã cân bằng
model_all_poly <- glm(Potability ~ poly(ph, 2) + poly(Hardness, 2) + poly(Solids, 2) + 
                        poly(Chloramines, 2) + poly(Sulfate, 2) + poly(Conductivity, 2) + 
                        poly(Organic_carbon, 2) + poly(Trihalomethanes, 2) + poly(Turbidity, 2), 
                      data = train_balanced, 
                      family = binomial)

# Xem bảng kết quả
summary(model_all_poly)

















