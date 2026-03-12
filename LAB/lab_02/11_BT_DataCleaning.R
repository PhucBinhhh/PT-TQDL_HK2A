###########Giới thiệu dataset 
# Đây là bộ dữ liệu y tế cực kỳ giá trị từ kho UCI, được thu thập từ năm 1992 đến 1995.
# tên là Myocardial Infarction Complications (Biến chứng nhồi máu cơ tim)

# Load data 
news <- read.csv("Myocardial infarction complications Database.csv")

#Hiểu Data

# Đọc 10 dòng đầu tiên 
head(news,10)

# Xem phân phối, cấu trúc tổng quát các cột
summary(news)
str(news)

# Xem số lượng cột/dòng
length(news) 
nrow(news)

dim(news) # Xem cả 2 thông tin 

# Xét Missing Value 
news[!complete.cases(news),]

# Đếm số lượng Missing 
#Tổng số lượng NA trong toàn bộ
sum(is.na(news))

# Số  dòng có ít nhất 1 NA 
sum(!complete.cases(news))

# ==> Số lượng NA cực kỳ lớn, không có bệnh nhân nào được ghi chép đầy đủ

# Xem số lượng NA của từng col
col_na <- colSums(is.na(news))
col_na
# Xem top 10 col có NA nhiều nhất 
sort(col_na, decreasing = TRUE)[1:10]

# Xử lý AGE
summary(news$AGE)
str(news$AGE)
# xử lý bằng median cho AGE -> không xóa -> tránh mất dữ liệu
news[is.na(news$AGE),] <- median(news$AGE , na.rm = TRUE)

















