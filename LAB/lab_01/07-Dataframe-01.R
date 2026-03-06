#Dataframe trong R
# là cấu trúc dữ liệu dùng để lưu trữ dữ liệu dạng bảng
#trong R  (cho phân tích thống kê và ML) 
# là môt danh sách các vector có cùng độ dài 
#1và thường có tên duy nhất


# 1. Tạo dataframe 
# được tạo từ các vector có chung độ dài



column1 <- c(1:3)
column2 <- c("Tung", "Tom", "Anna")
column3 <- c(T,T,F)

dataset1 <- data.frame(column1,column2,column3)

# Hiển thị ra console 

dataset1 
print(dataset1)
View(dataset1) # hiện một của sổ khác, nhìn tổng quan về table

# Xem tên của các cột
colnames(dataset1)

#Đổi tên cột

colnames(dataset1 ) [2] <- "Name"
colnames(dataset1[2])
    # Đổi hàng loạt 
colnames(dataset1) <- c("#","Name", "Check")

dataset1 

# Thêm dòng mới cho DATAFRAME

newRow <- c(4,"NTung", T) # Ghép df với vector

dataset2 <- rbind(dataset1 ,newRow)
dataset2 

newRowDF <- data.frame(5,"Lisa", F) # Ghép với DF thì 2 DF cần giống tên cột
names(newRowDF) <- c("#","Name", "Check")
dataset3 <- rbind (dataset2, newRowDF) # 
dataset3 


# Thêm một cột mới (tên của vector sẽ là tên cột) 

newColumn <- c(2,4,6,8,10)
dataset4 <- cbind(dataset3, newColumn)
dataset4

dataset4 $ newColumn2 <- c(2,4,6,8,10)



# Truy xuất dữ liệu 
    # Truy xuất bằng chỉ số
dataset4[3,2] # dòng 3 cột 2 

    # bằng chỉ số và tên cột 
dataset4[3,"Name"]
    
    #bằng tên cột
dataset4["Name"]
dataset4[,"Name"]
dataset4$

  
# Các hàm thường dùng 
  #Hiển thị 5 dòng đầu 
head(dataset4)
  #Hiển thị 5 dòng cuối 
tail(dataset4)
  #Hiển thi cấu trúc dữ liệu
str(dataset4)

summary(dataset4)


dataset4$Check <- as.logical(dataset4$Check)
summary(dataset4)


data()
iris
View(iris)


