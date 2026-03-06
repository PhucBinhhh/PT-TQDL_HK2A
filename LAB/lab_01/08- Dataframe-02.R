# 1 . Các thao tác nâng cao (Data wrangling )
set1 <- data.frame(IdClient = c(1:6 , 8 ) , Product = c(rep("Thit kho",4), rep("Ca kho", 3 )))
set2 <- data.frame(IdClient = c(1,2,5,9) , Region = c(rep("TP. HCM ",3), "Dong Nai"))
                                      
set1
set2


# Gộp bảng dữ liệu 
# a. Inner join (lấy phần tử có cả ở 2 set)

set3 <- merge (set1, set2, by= "IdClient")
set3


#b . outer join (lấy tất cả, cái nào thiếu thì điền NA)
set3 <- merge (set1, set2, by= "IdClient", all = TRUE)
set3 


#c. left join (lấy tất cả set1 ( bên trái) những phần tử
#không có ở set2 sẽ được điền NA)
set4 <- merge (set1, set2, by= "IdClient", all.x = TRUE)
set4

#d. right join (lấy tất cả set2 ( bên phải) những phần tử
#không có ở set1 sẽ được điền NA)
set3 <- merge (set1, set2, by= "IdClient", all.y = TRUE)
set3



# sắp xếp dữ liệu 

sort(set1$IdClient) # mặc định tăng dần theo giá tri 
order(set1$IdClient) # mặc định tăng dần theo index

sort(set1$IdClient, decreasing =  TRUE) # giảm dần 
order(set1$IdClient, decreasing =  TRUE) # giảm dần 
#sắp xếp bảng theo chiều của một cột bằng index 

set1[order(set1$IdClient),]

View(iris[order(iris$Sepal.Width),] )


# LỌc dữ liệu DF 
set4[set4$Region,]

set4[set4$Region=="TP. HCM " & !is.na(set4$Region),]

set4[set4$Region=="TP. HCM " & !is.na(set4$Region)  & set4$Product=="Ca kho",]

set4[set4$Region=="TP. HCM " & !is.na(set4$Region) & set4$IdClient < 5, ]


set4$Product <- as.factor(set4$Product)
set4$Region <- as.factor(set4$Region)

summary(set4)
str(set4)


# Data Cleaning

  # Phát hiện missing value 
is.na(set4)
sum(is.na(set4))
  
  #Phát hiện dòng chứa NA
    #complete.case : kiểm tra dòng đã có đầy đủ 
set4[is.na(set4$Region),]
set4[complete.cases(set4),] # lọc ra dòng đã đầy đủ
set4[!complete.cases(set4),] # lọc ra dòng có ít nhất 1 phần tử NA 


  # Xóa đi các NA
set4_clean <- na.omit(set4) 
set4_clean

  # Điền missing value 
set4$Region <- as.character(set4$Region)
set4[is.na(set4$Region), "Region"] <- "Viet Nam" 
set4
















