# Khai báo thông số
chieudai <- 20
chieurong <-7

# Tính toán chu vi và diện tích
chuvi <- (chieudai + chieurong) *2 
dientich <- chieudai * chieurong 

# So sánh 
check_dk <- (dientich > 150) & (chieudai > chieurong)

# In kết quả
print(paste("chu vi bằng :", chuvu ,don_vi))
kq <- paste ("diện tích là :", dientich, don_vi)

#Kiểm tra
help(class)

# 
check_dk<- as.character()


############## VECTOR
v <- c(1,2,3,4,5,6) # vector cơ bản 

# hàm seq()
v0 <- seq (1,5)
v1 <- seq(2,10 , by = 2 )
today <- as.Date(Sys.Date())
v5 <- seq (today, as.Date("2026-7-1"))
len <- length(v5)
len

# hàm rep()
v2 <- rep(1, times = 10 )
v3 <- rep(c(2,3) , times = 3 )
v4 <- rep(c(1,2), times =3 , each = 2)
 

v1+ v0

v[-3]
v[2]













