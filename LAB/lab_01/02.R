# VECTOR (VECTƠ)
# Vector là một dãy dữ liệu cùng kiểu (cùng class).
# Vector là cấu trúc dữ liệu cơ bản trong R.
# Mỗi biến đơn lẻ trong R thực chất chính là một vector có độ dài bằng 1.

# PHƯƠNG THỨC TẠO vector
# Lệnh cơ bản để tạo vector là c() (combine - kết hợp).
# Ngoài ra còn có các hàm hữu ích khác như:
#   
#   rep() (replicate - sao chép)
#   seq() (sequence - tạo dãy số)
# 
# Kiểu dữ liệu của vector chính là kiểu dữ liệu của các phần tử trong nó.

# Sử dụng c()
v1<- c(1,2,3,4,5)
v2 <- c("A", "B", "C")

# Lưu ý:
#   
# Vector luôn chứa các phần tử cùng kiểu
# Nếu trộn các kiểu, R sẽ chuyển đổi tất cả về cùng một kiểu
# Thường là chuyển về kiểu dữ liệu "mạnh" nhất:
#   logical < integer < numeric < character
v_mix <-c(TRUE, 1, "A", 2, 1.5) 

# Sử dụng hàm khác
v3<-1:10
class(v3)

v4<-c(1.5:3.5)

v7<-rep(1, times=10)
v8<-rep(c(1,2), times=3)
v9<-rep(c(1,2,3), each=3)
v10<-rep(c(1,2), times=3, each=3)
v10

s1<-seq(1,5)
s2<-seq(from=10, to=20)
s3<-seq(from=1, to=10, by=2)

today<-as.Date(Sys.Date())
dates<-seq(today, as.Date("2030-12-31"), by="day")
dates

# Các phép toán
x<-c(1:4)
y<-seq(2,8, by=2)

x+y
x*y
x-y
x/y


#INDEX
x[1]
x[1:3]
y[c(1,3)]

c(c(2:5), x[1:4])

y[-2]

vectorNamed <- c("Tung", "Le", "18 years old")
names(vectorNamed)<-c("name", "surname", "age")
vectorNamed[1]
vectorNamed["name"]
vectorNamed[c("surname", "age")]


# 1. Bài tập về chuyển đổi kiểu dữ liệu số: 
# Tạo một số thập phân, sau đó chuyển đổi số đó thành số nguyên 
# rồi thành ký tự. Quan sát và giải thích sự thay đổi về giá trị 
# và cách hiển thị của số đó qua mỗi lần chuyển đổi.

a <- 3.14
class(a) # mặc định khởi tạo số sẽ là numeric 
b <- as.integer(a)
class(b) # chuyển sang integer thì phần thập phân biên mất -> không chỉ thay kiểu và còn biến đổi số cho đúng kiểu
c <- as.character(a)
c # thay đổi giá trị sao cho đúng kiểu đã ép



# 2. Bài tập về ghép chuỗi: 
# Tạo hai biến chứa văn bản, tìm hiểu tài liệu về hàm paste() 
# và sử dụng nó để ghép các biến văn bản đã tạo. So sánh kết quả 
# của hàm paste() với hàm c() và giải thích sự khác biệt giữa chúng.

t1 <- "tan"
t2 <-"phat"

paste(t1, t2)

c(t1,t2)

# -> kết quả khác nhau : paste sẽ biến 2 đoạn text rieegn biệt thành 1 nằm trong 1 "", còn c sẽ xem chúng là chung như 2 người chung 1 nhà.



# 3. Bài tập về xử lý ngày tháng: 
# Cho vector vecDate <- c("09:12:12", "28:02:16", "31:05:22"). 
# Hãy: 
# a) Chuyển đổi vector này sang kiểu Date
# b) Tính số ngày giữa các ngày trong vector với ngày hiện tại.

vecDate <- c("09:12:12", "28:02:16", "31:05:22")
new_vecDate <- as.Date(vecDate, format = "%d:%m:%y")
new_vecDate
today <- Sys.Date() 

songay_tl <- today - new_vecDate
as.numeric (songay_tl)



# 4. Bài tập tạo vector số: 
# Tạo vector "vec1" chứa các số từ 2 đến 8 và từ 17 đến 30 
# bằng cách viết code ngắn gọn nhất có thể.

vec1 <- c(seq(2,8),seq(17,30)) # c(2:8,17:30)
vec1

# 5. Bài tập sử dụng hàm seq(): 
# Tạo vector "vec2" có cấu trúc: (2, 8, 14, 20, 26, 32) 
# bằng cách sử dụng hàm seq().
vec2 <- seq(2,32 , by = 6) # seq(from = 2 , to = 32, by = 6)
vec2

# 6. Bài tập lặp chuỗi: 
# Tạo một vector có cấu trúc: "2", "7", "a", "2", "7", "a", 
# "2", "7", "a" bằng cách sử dụng hàm rep().

vec3 <- rep(c("2","7","a"), times = 2)
vec3


# 7. Bài tập về số chia hết: 
# Tạo một vector độ dài 100 chứa các số liên tiếp chia hết cho 3.

vec4 <- 3 * (1:100)
vec4

vec5 <- seq(from = 0 , by= 3 ,length.out = 100)
vec5


# 8. Bài tập tạo mẫu phức tạp: 
# Sử dụng một dòng code duy nhất để tạo vector "vec3" có cấu trúc: 
#   (1, 1, 3, 3, 5, 5, 7, 7, 9, 9) lặp lại 3 lần.

vec6 <- rep(c(1,3,5,7,9), times = 3 , each = 2)
vec6

# 9. Bài tập về số ngẫu nhiên: 
# Tạo vector "vec4" gồm 50 số bằng hàm runif(). 
# Giải thích chức năng của hàm runif() và sử dụng các số đã tạo 
# để tạo vector mới chứa 50 số nguyên ngẫu nhiên trong khoảng 0-20.
# runif() tạo số ngẫu nhiên phân phối đều 
# floor : chuyển sang số nguyên 

vec7 <- runif(50)
vec7

vec8 <- vec7*21
vec9 <- floor(vec8)
vec9

# 10. Bài tập truy xuất phần tử: 
# In ra các giá trị của phần tử thứ 5, 10 và 26 từ vector
# vừa tạo ở câu 9
vec9[5]
vec9[10]
vec9[26]


# 11. Bài tập về dãy có quy luật: 
# In ra giá trị của các phần tử cách đều nhau trong vector từ câu 9, 
# bắt đầu từ phần tử thứ 5 và lấy cứ mỗi phần tử thứ hai. 
# Gợi ý: Sử dụng hàm seq().

indices <- seq(from=5, to=length(vec4), by=2)
indices

result <- vec4[indices]
result


