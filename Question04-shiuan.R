
# First, simulate 100 observations from a mixed distribution of beta(2,3),
# each with probability 0.5. Then, use at least 3 density estimating methods
# to smooth the observations. 
# You need to specify the parameters in the smoothing methods, 
# and compare the results.

# 取出我們要的亂數
set.seed(106354012)
m = 100
x = rbeta(m,2,3)
# 畫個點的散佈圖
stripchart(x,pch=16,cex=0.5,col=3,main="Dotplot")
# 然後畫個直方圖
y = seq(0,1,length=100)
hist( x, breaks = 10,probability = T,ylim = c(0,3),xlim= c(0,1),col = "#AAAAAA",main = "Density Estimation (h=0.1)")
# 這是真實的beta(2,3)圖形
lines(y , dbeta(y,2,3) , col = "#00AAFF" , lwd = 3)
legend("topright" , "Beta(2,3)" , lty = 1 , col="#00AAFF" , lwd = 3)
# 這是直接用套件模擬出來的函數值
kernal_g = density(x, kernel = c("gaussian"), width = 0.1)
kernal_r = density(x, kernel = c("rectangular"), width = 0.1)
kernal_t = density(x, kernel = c("triangular"), width = 0.1)
# 把他們畫在plot裡面囉
lines(kernal_g, col = "#FF0000" , lwd = 2)
lines(kernal_r, col = "#00FF00" , lwd = 2)
lines(kernal_t, col = "#FFFF00" , lwd = 2)
legend("right",legend=c("gaussian","rectangular","triangular"), col=c("#FF0000","#00FF00","#FFFF00"),lwd=2,cex=1)
box()

# (h=0.2)
hist( x, breaks = 5,probability = T,ylim = c(0,3),xlim= c(0,1),col = "#AAAAAA",main = "Density Estimation (h=0.2)")
# 這是真實的beta(2,3)圖形
lines(y , dbeta(y,2,3) , col = "#00AAFF" , lwd = 3)
legend("topright" , "Beta(2,3)" , lty = 1 , col="#00AAFF" , lwd = 3)
# 這是直接用套件模擬出來的函數值
kernal_g = density(x, kernel = c("gaussian"), width = 0.2)
kernal_r = density(x, kernel = c("rectangular"), width = 0.2)
kernal_t = density(x, kernel = c("triangular"), width = 0.2)
# 把他們畫在plot裡面囉
lines(kernal_g, col = "#FF0000" , lwd = 2)
lines(kernal_r, col = "#00FF00" , lwd = 2)
lines(kernal_t, col = "#FFFF00" , lwd = 2)
legend("right",legend=c("gaussian","rectangular","triangular"), col=c("#FF0000","#00FF00","#FFFF00"),lwd=2,cex=1)
box()