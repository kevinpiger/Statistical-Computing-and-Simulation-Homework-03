##統模擬五
require(plyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(igraph)

# running mean
set.seed(106354012)

a <- seq(0,2*pi, length=100)
b <- sin(seq(0,2*pi, length=100)) + rnorm(100,0,0.09)
c <- sin(seq(0,2*pi, length=100))

mse = NULL
for (k in 1:20){
r <- running_mean(b, binwidth=k)
x = NULL
for(i in 1:(100-k+1)){
x[i] <- mean(a[i:(i+k-1)])
}
mse[k] <- mean((sin(x)-r)^2)
num = which(mse==min(mse))
}
mse
paste("the minimum mse of k is",num)
paste("the minimum mse is",min(mse))

# plot

b <- running_mean(b, binwidth=which(mse==min(mse)))
plot(b)
model <- lm(b ~ poly(seq(0,2*pi, length=100-num+1),3))
y <- fitted.values(model)
lines(fitted.values(model))  # 模擬出來的資料線段
lines(c)                     # 真實的線段
data <- cbind(a[1:length(b)],b,c[1:length(b)])%>%as.data.frame()
colnames(data) <- c("x","running mean","sin(x)")
library(magrittr)
data2 <- gather(data,key = "type",value = "value",2:3)
data2$type %<>% as.factor()
ggplot(data2)+ labs(title = "Running mean of sin(x)")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlim(0,2*pi)+ ylim(-1,1) +
  geom_vline(xintercept = 0,size=1)+
  geom_hline(yintercept = 0,size=1)+
  geom_line(mapping = aes(x=x,y=value,color=type,group=type),lwd=1.87)+
  geom_point(mapping = aes(x=x,y=value),color="blue",size=1)+
  theme(legend.text = element_text(size = 16))+
  theme(legend.position = c(0.8,0.8))+
  theme(legend.background = element_rect(size=0.5, linetype="solid",fill ="#FFFFF0",colour ="black"))+
  theme(panel.background = element_rect(color='#000000',size=2))+
  theme(plot.title = element_text(size = 30, face = "bold"))+
  theme(legend.title=element_text(size=24))+
  theme(legend.text=element_text(size=20))
  
# 1,000 simulation runs ( 設定k=2 )

a <- seq(0,2*pi, length=100)
b <- sin(seq(0,2*pi, length=100)) + rnorm(100,0,0.09)
c <- sin(seq(0,2*pi, length=100))
d <- c()
for (i in 1:1000) {
b <- NULL
b <- sin(seq(0,2*pi, length=100)) + rnorm(100,0,0.09)
b1 <- running_mean(b, binwidth=2)
d <- rbind(d,b1)
}
# MSE
bias <- c()
var <- c()
for (j in 1:100) {
  bias[j] = mean(d[j,])-c[j] 
  var[j] = var(d[j,])
}
MSE <- sum(bias^2)+sum(var) ; MSE

