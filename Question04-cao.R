# question04 自己寫函數

# 這是不想用套件的人寫的code~

set.seed(106354012)
x <- rbeta(100,2,3)
h=0.1

# histogram density estimator (直方圖)

histogram = function(x,h){
  g = function(x,a,b){
    if (x<=b & x>=a) {return(1)}
    else {return(0)}}            # 寫出一個計算個數前需要的函數
  n <- length(x)
  seq <- seq(min(x),max(x),by=h) # 從(0,1)間隔h做出切割點
  a = seq[-length(seq)]          # 每組下界
  b = seq[-1]                    # 每組上界
  ni = NULL
  for (k in 1:length(a)){
    ni[k] = sum(x<=b[k] & x>=a[k])}
  y_hat = NULL
  for (i in sort(x)){
    I=NULL
    for (j in 1:length(a)){
      gi=g(i,a[j],b[j])
      I=c(I,gi)}                 # 指標函數
    y=1/n*sum(ni/h*I)            # 模擬樣本之函數估計值 
    y_hat=c(y_hat,y)}
  return(y_hat)}                 

# naive density estimator
naive =function(x,h){
  w = function(y){
    if (abs(y)<1) {return(1/2)}
    else {return(0)}}               # 寫出公式裡的函數w
  n = length(x)
  seq = seq(min(x),max(x),length=length(x))        
  y_hat = NULL
  for (i in seq){
    W=NULL
    for (j in x){
      wi=w((i-j)/h)                        
      W=c(W,wi)}
    y=1/n*sum(1/h*W)                # 樣本之函數估計值           
    y_hat=c(y_hat,y)}
  return(y_hat)}

# kernel density estimator
# norm
kernel_norm=function(x,h){
  w=function(y){dnorm(y)}           # 核密度函數(常態)
  n = length(x)
  seq = seq(min(x),max(x),length=length(x))
  y_hat = NULL
  for (i in seq){
    W=NULL
    for (j in x){
      wi=w((i-j)/h)
      W=c(W,wi)}
    y=1/n*sum(1/h*W)
    y_hat=c(y_hat,y)}
  return(y_hat)}

# Gamma核函數
kernel_gamma=function(x,h){
  w=function(y){dgamma(y,shape = 1)}           # 核密度函數(Gamma)
  n = length(x)
  seq = seq(min(x),max(x),length=length(x))
  y_hat = NULL
  for (i in seq){
    W=NULL
    for (j in x){
      wi=w((i-j)/h)
      W=c(W,wi)}
    y=1/n*sum(1/h*W)
    y_hat=c(y_hat,y)}
  return(y_hat)}

# 將亂數帶入函數找出估計函數值(h=0.1)
y1 <- histogram(x,0.1)              #h=0.1，也可使用其他h值
y2 <- naive(x,0.1)                  #h=0.1，也可使用其他h值
y3 <- kernel_norm(x,0.1)            #h=0.1，也可使用其他h值
y4 <- kernel_gamma(x,0.1)           #h=0.1，也可使用其他h值
# 畫圖囉
xx <- sort(x)
yy <- dbeta(xx,2,3)
y2 <- naive(xx,0.1)
y3 <- kernel_norm(xx,0.1)
y4 <- kernel_gamma(xx,0.1)
data <- cbind(xx,yy,y2,y3,y4) %>% as.data.frame()
colnames(data) <- c("sample","Beta(2,3)","naive","kernal(norm)","kernal(gamma)")
LBJ <- gather(data,key = "type",value = "value",2:5)
colnames(LBJ) <- c("sample","LineType","value")  
library(magrittr)
LBJ$LineType %<>% as.factor()
library(ggplot2)
ggplot(data = LBJ) + labs(title = "Density Estimation (h=0.1)")+
  xlim(0,1)+ ylim(0,2.5) +
  geom_histogram(mapping = aes(x=sample,y=..density..),color="black",fill="gray",binwidth = 0.1)+
  geom_line(mapping = aes(x=sample,y=value,color=LineType,group=LineType),size=1.2)+
  theme(legend.title = element_text(colour="royalblue", size=20, face="bold"))+
  theme(legend.text = element_text(size = 16))+
  theme(legend.position = c(0.87,0.6))+
  theme(legend.background = element_rect(fill="#FFFFF0",size=1, linetype="solid",colour ="darkblue"))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(panel.background = element_rect(fill="#EEFFFF",colour="black",size = 2))

# 畫圖囉 (h=0.2)
xx <- sort(x)
yy <- dbeta(xx,2,3)
y1 <- histogram(xx,0.2)
y2 <- naive(xx,0.2)
y3 <- kernel_norm(xx,0.2)
y4 <- kernel_gamma(xx,0.2)
data <- cbind(xx,yy,y2,y3,y4) %>% as.data.frame()
colnames(data) <- c("sample","Beta(2,3)","naive","kernal(norm)","kernal(gamma)")
LBJ <- gather(data,key = "type",value = "value",2:5)
colnames(LBJ) <- c("sample","LineType","value")  
library(magrittr)
LBJ$LineType %<>% as.factor()
plot(sort(x),y1,typ="l",xlab="x",ylab="f(x)",xlim=c(0,1), ylim=c(0,3))
library(ggplot2)
ggplot(data = LBJ) + labs(title = "Density Estimation (h=0.2)")+
  xlim(0,1)+ ylim(0,2.5) +
  geom_histogram(mapping = aes(x=sample,y=..density..),color="black",fill="gray",binwidth = 0.2)+
  geom_line(mapping = aes(x=sample,y=value,color=LineType,group=LineType),size=1.2)+
  theme(legend.title = element_text(colour="royalblue", size=20, face="bold"))+
  theme(legend.text = element_text(size = 16))+
  theme(legend.position = c(0.87,0.6))+
  theme(legend.background = element_rect(fill="#FFFFF0",size=1, linetype="solid",colour ="darkblue"))+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  theme(panel.background = element_rect(fill="#EEFEFF",colour="black",size = 2))
