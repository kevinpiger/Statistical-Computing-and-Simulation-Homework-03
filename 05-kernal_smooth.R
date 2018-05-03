# kernel smooth
library(plyr)
library(dplyr)
library(KernSmooth)

set.seed(106354012)

a <- seq(0,2*pi, length=100)
b <- sin(a) + rnorm(100,0,0.09)
c <- sin(a)

# kernel smooth (kernel is norm)
data <- ksmooth(a, b, kernel = "normal", bandwidth = 0.1)%>% as.data.frame()
data <- cbind(b,c,data)%>% as.data.frame()
ggplot(data,aes(x=x))+ labs(title="kernel smooth of sin(x) [h=0.1]",x="x",y="sin(x)")+
  geom_point(aes(y=b),col="#3333FF")+
  geom_vline(xintercept = 0,size=1)+
  geom_hline(yintercept = 0,size=1)+
  geom_line(aes(y=y),col="#00D0FF",lwd=1)+
  scale_x_continuous(breaks = c(0:2*pi))+
  theme(panel.grid.major = element_line(NA),panel.grid.minor =element_line(NA))+
  theme(panel.background = element_rect(color = "black",size = 2))+
  theme(plot.title = element_text(size = 30, face = "bold"))+
  theme(legend.title=element_text(size=24))+
  theme(legend.text=element_text(size=20))

# MSE
d <- c()
e <- c()
for(j in 1:1000){
  b <- sin(a) + rnorm(100,0,0.09)
  d <- ksmooth(a, b, kernel = "normal", bandwidth = 0.1)
  e <- rbind(e,d$y)
}
bias <- c()
var <- c()
for(i in 1:100){
  bias[i] = mean(e[i,])-c[i] 
  var[i] = var(e[i,])
}
MSE <- sum(bias^2)+sum(var) ; MSE


