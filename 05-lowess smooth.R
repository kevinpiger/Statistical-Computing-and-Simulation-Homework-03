# lowess
library(plyr)
library(dplyr)
set.seed(106354012)

a <- seq(0,2*pi, length=100)
b <- sin(a) + rnorm(100,0,0.09)
c <- sin(a)

# 計算f值為0.23
lowess(x = a, y = b, f = 0.23)

# plot
data <- lowess(x = a, y = b, f = 0.23) %>% as.data.frame()
data <- cbind(b,c,data)%>% as.data.frame()
ggplot(data,aes(x=x))+ labs(title="lowess smooth of sin(x) [f=0.23]",x="x",y="sin(x)")+
  geom_point(aes(y=b))+
  geom_vline(xintercept = 0,size=1)+
  geom_hline(yintercept = 0,size=1)+
  geom_line(aes(y=c),col="#00D0FF",lwd=1)+
  geom_line(aes(y=y),col="blue",lwd=1)+
  scale_x_continuous(breaks = c(0:2*pi))+
  theme(panel.background = element_rect(colour = "black",size=2))+
  theme(panel.grid.major = element_line(NA),panel.grid.minor =element_line(NA))+
  theme(plot.title = element_text(size = 30, face = "bold"))+
  theme(legend.title=element_text(size=24))+
  theme(legend.text=element_text(size=20))

# MSE
MSE <- c()
for (i in 1:1000) {
  b <- NULL
  b <- sin(seq(0,2*pi, length=100)) + rnorm(100,0,0.09)
  b1 <- lowess(x = a, y = b, f = 0.23)$y
  MSE[i] <- mean((b1-c)^2)
}
MSE <- mean(MSE) ; MSE


# plot
data <- lowess(x = a, y = b, f = 0.23) %>% as.data.frame()
data <- cbind(b,c,data)%>% as.data.frame()
ggplot(data,aes(x=x))+ labs(title="simulation of sin(x)",x="x",y="sin(x)")+
  geom_point(aes(y=b))+
  geom_vline(xintercept = 0,size=1)+
  geom_hline(yintercept = 0,size=1)+
  geom_line(aes(y=b),col="royalblue",lwd=1.27)+
  scale_x_continuous(breaks = c(0:2*pi))+
  theme(panel.background = element_rect(colour = "black",size=2))+
  theme(panel.grid.major = element_line(NA),panel.grid.minor =element_line(NA))+
  theme(plot.title = element_text(size = 30, face = "bold"))+
  theme(legend.title=element_text(size=24))+
  theme(legend.text=element_text(size=20))
