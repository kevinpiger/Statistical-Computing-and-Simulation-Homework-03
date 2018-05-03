# spline

library(plyr)
library(dplyr)
library(broom)


# plot
df_values <- c(3, 4, 8, 16)

x = seq(from=0, to=2*pi, length=100)
f_x = sin(x)
epsilon = rnorm(100, 0, sd = 0.3)
y = f_x + epsilon

values <- data_frame(
  x = seq(from=0, to=2*pi, length=100),
  f_x = sin(x),
  epsilon = rnorm(100, 0, sd = 0.3),
  y = f_x + epsilon
)
overall <- NULL
for(df in df_values){
  overall <- smooth.spline(values$x, values$y, df=df) %>%
    augment() %>%
    mutate(df=df) %>%
    bind_rows(overall)
}
overall <- cbind(overall,f_x) %>% as.data.frame()
multiple_df <- overall %>% 
  ggplot(aes(x=x)) +
  geom_point(aes(y=y))+
  geom_line(aes(y=.fitted,color="#123456"),size=1) +
  facet_wrap(~df, nrow=2) +
  labs(title="Splines fit w / different degrees of freedom")+
  theme(legend.title=element_blank())+
  theme(legend.text = element_blank())+
  theme(panel.background = element_rect(colour = "black"))+
  geom_vline(xintercept = 0,size=1)+
  geom_hline(yintercept = 0,size=1)+
  theme(panel.grid.major = element_line(NA),panel.grid.minor =element_line(NA))+
  theme(panel.background = element_rect(color='#000000',size=2))+
  theme(plot.title = element_text(size = 30, face = "bold"))

multiple_df

get.spline.info<-function(object){
  data.frame(x=object$x,y=object$y,df=object$df)
}

# plot

a <- seq(0,2*pi, length=100)
b <- sin(seq(0,2*pi, length=100)) + rnorm(100,0,0.09)
c <- sin(seq(0,2*pi, length=100))

spline1 <- smooth.spline(x = b, df = 3)
spline2 <- smooth.spline(x = b, df = 4)
spline3 <- smooth.spline(x = b, df = 8)
spline4 <- smooth.spline(x = b, df = 16)

data<-ldply(list(spline1,spline2,spline3,spline4),get.spline.info)
data <- cbind(a,b,c,data) %>% as.data.frame()
ggplot(data,aes(x=a))+ labs(title="Spline smoother of sin(x)") +
  geom_line(aes(y=c),col="gray",size=2)+
  geom_point(aes(y=b))+
  geom_vline(xintercept = 0,size=1)+
  geom_hline(yintercept = 0,size=1)+
  geom_line(mapping = aes(y=y,color=factor(round(df,0)),group=df),lwd=1.2)+
  scale_color_discrete("The number of nodes")+
  theme(legend.key.size = unit(1.5, "line"))+
  theme(plot.title = element_text(size = 30, face = "bold"))+
  theme(legend.title=element_text(size=24))+
  theme(legend.text=element_text(size=20))+
  theme(legend.position = c(0.8,0.8))+
  theme(legend.background = element_rect(fill="#FFFFF0",colour = "black"))+
  theme(panel.grid.major = element_line(NA),panel.grid.minor =element_line(NA))+
  theme(panel.background = element_rect(colour = "black",size=2))
 
# 最後我們將各節點數分別計算MSE

spline1$y
# ns=3
d <- c()
for (i in 1:1000) {
  b <- NULL
  b <- sin(seq(0,2*pi, length=100)) + rnorm(100,0,0.09)
  b1 <- smooth.spline(x = b,df=3)$y
  d <- rbind(d,b1)
}
# MSE
bias <- c()
var <- c()
for (j in 1:100) {
  bias[j] = mean(d[j,])-c[j] 
  var[j] = var(d[j,])
}
MSE <- c()
MSE[1] <- sum(bias^2)+sum(var) ; MSE

# nodes=4
d <- c()
for (i in 1:1000) {
  b <- NULL
  b <- sin(seq(0,2*pi, length=100)) + rnorm(100,0,0.09)
  b1 <- smooth.spline(x = b,df=4)$y
  d <- rbind(d,b1)
}
# MSE
bias <- c()
var <- c()
for (j in 1:100) {
  bias[j] = mean(d[j,])-c[j] 
  var[j] = var(d[j,])
}
MSE[2] <- sum(bias^2)+sum(var) ; MSE

# nodes=8
d <- c()
for (i in 1:1000) {
  b <- NULL
  b <- sin(seq(0,2*pi, length=100)) + rnorm(100,0,0.09)
  b1 <- smooth.spline(x = b,df=8)$y
  d <- rbind(d,b1)
}
# MSE
bias <- c()
var <- c()
for (j in 1:100) {
  bias[j] = mean(d[j,])-c[j] 
  var[j] = var(d[j,])
}
MSE[3] <- sum(bias^2)+sum(var) ; MSE

# nodes=16
d <- c()
for (i in 1:1000) {
  b <- NULL
  b <- sin(seq(0,2*pi, length=100)) + rnorm(100,0,0.09)
  b1 <- smooth.spline(x = b,df=16)$y
  d <- rbind(d,b1)
}
# MSE
bias <- c()
var <- c()
for (j in 1:100) {
  bias[j] = mean(d[j,])-c[j] 
  var[j] = var(d[j,])
}
MSE[4] <- sum(bias^2)+sum(var) ; MSE

# MSE比較
MSE
# 我們發現當節點為3的時候MSE較小其曲線也較為平滑 (怪怪)
