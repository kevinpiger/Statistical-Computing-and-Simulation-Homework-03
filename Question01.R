library(dplyr)
library(ggplot2)
options(scipen = 100)
set.seed(15) # 3 + 12
# Question 1
# X ~ Cauchy(0,1)
# 直接算機率
1 - pcauchy(1)
x = seq(0,3,by = 0.001)
fx = dcauchy(x)
data <- data.frame(x = x, Cauchy = dcauchy(x))
data <- mutate(data,new = ifelse(x > 1, "blue", "white"))
temp1 <- data[1:1000,]
temp2 <- data[1001:length(x),]

ggplot()+
  geom_line(data = data,aes(x = x, y = Cauchy))+
  geom_vline(xintercept = 1,linetype = 2)+
  geom_ribbon(data = temp1, aes(x = x,ymin = 0, ymax = Cauchy), fill = "white",alpha = 0.5)+
  geom_ribbon(data = temp2, aes(x = x,ymin = 0, ymax = Cauchy), fill = "#FF8888", alpha = 0.7)+
  labs(title = "P(X > 1)")

ggplot()+
  geom_line(data = data,aes(x = x, y = Cauchy))+
  geom_vline(xintercept = 1,linetype = 2)+
  geom_ribbon(data = temp1, aes(x = x,ymin = 0, ymax = Cauchy), fill = "#FF8888",alpha = 0.5)+
  geom_ribbon(data = temp2, aes(x = x,ymin = 0, ymax = Cauchy), fill = "white", alpha = 0.7)+
  labs(title = "P(0 < X <= 1)")


# Cauchy為中心點為0的對稱分佈
# P(X>1)=0.25、P(0<X<1)=0.25
# 接著我們使用下列方法來降低變異數
# 都取1000個theta.hat去估計theta
N = 1000
# Monte-Carlo Integration -------------------------------------------------

cauchy <- function(x){
  return(1/(pi*(1+x^2)))
}
theta.hat <- NULL
for(i in 1:1000){
  # 從cauchy抽取樣本
  x <- runif(N)
  x <- cauchy(x)
  theta.hat[i]<-0.5 - mean(x)}
Mc.mean <- mean(theta.hat)
Mc.var <- var(theta.hat)
    
# Hit or miss1 ------------------------------------------------------------


theta.hat <- NULL
for ( i in 1:1000){
x <- rcauchy(N)
theta.hat[i] <- mean(x > 1)}
Hm1.mean <- mean(theta.hat)
Hm1.var <- var(theta.hat)


# Hit or miss2 ------------------------------------------------------------


theta.hat = NULL
for(i in 1:1000){
  x <- rcauchy(1000) %>% abs()
  theta.hat[i] <- 0.5*mean(x > 1)
}
Hm2.mean <- mean(theta.hat)
Hm2.var <- var(theta.hat)

# Hit or miss3 ------------------------------------------------------------


theta.hat <- NULL
cauchy <- function(x){
  return(1/(pi*(1+x^2)))}
for(i in 1:1000){
  x <- runif(N)
  theta.hat[i] <- (1- mean(2*cauchy(x)))/2
  }
Hm3.mean <- mean(theta.hat)
Hm3.var <- var(theta.hat)

# Antithetic Variate ------------------------------------------------------

for( i in 1:1000){
    x <- runif(N/2)
    y <- 1 - x
    temp1 <- cauchy(x)
    temp2 <- cauchy(y)
    theta.hat[i] <-0.5 - 0.5*(mean(temp1)+mean(temp2))}
  AV.mean <- mean(theta.hat)
  AV.var <- var(theta.hat)

# Importance Sampling -----------------------------------------------------

hes <- function(x){
  return(1/(pi*(1 + x^(-2))))
} 

theta.hat <- NULL
for(i in 1:1000){
  U <- runif(N)
  x <- 1/U
  theta.hat[i] <- mean(hes(x))
}
Is.mean <- mean(theta.hat)
Is.var <- var(theta.hat)

# Control variate ---------------------------------------------------------
# 令另一個function為1/(1+x)

f <- function(x){
  return(1/(1+x))
  }
cauchy <- function(x){
  return(1/(pi*(1+x^2)))
}
theta.hat = NULL
for(i in 1:N){
u <- runif(1000)
B <- f(u)
A <- cauchy(u)
a <- -cov(A,B) / var(B) #est of c*

x <- runif(N)
T1 <-0.5 -  cauchy(x)
theta.hat[i]<-  0.5 - mean(T1 + a * (f(x) -log(2, base = exp(1))))
}
Cv.mean <- mean(theta.hat)
Cv.var <- var(theta.hat)
# Stratified Sampling -----------------------------------------------------
# 切5層

cauchy <- function(x){
  return(1/(pi*(1+x^2)))
}

SS = NULL
for(i in 1:N){
x1 <- runif(N/5, 0, 0.2)
x2 <- runif(N/5, 0.2, 0.4)
x3 <- runif(N/5, 0.4, 0.6)
x4 <- runif(N/5, 0.6, 0.8)
x5 <- runif(N/5, 0.8, 1)
S1 <- 0.5 - mean(cauchy(x1))
S2 <- 0.5 - mean(cauchy(x2))
S3 <- 0.5 - mean(cauchy(x3))
S4 <- 0.5 - mean(cauchy(x4))
S5 <- 0.5 - mean(cauchy(x5))
SS[i] <- mean(c(S1, S2, S3, S4, S5))
}
SS.mean <- mean(SS)
SS.var <- var(SS)

tmp1 <- c(Mc.mean, Hm1.mean, Hm2.mean, Hm3.mean, AV.mean, Is.mean, Cv.mean, SS.mean)
tmp2 <- c(Mc.var, Hm1.var, Hm2.var, Hm3.var, AV.var, Is.var, Cv.var, SS.var)
table <- rbind(tmp1, tmp2)
rownames(table) <- c("Mean","Variance")
colnames(table) <- c("Monte-Carlo", "Hit or miss1", "Hit or miss2", "Hit or miss3", "Antithetic",
                     " Importance", "Control", "Stratified(5)")
table %>% round(., digits = 8)

