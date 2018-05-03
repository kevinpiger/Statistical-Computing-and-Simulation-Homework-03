library(dplyr)
options(scipen = 999)
set.seed(15) # 3 + 12
# Question 2
# define function
Q2 <- function(x){
 y <- (exp(x)-1)/(exp(1)-1)
 return(y)}

x = seq(0,1,by = 0.001)
fx = Q2(x)
data <- data.frame(x = x, fx = Q2(x))

ggplot()+
  geom_line(data = data,aes(x = x, y = fx))+
  geom_vline(xintercept = 1,linetype = 2)+
  geom_ribbon(data = data, aes(x = x,ymin = 0, ymax = fx), fill = "#00FF00",alpha = 0.7)+
  labs(title = "Question 02")

N = 1000
# Monte-Carlo Integration -------------------------------------------------

theta.hat = NULL
for(i in 1:N){
x <- runif(N)
theta.hat[i] <- mean(Q2(x))}
Mc.mean <- mean(theta.hat)
Mc.var <-var(theta.hat)

# Antithetic Variate ------------------------------------------------------

theta.hat = NULL
for(i in 1:N){
x <- runif(N/2)
y <- 1 - x
temp1 <- mean(Q2(x))
temp2 <- mean(Q2(y))
theta.hat[i] <- 0.5*(temp1+temp2)}
AV.mean <- mean(theta.hat)
AV.var <- var(theta.hat)

# Importance Sampling -----------------------------------------------------
# 決定哪個好
theta.hat1 = NULL
theta.hat2 = NULL
theta.hat3 = NULL
theta.hat4 = NULL
for( i in 1:N){
  
  u <- runif(N)     #f3, inverse transform method
  x <- - log(1 - u * (1 - exp(-1)))
  fg <- Q2(x) / (exp(-x) / (1 - exp(-1)))
  theta.hat1[i] <- mean(fg)
  
  u <- runif(N/2)     #f3, inverse transform method + Antithetic Variate
  v <- 1 - u
  x1 <- - log(1 - u * (1 - exp(-1)))
  x2 <- - log(1 - v * (1 - exp(-1)))
  fg1 <- Q2(x1) / (exp(-x1) / (1 - exp(-1)))
  fg2 <- Q2(x2) / (exp(-x2) / (1 - exp(-1)))
  fg <- 0.5*(fg1+fg2)
  theta.hat2[i] <- mean(fg)

  u <- runif(N)    #f4, inverse transform method
  x <- tan(pi * u / 4)
  fg <- Q2(x) / (4 / ((1 + x^2) * pi))
  theta.hat3[i] <- mean(fg)

  u <- runif(N/2)    #f4, inverse transform method + Antithetic Variate
  v <- 1 - u
  x1 <- tan(pi * u / 4)
  x2 <- tan(pi * v / 4)
  fg1 <- Q2(x1) / (4 / ((1 + x1^2) * pi))
  fg2 <- Q2(x2) / (4 / ((1 + x2^2) * pi))
  fg <- 0.5*(fg1+fg2)
  theta.hat4[i] <- mean(fg)
}

mean11 <- c(mean(theta.hat1 ),mean(theta.hat2 ),mean(theta.hat3 ),mean(theta.hat4 ))
var11 <- c(var(theta.hat1 ),var(theta.hat2 ),var(theta.hat3 ),var(theta.hat4))
Is.mean1 <- mean(theta.hat3 )
Is.mean2 <- mean(theta.hat4)
Is.var1 <- var(theta.hat3)
Is.var2 <- var(theta.hat4)
# Control variate ---------------------------------------------------------
f <- function(x){
  x}
theta.hat = NULL
for( i in 1:N){
  u <- runif(10000)
  B <- f(u)
  A <- Q2(u)
  a <- -cov(A,B) / var(B)
  u <- runif(N)
  T1 <- Q2(u)
  T2 <- T1 + a * (f(u) - 1/2)
  theta.hat[i] <- mean(T2)
  }
Cv.mean <- mean(theta.hat)
Cv.var <- var(theta.hat)

# Stratified Sampling -----------------------------------------------------
Q2 <- function(x){
  y <- (exp(x)-1)/(exp(1)-1)
  return(y)}
N <- 1000
SS = NULL
for(i in 1:N){
  x1 <- runif(N/5, 0, 0.2)
  x2 <- runif(N/5, 0.2, 0.4)
  x3 <- runif(N/5, 0.4, 0.6)
  x4 <- runif(N/5, 0.6, 0.8)
  x5 <- runif(N/5, 0.8, 1)
  S1 <- mean(Q2(x1))
  S2 <- mean(Q2(x2))
  S3 <- mean(Q2(x3))
  S4 <- mean(Q2(x4))
  S5 <- mean(Q2(x5))
  SS[i] <- mean(c(S1, S2, S3, S4, S5))
}
SS.mean <- mean(SS)
SS.var <- var(SS)

tmp1 <- c(Mc.mean, AV.mean, Is.mean1,Is.mean2, Cv.mean, SS.mean)
tmp2 <- c(Mc.var, AV.var, Is.var1, Is.var2, Cv.var, SS.var)
table <- rbind(tmp1, tmp2)
rownames(table) <- c("Mean","Variance")
colnames(table) <- c("Monte-Carlo", "Antithetic"," Importance", "Importance+Antithetic","Control", "Stratified(5)")
table %>% round(., digits = 8)


