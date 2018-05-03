library(dplyr)
# pit機率積分轉換
Q3 <- function(y){
  return( -log(1-y))
}

# Hit or miss -------------------------------------------------------------
temp2 <- NULL
for(j in 1:1000){
  temp <- NULL
  for( i in 1:1000){
    tmp <- rexp(5)
    temp[i] <- sum(tmp*c(1:5))}
  temp2[j] <- sum(temp>21.6)/1000
}

mean(temp2)
var(temp2)

# Monte-Carlo Integration -------------------------------------------------

t11 <- NULL
for(i in 1:1000){
  a = 0
  for(j in 1:5){
    x <- runif(N)
    a <- a + j*Q3(x)
  }
  t11[i] <- sum(a >21.6)/1000
}
mean(t11)
var(t11)


# Antithetic Variate ------------------------------------------------------

t11 <- NULL
for(i in 1:1000){
  a = 0;b = 0
  for(j in 1:5){
    x <- runif(N/2)
    y <- 1 - x
    a <- a + j*Q3(x)
    b <- b + j*Q3(y)
  }
  temp1 <- sum(a > 21.6)/500
  temp2 <- sum(b > 21.6)/500
  t11[i] <- 0.5*(temp1+temp2)
  }
mean(t11)
var(t11)

# Importance Sampling -----------------------------------------------------
# 決定哪個好
theta.hat1 = NULL
theta.hat2 = NULL
theta.hat3 = NULL
theta.hat4 = NULL
for( i in 1:N){
  
  u <- runif(N)     #f3, inverse transform method
  x <- - log(1 - u * (1 - exp(-1)))
  fg <- Q3(x) / (exp(-x) / (1 - exp(-1)))
  theta.hat1[i] <- mean(fg)
  
  
  u <- runif(N)    #f4, inverse transform method
  x <- tan(pi * u / 4)
  fg <- Q3(x) / (4 / ((1 + x^2) * pi))
  theta.hat3[i] <- mean(fg)

}

mean11 <- c(mean(theta.hat1 ),mean(theta.hat3 ))
var11 <- c(var(theta.hat1 ),var(theta.hat3 ))
Is.mean <- mean(theta.hat3 )
Is.var <- var(theta.hat3)

# 區隔 ----------------------------------------------------------------------
# 直接爆機率形式

Q3<-function(x){
  (312.5/60)*exp((-1/5)*x)-
    (640/60)*exp((-1/4)*x)+
    (405/60)*exp((-1/3)*x)+
    (1/24)*exp((-1)*x)+
    (-4/3)*exp((-1/2)*x)
}

set.seed(15) # 3 + 12
# Hit or miss -------------------------------------------------------------
temp2 <- NULL
for(j in 1:1000){
  temp <- NULL
  for( i in 1:1000){
    tmp <- rexp(5)
    temp[i] <- sum(tmp*c(1:5))}
  temp2[j] <- sum(temp>21.6)/1000
}

Hm.mean <- mean(temp2)
Hm.var <- var(temp2)


# Monte-Carlo Integration -------------------------------------------------
N <- 1000
theta.hat <- NULL
for(i in 1:1000){
  x <- runif(N,0,21.6)
  theta.hat[i] <-1 -  mean(Q3(x)*21.6)
}
Mc.mean <- mean(theta.hat)
Mc.var <- var(theta.hat)

# Antithetic Variate ------------------------------------------------------

N <- 1000
theta.hat <- NULL
for(i in 1:1000){
  x <- runif(N/2,0,21.6)
  y <- 21.6 - x
  theta.hat[i] <- (2 - mean(Q3(x)*21.6) - mean(Q3(y)*21.6))*0.5
}
Av.mean <- mean(theta.hat)
Av.var <- var(theta.hat)
# Stratified Sampling -----------------------------------------------------

N <- 1000
theta.hat <- NULL
for(i in 1:1000){
  tmp <- c()
  for(j in 1:5){
    x <- runif(N/5,(j - 1)/5*21.6,j/5*21.6)
    tmp <- c(tmp, Q3(x)*21.6)
  }
  theta.hat[i] <- 1 - mean(tmp)
}
SS.mean <- mean(theta.hat)
SS.var <- var(theta.hat)

table.mean <- c(Mc.mean, Av.mean, SS.mean, Hm.mean)
table.var <- c(Mc.var, Av.var, SS.var, Hm.var)
table <- rbind(table.mean, table.var)
rownames(table) <- c("mean", "var")
colnames(table) <- c("Monte", "Anti", "Stratified", "Hit")
table %>% round(., digits = 8)
