library(ggplot2)
<<<<<<< HEAD
library(dplyr)
=======
# 1.	First, simulate 100 observations from a mixed distribution of beta(2,3)
# , each with probability 0.5. Then, use at least 3 density estimating methods
# to smooth the observations. 
# You need to specify the parameters in the smoothing methods, 
# and compare the results. 

>>>>>>> c66219c34e51a361f9ed756f0bb4e9d5b6236d13
set.seed(106354012)

x <- rbeta(100, shape1 = 2, shape2 = 3, ncp = 0)


# Histogram density -------------------------------------------------------
# 決定每個組寬，越小越好，但這跟樣本數有關，如何決定最佳組寬
hist(x,freq=FALSE)
lines(density(x), col="red")
H <- x %>% as.data.frame()
ggplot(H, aes(x=H$.)) +
  geom_histogram(aes(y = ..density..), binwidth=density(H$.)$bw) +
  theme_bw() +
  xlab('') +
  ylab('')

# The Naïve Density Estimator ---------------------------------------------
# moving-window histogram.

NDE <- function(data,X,h=0.01){
  f <- sum(data < (X+h) & data > (X-h))/(2*100*h)
  return(f)
}
hist(x)
c <- seq(min(x),max(x),(max(x)-min(x))/99)

a <- NULL
for (i in 1:length(c)) {
  a <- c(a,NDE(x,c[i],h=0.2))}
a
hist(x,breaks = 8,freq = F,ylim = c(0,3), xlim = c(0,1), xlab = NULL, ylab = NULL)
par(new = T)
plot(c,a,type = "l", ylim = c(0,3), xlim = c(0,1))

s <- sort(a)
sum((s-sort(x))^2)



# Kernel density ----------------------------------------------------------
hist(x,freq=FALSE)
lines(density(x), col="red")
H <- x %>% as.data.frame()
k <- density(x,bw = 0.05,kernel = "gaussian") # 設定組寬為1, kernel = Gauss

addmse = function(data,x){
  ys <- density(data,bw = x,n = 80)
  mse <- sum((sort(ys$y)-sort(data))^2)
  return(mse)
}

ggplot(H, aes(x=H$.)) +
  geom_histogram(aes(y = ..density..), binwidth=k$bw) +
  geom_density(fill="white", alpha = 0.2) +
  theme_bw() +
  xlab('') +
  ylab('')

# Spline ------------------------------------------------------------------

addmse = function(data,x){
  ys <- density(data,bw = x,n = 80)
  mse <- sum((sort(ys$y)-sort(data))^2)
  return(mse)}

ys <- smooth.spline(x,df = 20)

