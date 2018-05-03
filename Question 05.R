# 原始資料
x <- runif(100,0,2*pi)
y <- sin(x) + rnorm(100,0,0.09)

# The Naïve Density Estimator ---------------------------------------------

NDE <- function(X,h=0.01){
  f <- sum(y < (X+h) & y > (X-h))/(2*100*h)
  return(f)
}

c <- seq(min(y),max(y),(max(y)-min(y))/99)
a <- NULL
for (i in 1:length(c)) {
  a <- c(a,NDE(c[i],h=0.2))
}

hist(y,breaks = 8,freq = F, ylim = c(0,1),xlim = c(-1.5,1.5))
par(new = T)
plot(c,a,type = "l",xlab = NULL,ylim = c(0,1),xlim = c(-1.5,1.5))

par(new = T)

s <- sort(a)
sum((s-sort(y))^2)



# Spline ------------------------------------------------------------------

ys <- smooth.spline(y,df = 100)
plot(ys$yin)
lines(ys$y)
s <- sort(ys$y)
sum((s-sort(y))^2)

# Kernel density ----------------------------------------------------------
K <- density(y, n= 100)
hist(y, freq = F)
lines(K)
s <- sort(K$y)
sum((s-sort(y))^2)
