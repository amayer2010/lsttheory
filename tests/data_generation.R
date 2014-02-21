
library(MASS)

########### dataset: CTT model for software appendix ###########

set.seed(222999)

N <- 300
eta <- rnorm(N,2,2)
y1 <- eta + rnorm(N,0,0.7)
y2 <- 0.5 + 0.8*eta + rnorm(N,0,0.6)
y3 <- 0.9 + 1.1*eta + rnorm(N,0,0.7)

taucong <- data.frame(y1,y2,y3)

save(taucong, file="../data/taucong.RData")


################## dataset: multistate model for software appendix ###################

set.seed(8882211)

N <- 400
sigma <- matrix(c(3,2,2,3),nrow=2,byrow=T)
mu <- c(0.5,1)

d <- mvrnorm(N,mu,sigma)

eta1 <- d[,1]
eta2 <- d[,2]

y11 <- eta1 + rnorm(N,0,0.5)
y21 <- 0.5 + 0.8*eta1 + rnorm(N,0,0.5)

y12 <- eta2 + rnorm(N,0,0.6)
y22 <- 0.7 + 0.9*eta2 + rnorm(N,0,0.5)

multistate <- data.frame(y11,y21,y12,y22)
save(multistate, file="../data/multistate.RData")



################## dataset: multistate model (older version with 3 time points) ###################

set.seed(3423532)

N <- 1000
sigma <- matrix(c(3,2,2,2,3,2,2,2,3),nrow=3,byrow=T)
mu <- c(0.5,1,0.5)

d <- mvrnorm(N,mu,sigma)

eta1 <- d[,1]
eta2 <- d[,2]
eta3 <- d[,3]

y11 <- 0 + 1*eta1 + rnorm(N)
y21 <- 0.3 + 1.2*eta1 + rnorm(N)
y31 <- 0.6 + 0.8*eta1 + rnorm(N)

y12 <- 0 + 1*eta2 + rnorm(N)
y22 <- 0.3 + 1.2*eta2 + rnorm(N)
y32 <- 0.6 + 0.8*eta2 + rnorm(N)

y13 <- 0 + 1*eta3 + rnorm(N)
y23 <- 0.3 + 1.2*eta3 + rnorm(N)
y33 <- 0.6 + 0.8*eta3 + rnorm(N)

multistate02 <- data.frame(y11,y21,y31,y12,y22,y32,y13,y23,y33)

save(multistate02, file="../data/multistate02.RData")



################ dataset: multitrait-multistate model ###################

set.seed(8454312)

N <- 1000
sigma <- matrix(c(3,2,2,3),nrow=2,byrow=T)
mu <- c(0.5,1)

d <- mvrnorm(N,mu,sigma)

xi1 <- d[,1]
xi2 <- d[,2]

eta1 <- 0 + 1*xi1 + rnorm(N)
eta2 <- 0.4 + 0.7*xi1 + rnorm(N)
eta3 <- 0 + 1*xi2 + rnorm(N)
eta4 <- 0.4 + 0.7*xi2 + rnorm(N)

y11 <- 0 + 1*eta1 + rnorm(N)
y21 <- 0.3 + 1.2*eta1 + rnorm(N)

y12 <- 0 + 1*eta2 + rnorm(N)
y22 <- 0.3 + 1.2*eta2 + rnorm(N)

y13 <- 0 + 1*eta3 + rnorm(N)
y23 <- 0.3 + 1.2*eta3 + rnorm(N)

y14 <- 0 + 1*eta4 + rnorm(N)
y24 <- 0.3 + 1.2*eta4 + rnorm(N)

x <- rbinom(N,1,plogis(xi2, location=1))

multitraitmultistate <- data.frame(y11,y21,y12,y22,y13,y23,y14,y24)
save(multitraitmultistate, file="../data/multitraitmultistate.RData")



################ dataset: multigroup-multitrait-multistate model ###################

set.seed(222999)

N <- 1000
x <- rbinom(N,1,0.5)
xi1 <- rnorm(N,0.5,sqrt(3))
xi2 <- 0.7 + 0.8*xi1 + 1*x + 0.4*x*xi1 + rnorm(N,0,1)

eta1 <- 0 + 1*xi1 + rnorm(N)
eta2 <- 0.4 + 0.7*xi1 + rnorm(N)
eta3 <- 0 + 1*xi2 + rnorm(N)
eta4 <- 0.4 + 0.7*xi2 + rnorm(N)

y11 <- 0 + 1*eta1 + rnorm(N)
y21 <- 0.3 + 1.2*eta1 + rnorm(N)

y12 <- 0 + 1*eta2 + rnorm(N)
y22 <- 0.3 + 1.2*eta2 + rnorm(N)

y13 <- 0 + 1*eta3 + rnorm(N)
y23 <- 0.3 + 1.2*eta3 + rnorm(N)

y14 <- 0 + 1*eta4 + rnorm(N)
y24 <- 0.3 + 1.2*eta4 + rnorm(N)


multitraitmultistate02 <- data.frame(y11,y21,y12,y22,y13,y23,y14,y24,x)
save(multitraitmultistate02, file="../data/multitraitmultistate02.RData")

