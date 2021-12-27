library(betareg)

library(boot)

# alpha
beta.0 = 0.5

# beta.1
beta.1 = 2

# beta.2
beta.2 = 0.8

# phi
phi = 30

# sample size
n = 100

# covariate
#x.1
x.1 =  runif(n)
# x.2
x.2 =  rnorm(n)

# linear predictor
eta = beta.0+beta.1*x.1+beta.2*x.2


# mu = g^1(eta) = inv.logit(eta)
mu = inv.logit(eta)

# p = mu*phi
p = mu*phi

# q = (1-mu)*phi
 q = (1-mu)*phi

# response beta
y=NULL
 for(i in 1:n){
  temp = rbeta(1, shape1 = p[i], shape2 = q[i] )
  y =c(y,temp)
 }
y

data = cbind(y=y, x.1=x.1, x.2=x.2)

write.table(data, "instanciabeta2.txt", sep="\t")
