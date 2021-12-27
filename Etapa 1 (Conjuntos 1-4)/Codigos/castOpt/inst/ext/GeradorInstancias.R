library(betareg)

library(boot)

# alpha
beta.0 = 0.1

# beta.1
beta.1 = 1.5

# beta.2
beta.2 = 0.5

# phi
phi = 15

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

write.table(data, "instanciabeta.txt", sep="\t")

getInstance <- function(betaVector, phi, N, seed_X = NULL, seed_Y = NULL){
  # Convariate
  xSize <- length(betaVector)
  if (!is.null(seed_X)) set.seed(seed_X)
  xMat <- matrix(c(rep(1, N), runif((xSize - 1)*N)), nrow = N, ncol = xSize, byrow = FALSE)
  # Linear Predictor
  eta = xMat %*% matrix(betaVector, ncol = 1)
  # mu = g^1(eta) = inv.logit(eta)
  ###
  inv.logit <- function(x)
    {
        out <- exp(x)/(1+exp(x))
        out[x==-Inf] <- 0
        out[x==Inf] <- 1
        out
    }
  ###
  mu =  inv.logit(eta)

  # p = mu*phi
  p = mu*phi

  # q = (1-mu)*phi
  q = (1-mu)*phi

  # response beta
  y=NULL
  for(i in 1:N){
    if (!is.null(seed_Y)) {
      set.seed(seed_Y + i)
    } else {
      set.seed(as.integer(Sys.time()))
    }


    temp = rbeta(1, shape1 = p[i], shape2 = q[i] )
    y =c(y,temp)
  }
  y
  return(cbind(y, xMat[,-1]))
}


