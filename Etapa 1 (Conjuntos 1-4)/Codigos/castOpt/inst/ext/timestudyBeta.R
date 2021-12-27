pkgnames <- c("GA",
              "rgenoud",
              "DEoptim",
              "soma",
              "cmaes",
              "GenSA",
              "pso",
              "NMOF",
              "nloptr",
              #"nloptwrap", # Problems in installation
              #"hydroPSO",# Problems in installation
              "Rmalschains")

## uncomment to install packages
# install.packages(pkgnames)

## hydromad is not on CRAN
#install.packages(c("zoo", "latticeExtra", "polynom", "car", "Hmisc","reshape"))
#Install hydromad
#install.packages("hydromad", repos="http://hydromad.catchment.org")

#require(hydromad)

for(i in pkgnames)
  require(i, character.only=TRUE)

require(castOpt)

objFun <- c("reg_beta")

funs <- c("ga",
          #"genoud",
          "DEoptim",
          #"soma",
          "cma_es",
          "GenSA",
          #"psoptim",
          "nloptr_crs",
          "nloptr_stogo",
          "nloptr_d",
          "nloptr_d_l",
          "nloptr_i",
          #"optim",
          #"DEopt",
          "malschains",
          #"hydroPSO",
          #"SCEoptim",
          "PSopt")

# Usar essa funcao quando nao houver opcao de Maximizar no metodo
# Colocar essa funcao dentro do pacote
goTestMax <- function(par, test_matrix, fnName=c("reg_beta")){
  fnName <- match.arg(fnName)
  sum = 0
  do.call(fnName, list(sum = sum, x=as.double(par), N=length(par), data = test_matrix))
  -1*sum
}

Replication <-  100

#ans#User <- ansSys <- ansTotal <- array(dim=c(length(objFun), length(funs), Replication),
       #                                dimnames=list(objFun,funs, NULL))

ansTime <- array(dim=c(length(objFun), length(funs), Replication),
             dimnames=list(objFun,funs, NULL))


library(readr)
test_matrix <- read_rds("inst/ext/test_matrix.rds")

ncols <- ncol(test_matrix)
nrows <- nrow(test_matrix)

# Number of Replications for each method

for(j in 1:Replication) {
  #getDefaultBounds(objFun[i]) # <- Modify
  bounds <- list(lower = c(rep(-100, ncols), 0),
                 upper = rep(100, ncols + 1))

  bMat <- cbind(bounds$lower, bounds$upper)
  par <- vector()
  checkDim <- FALSE
  for(ii in 1:length(bounds$lower))
    par <- append(par,runif(1,bounds$lower[ii],bounds$upper[ii]))


  ## ga

  goTestMin <- function(...)
    goTestBetaReg(...)

  start <- Sys.time()
  out <- try(ga(type="real-valued", fitness = goTestMin,
                fnName = objFun, test_matrix = test_matrix,
                lower=bounds$lower, upper = bounds$upper, maxiter = 245))
  end <- Sys.time()
  dif <- end - start
  if(class(out) == "try-error"){
    ansTime[1,"ga",j] <- NA
  }else
    ansTime[1,"ga",j] <- dif

  # ## rgenoud
  # evalCnt<-0
  # out <- try(system.time(genoud(fn=goTest, nvars=length(bounds$lower),
  #                               max.generations=100, print.level=0,
  #                               Domains = matrix(c(bounds$lower,bounds$upper),ncol=2),
  #                               fnName=objFun[i], checkDim=F)))
  # if(class(out) == "try-error")
  #   ansUser[i,"genoud",j] <- ansSys[i,"genoud",j] <- ansTotal[i,"genoud",j] <- NA
  # else {
  #   ansUser[i,"genoud",j] <- out[1]
  #   ansSys[i,"genoud",j] <-  out[2]
  #   ansTotal[i,"genoud",j] <- out[3]
  # }


  ## DEoptim

  ff <- 10*length(par)
  mi <- round(10000/ff)
  start <- Sys.time()
  out <- try(DEoptim(fn=goTestMax, test_matrix = test_matrix, lower=bounds$lower, upper=bounds$upper,
                     fnName=objFun, control=list(itermax=mi)))

  end <- Sys.time()
  dif <- end - start
  if(class(out) == "try-error"){
    ansTime[1,"DEoptim",j] <- NA
  }else
    ansTime[1,"DEoptim",j] <- dif

  # ## soma
  #
  # out <- try(system.time(soma(costFunction=goTest,
  #                             bounds=list(min=bounds$lower,
  #                                         max=bounds$upper),
  #                             options = list(nMigrations=180),
  #                             fnName=objFun[i], checkDim=F)))
  # if(class(out) == "try-error")
  #   ansUser[i,"soma",j] <- ansSys[i,"soma",j] <- ansTotal[i,"soma",j] <- NA
  # else {
  #   ansUser[i,"soma",j] <- out[1]
  #   ansSys[i,"soma",j] <-  out[2]
  #   ansTotal[i,"soma",j] <- out[3]
  # }
  #
  ##  cmaes

  start <- Sys.time()
  out <- try(cmaes::cma_es(par=par, fn=goTestMax, test_matrix = test_matrix,
                           lower=bounds$lower, upper=bounds$upper,
                           control=list(maxit=1000),
                           fnName=objFun))
  end <- Sys.time()
  dif <- end - start
  if(class(out) == "try-error"){
    ansTime[1,"cma_es",j] <- NA
  }else
    ansTime[1,"cma_es",j] <- dif


  ##  GenSA
  start <- Sys.time()
  out <- try(GenSA(fn=goTestMax, test_matrix = test_matrix,
                   lower=bounds$lower, upper=bounds$upper,
                   control=list(max.call=10000),
                   fnName=objFun))

  end <- Sys.time()
  dif <- end - start
  if(class(out) == "try-error"){
    ansTime[1,"GenSA",j] <- NA
  }else
    ansTime[1,"GenSA",j] <- dif

  # ##  pso
  #
  # out <- try(system.time(psoptim(par=par,fn=goTest,
  #                                lower=bounds$lower, upper=bounds$upper,
  #                                control=list(maxit=4170),
  #                                fnName=objFun[i], checkDim=F)))
  # if(class(out) == "try-error")
  #   ansUser[i,"psoptim",j] <- ansSys[i,"psoptim",j] <- ansTotal[i,"psoptim",j] <- NA
  # else {
  #   ansUser[i,"psoptim",j] <- out[1]
  #   ansSys[i,"psoptim",j] <-  out[2]
  #   ansTotal[i,"psoptim",j] <- out[3]
  # }


  ## nloptr crs

  start <- Sys.time()
  out <- try(crs2lm(x0=par, fn=goTestMax, test_matrix = test_matrix,
                    lower=bounds$lower, upper=bounds$upper,
                    maxeval=10000,
                    fnName=objFun))
  end <- Sys.time()
  dif <- end - start
  if(class(out) == "try-error"){
    ansTime[1,"nloptr_crs",j] <- NA
  }else
    ansTime[1,"nloptr_crs",j] <- dif
  ## nloptr stogo

  start <- Sys.time()
  out <- try(stogo(x0=par, fn=goTestMax, test_matrix = test_matrix,
                   lower=bounds$lower, upper=bounds$upper,
                   maxeval=10000,
                   fnName=objFun))
  end <- Sys.time()
  dif <- end - start
  if(class(out) == "try-error"){
    ansTime[1,"nloptr_stogo",j] <- NA
  }else
    ansTime[1,"nloptr_stogo",j] <- dif

  ## nloptr direct_1

  start <- Sys.time()
  out <- try(nloptr(x0=par, eval_f=goTestMax, test_matrix=test_matrix,
                    lb=bounds$lower, ub=bounds$upper,
                    opts=list(algorithm="NLOPT_GN_DIRECT_L",
                              maxeval=10000),
                    fnName=objFun))
  end <- Sys.time()
  dif <- end - start
  if(class(out) == "try-error"){
    ansTime[1,"nloptr_d",j] <- NA
  }else
    ansTime[1,"nloptr_d",j] <- dif

  ## nloptr direct

  start <- Sys.time()
  out <- try(nloptr(x0=par, eval_f=goTestMax, test_matrix=test_matrix,
                    lb=bounds$lower, ub=bounds$upper,
                    opts=list(algorithm="NLOPT_GN_DIRECT",
                              maxeval=10000),
                    fnName=objFun))
  end <- Sys.time()
  dif <- end - start
  if(class(out) == "try-error"){
    ansTime[1,"nloptr_d_l",j] <- NA
  }else
    ansTime[1,"nloptr_d_l",j] <- dif

  ## nloptr isres

  start <- Sys.time()
  out <- try(nloptr(x0=par, eval_f=goTestMax, test_matrix=test_matrix,
                    lb=bounds$lower, ub=bounds$upper,
                    opts=list(algorithm="NLOPT_GN_ISRES",
                              maxeval=10000),
                    fnName=objFun))

  end <- Sys.time()
  dif <- end - start
  if(class(out) == "try-error"){
    ansTime[1,"nloptr_i",j] <- NA
  }else
    ansTime[1,"nloptr_i",j] <- dif

  # ## optim
  #
  # out <- try(system.time(optim(par=par, fn=goTest, method="SANN",
  #                              control=list(maxit=50000),
  #                              fnName=objFun[i], checkDim=F)))
  # if(class(out) == "try-error")
  #   ansUser[i,"optim",j] <- ansSys[i,"optim",j] <- ansTotal[i,"optim",j] <- NA
  # else {
  #   ansUser[i,"optim",j] <- out[1]
  #   ansSys[i,"optim",j] <-  out[2]
  #   ansTotal[i,"optim",j] <- out[3]
  # }
  #
  # ## SCEoptim
  #
  # out <- try(system.time(SCEoptim(par=par, FUN=goTest,
  #                                 control=list(maxeval=50000),
  #                                 fnName=objFun[i], checkDim=F)))
  # if(class(out) == "try-error")
  #   ansUser[i,"SCEoptim",j] <- ansSys[i,"SCEoptim",j] <- ansTotal[i,"SCEoptim",j] <- NA
  # else {
  #   ansUser[i,"SCEoptim",j] <- out[1]
  #   ansSys[i,"SCEoptim",j] <-  out[2]
  #   ansTotal[i,"SCEoptim",j] <- out[3]
  # }
  #
  # ## DEopt
  #
  # out <- try(system.time(DEopt(OF=goTest,
  #                              algo=list(min=bounds$lower, max=bounds$upper,
  #                                        nG=1000, minmaxConstr=TRUE,printDetail=F,
  #                                        printBar=F),
  #                              fnName=objFun[i], checkDim=F)))
  # if(class(out) == "try-error")
  #   ansUser[i,"DEopt",j] <- ansSys[i,"DEopt",j] <- ansTotal[i,"DEopt",j] <- NA
  # else {
  #   ansUser[i,"DEopt",j] <- out[1]
  #   ansSys[i,"DEopt",j] <-  out[2]
  #   ansTotal[i,"DEopt",j] <- out[3]
  # }
  ## PSopt

  start <- Sys.time()
  out <- try(PSopt(OF=goTestMax, test_matrix = test_matrix,
                   algo=list(min=bounds$lower, max=bounds$upper,
                             printBar=F, printDetail=F, nG=100),
                   fnName=objFun))

  end <- Sys.time()
  dif <- end - start
  if(class(out) == "try-error"){
    ansTime[1,"PSopt",j] <- NA
  }else
    ansTime[1,"PSopt",j] <- dif

  ## malschains


  goTest1 <- function(par,fnName=objFun)
    goTestMax(par, test_matrix = test_matrix, fnName)

  start <- Sys.time()
  out <- try(malschains(fn=goTest1, lower=bounds$lower,
                        upper=bounds$upper,
                        maxEvals=10000))

  end <- Sys.time()
  dif <- end - start
  if(class(out) == "try-error"){
    ansTime[1,"malschains",j] <- NA
  }else
    ansTime[1,"malschains",j] <- dif

  # ## hydroPSO
  #
  # out <- try(system.time(hydroPSO(fn=goTest, lower=bounds$lower,
  #                                 upper=bounds$upper,
  #                                 control=list(maxfn=50000,write2disk=F),
  #                                 fnName=objFun[i], checkDim=F)))
  # if(class(out) == "try-error")
  #   ansUser[i,"hydroPSO",j] <- ansSys[i,"hydroPSO",j] <- ansTotal[i,"hydroPSO",j] <- NA
  # else {
  #   ansUser[i,"hydroPSO",j] <- out[1]
  #   ansSys[i,"hydroPSO",j] <-  out[2]
  #   ansTotal[i,"hydroPSO",j] <- out[3]
  # }

  ##
  save(ansTime, file="ResultsTime.RData")
  cat("Finished run", j, "under objective function", objFun[1], "\n")
}
