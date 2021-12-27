############################################################################
## Unless otherwise noted, the version after the package name refers to a
## copy on CRAN, http://cran.r-project.org
## This study was performed using R version 3.0.0
############################################################################
## GA 1.1
## genopt fuction downloaded 11/04/12 from
## http://www.burns-stat.com/pages/Freecode/genopt.R
## rgenoud 5.7-12
## DEoptim 2.2-2
## soma 1.1.0
## cmaes 1.0-11
## GenSA 1.1.3
## pso 1.0.3
## NMOF 0.28-0
## nloptr 0.9.1
## nloptwrap 0.5-1
## hydroPSO 0.3-3
## Rmalschains 0.2-1
## hydromad 0.9-18

pkgnames <- c("GA",
              "rgenoud",
              "DEoptim",
              "soma",
              "cmaes",
              "GenSA",
              "pso",
              "NMOF",
              "nloptr",
              "nloptwrap",
              "hydroPSO",
              "Rmalschains")

## uncomment to install packages
 install.packages(pkgnames)

## hydromad is not on CRAN
install.packages(c("zoo", "latticeExtra", "polynom", "car", "Hmisc","reshape"))
#Install hydromad
install.packages("hydromad", repos="http://hydromad.catchment.org")

require(hydromad)

for(i in pkgnames)
  require(i, character.only=TRUE)

require(castOpt)

objFun <- c("reg_beta")

funs <- c("ga",
          "genoud",
          "DEoptim",
          "soma",
          "cma_es",
          "GenSA",
          "psoptim",
          "nloptr_crs",
          "nloptr_stogo",
          "nloptr_d",
          "nloptr_d_l",
          "nloptr_i",
          "optim",
          "DEopt",
          "malschains",
          "hydroPSO",
          "SCEoptim",
          "PSopt")

ans <- array(dim=c(length(objFun), length(funs), 2),
             dimnames=list(objFun,funs, NULL))

for(i in 1:length(objFun)) {
  for(j in 1:2) {
    bounds <- getDefaultBounds(objFun[i])
    bMat <- cbind(bounds$lower, bounds$upper)
    par <- vector()
    checkDim <- FALSE
    for(ii in 1:length(bounds$lower))
      par <- append(par,runif(1,bounds$lower[ii],bounds$upper[ii]))
    ## ga

    goTestMin <- function(...)
      -goTest(...)
    out <- try(ga(type="real-valued", fitness=goTestMin,
              fnName=objFun[i], checkDim=F,
              min=bounds$lower, max=bounds$upper, maxiter=245))
    if(class(out) == "try-error")
       ans[i,"ga",j] <- NA
    else
      ans[i,"ga",j] <- -out@fitnessValue

    ## rgenoud

    out <- try(genoud(fn=goTest, nvars=length(bounds$lower),
                      max.generations=11,
                      Domains = matrix(c(bounds$lower,bounds$upper),ncol=2),
                      boundary.enforcement=2,
                      fnName=objFun[i], checkDim=F))
    if(class(out) == "try-error")
      ans[i,"genoud",j] <- NA
    else
      ans[i,"genoud",j] <- out$value
    ## DEoptim

    ff <- 10*length(par)
    mi <- round(10000/ff)
    out <- try(DEoptim(fn=goTest,lower=bounds$lower, upper=bounds$upper,
                   fnName=objFun[i], checkDim=F,
                   control=list(itermax=mi)))
     if(class(out) == "try-error")
      ans[i,"DEoptim",j] <- NA
     else
       ans[i,"DEoptim",j] <- out$optim$bestval
    ## soma

    out <- try(soma(costFunction=goTest,bounds=list(min=bounds$lower,
                                      max=bounds$upper),
                options = list(nMigrations=36),
                fnName=objFun[i], checkDim=F))
    if(class(out) == "try-error")
      ans[i,"soma",j] <- NA
    else
      ans[i,"soma",j] <- out$cost[out$leader]
    ##  cmaes

    out <- try(cmaes::cma_es(par=par, fn=goTest,
                  lower=bounds$lower, upper=bounds$upper,
                  control=list(maxit=1000),
                  fnName=objFun[i], checkDim=F))
    if(class(out) == "try-error")
      ans[i,"cma_es",j] <- NA
    else
      ans[i,"cma_es",j] <- out$value

    ##  GenSA
    out <- try(GenSA(fn=goTest,
                     lower=bounds$lower, upper=bounds$upper,
                     control=list(max.call=10000),
                     fnName=objFun[i], checkDim=F))

    if(class(out) == "try-error")
      ans[i,"GenSA",j] <- NA
    else
      ans[i,"GenSA",j] <- out$value
    ##  pso

    out <- try(psoptim(par=par,fn=goTest,
                   lower=bounds$lower, upper=bounds$upper,
                   control=list(maxit=835),
                   fnName=objFun[i], checkDim=F))
    if(class(out) == "try-error")
      ans[i,"psoptim",j] <- NA
    else
      ans[i,"psoptim",j] <- out$value
    ## nloptr crs

    out <- try(crs2lm(x0=par, fn=goTest,
                      lower=bounds$lower, upper=bounds$upper,
                      maxeval=10000,
                      fnName=objFun[i], checkDim=F))
    if(class(out) == "try-error")
       ans[i,"nloptr_crs",j] <- NA
     else
       ans[i,"nloptr_crs",j] <- out$value
     ## nloptr stogo

    out <- try(stogo(x0=par, fn=goTest,
                      lower=bounds$lower, upper=bounds$upper,
                      maxeval=10000,
                      fnName=objFun[i], checkDim=F))
    if(class(out) == "try-error")
       ans[i,"nloptr_stogo",j] <- NA
     else
       ans[i,"nloptr_stogo",j] <- out$value
    ## nloptr direct_1

    out <- try(nloptr(x0=par, eval_f=goTest,
                  lb=bounds$lower, ub=bounds$upper,
                  opts=list(algorithm="NLOPT_GN_DIRECT_L",
                    maxeval=10000),
                  fnName=objFun[i], checkDim=F))
     if(class(out) == "try-error")
       ans[i,"nloptr_d_l",j] <- NA
     else
       ans[i,"nloptr_d_l",j] <- out$objective
    ## nloptr direct

    out <- try(nloptr(x0=par, eval_f=goTest,
                  lb=bounds$lower, ub=bounds$upper,
                  opts=list(algorithm="NLOPT_GN_DIRECT",
                    maxeval=10000),
                  fnName=objFun[i], checkDim=F))
     if(class(out) == "try-error")
       ans[i,"nloptr_d",j] <- NA
     else
       ans[i,"nloptr_d",j] <- out$objective
    ## nloptr isres

    out <- try(nloptr(x0=par, eval_f=goTest,
                  lb=bounds$lower, ub=bounds$upper,
                  opts=list(algorithm="NLOPT_GN_ISRES",
                    maxeval=10000),
                  fnName=objFun[i], checkDim=F))
    if(class(out) == "try-error")
      ans[i,"nloptr_i",j] <- NA
    else
      ans[i,"nloptr_i",j] <- out$objective
    ## optim

    out <- try(optim(par=par, fn=goTest, method="SANN",
                 control=list(maxit=10000),
                 fnName=objFun[i], checkDim=F))
    if(class(out) == "try-error")
      ans[i,"optim",j] <- NA
    else
      ans[i,"optim",j] <- out$value

     ## SCEoptim

    out <- try(SCEoptim(par=par, FUN=goTest,
                 control=list(maxeval=10000),
                 fnName=objFun[i], checkDim=F))
    if(class(out) == "try-error")
      ans[i,"SCEoptim",j] <- NA
    else
      ans[i,"SCEoptim",j] <- out$value

    ## DEopt

    out <- try(DEopt(OF=goTest, algo=list(min=bounds$lower, max=bounds$upper,
                              nG=200, minmaxConstr=TRUE),
                 fnName=objFun[i], checkDim=F))
    if(class(out) == "try-error")
      ans[i,"DEopt",j] <- NA
    else
      ans[i,"DEopt",j] <- out$OFvalue

     ## PSopt

     out <- try(PSopt(OF=goTest, algo=list(min=bounds$lower, max=bounds$upper,
                                   printBar=F, printDetail=F, nG=100),
                      fnName=objFun[i], checkDim=F))
    if(class(out) == "try-error")
      ans[i,"PSopt",j] <- NA
    else
      ans[i,"PSopt",j] <- out$OFvalue


    ## malschains


    goTest1 <- function(par,fnName=objFun[i],checkDim=F)
      goTest(par,fnName, checkDim)

    out <- try(malschains(fn=goTest1, lower=bounds$lower,
                          upper=bounds$upper,
                 maxEvals=10000, trace=FALSE))

    if(class(out) == "try-error")
      ans[i,"malschains",j] <- NA
    else
      ans[i,"malschains",j] <- out$fitness
    ## hydroPSO

    out <- try(hydroPSO(fn=goTest, lower=bounds$lower,
                    upper=bounds$upper,
                        control=list(maxfn=10000),
                        fnName=objFun[i], checkDim=F))
    if(class(out) == "try-error")
      ans[i,"hydroPSO",j] <- NA
    else
      ans[i,"hydroPSO",j] <- out$value
    ##
    save(ans, file="Results.RData")
    cat("Finished run", j, "under objective function", objFun[i], "\n")
  }
}


