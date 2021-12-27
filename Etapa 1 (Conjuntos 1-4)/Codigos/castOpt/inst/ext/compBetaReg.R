
# check.packages function: install and load multiple R packages.
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

############################################################################

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
install.packages(pkgnames)

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
          "soma",
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

# Number of Replications for each method
Replication <-  10 #100

ans <- array(dim=c(length(objFun), length(funs), Replication),
             dimnames=list(objFun,funs, NULL))

ans_df <- data.frame()

library(readr)

#test_matrix <- read_rds("inst/ext/test_matrix.rds")

  for(j in 1:Replication) {
    test_matrix <- getInstance(betaVector = c(0.1, 1.5, 0.5), phi = 15, N = 100, seed_X = 123)
    ncols <- ncol(test_matrix)
    nrows <- nrow(test_matrix)
    #getDefaultBounds(objFun[i]) # <- Modify
    bounds <- list(lower = c(rep(-100, ncols), 0),
                   upper = rep(100, ncols + 1))

    bMat <- cbind(bounds$lower, bounds$upper)
    par <- vector()
    checkDim <- FALSE
    for(ii in 1:length(bounds$lower))
      par <- append(par,runif(1,bounds$lower[ii],bounds$upper[ii]))

    ## ga / OK

    goTestMin <- function(...)
      goTestBetaReg(...)


    out <- try(ga(type="real-valued", fitness = goTestMin,
              fnName = objFun, test_matrix = test_matrix,
              lower=bounds$lower, upper = bounds$upper, maxiter = 245))


    if(class(out) == "try-error"){
       ans[1,"ga",j] <- NA
    }else
      ans[1,"ga",j] <- out@fitnessValue

    # ## rgenoud / ERRO: VALOR NAO FINITO FORNECIDO POR OPTIM (L-BFGS-B) // BFGS requires the gradient of the function being minimized. If you don't pass one it will try to use finite-differences to estimate it.
    #
    # out <- try(genoud(fn=goTestBetaReg, test_matrix = test_matrix, nvars=length(bounds$lower),
    #                   max.generations=11,
    #                   Domains = matrix(c(bounds$lower,bounds$upper),ncol=2),
    #                   boundary.enforcement=2,
    #                   fnName=objFun))
    # if(class(out) == "try-error")
    #   ans[1,"genoud",j] <- NA
    # else
    #   ans[1,"genoud",j] <- out$value

    ## DEoptim / OK

    ff <- 10*length(par)
    mi <- round(10000/ff)
    out <- try(DEoptim(fn=goTestMax, test_matrix = test_matrix, lower=bounds$lower, upper=bounds$upper,
                   fnName=objFun, control=list(itermax=mi)))
     if(class(out) == "try-error"){
      ans[1,"DEoptim",j] <- NA
     }else
       ans[1,"DEoptim",j] <- -out$optim$bestval


    ## soma / RESULTADOS VARIANDO BASTANTE, ACREDITO SER ESPERADO

    out <- try(soma(costFunction=goTestMax, test_matrix = test_matrix, bounds=list(min=bounds$lower,
                                      max=bounds$upper),
                options = list(nMigrations=36),
                fnName=objFun), silent = TRUE)
    if(class(out) == "try-error"){
      ans[1,"soma",j] <- NA
    }else
      ans[1,"soma",j] <- -out$cost[out$leader]

    ##  cmaes / OK AS VEZES

    out <- try(cmaes::cma_es(par=par, fn=goTestMax, test_matrix = test_matrix,
                  lower=bounds$lower, upper=bounds$upper,
                  control=list(maxit=1000),
                  fnName=objFun), silent = TRUE)
    if(class(out) == "try-error"){
      ans[1,"cma_es",j] <- NA
    }else
      ans[1,"cma_es",j] <- -out$value

    ##  GenSA / OK
    out <- try(GenSA(fn=goTestMax, test_matrix = test_matrix,
                     lower=bounds$lower, upper=bounds$upper,
                     control=list(max.call=10000),
                     fnName=objFun), silent = TRUE)

    if(class(out) == "try-error"){
      ans[1,"GenSA",j] <- NA
    }else
      ans[1,"GenSA",j] <- -out$value

    # ##  pso // (ESPERADO) Error in if (f.x[i] < f.p[i]) { : valor ausente onde TRUE/FALSE necessário
    #
    # out <- try(psoptim(par=par,fn=goTestMax, test_matrix = test_matrix,
    #                lower=bounds$lower, upper=bounds$upper,
    #                control=list(maxit=835),
    #                fnName=objFun))
    # if(class(out) == "try-error")
    #   ans[1,"psoptim",j] <- NA
    # else
    #   ans[1,"psoptim",j] <- out$value

    ## nloptr crs / OK

    out <- try(crs2lm(x0=par, fn=goTestMax, test_matrix = test_matrix,
                      lower=bounds$lower, upper=bounds$upper,
                      maxeval=10000,
                      fnName=objFun), silent = TRUE)
    if(class(out) == "try-error"){
       ans[1,"nloptr_crs",j] <- NA
     }else
       ans[1,"nloptr_crs",j] <- -out$value

     ## nloptr stogo / Error in is.nloptr(ret) : gradient of objective in x0 returns NA

    out <- try(stogo(x0=par, fn=goTestMax, test_matrix = test_matrix,
                      lower=bounds$lower, upper=bounds$upper,
                      maxeval=10000,
                      fnName=objFun), silent = TRUE)
    if(class(out) == "try-error")
       ans[1,"nloptr_stogo",j] <- NA
     else
       ans[1,"nloptr_stogo",j] <- -out$value

    ## nloptr direct_1 / OK

    out <- try(nloptr(x0=par, eval_f=goTestMax, test_matrix=test_matrix,
                  lb=bounds$lower, ub=bounds$upper,
                  opts=list(algorithm="NLOPT_GN_DIRECT_L",
                    maxeval=10000),
                  fnName=objFun), silent = TRUE)
     if(class(out) == "try-error")
       ans[1,"nloptr_d_l",j] <- NA
     else
       ans[1,"nloptr_d_l",j] <- -out$objective

    ## nloptr direct / OK

    out <- try(nloptr(x0=par, eval_f=goTestMax, test_matrix=test_matrix,
                  lb=bounds$lower, ub=bounds$upper,
                  opts=list(algorithm="NLOPT_GN_DIRECT",
                    maxeval=10000),
                  fnName=objFun), silent = TRUE)
     if(class(out) == "try-error")
       ans[1,"nloptr_d",j] <- NA
     else
       ans[1,"nloptr_d",j] <- -out$objective

    ## nloptr isres / OK

    out <- try(nloptr(x0=par, eval_f=goTestMax, test_matrix=test_matrix,
                  lb=bounds$lower, ub=bounds$upper,
                  opts=list(algorithm="NLOPT_GN_ISRES",
                    maxeval=10000),
                  fnName=objFun), silent = TRUE)
    if(class(out) == "try-error")
      ans[1,"nloptr_i",j] <- NA
    else
      ans[1,"nloptr_i",j] <- -out$objective

    ## optim / VALOR ABSURDO

    # out <- try(optim(par=par, fn=goTestMax, test_matrix=test_matrix,
    #                  method="SANN",
    #                  control=list(maxit=10000),
    #              fnName=objFun))
    # if(class(out) == "try-error")
    #   ans[1,"optim",j] <- NA
    # else
    #   ans[1,"optim",j] <- out$value


    #  ## SCEoptim / PRECISA DO HYDROMAD, NAO ESTA DISPONIVEL
    #
    # out <- try(SCEoptim(par=par, FUN=goTest,
    #              control=list(maxeval=10000),
    #              fnName=objFun[i], checkDim=F))
    # if(class(out) == "try-error")
    #   ans[1,"SCEoptim",j] <- NA
    # else
    #   ans[1,"SCEoptim",j] <- out$value

    ## DEopt / (ESPERADO) Error in mP[, logik] <- mPv[, logik, drop = FALSE] : NAs não são permitidos em atribuições por subscritos

    # out <- try(DEopt(OF=goTestMax, test_matrix=test_matrix,
    #                  algo=list(min=bounds$lower, max=bounds$upper,
    #                           nG=200, minmaxConstr=TRUE),
    #              fnName=objFun))
    # if(class(out) == "try-error")
    #   ans[1,"DEopt",j] <- NA
    # else
    #   ans[1,"DEopt",j] <- out$OFvalue

     ## PSopt / OK

     out <- try(PSopt(OF=goTestMax, test_matrix = test_matrix,
                      algo=list(min=bounds$lower, max=bounds$upper, minmaxConstr = TRUE,
                                   printBar=FALSE, printDetail=FALSE, nG=100),
                      fnName=objFun))
    if(class(out) == "try-error"){
      ans[1,"PSopt",j] <- NA
    }else
      ans[1,"PSopt",j] <- -out$OFvalue


    ## malschains / AS VEZES DA: NaN value of objective function!

    goTest1 <- function(par,fnName=objFun)
      goTestMax(par, test_matrix = test_matrix, fnName)

    out <- try(malschains(fn=goTest1, lower=bounds$lower,
                          upper=bounds$upper,
                 maxEvals=10000), silent = TRUE)

    if(class(out) == "try-error"){
      ans[1,"malschains",j] <- NA
    }else
      ans[1,"malschains",j] <- -out$fitness


    ## hydroPSO / PACOTE NAO DISPONIVEL
#
#     out <- try(hydroPSO(fn=goTest, lower=bounds$lower,
#                     upper=bounds$upper,
#                         control=list(maxfn=10000),
#                         fnName=objFun[i], checkDim=F))
#     if(class(out) == "try-error")
#       ans[1,"hydroPSO",j] <- NA
#     else
#       ans[1,"hydroPSO",j] <- out$value
#     ##
     last_it <- as.data.frame(ans[,,j])
     last_it$Method <- row.names(last_it)
     row.names(last_it) <- NULL
     last_it$Replication <- j
     last_it$Fitness <- last_it$`ans[, , j]`
     last_it$`ans[, , j]` <- NULL
     ans_df <- rbind(ans_df, last_it)
     last_it <- NULL

     save(ans, file="Results.RData")
     write_rds(ans_df, "Result_df.rds")
     cat("Finished run", j, "under objective function", objFun, "\n")
}


library(ggplot2)

# ggplot(ans_df[!(ans_df$Method %in% c("nloptr_stogo", "soma")),]) +
#   geom_boxplot(aes(x = reorder(Method, -Fitness), y = Fitness, fill = Method)) +
#   geom_hline(yintercept = max(ans_df$Fitness), size = 2, color = "red")

library(tidyverse)
ans_df %>% filter(!(Method %in% c("nloptr_stogo", "soma"))) %>%  ggplot() +
  geom_hline(yintercept = max(ans_df$Fitness, na.rm = TRUE), size = 1, color = "red", na.rm = TRUE) +
  geom_boxplot(aes(x = reorder(Method, -Fitness), y = Fitness, fill = Method), na.rm = TRUE) +
  geom_jitter(aes(x = reorder(Method, -Fitness), y = Fitness, fill = Method), alpha = .75, na.rm = TRUE)

ans_df %>% group_by(Method) %>% summarise(Média = mean(Fitness, na.rm = TRUE),
                                          Mínimo = min(Fitness, na.rm = TRUE),
                                          Máximo = max(Fitness, na.rm = TRUE),
                                          Desvio = sd(Fitness, na.rm = TRUE),
                                          Falha = sum(is.na(Fitness))) %>% arrange(-Média)

