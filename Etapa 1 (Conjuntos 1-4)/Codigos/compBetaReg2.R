#
# # check.packages function: install and load multiple R packages.
# # Check to see if packages are installed. Install them if they are not, then load them into the R session.
# check.packages <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg))
#     install.packages(new.pkg, dependencies = TRUE)
#   sapply(pkg, require, character.only = TRUE)
# }

############################################################################

pkgnames <- c("GA",
              "DEoptim",
              "soma",
              "cmaes",
              "GenSA",
              "pso",
              "NMOF",
              "nloptr",
              "Rmalschains")

## uncomment to install packages
#install.packages(pkgnames)

for(ai in pkgnames)
  require(ai, character.only=TRUE)

require(castOpt)

library(tidyverse) #<---------

objFun <- c("reg_beta")

funs <- c("ga",
          "DEoptim",
          "cma_es",
          "GenSA",
          "nloptr_crs",
          "nloptr_d",
          "nloptr_d_l",
          "nloptr_i",
          "malschains",
          "PSopt")


# Number of Replications for each method
Replication <- 100 #100
samplesize <- 30
b0 <- -2.5
b1 <- -1.2
#b2 <- 0.5
phi <- 12
samplenumber <- 50 #50

ans <- array(dim=c(length(funs), 10, Replication),
             dimnames=list(funs, c("Fitness", "e_b0", "e_b1", "e_phi", "runtime", "b0", "b1", "phi",
                                   "samplesize", "samplenumber"), NULL))

ans_df <- data.frame()

#library(readr)

#test_matrix <- getInstance(betaVector = c(b0, b1, b2), types = c("runif", "rnorm"), phi = phi, N = samplesize)

#test_matrix <- read_rds("inst/ext/test_matrix.rds")

inicio <- Sys.time()
for(i in 1:samplenumber) {
  #seed 519 para os testes
  test_matrix <- getInstance(betaVector = c(b0, b1), types = c("runif"), phi = phi, N = samplesize, seed_X = 519)
#  test_matrix <- getInstance(betaVector = c(b0, b1, b2), types = c("runif", "rnorm"), phi = phi, N = samplesize, seed_X = 519)
  for(j in 1:Replication) {
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

    start <- Sys.time()
    out <- try(ga(type="real-valued", fitness = goTestMin,
              fnName = objFun, test_matrix = test_matrix,
              lower=bounds$lower, upper = bounds$upper, maxiter = 245))
    end <- Sys.time()
    dif <- end - start

    if(class(out) == "try-error"){
      ans["ga", ,j] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, i)
    }else
      ans["ga", ,j] <- c(out@fitnessValue, out@solution, dif, b0, b1, phi, samplesize, i)


    ## DEoptim / OK

    ff <- 10*length(par)
    mi <- round(10000/ff)
    start <- Sys.time()
    out <- try(DEoptim(fn=goTestMax, test_matrix = test_matrix, lower=bounds$lower, upper=bounds$upper,
                   fnName=objFun, control=list(itermax=mi)))
    end <- Sys.time()
    dif <- end - start

     if(class(out) == "try-error"){
      ans["DEoptim", ,j] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, i)
     }else
       ans["DEoptim", ,j] <- c(-out$optim$bestval, out$optim$bestmem, dif, b0, b1, phi, samplesize, i)

    ##  cmaes / OK AS VEZES

    start <- Sys.time()
    out <- try(cmaes::cma_es(par=par, fn=goTestMax, test_matrix = test_matrix,
                  lower=bounds$lower, upper=bounds$upper,
                  control=list(maxit=1000),
                  fnName=objFun), silent = TRUE)
    end <- Sys.time()
    dif <- end - start
    if(class(out) == "try-error"){
      ans["cma_es", ,j] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, i)
    }else
      ans["cma_es", ,j] <- c(-out$value, out$par, dif, b0, b1, phi, samplesize, i)

    ##  GenSA / OK

    start <- Sys.time()
    out <- try(GenSA(fn=goTestMax, test_matrix = test_matrix,
                     lower=bounds$lower, upper=bounds$upper,
                     control=list(max.call=10000),
                     fnName=objFun), silent = TRUE)
    end <- Sys.time()
    dif <- end - start

    if(class(out) == "try-error"){
      ans["GenSA", ,j] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, i)
    }else
      ans["GenSA", ,j] <- c(-out$value, out$par, dif, b0, b1, phi, samplesize, i)

    ## nloptr crs / OK

    start <- Sys.time()
    out <- try(crs2lm(x0=par, fn=goTestMax, test_matrix = test_matrix,
                      lower=bounds$lower, upper=bounds$upper,
                      maxeval=10000,
                      fnName=objFun), silent = TRUE)
    end <- Sys.time()
    dif <- end - start
    if(class(out) == "try-error"){
       ans["nloptr_crs", ,j] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, i)
     }else
       ans["nloptr_crs", ,j] <- c(-out$value, out$par, dif, b0, b1, phi, samplesize, i)

    ## nloptr direct_1 / OK

    start <- Sys.time()
    out <- try(nloptr(x0=par, eval_f=goTestMax, test_matrix=test_matrix,
                  lb=bounds$lower, ub=bounds$upper,
                  opts=list(algorithm="NLOPT_GN_DIRECT_L",
                    maxeval=10000),
                  fnName=objFun), silent = TRUE)
    end <- Sys.time()
    dif <- end - start
     if(class(out) == "try-error")
       ans["nloptr_d_l", ,j] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, i)
     else
       ans["nloptr_d_l", ,j] <- c(-out$objective, out$solution, dif, b0, b1, phi, samplesize, i)

    ## nloptr direct / OK
    start <- Sys.time()
    out <- try(nloptr(x0=par, eval_f=goTestMax, test_matrix=test_matrix,
                  lb=bounds$lower, ub=bounds$upper,
                  opts=list(algorithm="NLOPT_GN_DIRECT",
                    maxeval=10000),
                  fnName=objFun), silent = TRUE)
    end <- Sys.time()
    dif <- end - start
     if(class(out) == "try-error")
       ans["nloptr_d", ,j] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, i)
     else
       ans["nloptr_d", ,j] <- c(-out$objective, out$solution, dif, b0, b1, phi, samplesize, i)

    ## nloptr isres / OK

    start <- Sys.time()
    out <- try(nloptr(x0=par, eval_f=goTestMax, test_matrix=test_matrix,
                  lb=bounds$lower, ub=bounds$upper,
                  opts=list(algorithm="NLOPT_GN_ISRES",
                    maxeval=10000),
                  fnName=objFun), silent = TRUE)
    end <- Sys.time()
    dif <- end - start
    if(class(out) == "try-error")
      ans["nloptr_i", ,j] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, i)
    else
      ans["nloptr_i", ,j] <- c(-out$objective, out$solution, dif, b0, b1, phi, samplesize, i)

     ## PSopt / OK

    start <- Sys.time()
     out <- try(PSopt(OF=goTestMax, test_matrix = test_matrix,
                      algo=list(min=bounds$lower, max=bounds$upper, minmaxConstr = TRUE,
                                   printBar=FALSE, printDetail=FALSE, nG=100),
                      fnName=objFun))
     end <- Sys.time()
     dif <- end - start
    if(class(out) == "try-error"){
      ans["PSopt", ,j] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, i)
    }else
      ans["PSopt", ,j] <- c(-out$OFvalue, out$xbest, dif, b0, b1, phi, samplesize, i)


    ## malschains / AS VEZES DA: NaN value of objective function!


    goTest1 <- function(par,fnName=objFun)
      goTestMax(par, test_matrix = test_matrix, fnName)
    start <- Sys.time()
    out <- try(malschains(fn=goTest1, lower=bounds$lower,
                          upper=bounds$upper,
                 maxEvals=10000), silent = TRUE)
    end <- Sys.time()
    dif <- end - start

    if(class(out) == "try-error"){
      ans["malschains", ,j] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, i)
    }else
      ans["malschains", ,j] <- c(-out$fitness, out$sol, dif, b0, b1, phi, samplesize, i)



     last_it <- as.data.frame(ans[,,j])
      last_it$Method <- row.names(last_it)
      row.names(last_it) <- NULL
      last_it$Replication <- j
      last_it$`ans[, , j]` <- NULL
     ans_df <- rbind(ans_df, last_it)
     last_it <- NULL

    # save(ans, file="Results.RData")
     write_rds(ans_df, "Result_uni_neg25_neg12_12_n30.rds")
     cat("Finished run", j, "under objective function", objFun, "\n")
  }
  final <- Sys.time()
}

tempoexe <- final - inicio
tempoexe


library(ggplot2)

# ggplot(ans_df[!(ans_df$Method %in% c("nloptr_stogo", "soma")),]) +
#   geom_boxplot(aes(x = reorder(Method, -Fitness), y = Fitness, fill = Method)) +
#   geom_hline(yintercept = max(ans_df$Fitness), size = 2, color = "red")

library(tidyverse)

table <- ans_df %>% group_by(Method) %>% summarise(Média = mean(Fitness, na.rm = TRUE),
                                          Mínimo = min(Fitness, na.rm = TRUE),
                                          Máximo = max(Fitness, na.rm = TRUE),
                                          Desvio = sd(Fitness, na.rm = TRUE),
                                          Falha = sum(is.na(Fitness)),
                                          Sucessos05 = sum(Fitness > Máximo -0.5),
                                          TempoMedio = mean(runtime, na.rm = TRUE)) %>% arrange(-Média)

ans_df %>%
  #filter(!(Method %in% c("soma"))) %>%
  ggplot() +
  geom_hline(yintercept = max(ans_df$Fitness, na.rm = TRUE),
             size = 1, color = "red", na.rm = TRUE) +
  geom_boxplot(aes(x = reorder(Method, -Fitness),
                   y = Fitness,
                   fill = Method),
               na.rm = TRUE) + ylim(0,350)
#+ ylim(300,350)
#  ylim(80, 95) + labs(x = "Metodo", y = "Valor da F.O.")
  #geom_jitter(aes(x = reorder(Method, -Fitness), y = Fitness, fill = Method), alpha = .75, na.rm = TRUE)


table %>%
  ggplot() +
  geom_col(aes(x = reorder(Method, Falha),
               y = Falha, color = Method,
               fill = Method, alpha = 0.5)) +
  labs(x = "Metodo", y = "Numero de falhas")

table %>%
  ggplot() +
  geom_col(aes(x = reorder(Method, -Sucessos05),
               y = Sucessos05,
               color = Method,
               fill = Method,
               alpha = 0.5)) +
  labs(x = "Metodo", y = "Numero de sucessos com d = 0.5")

table %>%
  ggplot() +
  geom_col(aes(x = reorder(Method, TempoMedio),
               y = TempoMedio,
               color = Method,
               fill = Method,
               alpha = 0.5)) +
  labs(x = "Metodo", y = "Tempo medio de execucao por iteracao")
