## This script reproduces plots for the paper
## ``Continuous Global Optimization in R''

## load results from timeStudy.R
load("ResultsTime.RData")

library("castOpt")

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

nFuns <- length(objFun)

# ansSum <- array(dim=c(length(objFun), length(funs), 1),
#                  dimnames=list(objFun,funs, NULL))
# sum <- 0
#
# for (j in 1:100) {
#   sum <- sum + ansTime[1,"ga",j]
# }
# ansSum[1, "ga", 1] <- sum
# sum <- 0
#
# for (j in 1:100) {
#   sum <- sum + ansTime[1,"DEoptim",j]
# }
# ansSum[1, "DEoptim", 1] <- sum
# sum <- 0
#
# for (j in 1:100) {
#   sum <- sum + ansTime[1,"cma_es",j]
# }
# ansSum[1, "cma_es", 1] <- sum
# sum <- 0
#
# for (j in 1:100) {
#   sum <- sum + ansTime[1,"GenSA",j]
# }
# ansSum[1, "GenSA", 1] <- sum
# sum <- 0
#
# for (j in 1:100) {
#   sum <- sum + ansTime[1,"nloptr_crs",j]
# }
# ansSum[1, "nloptr_crs", 1] <- sum
# sum <- 0
#
# for (j in 1:100) {
#   sum <- sum + ansTime[1,"nloptr_stogo",j]
# }
# ansSum[1, "nloptr_stogo", 1] <- sum
# sum <- 0
#
# for (j in 1:100) {
#   sum <- sum + ansTime[1,"nloptr_d",j]
# }
# ansSum[1, "nloptr_d", 1] <- sum
# sum <- 0
#
# for (j in 1:100) {
#   sum <- sum + ansTime[1,"nloptr_d_l",j]
# }
# ansSum[1, "nloptr_d_l", 1] <- sum
# sum <- 0
#
# for (j in 1:100) {
#   sum <- sum + ansTime[1,"nloptr_i",j]
# }
# ansSum[1, "nloptr_i", 1] <- sum
# sum <- 0
#
# for (j in 1:100) {
#   sum <- sum + ansTime[1,"malschains",j]
# }
# ansSum[1, "malschains", 1] <- sum
# sum <- 0
#
# for (j in 1:100) {
#   sum <- sum + ansTime[1,"PSopt",j]
# }
# ansSum[1, "PSopt", 1] <- sum
# sum <- 0
#
# colnames(ansSum) <- colnames(ansTime)
#
# ord <- names(sort(colSums(ansSum), decreasing=TRUE))

mTimes <- matrix(nrow=1, ncol=ncol(ansTime))
colnames(mTimes) <- colnames(ansTime)

sAns <- names(sort(rowSums(ansTime[1,,],na.rm=T)))
timeMat <- ansTime[1,,]
for(j in 1:length(sAns))
  timeMat[j,] <- ansTime[1,sAns[j],]
rownames(timeMat) <- sAns

mTimes[1, ] <- rowMeans(ansTime[1,,],na.rm=T)

# pdf(paste(1,"time.pdf",sep=""),width=9,height=7)
#
# par(las=1,mar=c(5.5, 9, 4, 2))
# boxplot(t(timeMat), cex.main=1.5,  cex.axis=1.5, cex.lab=1.5,
#         main=paste(getProblemDimen(objFun[i]),"-parameter ",
#                    objFun[i]," problem", sep=""), xlab="seconds",
#         col=2, horizontal=T)
#
# dev.off()
# axlist <- list(c(0,7),c(0,7),c(0,7),c(0,7))
#
# pdf(paste(1,"timeAlt.pdf",sep=""),width=9,height=7)
#
# par(las=1,mar=c(5.5, 9, 4, 2))
# boxplot(t(timeMat), cex.main=1.5,  cex.axis=1.5,
#         cex.lab=1.5, ylim=axlist[[1]],  xlab="seconds",
#         main=paste(getProblemDimen(objFun),"-parameter ",
#                    objFun[i]," problem (alternative axis limits)", sep=""),
#         col=2, horizontal=T)
#
# dev.off()



pdf("timesum.pdf", width=5, height=3.5)

par(cex.axis=.8,cex.main=1,las=2,mar=c(6.5, 5, 1, 1))

barplot(sort(colMeans(mTimes), decreasing=F), ylim = c(0,5),
        ylab="Segundos")

dev.off()


# pdf("timesum1.pdf", width=5, height=3.5)
#
# par(cex.axis=.8,cex.main=1,las=2,mar=c(6.5, 5, 1, 1))
#
# barplot(sort(colMeans(mTimes), decreasing=F)[-c(15:18)],
#         ylab="Segundos")
#
# dev.off()

