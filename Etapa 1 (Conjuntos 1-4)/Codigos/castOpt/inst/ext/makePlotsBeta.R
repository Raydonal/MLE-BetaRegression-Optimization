load("Results.RData")

library(castOpt)

objFun <- c("reg_beta")

## Grafico do numero de sucessos (sendo um sucesso com margem de 0.5 do valor otimo, considerando o resultado do betareg)

nAlgs <- dim(ans)[2]
nFuns <- length(objFun)

anM <- matrix(nrow=nFuns,ncol=nAlgs)

op <- c()
op <- append(op, 87.3)
for(j in 1:nAlgs) {
  anM[1,j] <- length(which(ans[1,j,] > op[1] - .5))
}

colnames(anM) <- colnames(ans)

cl <- rep("grey",nAlgs)

ord <- names(sort(colSums(anM), decreasing=TRUE))

## these are methods that end in error for some runs
ii <- match(c("soma", "cma_es", "nloptr_stogo", "malschains"), ord)

cl[ii] <- hcl(h=0,alpha=.8)

pdf("successP.pdf", width=5, height=3.5)

par(cex.axis=.8,cex.main=1,las=2,mar=c(6.5, 5, 0, 0), mgp=c(4,1,0))
barplot(sort(colSums(anM), decreasing=TRUE), ylim=c(0,110), col=cl,
        ylab="`Successful' runs")

dev.off()

dim <- 4

## Boxplots dos resultados (de 65 a 87.3)

  pdf(paste(1,".pdf",sep=""),width=9,height=5)

  par(las=2,mar=c(8.5, 7, 4, 2), mgp=c(5,1,0))

  ylim <- c(65,87.3)

  if(!is.na(ylim)){
      boxplot(t(ans[1,,]), cex.main=1.5,  cex.axis=1.5, cex.lab=1.5,
        main=paste(dim,"-parameter ",
          objFun," problem", sep=""), ylim=ylim,
              ylab="Obj. fun. value",
              col=palette(rainbow(13)))
  }else
     boxplot(t(ans[1,,]), cex.main=1.5,  cex.axis=1.5, cex.lab=1.5,
             main=paste(dim,"-parameter ",
               objFun," problem", sep=""),
             ylab="Obj. fun. value",
             col=palette(rainbow(13)))
     abline(h=87.3, col=2, lwd=1)

  dev.off()



    ylim <- c(65,87.3)

    boxplot(t(ans[1,,]), cex.main=1.5,  cex.axis=1.5, cex.lab=1.5,
            main=paste(dim,"-parameter ",
              objFun," problem (alternative axis limits)", sep=""),
            ylim=ylim,
            ylab="Obj. fun. value",
            col=palette(rainbow(13)))

    abline(h=87.3, col=2, lwd=1)

    dev.off()

## Boxplot dos resultados de 80 a 87.3

    pdf(paste(2,".pdf",sep=""),width=9,height=5)

    par(las=2,mar=c(8.5, 7, 4, 2), mgp=c(5,1,0))

    ylim <- c(87,87.3095)

    if(!is.na(ylim)){
      boxplot(t(ans[1,,]), cex.main=1.5,  cex.axis=1.5, cex.lab=1.5,
              main=paste(dim,"-parameter ",
                         objFun," problem", sep=""), ylim=ylim,
              ylab="Obj. fun. value",
              col=palette(rainbow(13)))
    }else
      boxplot(t(ans[1,,]), cex.main=1.5,  cex.axis=1.5, cex.lab=1.5,
              main=paste(dim,"-parameter ",
                         objFun," problem", sep=""),
              ylab="Obj. fun. value",
              col=palette(rainbow(13)))
    abline(h=87.3095, col=2, lwd=1)

    dev.off()



    ylim <- c(87, 87.3095)

    boxplot(t(ans[1,,]), cex.main=1.5,  cex.axis=1.5, cex.lab=1.5,
            main=paste(dim,"-parameter ",
                       objFun," problem (alternative axis limits)", sep=""),
            ylim=ylim,
            ylab="Obj. fun. value",
            col=palette(rainbow(13)))

    abline(h=87.3095, col=2, lwd=1)

    dev.off()

    ## Boxplots dos resultados (de 65 a 87.3)

    pdf(paste(3,".pdf",sep=""),width=9,height=5)

    par(las=2,mar=c(8.5, 7, 4, 2), mgp=c(5,1,0))

    ylim <- c(65,87.3095)

    if(!is.na(ylim)){
      boxplot(t(ans[1,,]), cex.main=1.5,  cex.axis=1.5, cex.lab=1.5,
              main=paste(dim,"-parameter ",
                         objFun," problem", sep=""), ylim=ylim,
              ylab="Obj. fun. value",
              col=palette(rainbow(13)))
    }else
      boxplot(t(ans[1,,]), cex.main=1.5,  cex.axis=1.5, cex.lab=1.5,
              main=paste(dim,"-parameter ",
                         objFun," problem", sep=""),
              ylab="Obj. fun. value",
              col=palette(rainbow(13)))
    abline(h=87.3095, col=2, lwd=1)

    dev.off()



    ylim <- c(65,87.3095)

    boxplot(t(ans[1,,]), cex.main=1.5,  cex.axis=1.5, cex.lab=1.5,
            main=paste(dim,"-parameter ",
                       objFun," problem (alternative axis limits)", sep=""),
            ylim=ylim,
            ylab="Obj. fun. value",
            col=palette(rainbow(13)))

    abline(h=87.3095, col=2, lwd=1)

    dev.off()
