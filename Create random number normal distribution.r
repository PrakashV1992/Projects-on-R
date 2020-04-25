set.seed(3000)
xseq<-seq(-4,4,.01)
densities<-dnorm(xseq, 0,1)
cumulative<-pnorm(xseq, 0, 1)
randomdeviates<-rnorm(1000,0,1)

par(mfrow=c(1,3), mar=c(3,4,4,2))

plot(xseq, densities, col="darkgreen",xlab="", ylab="Density", type="l",lwd=2, cex=2, main="PDF of Standard Normal", cex.axis=.8)

plot(xseq, cumulative, col="darkorange", xlab="", ylab="Cumulative Probability",type="l",lwd=2, cex=2, main="CDF of Standard Normal", cex.axis=.8)

hist(randomdeviates, main="Random draws from Std Normal", cex.axis=.8, xlim=c(-4,4))

xseq<-seq(-4,4,.01)
y<-2*xseq + rnorm(length(xseq),0,5.5)
 
  hist(y, prob=TRUE, ylim=c(0,.06), breaks=20)
curve(dnorm(x, mean(y), sd(y)), add=TRUE, col="darkblue", lwd=2)
