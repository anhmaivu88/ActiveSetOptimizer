
data.T <- read.csv("res_True",sep=' ',header=F)
data.F <- read.csv("resFalse",sep=' ',header=F)
data.T.2 <- read.csv("res2_True",sep=' ',header=F)
data.F.2 <- read.csv("res2_False",sep=' ',header=F)


png("runtimes.png")
plot(x=5:20,data.T[,3],main="Average Runtime vs Number of Nodes on Log Scale",xlab="N",ylab="Average Runtime in ms",ylim=c(51,450000),type='b',col='red',log="y")
legend("topleft",9.5,c("DS Cubic","S Cubic","DS Random","S Random"),col=c('red','blue','red','blue'),lty=c(1,1,2,2))
lines(x=5:16,data.F[,3],type='b',col='blue')
lines(x=5:20,data.T.2[,3],type='b',col='red',lty=2)
lines(x=5:16,data.F.2[,3],type='b',col='blue',lty=2)
dev.off()

png("attempts.png")
plot(x=5:20,data.T[,2],main="Average Number of Attempts to Find Hamiltonian Path\n vs Number of Nodes",xlab="N",ylab="Average Number of Attempts",ylim=c(1,10),type='b',col='red')
legend("topleft",9.5,c("DS Cubic","S Cubic","DS Random","S Random"),col=c('red','blue','red','blue'),lty=c(1,1,2,2))
lines(x=5:16,data.F[,2],type='b',col='blue')
lines(x=5:20,data.T.2[,2],type='b',col='red',lty=2)
lines(x=5:16,data.F.2[,2],type='b',col='blue',lty=2)
dev.off()

png("successrate.png")
plot(x=5:20,data.T[,1],main="Success Rate in Ten Trials vs Number of Nodes",xlab="N",ylab="Fraction Hamiltonian Cycles Found",ylim=c(0,1),type='b',col='red')
legend("bottomleft",9.5,c("DS Cubic","S Cubic","DS Random","S Random"),col=c('red','blue','red','blue'),lty=c(1,1,2,2))
lines(x=5:16,data.F[,1],type='b',col='blue')
lines(x=5:20,data.T.2[,1],type='b',col='red',lty=2)
lines(x=5:16,data.F.2[,1],type='b',col='blue',lty=2)
dev.off()

