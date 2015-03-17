
ppet40 <- read.table(file="c:/simulacao/arrecadacao ppet 40.txt")
ppet70 <- read.table(file="c:/simulacao/arrecadacao ppet 70.txt")

par(mfrow=c(1,2))

###################################################
# ARRECADA??O DE TRIBUTOS SOBRE O ?LEO DO PR?-SAL #
################################################### 
#arrecadacao <- data.frame(imposto_renda+contribuicao_social,royano,part_esp,val_tind)

matplot(
	ppet40[,1],ppet40[,2:ncol(ppet40)]/1000000000,
	bty="l",type="l",
	lty=c(1,2,3,4,5),
	xaxs="r",yaxs="i",
	lab=c(6, 6, 7),
	col=c('turquoise3','blue','black','green'),
	xlab="",ylab="bilh?es de US$",main="barril a US$ 40",cex.main=1
)

matplot(
	ppet70[,1],ppet70[,2:ncol(ppet70)]/1000000000,
	bty="l",type="l",
	lty=c(1,2,3,4,5),
	xaxs="r",yaxs="i",
	lab=c(6, 6, 7),
	col=c('turquoise3','blue','black','green'),
	xlab="",ylab="bilh?es de US$",main="barril a US$ 70",cex.main=1
)

savePlot("arrecada??o de tributos sobre o ?leo do pr?-sal",type=c("jpeg"))
