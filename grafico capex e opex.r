#################
# CARREGA DADOS #
#################

ppet40 <- read.table(file="c:/simulacao/resultados ppet 40.txt",header=TRUE)
ppet70 <- read.table(file="c:/simulacao/resultados ppet 70.txt",header=TRUE)

#############################################################
# CAPEX E OPEX PARA DIFERENTES PRE?OS DO BARRIL DE PETR?LEO #
#############################################################

# ajusta tamanho do opex com ppet40
opex40 <- c(ppet40[,8],seq(0,0,length.out=nrow(ppet70)-nrow(ppet40)))
capex70<- ppet70[,7]
opex70 <- ppet70[,8]
# substitui zeros por NAs
opex40[opex40 == 0] <- NA
opex70[opex70 == 0] <- NA
capex70[capex70 == 0] <- NA
custos <- data.frame(capex70,opex40,opex70)
matplot(
	ppet70[,1],custos/1000000000,
	bty="l",type="l",
	lty=c(1,2,3),
	xaxs="r",yaxs="i",
	lab=c(6, 6, 7),
	col=c('black','blue','black'),
	xlab="",ylab="bilh?es de US$",main="",cex.main=1
)
leg.txt <- c("capex","opex a US$ 40","opex a US$ 70")
legend(
	2023,5,leg.txt,
	#type=c("l","l"),
	bty="n",lty = c(1,2,3),
	col=c('black','blue','black'),merge = TRUE)
savePlot("capex e opex para diferentes pre?os do petr?leo",type=c("jpeg"))
