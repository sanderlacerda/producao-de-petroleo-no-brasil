#################
# CARREGA DADOS #
#################

ppet40 <- read.table(file="c:/simulacao/resultados ppet 40.txt",header=TRUE)
ppet70 <- read.table(file="c:/simulacao/resultados ppet 70.txt",header=TRUE)
par(mfrow=c(1,2))

###########################################
# RAZ?O ENTRE O VALOR DA PRODU??O E O PIB #
###########################################

# ajusta tamanho 
valprod_pib40 <- c(ppet40[,27],seq(0,0,length.out=nrow(ppet70)-nrow(ppet40)))
valprod_pib70 <- ppet70[,27]

# substitui zeros por NAs
valprod_pib40[valprod_pib40 == 0] <- NA
valprod_pib70[valprod_pib70 == 0] <- NA
grafico1 <- data.frame(valprod_pib70,valprod_pib40)
matplot(
	ppet70[,1],grafico1*100,
	bty="l",type="l",
	lty=c(1,2,3),
	xaxs="r",yaxs="i",
	lab=c(6, 6, 7),
	col=c('black','blue','black'),
	xlab="",ylab="% do PIB",main="Valor da produ??o como % do PIB",cex.main=1
)
leg.txt <- c("barril a US$ 70","barril a US$ 40")
legend(
	2040,5,leg.txt,
	#type=c("l","l"),
	bty="n",lty = c(1,2,3),
	col=c('black','blue','black'),merge = TRUE)

###############################################	
# RAZ?O ENTRE O VALOR DAS EXPORTA??ES E O PIB #
###############################################

# ajusta tamanho 
comex_pib40 <- ppet40[1:41,29]
comex_pib70 <- ppet70[1:41,29]

# substitui zeros por NAs
#comex_pib40[comex_pib40 <= 0] <- NA
#comex_pib70[comex_pib70 <= 0] <- NA
grafico2 <- data.frame(comex_pib70,comex_pib40)
matplot(
	ppet70[1:41,1],grafico2*100,
	bty="l",type="l",
	lty=c(1,2,3),
	xaxs="r",yaxs="i",
	lab=c(6, 6, 7),
	col=c('black','blue','black'),
	xlab="",ylab="% do PIB",main="Valor das exporta??es como % do PIB",cex.main=1
)

savePlot("produ??o e exporta??es com % do PIB",type=c("jpeg"))
