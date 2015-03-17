#######################################################################################################
############################### SIMULA??O DA PRODU??O DE PETR?LEO #####################################
#######################################################################################################
#--------------------------------------- PAR?METROS --------------------------------------------------#
# fator de convers?o de m3 para barris
m3_barris=6.28981 
################################### PAR?METROS FISCAIS ################################################
### PAR?METROS PARA C?LCULO DA PARTICIPA??O ESPECIAL
parametro <- read.table(file="c:/simulacao/input/participacao_especial_parametros.txt")# par?metros         
volum     <- read.table(file="c:/simulacao/input/participacao_especial_volumes.txt")   # volumes tributados 
aliquota  <- read.table(file="c:/simulacao/input/participacao_especial_aliquotas.txt") # aliquotas nominais 
### AL?QUOTAS DE TRIBUTOS E GASTOS OBRIGAT?RIOS                                                         
aliq_royalties   <- 0.1      # al?quota dos royalties
aliq_ir          <- 0.25     # al?quota do imposto de renda de pessoas jur?dicas
aliq_csll        <- 0.09     # al?quota da contribui??o social sobre o lucro l?quido
aliq_ped         <- 0.01     # al?quota dos gastos obrigat?rios em pesquisa e desenvolvimento
trib_ind_op      <- 0.2      # tributos indiretos incidentes sobre os custos operacionais
trib_ind_ds      <- 0.06     # tributos indiretos incidentes sobre os custos de desenvolvimento da produ??o
### PAR?METROS MACROECON?MICOS
pibr             <- 2889718577034.63 # PIB de 2008 em reais
taxa_cambio      <- 2                # taxa de c?mbio 
taxa_cs_pib      <- 0.035            # taxa de crescimento do PIB
### PAR?METROS ECON?MICOS E FINANCEIROS     
taxa_juros_terc  <- 0.08     # taxa de juros incidente sobre o capital de terceiros
taxa_deprec      <- 0.05     # taxa de depreciacao dos equipamentos 
ano_inicial      <- 2010     # ano em que o projeto ? iniciado
tangiveis        <- 0.7      # porcentagem de tangiveis no investimento
debeq            <- 0.5      # relacao entre d?vida e capital
gastos_locais    <- 0.65     # % do investimento total destinado a compras no mercado dom?stico
vida_util_platf  <- 25       # vida da plataforma de acordo com seu projeto, em anos
tempo_construcao <- 4        # tempo necess?rio para a contrata??o e a constru??o de uma unidade de produ??o
### PRE?OS E CUSTOS 
custo_op_anual   <- 4*120000*365      # custo operacional anual de uma unidade de produ??o, sem tributos indiretos
taxa_desc        <- 0.08              # taxa de desconto
custos_desenv    <- c(0.9,0.9,0.9,0.9)*1000000000
#read.table(file="c:/simulacao/input/custos_desenvolvimento_producao.txt")# CUSTOS DE DESENVOLVIMENTO DA PRODU??O
custos_desenv=as.matrix(custos_desenv)                                             # transforma em mode numeric
# PRODU??O
platf_padrao     <- read.table(file="c:/simulacao/input/platf_padrao1.txt")        # CURVA DE PRODU??O DE UMA PLATAFORMA 
cenario          <- read.table(file="c:/simulacao/input/cenario2.txt")             # QUANTIDADE DE PLATAFORMAS QUE ENTRAM EM OPERA??O A CADA ANO
cenario          <- cenario[,2]                                                    # seleciona apenas a coluna com dados de plataformas que entram em opera??o e ignora a coluna de anos
# numero de anos entre a primeira e a ?ltima das unidades de produ??o colocadas em opera??o
dsplrows         <- length(cenario)                                                                   

################################# INICIO DO LOOP DE PRE?OS DO PETR?LEO ###################################
# PRE?O DO PETR?LEO (US$ por barril)
precos_pet     <- c()
res_ec_recup   <- c()
vida_ec_platf  <- c()
for (ppet in 150:40) {

###########################################################################################################
################################                  PRODU??O                #################################
###########################################################################################################
################################   HORIZONTE DE PRODUCAO DE CADA UNIDADE  #################################
# custo operacional de uma plataforma (incluindo tributos indiretos), durante a sua vida util
opex_vida_util    <- custo_op_anual*(1+trib_ind_op)*(seq(1,1,length.out=vida_util_platf))
resultado_op_platf<-((1-aliq_royalties-aliq_ped)*(ppet*platf_padrao*365))-opex_vida_util
horiz_plat        <- sum(resultado_op_platf>0)         # horizonte de produ??o de uma plataforma
horizonte         <- horiz_plat+4+dsplrows-1           # horizonte de produ??o do campo
prod_plat         <- platf_padrao[[1]][1:horiz_plat]   # produ??o da plataforma em cada ano de sua vida econ?mica
prod_plat_total   <- sum(prod_plat*365)                # produ??o da plataforma durante sua vida econ?mica 
opexunit          <- opex_vida_util[1:horiz_plat]      # custo operacional da plataforma em cada ano de sua vida econ?mica 
opexmedio         <- sum(opexunit)/prod_plat_total     # custo operacional m?dio da plataforma 
### DATAS #
anos <- c()
for (i in 1:horizonte) {
	anos[i] <- ano_inicial+i-1                        # anos ? o vetor com as datas do projeto
}
### Proje??o do PIB (em US$) #
pibc <- c()
for (i in 1:(horizonte+1)) {                           # acrescenta 1 a horizonte para ajustar ano inicial da proje??o do PIB (2009) com o ano inicial do horizonte do projeto (2010))
	pibc[i] <- (pibr/taxa_cambio)*((1+taxa_cs_pib)^(i))
}
pib <- pibc[2:(horizonte+1)]                           # PIB a partir de 2010
write.table(data.frame(anos,pib),file = "c:/simulacao/pib.txt") # grava a s?rie do PIB 
###########################################################################################################
###############################    PRODU??O DO CONJUNTO DAS UNIDADES      #################################
###########################################################################################################
ptempo <- c() # inicializa a vari?vel ptempo
for (i in 1:dsplrows) {
	aux         <- c(seq(0,0,length.out=tempo_construcao+i-1),prod_plat,seq(0,0,length.out=dsplrows-i))
	aux1        <- cenario[i]*aux
	ptempo      <- c(ptempo,aux1)
}
dim(ptempo)      <- c(horizonte,dsplrows)                   # estabelece as dimens?es de ptempo 
presal           <- rowSums(ptempo)                         # produ??o do campo em barris por dia
proano           <- 365*presal                              # produ??o do campo em barris por ano
prodtotal        <- sum(proano)                             # volume total da produ??o do campo durante sua vida econ?mica
valprod          <- ppet*proano                             # valor da produ??o do campo em cada ano de sua vida econ?mica
valprodtotal     <- sum(valprod)                            # valor da produ??o total do campo durante sua vida econ?mica
pico             <- max(presal)                             # pico de produ??o
anodopico        <- anos[presal==max(presal)]               # ano do pico de produ??o do pr?-sal
write.table(ptempo,file = "c:/simulacao/ptempo.txt")        # grava ptempo
write.table(data.frame(anos,presal),file = "c:/simulacao/presal.txt")        # grava presal
###########################################################################################################
###############################                    CUSTOS                   ###############################
###########################################################################################################
###############################     CUSTOS DE DESENVOLVIMENTO DA PRODU??O    ##############################
mdspl            <- c() # inicializa a vari?vel 
for (i in 1:dsplrows) {
	aux         <- c(seq(0,0,length.out=i-1),(1+trib_ind_ds)*custos_desenv,seq(0,0,length.out=horizonte-4-i+1))
	aux1        <- cenario[i]*aux
	mdspl       <- c(mdspl,aux1)      # mdspl ? a matriz com os custos do desenvolvimento das plataforma ao longo do tempo
}
dim(mdspl)       <- c(length(aux),dsplrows) 
capex            <-  rowSums(mdspl)               # capex, com tributos indiretos
capexpib         <-  capex/pib                    # investimento como % PIB 
capexpibmax      <-  max(capexpib)                # investimento maximo como % PIB
capextotal       <-  sum(capex)                   # investimento total
######################################## CUSTOS OPERACIONAIS ##############################################
opexmat          <- c()
for (i in 1:dsplrows) {
	aux         <- c(seq(0,0,length.out=tempo_construcao+i-1),opexunit,seq(0,0,length.out=dsplrows-i))
	aux1        <- cenario[i]*aux
	opexmat     <- c(opexmat,aux1)     
}
dim(opexmat)     <- c(length(aux),dsplrows) 
opex             <- rowSums(opexmat);             # custo operacional
opextotal        <- sum(opex);                    # custo operacional total
custos=data.frame(capex,opex)
custos[custos==0]<- NA                            # substitui zero por NA
write.table(mdspl,file = "c:/simulacao/mdslp.txt")# grava 
write.table(opexmat,file = "c:/simulacao/opexmat.txt") # grava 
########################################### DEPRECIACAO ##################################################
# ajusta periodo de depreciacao para vida economica da plataforma menor do que 20 anos
if (horiz_plat<20) prazo  <-  horiz_plat else prazo=20
# valor dos equipamentos (tang?veis) de uma unidade de produ??o ao longo do per?odo de deprecia??o (20 anos)
valorcont <- c()
for (i in 1:prazo) {
	if (i==1) {
		valorcont <- c(valorcont,(1+trib_ind_ds)*colSums(custos_desenv)*tangiveis)
	} 
	else {
		valorcont <- c(valorcont,valorcont[i-1]-(taxa_deprec*valorcont[1]))
	}
}
capital_fis_m      <- seq(0,0,length.out=horizonte*dsplrows)
dim(capital_fis_m) <- c(horizonte,dsplrows)
depreciacao_m      <- seq(0,0,length.out=horizonte*dsplrows)
dim(depreciacao_m) <- c(horizonte,dsplrows)
for (i in 1:dsplrows) {
	capital_fis_m[(4+i):(3+i+prazo),i]=cenario[i]*valorcont
	depreciacao_m[(4+i):(3+i+prazo),i]=taxa_deprec*valorcont[1]*seq(1,1,length.out=prazo)
}
capital_fis <- rowSums(capital_fis_m)
depreciacao <- rowSums(depreciacao_m)
######################################### COMPRAS ###########################################################
domest           <- gastos_locais*capex        # gastos de investimento no mercado domestico
domestotal       <- sum(domest)
import           <- (1-gastos_locais)*capex    # gastos de investimentos em importacoes
importotal       <- sum(import)
###################################### TRIBUTOS INDIRETOS ####################################################
val_tiprod       <- trib_ind_op*opex           # tributos indiretos incidentes sobre o custo operacional
val_tiprodtotal  <- sum(val_tiprod)            # total
val_tids         <- trib_ind_ds*capex          # tributos indiretos incidentes sobre os custos de desenvolvimento
val_tidstotal    <- sum(val_tids)              # total
val_tind         <- val_tiprod+val_tids        # tributos indiretos
val_tindtotal    <- sum(val_tind)              # tributos indiretos total
########################################## ROYALTIES #########################################################
royano           <- aliq_royalties*valprod     # pagamentos anuais de royalties
roytotal         <- sum(royano)                # pagamentos de royalties total
#################################### GASTOS OBRIGATORIOS EM P&D ##############################################
ped              <- aliq_ped*valprod           # gastos obrigatorios em PeD
pedtotal         <- sum(ped)                   # gastos obrigatorios em PeD total
########################################## RESULTADO OPERACIONAL #############################################
# receita liquida (receita bruta menos capex e menos opex)
rec_liqtotal     <- sum(valprod-capex-opex)    # receita a ser divida entre governo e operadora
# para calculo da amortizacao de gastos dedutiveis e para a incidencia da participacao especial 
res_op           <- valprod-capex-opex-royano-ped
res_optotal      <- sum(res_op)                # valor liquido da producao total
res_opac         <- cumsum(res_op)             # valor liquido da producao acumulado
###################################### CAPITAL OPERACIONAL DO PROJETO #########################################
capital_op       <- -min(res_opac)
####################################### DESPESAS ADMINISTRATIVAS ##############################################
desp_admtotal    <- (1.94/taxa_cambio)*prodtotal
desp_adm         <- desp_admtotal/horizonte*seq(1,1,length.out=horizonte)
############################################ FINANCIAMENTO ####################################################
### financiamento sem capitalizacao de juros - SAC ###
carencia         <- 5
prazo            <- carencia+7
# inicializa??o #
amort_princ      <- seq(0,0,length.out=prazo)
prest            <- seq(0,0,length.out=prazo)
desp_fin         <- seq(0,0,length.out=horizonte)  # entra no calculo da part. esp. e por isso tem esse tamanho
juros            <- seq(0,0,length.out=prazo)
desemb           <- seq(0,0,length.out=prazo)
principal        <- seq(0,0,length.out=prazo)
saldo_dev        <- seq(0,0,length.out=prazo)
# valores iniciais # 
desemb[1]        <- debeq*(res_op[1])
principal[1]     <- desemb[1]
saldo_dev[1]     <- principal[1]
# desembolsos #
for (i in 2:(carencia+1)) { # resultado operacional positivo e dentro do per?odo de car?ncia
	if (res_op[i]>0) break
	desemb[i]   <- debeq*res_op[i]
}
for (i in 2:prazo) {
	juros[i] <- taxa_juros_terc*principal[i-1]
		if (i<=carencia) {
			amort_princ[i] <- 0
			principal[i]   <- principal[i-1]+desemb[i]
		}
		if (i>carencia) {
			amort_princ[i] <- principal[i-1]/(prazo-i+1)
			principal[i] <- principal[i-1]+desemb[i]-amort_princ[i]
		}
	prest[i]         <- juros[i]+amort_princ[i]
	desp_fin[i]      <- -juros[i]
	saldo_dev[i]     <- principal[i]
}
desp_fintotal    <- sum(desp_fin)
############################## PARTICIPACAO ESPECIAL e LUCRO ANTES DOS TRIBUTOS DIRETOS ##############################
aliq        <- seq(0,0,length.out=horizonte)  # inicializa matriz de aliquotas nominais
aliqefet    <- seq(0,0,length.out=horizonte)  # inicializa matriz de aliquotas efetivas
par         <- seq(0,0,length.out=horizonte)  # inicializa matriz de parametros para calculo da part. esp
### encontrando o primeiro ano de producao da primeira plataforma ###
for (i in 1:length(presal)) {if (presal[i]>0) break}
### encontrando valores de par?metros e al?quotas nominais para cada volume de produ??o, do 1o ao 4o ano de produ??o ###
for (t in 0:3) {
	if (presal[i+t] > volum[1,t+1] && presal[i+t] <= volum[2,t+1]) {
		par[i+t]   <- parametro[1,t+1]
		aliq[i+t]  <- aliquota[1,1]
	}
	if (presal[i+t] > volum[2,t+1] && presal[i+t] <= volum[3,t+1]) {
		par[i+t]   <- parametro[2,t+1]
		aliq[i+t]  <- aliquota[2,1]
	}
	if (presal[i+t] > volum[3,t+1] && presal[i+t] <= volum[4,t+1]) {
		par[i+t]   <- parametro[3,t+1]
		aliq[i+t]  <- aliquota[3,1]
	}
	if (presal[i+t] > volum[4,t+1] && presal[i+t] <= volum[5,t+1]) {
		par[i+t]   <- parametro[4,t+1]
		aliq[i+t]  <- aliquota[4,1]
	}
	if (presal[i+t] > volum[5,t+1]) {
		par[i+t]   <- parametro[5,t+1]
		aliq[i+t]  <- aliquota[5,1]
	}
}
### 4o ano em diante ###
for (t in 4:(horizonte-i)) {
	if (presal[i+t] >  volum[1,4] && presal[i+t] <= volum[2,4]) {
		par[i+t]   <- parametro[1,4]
		aliq[i+t]  <- aliquota[1,1]
	}
	if (presal[i+t] >  volum[2,4] && presal[i+t] <= volum[3,4]) {
		par[i+t]   <- parametro[2,4]
		aliq[i+t]  <- aliquota[2,1]
	}
	if (presal[i+t] >  volum[3,4] && presal[i+t] <= volum[4,4]) {
		par[i+t]   <- parametro[3,4]
		aliq[i+t]  <- aliquota[3,1]
	}
	if (presal[i+t] >  volum[4,4] && presal[i+t] <= volum[5,4]) {
		par[i+t]   <- parametro[4,4]
		aliq[i+t]  <- aliquota[4,1]
	}
	if (presal[i+t] >  volum[5,4]) {
		par[i+t]   <- parametro[5,4]
		aliq[i+t]  <- aliquota[5,1]
	}
}
### montando aliquotas efetivas ###
for (i in 1:horizonte) {
	if (presal[i]==0) aliqefet[i] <- 0 # se a producao ? nula, a al?quota ? zero
	else aliqefet[i] <- (1-(par[i]/presal[i]))*aliq[i]
}
### C?LCULO DA PE e dos tributos diretos em um mesmo loop 
res_ex_ant_pe  <- seq(0,0,length.out=length(presal))  # resultado de exerc?cios anteriores para fins de c?lculo da participa??o especial
part_esp       <- seq(0,0,length.out=length(presal))
### parametros de tributos diretos ###
lucro_antes_td <- seq(0,0,length.out=length(presal))
for (i in 1:horizonte) {
	if (base_inc_pe[i]<0) {                        # se a base de incid?ncia da participa??o especial ? negativa ...
		part_esp[i]      <- 0                     # ... n?o se paga participa??o especial ... 
		res_ex_ant_pe[i] <- sum(base_inc_pe[1:i]) # ... e o resultado de exerc?cios anteriores ? a soma dos (valores negativos dos) resultados operacionais deduzidos da deprecia??o
	}
	if (base_inc_pe[i]>0 && res_ex_ant_pe[i-1]<0 && res_ex_ant_pe[i-1]<=-base_inc_pe[i]) {  # se a base de incid?ncia da participa??o especial ? positiva e o valor de resultados anteriores ? negativo e menor ou igual ao teto de despesas dedut?veis
		res_ex_ant_pe[i] <- res_ex_ant_pe[i-1]+base_inc_pe[i]
	}
	if (base_inc_pe[i]>0 && res_ex_ant_pe[i-1]<0 && -res_ex_ant_pe[i-1]<=base_inc_pe[i]) {    # esgota-se a deducao de resultados operacionais negativos de periodos anteriores 
		part_esp[i]      <- (base_inc_pe[i]+res_ex_ant_pe[i-1])*aliqefet[i]
	}
	if (base_inc_pe[i]>0 && res_ex_ant_pe[i-1]==0) {
		part_esp[i] <- base_inc_pe[i]*aliqefet[i]
	}
	# uma vez calculada a participa??o especial e o resultado de exerc?cios anteriores ...
	# ... encontramos o lucro antes dos tributos diretos, que depender? de haver lucro ...
	if (res_op[i]-part_esp[i]-desp_fin[i]-desp_adm[i]-depreciacao[i]<0) {   
		lucro_antes_td[i] <- 0                                              	
	}
	if (res_op[i]-part_esp[i]-desp_fin[i]-desp_adm[i]-depreciacao[i]>0) {
		lucro_antes_td[i] <- res_op[i]-part_esp[i]-desp_fin[i]-desp_adm[i]-depreciacao[i]  # ... ou n?o
	}
}
part_esptotal <- sum(part_esp)                              # participacao especial total
##################################### AMORTIZACAO DAS DESPESAS DEDUTIVEIS ###########################
amort_dd <- seq(0,0,length.out=length(presal))
for (i in 1:length(presal)) {
	if (res_op[i]>0 && res_opac[i]<0) {                    # se o resultado operacional ? positivo mas o resultado operacional acumulado ? negativo ...
		amort_dd[i] <- res_op[i]-part_esp[i]              # ... a amortiza??o das despesas dedut?veis ? a diferen?a entre o resultado operacional e a participa??o especial
	}
	if (res_op[i]>0 && res_opac[i-1]<0 && res_opac[i]>0) { # ?ltima amortiza??o
		amort_dd[i] <- res_op[i]-res_opac[i]
	}
}
########################################## TRIBUTOS DIRETOS ########################################
imposto_renda       <- seq(0,0,length.out=length(presal))
contribuicao_social <- seq(0,0,length.out=length(presal))
for (i in 1:length(presal)) {
	if (lucro_antes_td[i]<=0) {                               # se o lucro antes do tributos diretos ? menor ou igual a zero ...
		imposto_renda[i]       <- 0                          # ... o imposto de renda ? igual a zero
		contribuicao_social[i] <- 0                          # ... assim como a contribui??o social ...
	}
	else {                                                    # ... caso contr?rio ...
		imposto_renda[i]       <- aliq_ir*lucro_antes_td[i]  # ... o imposto de renda ? calculado sobre o lucro antes dos impostos diretos ...
		contribuicao_social[i] <- aliq_csll*lucro_antes_td[i]# ... e a contribui??o social idem
	}
}
imposto_rendatotal <- sum(imposto_renda)                       # imposto de renda total
contribuicao_socialtotal <- sum(contribuicao_social)           # CSLL total
trib_dir         <- imposto_renda+contribuicao_social          # tributos diretos incidentes sobre o lucro
trib_dirtotal    <- sum(trib_dir)                              # total dos tributos diretos
#################### LUCRO, FLUXO DE CAIXA E NECESSIDADE DE FINANCIAMENTO #######################
lucro            <- res_op-part_esp-desp_fin-desp_adm-imposto_renda-contribuicao_social
fcac             <- cumsum(lucro)                              # fluxo de caixa acumulado
fin              <- -min(fcac)                                 # necessidade de financiamento
lucrototal       <- sum(lucro)                                 # lucro total
################################### GOVERNMENT & COMPANY TAKE ####################################
rec_gov          <- royano+part_esp+trib_dir                   # fluxo da receita do governo 
rec_gov_pib      <- rec_gov/pib                                # receita do governo (excluindo os tributos indiretos) como % do PIB
rec_govtotal     <- sum(rec_gov)                               # total da receita do governo
gvtakevlp        <- rec_govtotal/rec_liqtotal                  # participa??o do governo 
comptakevlp      <- (rec_liqtotal-rec_govtotal)/rec_liqtotal   # participa??o privada
##################################    INDICADORES DE CUSTO    #####################################
opexbarril       <- opextotal/prodtotal                          # custo operacional por barril produzido (sem participa??es governamentais)
opexbarrilpg     <- (opextotal+roytotal+part_esptotal)/prodtotal # custo operacional por barril produzido (com participa??es governamentais)
capexbarril      <- capextotal/prodtotal                         # custo de investimento por barril produzido
########################################### FINANCEIRO ############################################
# vetor com fatores de desconto
desc=c()
for (i in 1:horizonte) {
	desc <- c(desc,1/((1+taxa_desc)^(i-1)))
}
fcdesc           <- lucro*desc         # fluxo de caixa descontado
fcdescac         <- cumsum(fcdesc)     # fluxo de caixa acumulado
VPL              <- sum(fcdesc)        # VPL 
# procurando ano de pay back 
for (i in 1:horizonte) {
	if (fcac[i]>0) break
}
payback <- anos[i]
########################################## RENTABILIDADE ##########################################
# operacional
rent_op  <- res_op/capital_op
# financeira
rent_fin <- lucro/fin
############################################## TIR ################################################
func_tir <- function(tir) {        #procura taxa de retorno que zera o VPL
	VPL1  <- 1
	t     <- tir
	while(VPL1>0.0001) {
		for (i in 1:horizonte) {
			desc1[i] <- 1/((1+t)^(i-1))
		}
		fcdesc1 <- lucro*desc1
		VPL1    <- sum(fcdesc1)
		t       <- t+0.001
	}
	return(t)
}
tir <- 100*sapply(0,func_tir)
################################# FIM DO LOOP DE PRE?OS DO PETR?LEO ################################
precos_pet         <- c(precos_pet,ppet)
res_ec_recup       <- c(res_ec_recup,prodtotal/1000000000)
vida_ec_platf      <- c(vida_ec_platf,horiz_plat)

if (tir<0) break
}
####################################################################################################
par(ask=TRUE)
mostra <- data.frame(precos_pet,res_ec_recup,vida_ec_platf)
write.table(mostra,file = "c:/simulacao/reservas ec recup.txt") # grava 
# RESERVAS ECONOMICAMENTE RECUPER?VEIS contra PRE?O DO PETR?LEO
plot(
	precos_pet,res_ec_recup,
	type=c("l"),pch=1,bty="l",lwd=c(1),
	col=c('royalblue1'),lty = c(1),
	xlab="pre?o do petr?leo (US$ por barril)",ylab="bilh?es de barris",main="")
grid(NULL,NULL,lwd = 1,col='gray41',lty="dotted")
#  VIDA ECON?MICA DAS PLATAFORMAS contra PRE?O DO PETR?LEO
plot(
	precos_pet,vida_ec_platf,
	type=c("l"),pch=1,bty="l",lwd=c(1),
	col=c('royalblue1'),lty = c(1),
	xlab="pre?o do petr?leo (US$ por barril)",ylab="anos",main="")
grid(NULL,NULL,lwd = 1,col='gray41',lty="dotted")
