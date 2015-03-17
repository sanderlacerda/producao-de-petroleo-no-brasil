#######################################################################################################
############################### SIMULA??O DA PRODU??O DE PETR?LEO #####################################
#######################################################################################################
#--------------------------------------- PAR?METROS --------------------------------------------------#
######################################
# PRE?O DO PETR?LEO (US$ por barril) #
######################################
ppet             <- 70
################################### PAR?METROS FISCAIS ################################################

# PAR?METROS PARA C?LCULO DA PARTICIPA??O ESPECIAL

parametro <- read.table(file="c:/simulacao/input/participacao_especial_parametros.txt")# par?metros da participa??o especial
volum     <- read.table(file="c:/simulacao/input/participacao_especial_volumes.txt")   # volumes tributados 
aliquota  <- read.table(file="c:/simulacao/input/participacao_especial_aliquotas.txt") # aliquotas nominais 

# AL?QUOTAS DE TRIBUTOS E GASTOS OBRIGAT?RIOS                                                         

aliq_royalties   <- 0.1      # al?quota dos royalties
aliq_ir          <- 0.25     # al?quota do imposto de renda de pessoas jur?dicas
aliq_csll        <- 0.09     # al?quota da contribui??o social sobre o lucro l?quido
aliq_ped         <- 0.01     # al?quota dos gastos obrigat?rios em pesquisa e desenvolvimento
trib_ind_op      <- 0.2      # tributos indiretos incidentes sobre os custos operacionais
trib_ind_ds      <- 0.06     # tributos indiretos incidentes sobre os custos de desenvolvimento da produ??o

# PAR?METROS MACROECON?MICOS

pibr             <- 2889718577034.63 # PIB de 2008 em reais
taxa_cambio      <- 2                # taxa de c?mbio 
taxa_cs_pib      <- 0.035            # taxa de crescimento do PIB

# PAR?METROS ECON?MICOS E FINANCEIROS     

taxa_juros_terc  <- 0.08     # taxa de juros incidente sobre o capital de terceiros
taxa_deprec      <- 0.05     # taxa de depreciacao dos equipamentos 
ano_inicial      <- 2010     # ano em que o projeto ? iniciado
tangiveis        <- 0.7      # porcentagem de tangiveis no investimento
debeq            <- 0.5      # relacao entre d?vida e capital
gastos_locais    <- 0.65     # % do investimento total destinado a compras no mercado dom?stico
vida_util_platf  <- 25       # vida da plataforma de acordo com seu projeto, em anos
tempo_construcao <- 4        # tempo necess?rio para a contrata??o e a constru??o de uma unidade de produ??o

# PRE?OS E CUSTOS 

custo_op_anual   <- 4*120000*365      # custo operacional anual de uma unidade de produ??o, sem tributos indiretos
taxa_desc        <- 0.08              # taxa de desconto
custos_desenv    <- c(0.9,0.9,0.9,0.9)*1000000000 # custos de desenvolvimento de uma unidade de produ??o
custos_desenv=as.matrix(custos_desenv)                                             # transforma em mode numeric

# PRODU??O

platf_padrao     <- read.table(file="c:/simulacao/input/platf_padrao1.txt")        # CURVA DE PRODU??O DE UMA PLATAFORMA 
cenario          <- read.table(file="c:/simulacao/input/cenario2.txt")             # QUANTIDADE DE PLATAFORMAS QUE ENTRAM EM OPERA??O A CADA ANO
cenario          <- cenario[,2]                                                    # seleciona apenas a coluna com dados de plataformas que entram em opera??o e ignora a coluna de anos
dsplrows         <- length(cenario)                                                # numero de anos entre a primeira e a ?ltima das unidades de produ??o colocadas em opera??o
m3_barris=6.28981                                                                  # fator de convers?o de m3 para barris

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
prod_plat_total   <- sum(prod_plat*365)                # produ??o total da plataforma durante sua vida econ?mica 
opexunit          <- opex_vida_util[1:horiz_plat]      # custo operacional da plataforma em cada ano de sua vida econ?mica 
opexmedio         <- sum(opexunit)/prod_plat_total     # custo operacional m?dio da plataforma 

# DATAS #

anos <- c()
for (i in 1:horizonte) {
	anos[i] <- ano_inicial+i-1                         # anos ? o vetor com as datas do projeto
}

# Proje??o do PIB (em US$)

pibc <- c()
for (i in 1:(horizonte+1)) {                           # acrescenta 1 a horizonte para ajustar ano inicial da proje??o do PIB (2009) com o ano inicial do horizonte do projeto (2010))
	pibc[i] <- (pibr/taxa_cambio)*((1+taxa_cs_pib)^(i))
}
pib <- pibc[2:(horizonte+1)]                           # PIB a partir de 2010

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
write.table(ptempo,file = "c:/simulacao/ptempo.txt")                         # grava ptempo
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
valorcont <- c()    # valor dos equipamentos (tang?veis) de uma unidade de produ??o ao longo do per?odo de deprecia??o (20 anos)
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
	capital_fis_m[(4+i):(3+i+prazo),i] <- cenario[i]*valorcont
	depreciacao_m[(4+i):(3+i+prazo),i] <- taxa_deprec*valorcont[1]*seq(1,1,length.out=prazo)
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

################################### FLUXO DE CAIXA OPERACIONAL ###############################################

# receita liquida (receita bruta menos capex e menos opex)
rec_liqtotal     <- sum(valprod-capex-opex)    # receita a ser divida entre governo e operadora
# resultado operacional 
fc_op           <- valprod-capex-opex-royano-ped
fc_optotal      <- sum(fc_op)                # valor liquido da producao total
fc_opac         <- cumsum(fc_op)             # valor liquido da producao acumulado

############################# BASE DE INCID?NCIA DA PARTICIPA??O ESPECIAL ####################################

# a base de incid?ncia da participa??o especial ? o resultado operacional deduzido da deprecia??o
base_inc_pe    <- fc_op-depreciacao
base_inc_peac  <- cumsum(base_inc_pe)          # base de incid?ncia acumulada

###################################### CAPITAL OPERACIONAL DO PROJETO #########################################

capital_op       <- -min(fc_opac)

####################################### DESPESAS ADMINISTRATIVAS ##############################################

desp_admtotal    <- (1.94/taxa_cambio)*prodtotal
desp_adm         <- desp_admtotal/horizonte*seq(1,1,length.out=horizonte)

############################################ FINANCIAMENTO ####################################################
# financiamento sem capitalizacao de juros #

carencia         <- 5
prazo            <- carencia+7
# inicializa??o da vari?veis
amort_princ      <- seq(0,0,length.out=prazo)
prest            <- seq(0,0,length.out=prazo)
desp_fin         <- seq(0,0,length.out=horizonte)  # entra no calculo da part. esp. e por isso tem esse tamanho
juros            <- seq(0,0,length.out=prazo)
desemb           <- seq(0,0,length.out=prazo)
principal        <- seq(0,0,length.out=prazo)
saldo_dev        <- seq(0,0,length.out=prazo)
# valores iniciais # 
desemb[1]        <- debeq*(fc_op[1])
principal[1]     <- desemb[1]
saldo_dev[1]     <- principal[1]
# desembolsos #
for (i in 2:(carencia+1)) { # resultado operacional positivo e dentro do per?odo de car?ncia
	if (fc_op[i]>0) break
	desemb[i]   <- debeq*fc_op[i]
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

# encontrando o primeiro ano de producao da primeira plataforma 

for (i in 1:length(presal)) {if (presal[i]>0) break}

# encontrando valores de par?metros e al?quotas nominais para cada volume de produ??o, do 1o ao 4o ano de produ??o ###

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

# 4o ano em diante #

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

# montando aliquotas efetivas #

for (i in 1:horizonte) {
	if (presal[i]==0) aliqefet[i] <- 0 # se a producao ? nula, a al?quota ? zero
	else aliqefet[i] <- (1-(par[i]/presal[i]))*aliq[i]
}

# C?LCULO DA PE E DA BASE DE INCID?NCIA DOS TRIBUTOS DIRETOS EM UM MESMO LOOP

res_ex_ant_pe  <- seq(0,0,length.out=length(presal))  # resultado de exerc?cios anteriores para fins de c?lculo da participa??o especial
part_esp       <- seq(0,0,length.out=length(presal))

# parametros de tributos diretos #

lucro_antes_td <- seq(0,0,length.out=length(presal))
for (i in 1:horizonte) {
	if (base_inc_pe[i]<0) {                        # se a base de incid?ncia da participa??o especial ? negativa ...
		part_esp[i]      <- 0                     # ... n?o se paga participa??o especial ... 
		res_ex_ant_pe[i] <- sum(base_inc_pe[1:i]) # ... e o resultado de exerc?cios anteriores ? a soma dos (valores negativos dos) resultados operacionais deduzidos da deprecia??o
	}
	if (base_inc_pe[i]>0 && res_ex_ant_pe[i-1]<0 && res_ex_ant_pe[i-1]<=-base_inc_pe[i]) {  
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

	if (fc_op[i]-part_esp[i]-desp_fin[i]-desp_adm[i]-depreciacao[i]<0) {   
		lucro_antes_td[i] <- 0                                              	
	}
	if (fc_op[i]-part_esp[i]-desp_fin[i]-desp_adm[i]-depreciacao[i]>0) {
		lucro_antes_td[i] <- fc_op[i]-part_esp[i]-desp_fin[i]-desp_adm[i]-depreciacao[i]  # ... ou n?o
	}
}
part_esptotal <- sum(part_esp)                              # participacao especial total

##################################### AMORTIZACAO DAS DESPESAS DEDUTIVEIS ###########################

amort_dd <- seq(0,0,length.out=length(presal))
for (i in 1:length(presal)) {
	if (base_inc_pe[i]>0 && base_inc_peac[i]<0) {          # se a base de incid?ncia da participa??o especial ? positiva mas existem despesas dedut?veis acumuladas ...
		amort_dd[i] <- base_inc_pe[i]-part_esp[i]         # ... a amortiza??o das despesas dedut?veis ? a diferen?a entre a base de incid?ncia da participa??o especial e a participa??o especial
	}
	if (base_inc_pe[i]>0 && base_inc_peac[i-1]<0 && base_inc_peac[i]>0) { # ?ltima amortiza??o
		amort_dd[i] <- base_inc_pe[i]-base_inc_peac[i]
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

############## LUCRO, FLUXO DE CAIXA FINANCEIRO E NECESSIDADE DE FINANCIAMENTO ###################

lucro            <- fc_op-part_esp-desp_fin-desp_adm-imposto_renda-contribuicao_social
fcac             <- cumsum(lucro)                              # fluxo de caixa financeiro acumulado
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

#############################################################################################################
# PREPARA SEQUENCIAS COM NAs PARA CONCATENAR SERIES PARA GR?FICOS

ult_ano         <- anos[length(anos)]     # ?ltimo ano da proje??o
fut  <- seq(0,0,length.out=ult_ano-2008)  # completa as s?ries de dados hist?ricos com zeros
pas  <- seq(0,0,length.out=2009-1970)     # completa as s?ries de proje??es com zeros
# transforma zeros em NAs
fut[fut==0] <- NA                         # transforma zeros em NA (no available)
pas[pas==0] <- NA

#############################################################################################################
# CARREGA S?RIES DE PRODU??O DE PETR?LEO NO BRASIL E A PROJE??O DE DA PRODU??O DAS ATUAIS RESERVAS PROVADAS
#############################################################################################################

# S?RIE HIST?RICA DA PRODU??O DE PETR?LEO NO BRASIL

prod_pet_historica <- read.table(file="c:/simulacao/input/pet_prod_m3.txt")

# S?RIE HIST?RICA DE CONSUMO DE PETR?LEO NO BRASIL

cons_pet_historica <- read.table(file="c:/simulacao/input/pet_cons_m3.txt")

# TRANSFORMA DADOS ANUAIS EM MEDIA DI?RIA

no.dias.ano   <- as.numeric(difftime(paste(prod_pet_historica[,1]+1,"-01-01",sep=""),paste(prod_pet_historica[,1],"-01-01",sep="")))
prod_pet_hist <- prod_pet_historica[,2]*m3_barris*1000/no.dias.ano
cons_pet_hist <- cons_pet_historica[,2]*m3_barris*1000/no.dias.ano
# completa com NAs
prod_pet_hist1 <- c(prod_pet_hist,fut)
cons_pet_hist1 <- c(cons_pet_hist,fut)

# PROJE??O DA PRODU??O DE PETR?LEO DAS ?REAS FORA DO PR?-SAL

prod_res_provadas    <- read.table(file="c:/simulacao/input/producao das reservas provadas.txt")

# AJUSTA DATAS DAS S?RIES DA PRODU??O DE PETR?LEO DO PR?-SAL E DE OUTRAS ?REAS

# A s?rie da proje??o da produ??o das reservas provadas inicia-se em 2009, enquanto a s?rie da simula??o da produ??o
# do pr?-sal inicia-se em 2010. A express?o abaixo ajusta as duas s?ries.
prod_res_prov        <- data.frame(prod_res_provadas[1:(length(presal)+1),2],c(0,presal))

# SOMA DAS PROJE??ES PARA A PRODU??O DO PR?-SAL E DE OUTRAS ?REAS

prod_pet_total   <- prod_res_prov[,1]+prod_res_prov[,2]

# RELA??O ENTRE A PRODU??O TOTAL DE PETR?LEO E O PIB

valprodpib       <- (prod_pet_total[2:length(prod_pet_total)]*365*ppet)/pib   # valor da produ??o brasileira de petr?leo como % do PIB
valprodpibmax    <- max(valprodpib)                         # valor m?ximo da rela??o entre valor da produ??o e PIB
ano_valprodpibmax<- anos[which(valprodpib==max(valprodpib))]# ano do valor m?ximo da rela??o entre valor da produ??o e PIB
# completa com NAs
prod_pet_total1 <- c(pas,prod_pet_total)
# PASSADO E PROJE??O DA OFERTA DE PETR?LEO
prod_pet        <- data.frame(prod_pet_hist1,prod_pet_total1)

#############################################################################################################
########################################## DEMANDA ##########################################################
#############################################################################################################

# CARREGA DADOS 

# CONSUMO DE PETR?LEO em 10^3 m3 por ano
pet_cons_m3     <- read.table(file="c:/simulacao/input/pet_cons_m3.txt")
# PIB REAL a pre?os de 2008
pib_real        <- read.table(file="c:/simulacao/input/pib_real.txt")
# raz?o entre consumo de petroleo e PIB real (barris por 10 mil US$)
pet_pib         <- (pet_cons_m3[,2]*1000*m3_barris)/(pib_real[,2]/taxa_cambio/10000)
# DATAS
passado         <- pet_cons_m3[,1]     # per?odo da s?rie hist?rica com os dados de produ??o e consumo de petr?leo
anos_total      <- seq(1970,ult_ano)   # per?odo completo

# M?NIMOS QUADRADOS NA RELA??O HIST?RICA ENTRE CONSUMO DE PETR?LEO E PIB

fit <- lm(pet_pib ~ passado)
# PREPARA DADOS PARA GR?FICO
pet_pib1  <- c(pet_pib,fut)               
ajuste    <- fit$coefficients[[1]]+fit$coefficients[[2]]*(1970:2008)     # gr?fico da curva de m?nimos quadrados para o per?odo 1970 a 2007
ajuste1   <- c(ajuste,fut)                                               # completa a s?rie com NAs
projecao  <- fit$coefficients[[1]]+fit$coefficients[[2]]*(2009:ult_ano)  # proje??o da curva de m?nimos quadrados para o futuro
projecao1 <-c(pas,projecao)                                              # completa a s?rie com NAs
demanda   <- data.frame(pet_pib1,ajuste1,projecao1)                      # matriz com os dados para construir gr?fico
# gr?fico da rela??o hist?rica entre consumo de petr?leo e PIB e sua proje??o
matplot(
	anos_total,demanda,
	type=c("l","l","l"),pch=1,bty="l",lwd=c(1,1,1),
	col=c('gray27','royalblue1','royalblue1'),lty = c(1,1,2),
	xlab="",ylab="Barris de petr?leo por US$ mil do PIB",main=""
)
savePlot("consumo de petr?leo e PIB",type=c("jpeg"))

# C?LCULO DA DEMANDA POR PETR?LEO a PARTIR DA PROJE??O DA RELA??O CONSUMO DE PETR?LEO E PIB

demanda_pet <- c()
for (i in 1:(horizonte+1)) {
	demanda_pet[i]=pibc[i]*(projecao[i]/10000)
}
demanda_pet  <-(demanda_pet/365)  # demanda diaria
demanda_pet1 <- c(pas,demanda_pet)

# CONSUMO DE PETR?LEO: S?RIE HIST?RICA E PROJE??O
pet_cons   <- data.frame(cons_pet_hist1,demanda_pet1)
# SALVA A PROJE??O DA RELA??O ENTRE CONSUMO DE PETR?LEO E PIB 
write.table(data.frame(anos_total,projecao1),file = "c:/simulacao/proj_demanda.txt") 

#############################################################################################################
######################################### IMPRIME DATA FRAME COM RESULTADOS #################################
#############################################################################################################

# completa s?ries com NAs
amort_princ <- c(amort_princ,seq(0,0,length.out=(horizonte-length(amort_princ))))
prest       <- c(prest,seq(0,0,length.out=(horizonte-length(prest))))
desp_fin    <- c(desp_fin,seq(0,0,length.out=(horizonte-length(desp_fin))))
juros       <- c(juros,seq(0,0,length.out=(horizonte-length(juros))))
desemb      <- c(desemb,seq(0,0,length.out=(horizonte-length(desemb))))
principal   <- c(principal,seq(0,0,length.out=(horizonte-length(principal))))
saldo_dev   <- c(saldo_dev,seq(0,0,length.out=(horizonte-length(saldo_dev))))
### ajusta series que come?am em 2009 para 2010
producao_reservas <- prod_res_prov[2:nrow(prod_res_prov),1]  # produ??o das reservas provadas
producao_total    <- prod_pet_total[2:length(prod_pet_total)]# produ??o total (pr?-sal mais reservas provadas)
demanda_petroleo  <- pet_cons[41:nrow(pet_cons),2]
### IMPORTA??ES E EXPORTA??ES DE PETR?LEO
comex_q           <- producao_total-demanda_petroleo         # com?rcio exterior de petr?leo em barris por dia
comex_v           <- comex_q*365*ppet                        # valor anual do com?rcio exterior de petr?leo
comexpib          <- comex_v/pib                             # valor anual do com?rcio exterior de petr?leo com % do PIB
comex_q[comex_q<0]<- 0                                       # seleciona apenas os anos com exporta??o l?quida de petr?leo
exp_pet_total     <- sum(comex_q*365)
exp_pet_total_prodtotal <- exp_pet_total/(prodtotal+12600000000)  # exporta??es de petr?leo com % da produ??o total de petr?leo

### monta data.frame
resultados  <- data.frame(
	anos,producao_reservas,presal,producao_total,demanda_petroleo,comex_q,                          # produ??o e demanda
	capex,opex,royano,ped,fc_op,fc_opac,amort_dd,aliqefet,part_esp,desp_adm,desp_fin,  
	depreciacao,lucro_antes_td,imposto_renda,contribuicao_social,lucro,fcac,                
	proano,valprod,pib,valprodpib,capexpib,comexpib,rec_gov_pib,capital_fis,rec_gov,                 
	desemb,juros,amort_princ,prest,saldo_dev,                                               # financiamento
	val_tiprod,val_tids,val_tind                                                            # tributos indiretos
)
write.table(resultados,file = "c:/simulacao/resultados.txt") 
#
###################################################################################################
####################################     GR?FICOS      ############################################
###################################################################################################

#pdf("graficos.pdf",width=7,height=7) # se habilitado: grava gr?ficos em formato pdf no arquivo graficos.pdf
par(ask=FALSE) # se habilitado (ask=TRUE): teclar "enter" para visulizar o pr?ximo gr?fico

# PRODU??O DAS RESERVAS PROVADAS E DO PR?-SAL

prod_res_prov[prod_res_prov==0] <- NA
matplot(
	anos,prod_res_prov[2:nrow(prod_res_prov),]/1000000,
	yaxs="i",
	type="l",bty="l",col='gray27',
	ylab="milh?es barris di?rios",main="",cex.main=1
)
savePlot("produ??o das reservas provadas e do pr?-sal",type=c("jpeg"))

# EVOLU??O DOS CUSTOS 

matplot(
	anos,custos/1000000000,
	type=c("l","l"),bty="l",
	col=c('black','black'),lty = c(1,2),
	xlab="",ylab="bilh?es de US$",main="",cex.main=1
)
leg.txt <- c("capex","opex")
legend(
	2030,3,leg.txt,
	#type=c("l","l"),
	bty="n",lty = c(1,2),
	col=c('black','black'),merge = TRUE)
savePlot("evolu??o dos custos",type=c("jpeg"))

# PETR?LEO: PRODU??O E CONSUMO (s?ries hist?ricas e proje??es)

pet <- data.frame(pet_cons,prod_pet)
matplot(
	anos_total,pet/1000000,
	bty="l",type=c("l","l","l","l"),lty=c(1,2,1,2),
	xaxs="r",yaxs="i",
	lab=c(6, 6, 7),
	col=c('turquoise3','turquoise3','black','black'),
	xlab="",ylab="Milh?es de barris di?rios",main="",cex.main=1
)
leg.txt <- c("Consumo","Produ??o")
legend(
	1975,6,leg.txt,
	bty="n",lty = c(1,1),
	col=c('turquoise3','black'),merge = TRUE)
savePlot("petr?leo - produ??o e consumo",type=c("jpeg"))

# ARRECADA??O DE TRIBUTOS SOBRE O ?LEO DO PR?-SAL

arrecadacao <- data.frame(imposto_renda+contribuicao_social,royano,part_esp,val_tind)
matplot(
	anos,arrecadacao/1000000000,
	bty="l",type="l",
	lty=c(1,2,3,4,5),
	xaxs="r",yaxs="i",
	lab=c(6, 6, 7),
	col=c('turquoise3','blue','black','green'),
	xlab="",ylab="bilh?es de US$",main="",cex.main=1
)
write.table(data.frame(anos,arrecadacao),file = "c:/simulacao/arrecadacao.txt") # grava a s?rie do PIB 

############################################################################################################
#dev.off() 
#############################################################################################################
########################################## GRAVA RELAT?RIO ##################################################
#############################################################################################################
cat(
"-------------------------- RESULTADOS: PRODU??O DO PR?-SAL  --------------------------------------------","\n",
"    INDICADORES OPERACIONAIS","\n",
"Produ??o total da plataforma durante sua vida econ?mica (milh?es de barris)...",format(prod_plat_total/1000000,digits=3,decimal.mark=","),"\n",
"Vida econ?mica da plataforma..............................",horiz_plat," anos","\n",
"Volume total da produ??o de petr?leo (bilh?es de barris)..",format(prodtotal/1000000000,digits=3,decimal.mark=","),"\n",
"Pico de produ??o (milh?es de barris por dia)...............",format(pico/1000000,digits=2,nsmall=1,decimal.mark=","),"\n",
"Ano do pico de produ??o...................................",anodopico,"\n",
"Receita bruta (bilh?es de US$)...........................",format(valprodtotal/1000000000,digits=2,nsmall=0,decimal.mark=",",big.mark="."),"\n",
"Gastos de investimento (bilh?es de US$):","\n",
"   No mercado dom?stico....................................",format(domestotal/1000000000,digits=2,nsmall=0,decimal.mark=","),"\n",
"   Em bens e servi?os importados...........................",format(importotal/1000000000,digits=2,nsmall=0,decimal.mark=","),"\n",
"Participa??o do governo como % da receita l?quida..........",format(gvtakevlp*100,digits=2,nsmall=0,decimal.mark=","),"%","\n",
"M?ximo da rela??o entre valor da produ??o e PIB...........",format(valprodpibmax*100,digits=2,nsmall=1),"%","\n",
"Volume exportado (bilh?es de barril)......................",format(exp_pet_total/1000000000,digits=2,nsmall=0,decimal.mark=","),"\n",
"Volume exportado como % do total produzido................",format(exp_pet_total_prodtotal*100,digits=2,nsmall=1),"%","\n",
"Custo operacional por barril (sem PG).....................",format(opexbarril,digits=2,nsmall=0,decimal.mark=","),"\n",
"Custo operacional por barril (com PG).....................",format(opexbarrilpg,digits=2,nsmall=0,decimal.mark=","),"\n",
"Custo de investimento por barril..........................",format(capexbarril,digits=2,nsmall=0,decimal.mark=","),"\n",
"    REPARTI??O DA RECEITA BRUTA (bilh?es de US$ e % da receita bruta)","\n",
"Receita do governo...........................................",format(rec_govtotal/1000000000,digits=2,nsmall=0,decimal.mark=",",big.mark="."),"  (",format(100*(rec_govtotal/valprodtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"Lucro das operadoras.........................................",format(lucrototal/1000000000,digits=2,nsmall=0,decimal.mark=",",big.mark="."),"  (",format(100*(lucrototal/valprodtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"Investimento.................................................",format(capextotal/1000000000,digits=2,nsmall=0,decimal.mark=",",big.mark="."),"  (",format(100*(capextotal/valprodtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"Custo operacional............................................",format(opextotal/1000000000,digits=2,nsmall=0,decimal.mark=",",big.mark="."),"  (",format(100*(opextotal/valprodtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"Despesas administrativas......................................",format(desp_admtotal/1000000000,digits=2,nsmall=0,decimal.mark=",",big.mark="."),"  (",format(100*(desp_admtotal/valprodtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"Gastos obrigat?rios em pesquisa e desenvolvimento.............",format(pedtotal/1000000000,digits=2,nsmall=0,decimal.mark=",",big.mark="."),"  (",format(100*(pedtotal/valprodtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"Despesas financeiras..........................................",format(desp_fintotal/1000000000,digits=2,nsmall=0,decimal.mark=",",big.mark="."),"  (",format(100*(desp_fintotal/valprodtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"    ARRECADA??O DE TRIBUTOS (em bilh?es de US$ e % da receita do governo):","\n",
"Imposto de renda..............................................................",format(imposto_rendatotal/1000000000,digits=2,nsmall=0,decimal.mark=","),"  (",format(100*(imposto_rendatotal/rec_govtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"CSLL..........................................................................",format(contribuicao_socialtotal/1000000000,digits=2,nsmall=0,decimal.mark=","),"  (",format(100*(contribuicao_socialtotal/rec_govtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"    Total dos tributos diretos................................................",format(trib_dirtotal/1000000000,digits=2,nsmall=0,decimal.mark=","),"  (",format(100*(trib_dirtotal/rec_govtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"Participa??o especial.........................................................",format(part_esptotal/1000000000,digits=2,nsmall=0,decimal.mark=","),"  (",format(100*(part_esptotal/rec_govtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"Royalties.....................................................................",format(roytotal/1000000000,digits=2,nsmall=0,decimal.mark=","),"  (",format(100*(roytotal/rec_govtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"    Total das participa??es governamentais....................................",format((part_esptotal+roytotal)/1000000000,digits=2,nsmall=0,decimal.mark=","),"  (",format(100*((part_esptotal+roytotal)/rec_govtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"Tributos indiretos incidentes sobre os custos operacionais....................",format(val_tiprodtotal/1000000000,digits=2,nsmall=0,decimal.mark=","),"  (",format(100*(val_tiprodtotal/rec_govtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"Tributos indiretos incidentes sobre os custos de desenvolvimento da produ??o...",format(val_tidstotal/1000000000,digits=2,nsmall=0,decimal.mark=","),"  (",format(100*(val_tidstotal/rec_govtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"    Total dos tributos indiretos..............................................",format(val_tindtotal/1000000000,digits=2,nsmall=0,decimal.mark=","),"  (",format(100*(val_tindtotal/rec_govtotal),digits=2,nsmall=1,decimal.mark=","),"%)","\n",
"    INDICADORES FINANCEIROS:","\n",
"Necessidade de financiamento (bilh?es de US$).....",format(fin/1000000000,digits=2,nsmall=0,decimal.mark=","),"\n",
"Ano do payback..................................",payback,"\n",
"Valor presente l?quido (bilh?es de US$).........",format(VPL/1000000000,digits=2,nsmall=0,decimal.mark=","),"\n",
"TIR..............................................",tir,"%","\n",
"---------------------------------------- PAR?METROS ------------------------------------","\n",
"PRE?OS E CUSTOS","\n",
"Pre?o do petr?leo (US$ por barril)...........................................................",ppet,"\n",
"Custo operacional anual de uma unidade de produ??o,sem tributos indiretos (milh?es de US$)...",format(custo_op_anual/1000000,digits=2,nsmall=0,decimal.mark=","),"\n",
"AL?QUOTAS DE TRIBUTOS E GASTOS OBRIGAT?RIOS: ","\n",
"Al?quota de royalties..........................................................",aliq_royalties*100,"%","\n",
"Al?quota de imposto de renda...................................................",aliq_ir*100,"%","\n",
"Al?quota da contribui??o social sobre o lucro l?quido...........................",aliq_csll*100,"%","\n",
"Al?quota dos gastos obrigat?rios em pesquisa e desenvolvimento..................",aliq_ped*100,"%","\n",
"Tributos indiretos incidentes sobre os custos operacionais.....................",trib_ind_op*100,"%","\n",
"Tributos indiretos incidentes sobre os custos de desenvolvimento da produ??o....",trib_ind_ds*100,"%","\n",
"PAR?METROS MACROECON?MICOS: ","\n",
"PIB de 2008 (trilh?es de R$)...",format(pibr/1000000000000,digits=2,nsmall=0),"\n",
"Taxa de c?mbio real/d?lar........",taxa_cambio,"\n",
"Taxa de crescimento do PIB.....",taxa_cs_pib*100,"%","\n",
"PAR?METROS FINANCEIROS","\n",
"Taxa de juros incidente sobre o capital de terceiros....",taxa_juros_terc*100,"%","\n",
"Taxa de desconto.......................................",taxa_desc*100,"%","\n",
"Taxa de deprecia??o dos equipamentos....................",taxa_deprec*100,"%","\n",
"Prazo do financiamento..................................",prazo," anos","\n",
"Car?ncia do financiamento...............................",carencia," anos","\n",
"OUTROS PAR?METROS: ","\n",
"Ano em que o projeto ? iniciado......................................",ano_inicial,"\n",
"Porcentagem de tang?veis no investimento..............................",tangiveis*100,"%","\n",
"Rela??o entre d?vida e capital........................................",debeq*100,"%","\n",
"Porcentagem do investimento destinado a compras no mercado dom?stico..",gastos_locais*100,"%","\n",
"Vida ?til da plataforma................................................",vida_util_platf," anos","\n",
"Tempo de contrata??o e constru??o de uma unidade de produ??o............",tempo_construcao," anos","\n",
file="relatorio.txt",sep="")

########################################################################################################
############################################ OUTROS GR?FICOS ###########################################
########################################################################################################

# PRODU??O DE UMA PLATAFORMA DURANTE SUA VIDA ?TIL
plot(
	unlist(platf_padrao)/1000,
	type="o",lty=1,bty="l",pch=3,col='black',
	lab=c(8,6,7),xaxs="i",yaxs="r",tck=-0.01,xlim=c(0,25),
	xlab="Anos",ylab="Mil barris por dia",main="",cex.main=1
	)
grid(10,NULL,lwd=1,col='gray41',lty="dotted")
savePlot("produ??o de uma plataforma durante sua vida ?til",type=c("jpeg"))

# PRODU??O DE UMA PLATAFORMA DURANTE SUA VIDA ECON?MICA
plot(
	prod_plat/1000,
	type="o",lty=1,bty="l",pch=3,col='turquoise3',
	lab=c(6,6,7),xaxs="i",yaxs="r",tck=-0.01,
	xlab="Anos",ylab="Mil barris di?rios",main="",cex.main=1
	)
grid(10,NULL,lwd=1,col='gray41',lty="dotted")
savePlot("produ??o de uma plataforma durante sua vida econ?mica",type=c("jpeg"))

# PROJE??O DO PIB 
plot(anos,pib/1000000000000,
	type="l",xlab="",bty="l",col='turquoise3',
	ylab="trilh?es de US$",main="Evolu??o projetada do PIB",cex.main=1
	)
savePlot("proje??o do PIB",type=c("jpeg"))
