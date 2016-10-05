# Project: Test
# 
# Author: kowa$
###############################################################################


library(simPop);library(VIM)
# Eusilc Austria sample data
data(eusilcS)
# adjust the weights to get the "real" population
eusilcS$db090 <- eusilcS$db090*100 
inp <- specifyInput(data=eusilcS, hhid="db030", hhsize="hsize", strata="db040", weight="db090")
simPop <- simStructure(data=inp, method="direct", basicHHvars=c("age", "rb090"))
simPop <- simCategorical(simPop, additional=c("pl030", "pb220a"), method="multinom", nr_cpus=1)
simPop <- simContinuous(simPop, additional="netIncome",regModel = ~rb090+hsize+pl030+pb220a, upper=200000, equidist=FALSE, nr_cpus=1)
p <- copy(pop(simPop))
# Define somehow an equalized income
p[,eqIncome:=sum(netIncome)/max(c(1,sum(as.numeric(age%in%as.character(16:65))))),by="db030"]
#Create matching variables
p[,Age:=as.character(as.numeric(age))]
p[,NUM_CHILDREN_0_10:=sum(as.numeric(Age%in%c(0:10))),by=db030]
p[,NUM_CHILDREN_11_17:=sum(as.numeric(Age%in%c(11:17))),by=db030]
p[,NUM_OCCUPANTS:=hsize]
p[,NUM_OCCUPANTS_70PLUS:=sum(as.numeric(Age%in%c(70:99))),by=db030]
p[,IS_HOME_DURING_DAYTIME:="N"]
p[pl030%in%c(3,5,6,7),IS_HOME_DURING_DAYTIME:="Y"]
#Approx. distribution of the Income groups in Australian data replicated
p[,quantile(eqIncome,c(.25,.5))]
p[,HHOLD_INCOME_GROUP_CD:="HI"]
p[eqIncome<20599,HHOLD_INCOME_GROUP_CD:="MED"]
p[eqIncome<13641,HHOLD_INCOME_GROUP_CD:="LOW"]
anyY <- function(x){
  if(any(x=="Y"))
    return("Y")
  else
    return("N")
}
p[,IS_HOME_DURING_DAYTIME:=anyY(IS_HOME_DURING_DAYTIME),by=db030]

library(data.table);
###Smart Meter data Australia
#Customer data
cust <- fread("/hdfs/datasets/smartmeters/customers/customers.csv")
matchVar <- intersect(colnames(cust),colnames(p))
#Data set of "Austrian" households
h <- p[!duplicated(db030),c(matchVar,"db030"),with=FALSE]
cust <- na.omit(cust[,c(matchVar,"CUSTOMER_KEY"),with=FALSE])
cust[HHOLD_INCOME_GROUP_CD=="DeclinedToAnswer",HHOLD_INCOME_GROUP_CD:=NA]
##Create factor variables
cust[,HHOLD_INCOME_GROUP_CD:=factor(HHOLD_INCOME_GROUP_CD)]
h[,HHOLD_INCOME_GROUP_CD:=factor(HHOLD_INCOME_GROUP_CD)]
cust[,IS_HOME_DURING_DAYTIME:=factor(IS_HOME_DURING_DAYTIME)]
h[,IS_HOME_DURING_DAYTIME:=factor(IS_HOME_DURING_DAYTIME)]
#Impute missing income groups
cust[,HHOLD_INCOME_GROUP_CD:=kNN(cust,imp_var = FALSE)$HHOLD_INCOME_GROUP_CD]
h[,:=NAHHOLD_INCOME_GROUP_CD]
smr <- fread("/hdfs/datasets/smartmeters/metering_data/metering_data.csv")
