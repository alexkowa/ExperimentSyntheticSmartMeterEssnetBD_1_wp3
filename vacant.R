library(robustbase)
library(data.table)
#library(forecast)
load("hhWithCustomerKey.RData") 

sampKey <- hy[!duplicated(CUSTOMER_KEY)][1:10,CUSTOMER_KEY]
rm(hy)

load("/mnt/meth/BIGDATA/Smartmeter/smr.RData")
smr[,YEAR:=substr(READING_DATETIME,1,4)]
smr <- smr[YEAR==2013,.(CUSTOMER_KEY,READING_DATETIME,GENERAL_SUPPLY_KWH)]
smr[,DAY:=substr(READING_DATETIME,6,10)]

#######################################################################
# EXAMPLES
smr1 <- smr[CUSTOMER_KEY%in%sampKey]
smr1[,DAY:=as.Date(DAY)]
smr1[CUSTOMER_KEY==11162831,plot(GENERAL_SUPPLY_KWH~as.Date(DAY),type="l")]
smr1[CUSTOMER_KEY==11162831&as.Date(DAY)=="2013-01-01",summary(GENERAL_SUPPLY_KWH)]
smr1[CUSTOMER_KEY==11162831,summary(DAY)]
x <- cbind(
  rep(seq(-pi,0,l=3),16),
  #rep(seq(-pi,0,l=4),12),
  #rep(seq(-pi,0,l=6),8),
  rep(seq(-pi,0,l=8),6),
  #rep(seq(-pi,0,l=12),4),
  rep(seq(-pi,0,l=16),3)
)
reg <- abs(sin(x))
vacant <- function(kwh,thr,ret="pred"){
  regx <- reg[1:length(kwh),]
  x <- try(m <- lm(kwh~regx-1),silent = TRUE)
  if(class(x)=="try-error")
    return(as.numeric(NA))
  if(ret=="pred")
    return(predict(m))
  else
    return(summary(m)$adj.r.squared)
}
vacant1 <- function(kwh,ret="x"){
  x <- try(aa <- auto.arima(ts(kwh,start = 1,frequency=2)))
  
  if(class(x)=="try-error")
    return(as.numeric(NA))
  else{
    if(ret=="pred")
      return(ts(kwh,start = 1,frequency=2)-aa$residuals)
    else
      return(cor(ts(kwh,start = 1,frequency=2)-aa$residuals,ts(kwh,start = 1,frequency=2)))
  }
}

vacant1 <- function(kwh,ret="x"){
  x <- try(aa <- auto.arima(ts(kwh,start = 1,frequency=2)))
  
  if(class(x)=="try-error")
    return(as.numeric(NA))
  else{
    if(ret=="pred")
      return(ts(kwh,start = 1,frequency=2)-aa$residuals)
    else
      return(cor(ts(kwh,start = 1,frequency=2)-aa$residuals,ts(kwh,start = 1,frequency=2)))
  }
}


smr1[,nobs:=.N,by=.(CUSTOMER_KEY,DAY)]
smr1 <- smr1[nobs==48]
smr1[,out:=vacant(GENERAL_SUPPLY_KWH,.5),by=.(CUSTOMER_KEY,DAY)]
smr1[,outvol:=sd(diff(log(GENERAL_SUPPLY_KWH))),by=.(CUSTOMER_KEY,DAY)]
smr1[,outts:=vacant1(GENERAL_SUPPLY_KWH,ret="pred"),by=.(CUSTOMER_KEY,DAY)]
v1 <- smr1[,.(out=vacant(GENERAL_SUPPLY_KWH,.5,ret="out")),by=.(CUSTOMER_KEY,DAY)]
v2 <- smr1[,.(outts=vacant1(GENERAL_SUPPLY_KWH)),by=.(CUSTOMER_KEY,DAY)]
v3 <- smr1[,.(outvol=sd(diff(scale(GENERAL_SUPPLY_KWH)))),by=.(CUSTOMER_KEY,DAY)]
v4 <- smr1[,.(outvar=var(GENERAL_SUPPLY_KWH)),by=.(CUSTOMER_KEY,DAY)]
smr1[,daytime:=!HOUR%in%c(0:7,20:23)]
v5 <- smr1[,.(outm=mean(GENERAL_SUPPLY_KWH[daytime])/mean(GENERAL_SUPPLY_KWH[!daytime])),by=.(CUSTOMER_KEY,DAY)]
par(ask=TRUE)
plotf <- function(x,y,z,m){
  plot(x~z,type="l",main=paste0("Correlation:", m),xlab="hour",ylab="kwh")
  points(y~z,type="l",col="red")
  zz<<-z
}
plot((mean(x)*reg[,1])~zz,type="l",col="blue")
plot((mean(x)*reg[,2])~zz,type="l",col="blue")
plot((mean(x)*reg[,3])~zz,type="l",col="blue")
plot((mean(x)*reg[,4])~zz,type="l",col="blue")
plot((mean(x)*reg[,5])~zz,type="l",col="blue")
plot((mean(x)*reg[,6])~zz,type="l",col="blue")
ind <- which(abs(v2$outts)>.9)[1:10]
ind <- sample(1:nrow(v2),10)
for(i in 1:10){
  smr1[CUSTOMER_KEY==v2[ind[i],CUSTOMER_KEY]&DAY==v2[ind[i],DAY],plotf(GENERAL_SUPPLY_KWH,outts,HOUR,round(v2[ind[i],outts],2))]  
}

summary(v1$out)
setkey(v1,out)
ind <- head(1:nrow(v1),10)
for(i in 1:10){
  smr1[CUSTOMER_KEY==v1[ind[i],CUSTOMER_KEY]&DAY==v1[ind[i],DAY],plotf(GENERAL_SUPPLY_KWH,out,HOUR,round(v1[ind[i],out],2))]  
}

summary(v3$outvol)
v3 <- v3[!is.na(outvol)]
setkey(v3,outvol)
ind <- tail(1:nrow(v3),20)
for(i in 1:10){
  smr1[CUSTOMER_KEY==v3[ind[i],CUSTOMER_KEY]&DAY==v3[ind[i],DAY],plot(GENERAL_SUPPLY_KWH~HOUR,main=round(v3[ind[4],outvol],2),type="l")]  
}
quantile(v4$outvar,c(.05,.95),na.rm=TRUE)
v4 <- v4[!is.na(outvar)]
setkey(v4,outvar)
ind <- head(1:nrow(v4),20)
for(i in 1:10){
  smr1[CUSTOMER_KEY==v4[ind[i],CUSTOMER_KEY]&DAY==v4[ind[i],DAY],plot(GENERAL_SUPPLY_KWH~HOUR,main=round(v4[ind[4],outvar],2),type="l")]  
}
quantile(v5$outm,c(.05,.95),na.rm=TRUE)
v5[,outma:=abs(outm-1)]
v5 <- v5[!is.na(outma)]
setkey(v5,outma)
ind <- tail(1:nrow(v5),20)
for(i in 1:10){
  smr1[CUSTOMER_KEY==v5[ind[i],CUSTOMER_KEY]&DAY==v5[ind[i],DAY],plot(GENERAL_SUPPLY_KWH~HOUR,main=round(v5[ind[4],outma],2),type="l")]  
}
plot(reg[,1]~zz,type="l",xlab="hour",ylab="Regressor",main="High frequency")
plot(reg[,2]~zz,type="l",xlab="hour",ylab="Regressor",main="Medium frequency")
plot(reg[,3]~zz,type="l",xlab="hour",ylab="Regressor",main="Low frequency")

#######################################################################
#######################################################################
# Use variance to determine if household is vacant

smr[,VAR.DAY:=var(GENERAL_SUPPLY_KWH),by=list(CUSTOMER_KEY,DAY)]

setkeyv(smr,c("CUSTOMER_KEY"))

# look at distribution of variances per either customer or day

smr.vac <- unique(subset(smr,select=c("CUSTOMER_KEY","DAY","VAR.DAY")))

customers <- unique(smr.vac$CUSTOMER_KEY)
h <- 10
for(i in 1:length(customers)){
  
  smr.plot <- smr.vac[CUSTOMER_KEY%in%customers[((i-1)*h+1):(i*h)]]
  
  p1 <- ggplot(smr.plot,aes(VAR.DAY))+
    geom_histogram(binwidth=0.01)+
    facet_grid(.~CUSTOMER_KEY,scales="free")
  plot(p1)
  readline(prompt="Press [enter] to continue")
}


# set household and day to be vacant if var exceeds threshold
# set threshold such that distribution over time spans of vaccant households seems meaningfull ~ majority week(s)

th <- 0.05

smr.vac[,VACCANT:=as.numeric(VAR.DAY<th)]

smr.vac.N <- smr.vac[,.N,by=list(CUSTOMER_KEY,VACCANT)]

p1 <- ggplot(smr.vac.N,aes(N))+
  geom_histogram()
plot(p1)

x <- smr.vac[CUSTOMER_KEY%in%unique(CUSTOMER_KEY)[1]]$VACCANT

consec.vaccant <- function(x){
  
  n <- length(x)
  x.diff <- diff(x)
  
  x.start <- which(x.diff==1)+1
  x.end <- which(x.diff==-1)+1
  
  if(x.start[1]>x.end[1]) x.start <- c(1,x.start)
  if(tail(x.end,1)<tail(x.start,1)) x.end <- c(x.end,n+1)
  
  out <- x.end-x.start
}

smr.vac <- smr.vac[,consec.vaccant(VACCANT),by="CUSTOMER_KEY"]


##############################################
# outlier detection on daily KWH

smr[,KWH:=sum(GENERAL_SUPPLY_KWH),by=list(CUSTOMER_KEY,DAY)]

setkeyv(smr,c("CUSTOMER_KEY","DAY"))

smr.sum <- unique(subset(smr,select=c("DAY","CUSTOMER_KEY","KWH")))

boxplot()








