library(robustbase);library(data.table);library(forecast)
load("hhWithCustomerKey.RData") 
sampKey <- hy[!duplicated(CUSTOMER_KEY)][1:10,CUSTOMER_KEY]
smr <- fread("/hdfs/datasets/smartmeters/metering_data/metering_data.csv")
smr <- smr[CUSTOMER_ID%in%cust[,CUSTOMER_KEY],]
setnames(smr,"CUSTOMER_ID","CUSTOMER_KEY")
setkey(smr,CUSTOMER_KEY)
#Remove unnecessary vars
smr[,c("EVENT_KEY","CALENDAR_KEY","CONTROLLED_LOAD_KWH","GROSS_GENERATION_KWH",
       "NET_GENERATION_KWH","OTHER_KWH"):=NULL]
#only keep 1 year (2013)
smr <- smr[year(READING_DATETIME)==2013]
smr1 <- smr[CUSTOMER_KEY%in%sampKey]
smr1[,date:=as.Date(READING_DATETIME)]
smr1[CUSTOMER_KEY==10640431,plot(GENERAL_SUPPLY_KWH~as.Date(READING_DATETIME),type="l")]
smr1[CUSTOMER_KEY==10640431&as.Date(READING_DATETIME)=="2013-01-01",summary(GENERAL_SUPPLY_KWH)]
smr1[CUSTOMER_KEY==10640431,summary(READING_DATETIME)]
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
smr1[,nobs:=.N,by=.(CUSTOMER_KEY,day)]
smr1 <- smr1[nobs==48]
smr1[,out:=vacant(GENERAL_SUPPLY_KWH,.5),by=.(CUSTOMER_KEY,day)]
smr1[,outvol:=sd(diff(log(GENERAL_SUPPLY_KWH))),by=.(CUSTOMER_KEY,day)]
smr1[,outts:=vacant1(GENERAL_SUPPLY_KWH,ret="pred"),by=.(CUSTOMER_KEY,day)]
v1 <- smr1[,.(out=vacant(GENERAL_SUPPLY_KWH,.5,ret="out")),by=.(CUSTOMER_KEY,day)]
v2 <- smr1[,.(outts=vacant1(GENERAL_SUPPLY_KWH)),by=.(CUSTOMER_KEY,day)]
v3 <- smr1[,.(outvol=sd(diff(scale(GENERAL_SUPPLY_KWH)))),by=.(CUSTOMER_KEY,day)]
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
  smr1[CUSTOMER_KEY==v2[ind[i],CUSTOMER_KEY]&day==v2[ind[i],day],plotf(GENERAL_SUPPLY_KWH,outts,date,round(v2[ind[i],outts],2))]  
}

summary(v1$out)
setkey(v1,out)
ind <- head(1:nrow(v1),10)
for(i in 1:10){
  smr1[CUSTOMER_KEY==v1[ind[i],CUSTOMER_KEY]&day==v1[ind[i],day],plotf(GENERAL_SUPPLY_KWH,out,date,round(v1[ind[i],out],2))]  
}

summary(v3$outvol)
v3 <- v3[!is.na(outvol)]
setkey(v3,outvol)
ind <- tail(1:nrow(v3),20)
for(i in 1:10){
  smr1[CUSTOMER_KEY==v3[ind[i],CUSTOMER_KEY]&day==v3[ind[i],day],plot(GENERAL_SUPPLY_KWH~date,main=round(v3[ind[4],outvol],2),type="l")]  
}

plot(reg[,1]~zz,type="l",xlab="hour",ylab="Regressor",main="High frequency")
plot(reg[,2]~zz,type="l",xlab="hour",ylab="Regressor",main="Medium frequency")
plot(reg[,3]~zz,type="l",xlab="hour",ylab="Regressor",main="Low frequency")
