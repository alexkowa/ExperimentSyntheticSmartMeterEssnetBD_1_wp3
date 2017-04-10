library(robustbase)
library(data.table)
library(cellWise)
library(tscount)
library(VIM)
library(fasttime)
library(ggplot2)
library(MASS)
library(geoR)
library(forecast)
load("hhWithCustomerKey.RData") 

sampKey <- hy[!duplicated(CUSTOMER_KEY)][1:100,CUSTOMER_KEY]
rm(hy)

load("/mnt/meth/BIGDATA/Smartmeter/smr.RData")

smr <- smr[CUSTOMER_KEY%in%sampKey]

smr[,YEAR:=substr(READING_DATETIME,1,4)]
smr <- smr[YEAR==2013,.(CUSTOMER_KEY,READING_DATETIME,GENERAL_SUPPLY_KWH)]
smr[,DAY:=substr(READING_DATETIME,6,10)]
smr[,HOUR:=substr(READING_DATETIME,12,13)]
smr[,READING_DATETIME:=NULL]
save(smr,file="/mnt/meth/BIGDATA/SmartMeter/SmartMeter_transformed_2013_02.RData",compress=TRUE)

load("M:/BIGDATA/SmartMeter/SmartMeter_transformed_2013.RData")
load("M:/BIGDATA/SmartMeter/SmartMeter_transformed_2013_02.RData")
smr[,GENERAL_SUPPLY_KWH:=KWH_sum]
smr[,KWH_sum:=NULL]
#######################################################################
# EXAMPLES
smr1 <- smr[CUSTOMER_KEY%in%sampKey]
smr1[,DAY:=as.Date(DAY)]
smr1[CUSTOMER_KEY==11162831,plot(GENERAL_SUPPLY_KWH~as.Date(DAY),type="l")]
smr1[CUSTOMER_KEY==11162831&as.Date(DAY)=="2013-01-01",summary(GENERAL_SUPPLY_KWH)]
smr1[CUSTOMER_KEY==11162831,summary(DAY)]
x <- cbind(
  rep(seq(-pi,0,l=3),8),
  #rep(seq(-pi,0,l=4),12),
  #rep(seq(-pi,0,l=6),8),
  rep(seq(-pi,0,l=6),4),
  #rep(seq(-pi,0,l=12),4),
  rep(seq(-pi,0,l=12),2)
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
#smr1[,out:=vacant(GENERAL_SUPPLY_KWH,.5),by=.(CUSTOMER_KEY,DAY)]
smr1[,outvol:=sd(diff(log(GENERAL_SUPPLY_KWH))),by=.(CUSTOMER_KEY,DAY)]
smr1[,outts:=vacant1(GENERAL_SUPPLY_KWH,ret="pred"),by=.(CUSTOMER_KEY,DAY)]
v1 <- smr1[,.(out=vacant(GENERAL_SUPPLY_KWH,.5,ret="out")),by=.(CUSTOMER_KEY,DAY)]
v2 <- smr1[,.(outts=vacant1(GENERAL_SUPPLY_KWH)),by=.(CUSTOMER_KEY,DAY)]
v3 <- smr1[,.(outvol=sd(diff(scale(GENERAL_SUPPLY_KWH)))),by=.(CUSTOMER_KEY,DAY)]
v4 <- smr1[,.(outvar=var(GENERAL_SUPPLY_KWH)),by=.(CUSTOMER_KEY,DAY)]
smr1[,daytime:=!as.numeric(HOUR)%in%c(0:7,20:23)]
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

smr1[,HOUR:=paste(HOUR,c(":00",":30"),sep="")]
smr1[,v1:=abs(vacant1(GENERAL_SUPPLY_KWH),by=.(CUSTOMER_KEY,DAY))]
smr1[,v2:=sd(diff(scale(GENERAL_SUPPLY_KWH))),by=.(CUSTOMER_KEY,DAY)]
smr1[,v3:=var(GENERAL_SUPPLY_KWH),by=.(CUSTOMER_KEY,DAY)]
smr1[,daytime:=!as.numeric(substr(HOUR,1,2))%in%c(0:7,20:23)]
smr1[,v4:=mean(GENERAL_SUPPLY_KWH[daytime])/mean(GENERAL_SUPPLY_KWH[!daytime]),by=.(CUSTOMER_KEY,DAY)]


plot.methods <- function(smr1,vx,n=5,d=4){
  
  q <- smr1[,quantile(unique(get(vx)),c(0.1,0.9),na.rm=TRUE)]
  
  select.top <- unique(smr1[get(vx)>q[2],c("CUSTOMER_KEY","DAY",vx),with=FALSE],by=vx)
  top <- sample(1:nrow(select.top),n)
  
  select.bottom <- unique(smr1[get(vx)<q[1],c("CUSTOMER_KEY","DAY",vx),with=FALSE],by=vx)
  bottom <- sample(1:nrow(select.bottom),n)
  
  setkeyv(smr1,c("CUSTOMER_KEY","DAY"))
  
  dat.plot <- smr1[rbind(select.top[top],select.bottom[bottom])]

  dat.plot[,c(vx):=round(get(vx),digits=d)]
  
  hour_breaks <- smr1[,unique(HOUR)]
  hour_labels[seq(2,48,by=2)] <- ""
  
  setnames(dat.plot,vx,"x")
  p1 <- ggplot(dat.plot,aes(HOUR,GENERAL_SUPPLY_KWH))+
    ylab("KWH USAGE")+
    geom_line(aes(group=x,colour=factor(CUSTOMER_KEY)))+
    scale_x_discrete(breaks=hour_breaks,labels=hour_labels)+
    theme(legend.position="none")+
    facet_grid(x~.,scales="free")
  
  plot(p1)
}

set.seed(123)
#ts
plot.methods(smr1,vx=c("v1"),d=4)
#variability
plot.methods(smr1,vx=c("v2"),d=4)
#variance
plot.methods(smr1,vx=c("v3"),d=6)
# day-night time
plot.methods(smr1,vx=c("v4"),d=4)



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
# do mv-outlier detection based on sciodemogrphic criteria and time
# for example per month and householdsize (need to check if enough obs for that present)
# cellwise outlier detection needs DetectDeviatingCells from package cellWise (for example) 


smr[,KWH:=sum(GENERAL_SUPPLY_KWH),by=list(CUSTOMER_KEY,DAY)]

setkeyv(smr,c("CUSTOMER_KEY","DAY"))

smr.sum <- unique(subset(smr,select=c("DAY","CUSTOMER_KEY","KWH")))

load("M:/BIGDATA/SmartMeter/smr2013.RData")

smr.sum <- smr;rm(smr)

smr.sum[,MON:=substr(DAY,1,2)]
smr.sum[,WEEK:=week(paste("2016",DAY,sep="-"))]
smr.sum[,WDAY:=wday(paste("2016",DAY,sep="-"))]

# calculate cellwise outliers

vaccant_out <- function(dat,impute=FALSE){
  
  id <- dat[,1,with=FALSE]
  
  dat <- as.matrix(dat[,-1,with=FALSE])
  # discard col if only exists of NA
  nacol <- unlist(apply(dat,2,function(z){all(is.na(z))}))
  dat <- dat[,!nacol]
  
  cell_out <- DetectDeviatingCells(dat,DDCpars=list(tolProb=.95))
  
  mat_out <- matrix(0,nrow=nrow(dat),ncol=ncol(dat))
  skip.row <- as.numeric(cell_out$namesNArow)
  # get upper and lower outliers
  if(length(skip.row)>0){
    index.upper <- dat[-skip.row,][cell_out$indcell]>cell_out$Xest[cell_out$indcell]
    index.lower <- dat[-skip.row,][cell_out$indcell]<cell_out$Xest[cell_out$indcell]
    #
    mat_out[-skip.row,][cell_out$indcell[index.upper]] <- 1
    mat_out[-skip.row,][cell_out$indcell[index.lower]] <- -1
  }else{
    index.upper <- dat[cell_out$indcell]>cell_out$Xest[cell_out$indcell]
    index.lower <- dat[cell_out$indcell]<cell_out$Xest[cell_out$indcell]
    #
    mat_out[cell_out$indcell[index.upper]] <- 1
    mat_out[cell_out$indcell[index.lower]] <- -1
  }



  return(cbind(id,mat_out))
}

load("M:/BIGDATA/SmartMeter/customers.RData")
cust <- subset(cust,CUSTOMER_KEY%in%smr.sum$CUSTOMER_KEY,select=c("CUSTOMER_KEY","TRIAL_REGION_NAME","LOCATION_TYPE_CD","SERVICE_TYPE",
                             "ASSRTD_HHOLD_INCOME_GROUP_CD","ASSRTD_CLIMATE_ZONE_CD","ASSRTD_CLIMATE_ZONE_DESC",
                             "ASSRTD_GAS_USAGE_GROUP_CD","ASSRTD_ELECTRICITY_USE_GRP_CD","HHOLD_INCOME_GROUP_CD",
                             "NUM_OCCUPANTS","NUM_REFRIGERATORS","NUM_ROOMS_HEATED","HAS_INTERNET_ACCESS","HAS_GAS",
                             "HAS_AIRCON","IS_HOME_DURING_DAYTIME"))
cust[HHOLD_INCOME_GROUP_CD%in%c("DeclinedToAnswer",""),HHOLD_INCOME_GROUP_CD:=NA]
##Create factor variables
cust[,HHOLD_INCOME_GROUP_CD:=factor(HHOLD_INCOME_GROUP_CD)]
cust[,IS_HOME_DURING_DAYTIME:=factor(IS_HOME_DURING_DAYTIME)]
occ <- c(rep(1,3),rep(2,2),rep(3,6))
cust[,NUM_OCCUPANTS_F:=factor(occ[NUM_OCCUPANTS+1],labels=c("low","mid","high"))]
#Impute missing income groups
cust[,HHOLD_INCOME_GROUP_CD:=kNN(cust,imp_var = FALSE)$HHOLD_INCOME_GROUP_CD]
save(cust,file="customers_test.RData",compress=TRUE)
load("customers_test.RData")
setkey(cust,CUSTOMER_KEY)

smr.mon <- dcast(smr.sum,MON+CUSTOMER_KEY~DAY,value.var="KWH",fill=NA)
smr.mon[,OUT.GROUP:=vaccant_out(.SD[,-1,with=FALSE]),by="MON"]

smr.week <- dcast(smr.sum,CUSTOMER_KEY+WEEK~WDAY,value.var="KWH",fill=NA)
smr.week[,OUT.GROUP:=vaccant_out(.SD[,-1,with=FALSE]),by="CUSTOMER_KEY"]

smr.day <- dcast(smr.sum,CUSTOMER_KEY~DAY,value.var="KWH",fill=NA)
setkey(smr.day,CUSTOMER_KEY)
smr.day <- cust[smr.day]

k <- which(colnames(smr.day)=="01-01")
ndates <- colnames(smr.day)[k:ncol(smr.day)]

# is kwh-consumption skeewed?
h <- 15
for(i in 1:ceiling(length(ndates)/h)){
  
  smr.plot <- smr.day_tf[,c("CUSTOMER_KEY",ndates[((i-1)*h+1):min((i*h),length(ndates))]),with=FALSE]
  smr.plot <- melt(smr.plot,id.vars="CUSTOMER_KEY")
  p1 <- ggplot(smr.plot,aes(value,fill=variable))+
    geom_density()+
    facet_grid(variable~.,scales="free")
  plot(p1)
  
  plot(p1)
  
  readline(prompt="Press [enter] to continue")
}

# apply boxcox trafo to adjust for skewing

bc_trafo <- function(x,lambda2){
  bc_out <- boxcoxfit(x,lambda2=lambda2)
  
  l1 <- bc_out$lambda[1]
  l2 <- bc_out$lambda[2]
  if(l1>0){
    x_out <- ((x+l2)^l1-1)/l1
  }else{
    x_out <- log(x+l2)
  }
  return(x_out)
  
}

smr.day_tf <- copy(smr.day)
for(i in 1:length(ndates)){
  set(smr.day_tf,j=ndates[i],value=bc_trafo(smr.day_tf[[ndates[i]]], lambda2=TRUE))
}

fac <- c("NUM_OCCUPANTS_F")
fac <- c("TRIAL_REGION_NAME")
outlier_inc <- smr.day_tf[,vaccant_out(.SD[,c("CUSTOMER_KEY",ndates),with=FALSE]),by=c(fac)]

outlier_inc <- vaccant_out(smr.day_tf[,c("CUSTOMER_KEY",ndates),with=FALSE])
out.plot <- melt(outlier_inc,id.vars=c(fac,"CUSTOMER_KEY"))

out.plot[,variable:=levels(variable)[variable]]
out.plot[,INDEX:=as.numeric(substr(variable,2,nchar(variable)))]
out.plot[,DAY:=paste(2013,ndates[INDEX],sep="-")]
out.plot[,DAY:=fastPOSIXct(DAY,required.components = 3L)]


ggplot(out.plot,aes(DAY,factor(CUSTOMER_KEY)))+
  geom_tile(aes(fill=factor(value)))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_fill_manual(values=c("red","white","yellow"),breaks=levels(factor(out.plot$value)))+
  facet_grid(ASSRTD_ELECTRICITY_USE_GRP_CD~.,scales="free")


out.plot[,.N,by=list(CUSTOMER_KEY,value)][value==-1&N>200]

# ziehe zufällig IDs
# und plote zugehörige Jahres verbräuche
# um Ergebnisse im Detail zu betrachten
s <- 20
smr.day[,S:=0]
out.plot[,CAND:=sum(value==-1)>10&sum(value==-1)<100,by=CUSTOMER_KEY]
sample.hh <- unique(out.plot[,.(CAND,CUSTOMER_KEY)])
sample.hh[,S:=0]
key_sample <- c()
for(i in 1:20){
  ID_sample <- sample(sample.hh[S==0&CAND==TRUE]$CUSTOMER_KEY,s)
  key_sample <- c(key_sample,ID_sample)
  sample.hh[CUSTOMER_KEY%in%ID_sample,S:=1]
  #ID_sample <- out.plot[,.N,by=list(CUSTOMER_KEY,value)][value==-1&N>200]$CUSTOMER_KEY
  #ID_sample <- sample(out.plot[,all(value!=-1),by="CUSTOMER_KEY"][V1==TRUE]$CUSTOMER_KEY,15)
  
  smr_sample <- smr.day[CUSTOMER_KEY%in%ID_sample][,c("CUSTOMER_KEY",ndates),with=FALSE]
  smr_sample <- melt(smr_sample,id.vars="CUSTOMER_KEY")
  smr_sample[,variable:=fastPOSIXct(paste("2013",variable,sep="-"))]
  
  out.plot_sample <- out.plot[CUSTOMER_KEY%in%ID_sample][,.(CUSTOMER_KEY,DAY,value)]
  setnames(out.plot_sample,c("DAY","value"),c("variable","IND"))
  
  setkeyv(smr_sample,c("CUSTOMER_KEY","variable"))
  setkeyv(out.plot_sample,c("CUSTOMER_KEY","variable"))
  
  smr_sample <- out.plot_sample[smr_sample]
  smr_sample[,CUSTOMER_KEY:=factor(CUSTOMER_KEY,labels=as.character(1:20))]
  
  p1 <- ggplot(smr_sample,aes(variable,value))+
    geom_line()+
    facet_grid(CUSTOMER_KEY~.,scales="free")+
    geom_point(data=smr_sample[IND== -1],aes(variable,value),colour="red")+
    geom_point(data=smr_sample[IND==1],aes(variable,value),colour="yellow")
  plot(p1)
  
  readline(prompt="Press [enter] to continue")
}

init.sample <- key_sample[c(5,c(10,20)+20,c(1,6,12,17)+40,c(15,20)+60,c(3,6,16)+80,c(2,4,5,7,9,14)+100,c(18,19,8,9,11,20)+120,c(17,19,20)+140,c(13,20)+160,c(9,11,14,16,18)+180)]
save(init.sample,file="Trainings_KEY.RData")

c(12,c(1,11)+20,c(4,11,13,14,19)+40,c(11,12,13,17)+60,c(7,10,12,14,15,16)+80,c(13,12)+100,c(15,16,18)+120,c(1,9,10,11,12,19,20)+140,c(8,10,11,16,19)+160,c(20,4,3,9,14,19)+180)
init.sample <- c(init.sample,key_sample[c(12,c(1,11)+20,c(4,11,13,14,19)+40,c(11,12,13,17)+60,c(7,10,12,14,15,16)+80,c(13,12)+100,c(15,16,18)+120,c(1,9,10,11,12,19,20)+140,c(8,10,11,16,19)+160,c(20,4,3,9,14,19)+180)])


# berechne regressoren für random forest model
# wähle als trainigsdatensatz CUSTOMER_KEY aus init.sample
load("/mnt/meth/BIGDATA/Smartmeter/smr.RData")
smr <- smr[1:10]
smr[,YEAR:=substr(READING_DATETIME,1,4)]
smr <- smr[YEAR==2013,.(CUSTOMER_KEY,READING_DATETIME,GENERAL_SUPPLY_KWH)]
smr[,DAY:=substr(READING_DATETIME,6,10)]
smr[,MON:=substr(READING_DATETIME,6,7)]
smr[,QUARTER:=if(MON[1]%in%c("03","04","05")){'Q2'}else if(MON[1]%in%c("06","07","08")){'Q3'}else if(MON[1]%in%c("09","10","11")){'Q4'}else{'Q1'},by=list(MON)]

# choose variables
smr[,VAR:=var(GENERAL_SUPPLY_KWH),by=list(CUSTOMER_KEY,DAY)]
smr[,VAR.Q:=var(GENERAL_SUPPLY_KWH),by=list(CUSTOMER_KEY,QUARTER)]

smr[,RANGE:=range(GENERAL_SUPPLY_KWH),by=list(CUSTOMER_KEY,DAY)]
smr[,RANGE.Q:=range(GENERAL_SUPPLY_KWH),by=list(CUSTOMER_KEY,QUARTER)]

smr[,MEAN:=mean(GENERAL_SUPPLY_KWH),by=list(CUSTOMER_KEY,DAY)]
smr[,MEAN.Q:=mean(GENERAL_SUPPLY_KWH),by=list(CUSTOMER_KEY,QUARTER)]

smr[,MEDIAN:=median(GENERAL_SUPPLY_KWH),by=list(CUSTOMER_KEY,DAY)]
smr[,MEDIAN.Q:=median(GENERAL_SUPPLY_KWH),by=list(CUSTOMER_KEY,QUARTER)]

smr[,SD_DIFF:=sd(diff(scale(GENERAL_SUPPLY_KWH))),by=list(CUSTOMER_KEY,DAY)]
smr[,SD_DIFF.Q:=sd(diff(scale(GENERAL_SUPPLY_KWH))),by=list(CUSTOMER_KEY,QUARTER)]


smr[,daytime:=!as.numeric(substr(READING_DATETIME,12,13))%in%c(0:7,20:23)]
smr[,DT_DIFF:=mean(GENERAL_SUPPLY_KWH[daytime])/mean(GENERAL_SUPPLY_KWH[!daytime]),by=list(CUSTOMER_KEY,DAY)]
smr[,DT_DIFF.Q:=mean(GENERAL_SUPPLY_KWH[daytime])/mean(GENERAL_SUPPLY_KWH[!daytime]),by=list(CUSTOMER_KEY,QUARTER)]

smr[,KWH:=sum(GENERAL_SUPPLY_KWH),by=list(CUSTOMER)]

smr.train <- unique(subset(smr,select=c("CUSTOMER_KEY","DAY","KWH","VAR","VAR.Q","RANGE","RANGE.Q",
                                                                    "MEAN","MEAN.Q","MEDIAN","MEDIAN.Q","SD_DIFF","SD_DIFF.Q","DT_DIFF","DT_DIFF.Q")))

save(out.plot,file="M:/BIGDATA/Smartmeter/cellwise_out.RData")

load("M:/BIGDATA/Smartmeter/smr_train.RData")
load("M:/BIGDATA/Smartmeter/cellwise_out.RData")
load("Trainings_KEY.RData")
library(ranger) # fast random Forest
library(e1071) # svm support vector machine
library(gbm)
library(data.table)
library(cellWise)
library(VIM)

load("M:/BIGDATA/SmartMeter/customers.RData")
cust <- subset(cust,CUSTOMER_KEY%in%smr.train$CUSTOMER_KEY,select=c("CUSTOMER_KEY","TRIAL_REGION_NAME","LOCATION_TYPE_CD","SERVICE_TYPE",
                                                                  "ASSRTD_HHOLD_INCOME_GROUP_CD","ASSRTD_CLIMATE_ZONE_CD","ASSRTD_CLIMATE_ZONE_DESC",
                                                                  "ASSRTD_GAS_USAGE_GROUP_CD","ASSRTD_ELECTRICITY_USE_GRP_CD","HHOLD_INCOME_GROUP_CD",
                                                                  "NUM_OCCUPANTS","NUM_REFRIGERATORS","NUM_ROOMS_HEATED","HAS_INTERNET_ACCESS","HAS_GAS",
                                                                  "HAS_AIRCON","IS_HOME_DURING_DAYTIME"))
cust[HHOLD_INCOME_GROUP_CD%in%c("DeclinedToAnswer",""),HHOLD_INCOME_GROUP_CD:=NA]
##Create factor variables
cust[,HHOLD_INCOME_GROUP_CD:=factor(HHOLD_INCOME_GROUP_CD)]
cust[,IS_HOME_DURING_DAYTIME:=factor(IS_HOME_DURING_DAYTIME)]
occ <- c(rep(1,3),rep(2,2),rep(3,6))
cust[,NUM_OCCUPANTS_F:=factor(occ[NUM_OCCUPANTS+1],labels=c("low","mid","high"))]
#Impute missing income groups
cust[,HHOLD_INCOME_GROUP_CD:=kNN(cust,imp_var = FALSE)$HHOLD_INCOME_GROUP_CD]
setkey(cust,CUSTOMER_KEY)
setkey(smr.train,CUSTOMER_KEY)

smr.train <- cust[smr.train]

dat.train <- smr.train[CUSTOMER_KEY%in%init.sample]

out.plot[,DAY:=substr(DAY,6,10)]

dat.train <- merge(dat.train,out.plot,by=c("CUSTOMER_KEY","DAY","NUM_OCCUPANTS_F"))

dat.train[,VACCANT:=if(value==-1){'YES'}else{'NO'},by=1:nrow(dat.train)]
dat.train[,VACCANT:=factor(VACCANT)]

dat.train[,c("variable","INDEX","CAND","value"):=NULL]

# calc models


rf.vaccant <- ranger(VACCANT~.,
                           data=as.data.frame(dat.train[(!is.na(VAR))&(!is.na(SD_DIFF))&(!is.na(DT_DIFF)),.(VACCANT,NUM_OCCUPANTS_F,KWH,VAR,VAR.Q,RANGE,RANGE.Q,MEAN,MEAN.Q,MEDIAN,MEDIAN.Q,SD_DIFF,SD_DIFF.Q,DT_DIFF,DT_DIFF.Q)]))


svm.vaccant <- svm(VACCANT~.,
                      data=as.matrix(dat.train[(!is.na(VAR))&(!is.na(SD_DIFF))&(!is.na(DT_DIFF)),.(VACCANT,NUM_OCCUPANTS_F,KWH,VAR,VAR.Q,RANGE,RANGE.Q,MEAN,MEAN.Q,MEDIAN,MEDIAN.Q,SD_DIFF,SD_DIFF.Q,DT_DIFF,DT_DIFF.Q)]))

boost.vaccant <- gmb(VACCANT~.,
                     data=as.matrix(dat.train[(!is.na(VAR))&(!is.na(SD_DIFF))&(!is.na(DT_DIFF)),.(VACCANT,NUM_OCCUPANTS_F,KWH,VAR,VAR.Q,RANGE,RANGE.Q,MEAN,MEAN.Q,MEDIAN,MEDIAN.Q,SD_DIFF,SD_DIFF.Q,DT_DIFF,DT_DIFF.Q)]),
                     interaction.depth=2,n.tress=1000)




