# Project: Test
# 
# Author: kowa$
###############################################################################
# visualize synthetic data
# merge households to regions and display via shape file
#
library(data.table)
library(ggplot2)
library(rgdal)


# read synthetic population for testing porpuses
load("/mnt/meth/BIGDATA/SmartMeter/hhWithCustomerKey.RData")
load("/mnt/meth/Gussenbauer/ESSNet BigData/data/synthetic_population.RData")
setnames(p,"db040","state")
p <- unique(subset(p,select=c("db030","state")))
setkey(p,"db030")
setkey(hy,"db030")
hy <- p[hy]
hy[,State.Size:=.N,by="state"]
rm(p);gc()


# read residence sizes per region
pop.size <- data.table(read.csv("/mnt/meth/Gussenbauer/ESSNet BigData/data/Einwohnerzahl_01012016.csv", sep=";"))
pop.size[,Size:=as.numeric(gsub("\\.","",Size))]
pop.size[,state.n:=substr(Number,1,1)]
pop.size[,state:=if(state.n==1){"Burgenland"}else if(state.n==2){"Carinthia"}else if(state.n==3){"Lower Austria"}else if(state.n==4){"Upper Austria"
}else if(state.n==5){"Salzburg"}else if(state.n==6){"Styria"}else if(state.n==7){"Tyrol"}else if(state.n==8){"Vorarlberg"}else{"Vienna"},by=1:nrow(pop.size)]
pop.size[,Status:=NULL]
pop.size[,prob:=Size/sum(Size),by="state"]

# assign households randomly to communities (per State)
hy.sub <- unique(hy[,.(state,State.Size)])
setkey(hy.sub,state)
setkey(pop.size,state)
pop.size <- hy.sub[pop.size]
pop.size[,Number:=as.character(Number)]

pop.size <- pop.size[,sample(Number,size=unique(State.Size),prob=prob,replace=TRUE),by="state"]
setnames(pop.size,"V1","id")

setkey(pop.size,state)
setkey(hy,state)

hy <- cbind(hy,pop.size[,.(id)])
rm(pop.size,hy.sub)

#####################################################################################################
# read SmartMeter data
load("/mnt/meth/BIGDATA/Smartmeter/smr.RData")
# half hourly energy consumption per household over a time period of ~2 years 
smr[,DAY:=substr(READING_DATETIME,1,10)]
smr[,HOUR:=substr(READING_DATETIME,12,13)]

# use GENERAL_SUPPLY_KWH
smr[,KWH_sum:=sum(GENERAL_SUPPLY_KWH),by=list(CUSTOMER_KEY,DAY,HOUR)]
setkeyv(smr,c("CUSTOMER_KEY","DAY","HOUR"))
smr <- unique(subset(smr,select=c("CUSTOMER_KEY","DAY","HOUR","KWH_sum")))

#####################################################################################################
# count Number of KEYS per HOUR and DAY
count.KEYS <- smr[,.N,by=list(HOUR,DAY)]
hist(count.KEYS$N)
#
# check if CUSTOMERS where monitored consecutively
monitored <- function(days,){
  
  all.days <- as.character(seq.Date(as.Date(min(days)),as.Date(max(days)),by="day"))
  
  if(all(all.days%in%days)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

smr[,MC:=monitored(DAY),by=CUSTOMER_KEY]
length(unique(smr[MC==FALSE]$CUSTOMER_KEY))

#####################################################################################################


smr[,WEEKDAY:=wday(DAY)]
smr[,MONTH:=substr(DAY,6,7)]
smr[,YEAR:=substr(DAY,1,4)]
smr[,QUARTER:=if(MONTH[1]%in%c("12","01","02")){"Q1"}else if(MONTH[1]%in%c("03","04","05")){"Q2"}else if(MONTH[1]%in%c("06","07","08")){"Q3"}else{"Q4"},by=list(MONTH)]
smr[,WEEK:=isoweek(DAY)]
# specify day during week and weekend
smr[,DAY_TYPE:=if(WEEKDAY[1]%in%c("1","2","3","4","5")){"WORKDAY"}else{"WEEKEND"},by="WEEKDAY"]
# specify morning, noon, and night
smr[,DAYTIME:=if(HOUR[1]%in%c("06","07","08","09","10","11","12")){"morning"}else if(HOUR[1]%in%c("23","00","01","02","03","04","05")){"night"}else{"noon"},by="HOUR"]

# discard days with missing values in it
smr[,N:=.N,by=c("CUSTOMER_KEY","DAY")]
smr <- smr[N==24]
smr[,N:=NULL]

# save data ==> KWH per Hour, Day and CUSTOMER_KEY
save(smr,file="SmartMeter_transformed.RData",compress=TRUE)
# start to aggregate KWH_mean
rm(smr)

#####################################################################################################
load("/data/home/gussenbauer/essnetBDwp3/SmartMeter_transformed.RData")

estimate_KWH <- function(smr,time.window="WEEK",window.part=NULL){
  
  ##################
  # check for errors
  time.window <- toupper(time.window)
  
  if(!is.null(window.part)){
    
    if(!window.part[[1]]%in%c("DAY_TYPE","WEEKDAY","DAYTIME","HOUR")){
      stop("ERROR : window.part must bei either 'DAY_TYPE', 'WEEKDAY', 'DAYTIME' or 'HOUR' if not NULL")
    }
    
    if(!time.window%in%c("QUARTER","MONTH","WEEK")){
      stop("ERROR : time.window must be either 'QUARTER', 'MONTH' or 'WEEK' if window.part not equal NULL")
    }
  }
  ##################
  
  ##################
  # use window.part to select only specific Months/Quarter/Weeks/Weekdays/Hours
  if(!is.null(window.part)&(length(window.part)==2)){
    smr.internal <- smr[get(window.part[[1]])%in%window.part[[2]]]
  }else{
    smr.internal <- smr
  }
  ##################
  
  ##################
  # look at number of data per time.window and CUSTOMER_KEY
  # missings can occur in data
  # discard each time.window for each CUSTOMER_KEY where only little data ist present
  
  if(time.window%in%c("QUARTER","MONTH","WEEK")){
    
    setkeyv(smr.internal,c("CUSTOMER_KEY","DAY"))
    
    count.Window <- unique(subset(smr.internal,select=c("CUSTOMER_KEY","YEAR","DAY",time.window)))
    count.Window <- count.Window[,.N,by=c("CUSTOMER_KEY","YEAR",time.window)]
    
    if(time.window%in%"WEEK"){
      # discard week if more than 1 day is missing from week
      keep <-  count.Window[N>=6]
    }else if(time.window=="QUARTER"){
      # keep quarter if at least 80 days are present (arbitrary value)
      keep <- count.Window[N>=80]
    }else{
      # keep month if at least 25 days are present (arbitrary value)
      keep <- count.Window[N>=24]
    }
    
    keynames <- colnames(keep)[!colnames(keep)=="N"]
    setkeyv(smr.internal,keynames)
    smr.internal <- smr.internal[keep[,keynames,with=FALSE]]
  }
  ##################
  
  
  ##################
  # aggregate KWH to time.window and window.part
  if(time.window%in%c("QUARTER","MONTH","WEEK")){
    smr.internal[,KWH_estimate:=sum(KWH_sum),by=c("CUSTOMER_KEY","YEAR",time.window,window.part[[1]])]
  }else if(time.window=="DAY_TYPE"){
    smr.internal[,KWH_estimate:=sum(KWH_sum),by=c("CUSTOMER_KEY","WEEK",time.window,window.part[[1]])]
  }else{
    smr.internal[,KWH_estimate:=sum(KWH_sum),by=c("CUSTOMER_KEY","DAY",time.window,window.part[[1]])]
  }
  
  smr.internal[,KWH_estimate:=mean.default(KWH_estimate),by=c("CUSTOMER_KEY",time.window,window.part[[1]])]
  smr.internal <- unique(subset(smr.internal,select=c("CUSTOMER_KEY","KWH_estimate",time.window,window.part[[1]])))
  fac1 <- "CUSTOMER_KEY"
  fac2 <- c(time.window,window.part[[1]])
  smr.internal <- dcast(smr.internal,reformulate(fac2,fac1),value.var="KWH_estimate")
  ##################
  
  # ##################
  # # merge smr.internal with hy
  # setkey(hy,CUSTOMER_KEY)
  # setkey(smr.internal,CUSTOMER_KEY)
  # 
  # hy.internal <- smr.internal[hy]
  # 
  # # calculate mean of KWH_estimate per category in hy.iternal
  # hy.internal[,KWH_estimate:=mean.default(KWH_estimate),by=c(category,time.window,window.part[[1]])]
  # setkeyv(hy.internal,c(category,time.window,window.part[[1]]))
  
  # output
  return(smr.internal)
  
}


# calculate estimates for KWH consumption
# save data to load in shiny up (calculations would be to long otherwise)
a <- c("QUARTER","MONTH","DAY_TYPE","WEEKDAY","DAYTIME")
for(i in 1:5){
  
  test <- estimate_KWH(smr,time.window=a[i])
  
  if(i==1){
    out <- copy(test)
    setkey(out,CUSTOMER_KEY)
  }else{
    setkey(test,CUSTOMER_KEY)
    out <- test[out]
  }
  
  if(i<3){
    for(j in 3:5){
      test <- estimate_KWH(smr,time.window=a[i],window.part = list(a[j]))
      setkey(test,CUSTOMER_KEY)
      out <- test[out]
    }
  }
}

save(out,file="KWH_estimates.RData")


####################################################################################################
# plot results

# read shape file
area <- readOGR(dsn="/mnt/meth/Gussenbauer/ESSNet BigData/data",layer="STATISTIK_AUSTRIA_GEM_20160101")
id.levels <- levels(area@data$ID)
area <- data.table(fortify(area))
area[,id_index:=as.numeric(id)+1]
area[,id:=id.levels[id_index]]
area[,id_index:=NULL]

#####################################################################################################
# write function to plot SmartMeter data
plotSmartMeter <- function(area,hy,smr,hy.split=NULL,time.step="HOUR_TYPE",time.split=NULL){
  
  # use only part of hy for plotting (should boost performance)
  hy.plot <- subset(hy,select=c("db030","id","CUSTOMER_KEY",hy.split))
  
  setkeyv(smr,time.step)
  t <- unique(smr[,time.step,with=FALSE])
  
  # loop over time steps and create screenshot each time
  for(i in 1:nrow(t)){
    
    # aggregate KWH_mean if necessary
    smr.t <- smr[.(t[i])]
    smr.t[,KWH_mean:=mean(KWH_mean),by=c("CUSTOMER_KEY",time.split)]
    smr.t <- unique(subset(smr.t,select=c("CUSTOMER_KEY",time.split,"KWH_mean")))
    
    # merge smr.t with hy.plot
    hy.t <- merge(hy.plot,smr.t,by="CUSTOMER_KEY")
    # calculate KWH_mean per Community
    hy.t[,KWH_mean:=mean(KWH_mean),by=c("id",hy.split)]
    hy.t <- unique(subset(hy.t,select=c("id",hy.split,"KWH_mean")))
    
    # merge hy.t with area
    area.t <- merge(area,hy.t,by="id",allow.cartesian=TRUE)
    
    # plot data
    p1 <- ggplot() + geom_polygon( data=area.t, aes(x=long, y=lat, group = group, fill=KWH_mean),colour="black",size=0.25)
    p1 <- p1 + theme_bw() + xlab("") + ylab("") +
      scale_fill_distiller(name="Mean KWH")+
      theme(axis.line=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text=element_blank(),
            axis.title=element_blank())
    
    if(!is.null(hy.split)){
      fac1 <- hy.split
      if(!is.null(time.split)){
        fac2 <- time.split
      }else{
        fac2 <- "."
      }
      p1 <- p1 + facet_grid(reformulate(fac2,fac1))
    }
    
    plot(p1)
    
    readline(prompt="Press [enter] to continue")
    
  }
}


# call Function to plot
plotSmartMeter(area=area,hy=hy,smr=smr,hy.split=NULL,time.step="HOUR",time.split=NULL)
