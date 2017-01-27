# Project: Test
# 
# Author: kowa$
###############################################################################
# visualize synthetic data
# merge households to regions and display via shape file
#
library(data.table)
library(ggplot2)
library(scales)
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
# pop.size <- data.table(read.csv("/mnt/meth/Gussenbauer/ESSNet BigData/data/Haushalte_pro_Gemeinde.csv", sep=";"))
# pop.size <- pop.size[JAHR==2011]
# pop.size[HH_SIZE%%1!=0,HH_SIZE:=1000*HH_SIZE]
# pop.size[,state.n:=substr(NUMBER,1,1)]
# pop.size[,state:=if(state.n==1){"Burgenland"}else if(state.n==2){"Carinthia"}else if(state.n==3){"Lower Austria"}else if(state.n==4){"Upper Austria"
# }else if(state.n==5){"Salzburg"}else if(state.n==6){"Styria"}else if(state.n==7){"Tyrol"}else if(state.n==8){"Vorarlberg"}else{"Vienna"},by=1:nrow(pop.size)]
# 
# # assign households randomly to communities (per State)
# hy.sub <- unique(hy[,.(state,State.Size)])
# setkey(hy.sub,state)
# setkey(pop.size,state)
# pop.size <- hy.sub[pop.size]
# pop.size[,NUMBER:=as.character(NUMBER)]
# 
# # reduce State.Size_new to Stat.Size
# # very naive approach, will suffice for testing purposes
# pop.size[,State.Size_new:=sum(HH_SIZE),by="state.n"]
# pop.size[HH_SIZE>100,N_Com:=.N,by="state.n"]
# pop.size[HH_SIZE>100,DIFF:=(State.Size_new-State.Size)/N_Com]
# pop.size[HH_SIZE>100,RANDOM_DIFF:=round(N_Com*(DIFF-floor(DIFF)))]
# pop.size[HH_SIZE>100,RANDOM_DIFF:=sample(c(rep(1,unique(RANDOM_DIFF)),rep(0,unique(N_Com-RANDOM_DIFF)))),by="state.n"]
# pop.size[HH_SIZE>100,DIFF:=RANDOM_DIFF+floor(DIFF)]
# pop.size[HH_SIZE>100,HH_SIZE:=HH_SIZE-DIFF]
# pop.size[,HH_SIZE:=round(HH_SIZE)]
# 
# 
# pop.size <- pop.size[,sample(rep(NUMBER,HH_SIZE)),by="state"]
# setnames(pop.size,"V1","id")
# 
# setkey(pop.size,state)
# setkey(hy,state)


pop.size <- data.table(read.csv("/mnt/meth/Gussenbauer/ESSNet BigData/data/Einwohnerzahl_01012016.csv", sep=";"))
pop.size[,state.n:=substr(Number,1,1)]
pop.size[,state:=if(state.n==1){"Burgenland"}else if(state.n==2){"Carinthia"}else if(state.n==3){"Lower Austria"}else if(state.n==4){"Upper Austria"
}else if(state.n==5){"Salzburg"}else if(state.n==6){"Styria"}else if(state.n==7){"Tyrol"}else if(state.n==8){"Vorarlberg"}else{"Vienna"},by=1:nrow(pop.size)]
pop.size[,Status:=NULL]
pop.size[Size%%1!=0,Size:=Size*1000]
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
hy[,HHOLD_INCOME_GROUP_CD:=factor(HHOLD_INCOME_GROUP_CD,levels=c("LOW","MED","HI"))]
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
smr[,DAYTYPE:=if(WEEKDAY[1]%in%c("1","2","3","4","5")){"WORKDAY"}else{"WEEKEND"},by="WEEKDAY"]
# specify morning, noon, and night
smr[,DAYTIME:=if(HOUR[1]%in%c("06","07","08","09","10","11","12")){"morning"}else if(HOUR[1]%in%c("23","00","01","02","03","04","05")){"night"}else{"noon"},by="HOUR"]

# discard days with missing values in it
smr[,N:=.N,by=c("CUSTOMER_KEY","DAY")]
smr <- smr[N==24]
smr[,N:=NULL]

# save data ==> KWH per Hour, Day and CUSTOMER_KEY
save(smr,file="/mnt/meth/BIGDATA/Smartmeter/SmartMeter_transformed.RData",compress=TRUE)
# load("SmartMeter_transformed.RData")
# start to aggregate KWH_mean
rm(smr)

#####################################################################################################
# estimate KWH per quarter/month/weekday ect...
load("/mnt/meth/BIGDATA/Smartmeter/SmartMeter_transformed.RData")

estimate_KWH <- function(smr,time.window="WEEK",window.part=NULL){
  
  ##################
  # check for errors
  time.window <- toupper(time.window)
  
  if(!is.null(window.part)){
    
    if(!window.part[[1]]%in%c("DAYTYPE","WEEKDAY","DAYTIME","HOUR")){
      stop("ERROR : window.part must bei either 'DAYTYPE', 'WEEKDAY', 'DAYTIME' or 'HOUR' if not NULL")
    }
    
    if(!time.window%in%c("QUARTER","MONTH","WEEK","DAYTYPE")){
      stop("ERROR : time.window must be either 'QUARTER', 'MONTH', 'WEEK' or 'DAYTYPE' if window.part not equal NULL")
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
      # discard week if more than 1 day is missing from week (arbitrary value)
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
  }else if(time.window=="DAYTYPE"){
    smr.internal[,KWH_estimate:=sum(KWH_sum),by=c("CUSTOMER_KEY","WEEK",time.window,window.part[[1]])]
  }else{
    smr.internal[,KWH_estimate:=sum(KWH_sum),by=c("CUSTOMER_KEY","DAY",time.window,window.part[[1]])]
  }
  
  smr.internal[,KWH_estimate:=mean.default(KWH_estimate),by=c("CUSTOMER_KEY",time.window,window.part[[1]])]
  smr.internal <- unique(subset(smr.internal,select=c("CUSTOMER_KEY","KWH_estimate",time.window,window.part[[1]])))
  fac1 <- "CUSTOMER_KEY"
  fac2 <- c(time.window,window.part[[1]])
  smr.internal <- dcast(smr.internal,reformulate(fac2,fac1),value.var="KWH_estimate")
  smr.cn <- colnames(smr.internal)
  setnames(smr.internal,smr.cn[smr.cn!="CUSTOMER_KEY"],paste(smr.cn[smr.cn!="CUSTOMER_KEY"],paste(c(time.window,window.part[[1]]),collapse="_"),sep="_"))
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
# save data to load in shiny app (calculations would be to long otherwise)
a <- c("QUARTER","MONTH","DAYTYPE","WEEKDAY","DAYTIME","HOUR")
for(i in 1:6){
  
  test <- estimate_KWH(smr,time.window=a[i])
  
  if(i==1){
    KWH_estimates <- copy(test)
    setkey(KWH_estimates,CUSTOMER_KEY)
  }else{
    setkey(test,CUSTOMER_KEY)
    KWH_estimates <- test[KWH_estimates]
  }
  
  if(i<4){
    if(i==3){
      for(j in 5:6){
        test <- estimate_KWH(smr,time.window=a[i],window.part = list(a[j]))
        setkey(test,CUSTOMER_KEY)
        KWH_estimates <- test[KWH_estimates]
      }
    }else{
      for(j in 3:6){
        test <- estimate_KWH(smr,time.window=a[i],window.part = list(a[j]))
        setkey(test,CUSTOMER_KEY)
        KWH_estimates <- test[KWH_estimates]
      }
    }
  }
}

save(KWH_estimates,file="KWH_estimates.RData")
load("KWH_estimates.RData")
rm(smr)

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
plotSmartMeter <- function(area,hy,KWH_estimates,hy.split=NULL,time.vars="HOUR",stat="mean"){
  
  # select collumns from KWH_estimates
  KWH_cn <- colnames(KWH_estimates)
  select_cn <- KWH_cn[unlist(lapply(strsplit(KWH_cn,"\\_"),length))==2&grepl(time.vars, substring(KWH_cn,nchar(KWH_cn)-nchar(time.vars)+1))]
  KWH_estimates_i <- subset(KWH_estimates,select=c("CUSTOMER_KEY",select_cn))
  
  # define limits for colouring in plot
  limits.col <- quantile(KWH_estimates_i[,select_cn,with=FALSE],c(0,.975),na.rm=TRUE)
  
  # use only part of hy for plotting (should boost performance)
  hy.plot <- subset(hy,select=c("db030","id","CUSTOMER_KEY",hy.split))
  # merge KWH_estimates_i with hy.plot
  hy.plot <- merge(hy.plot,KWH_estimates_i,by="CUSTOMER_KEY")
  
  # loop over time steps(~collumns) and create screenshot each time
  n.time <- length(select_cn)
  for(i in 1:n.time){
    
    # select collumns needed for plot
    hy.t <- subset(hy.plot,select=c("db030","id","CUSTOMER_KEY",hy.split,select_cn[i]))
    setnames(hy.t,select_cn[i],"KWH")
    # calculate KWH_mean per Community and hy.split
    if(stat=="mean"){
      hy.t[,KWH:=mean(KWH,na.rm=TRUE),by=c("id",hy.split)]
    }else{
      hy.t[,KWH:=sum(KWH,na.rm=TRUE),by=c("id",hy.split)]
    }
    
    hy.t <- unique(subset(hy.t,select=c("id",hy.split,"KWH")))
    
    # merge hy.t with area
    area.t <- merge(area,hy.t,by="id",allow.cartesian=TRUE)
    
    title.plot <- unlist(strsplit(select_cn[i],"\\_"))
    if(length(title.plot)==4){
      if(title.plot[4]=="WEEKDAY"){
        weekdays <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
        title.plot[2] <-  weekdays[as.numeric(title.plot[2])]
      }else if(title.plot[3]=="MONTH"){
        month <- c("January","February","March","April","May","June","July","August","September","October","November","Dezember")
        title.plot[1] <-  month[as.numeric(title.plot[1])]
      }
      title.plot <- paste(title.plot[[1]],title.plot[[2]],sep="-")
    }else{
      if(title.plot[2]=="WEEKDAY"){
        weekdays <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
        title.plot <-  weekdays[as.numeric(title.plot[1])]
      }else if(title.plot[2]=="HOUR"){
        title.plot <- paste(title.plot[1],":00",sep="")
      }else if(title.plot[2]=="MONTH"){
        month <- c("January","February","March","April","May","June","July","August","September","October","November","Dezember")
        title.plot <-  month[as.numeric(title.plot[1])]
      }else{
        title.plot <- title.plot[1]
      }
    }

    
    # plot data
    p1 <- ggplot() + geom_polygon( data=area.t, aes(x=long, y=lat, group = group, fill=KWH),colour="black",size=0.25)
    p1 <- p1 + theme_bw() + xlab("") + ylab("") +
      scale_fill_continuous(name="Mean KWH",high = "#132B43", low = "#56B1F7",limits=limits.col)+
      ggtitle(title.plot)+
      theme(axis.line=element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            axis.ticks = element_blank(),
            axis.text=element_blank(),
            axis.title=element_blank(),
            legend.title=element_text(size=50),
            legend.text=element_text(size=40),
            plot.title=element_text(size=64),
            legend.key.size =unit(2.5, "cm"))
    
    if(!is.null(hy.split)){
      
      if(hy.split=="NUM_OCCUPANTS"){
        p1 <- p1 + facet_wrap(formula(paste("~",hy.split,sep="")))+
          theme(strip.text.x = element_text(size = 40))
      }else{
        p1 <- p1 + facet_grid(reformulate(".",hy.split))+
          theme(strip.text.y = element_text(size = 40))
      }
    }
    
    # turn on grDevice
    filename <- paste(c("Animation",hy.split,select_cn[i]),collapse="-")
    if(!is.null(hy.split)&hy.split=="NUM_OCCUPANTS"){
      y <- 2000
      x <- 6000
    }else{
      y <- 1000*max(1,nrow(unique(hy.t[,hy.split,with=FALSE])))
      x <- 2000
    }
    jpeg(file=paste("/mnt/meth/Projekte/ESSNet BigData/SyntheticData/Animation/",filename,".jpeg",sep=""),width=x,height=y)
    plot(p1)
    dev.off()
    # turn off grDevice
  }
}


# call Function to plot
a <- c("QUARTER","MONTH","DAYTYPE","WEEKDAY","DAYTIME","HOUR")
b <- c("HHOLD_INCOME_GROUP_CD","NUM_OCCUPANTS","IS_HOME_DURING_DAYTIME")
for(i in 1:6){
  
  #plotSmartMeter(area=area,hy=hy,KWH_estimates=KWH_estimates,hy.split=NULL,time.vars=a[i])
  
  #for(y in 2:length(b)){
    plotSmartMeter(area=area,hy=hy,KWH_estimates=KWH_estimates,hy.split=b[2],time.vars=a[i])
  #}
  
}


#  if(i<3){
#    for(j in 3:5){
#      plotSmartMeter(area=area,hy=hy,KWH_estimates=KWH_estimates,hy.split=NULL,time.vars=paste(a[i],a[j],sep="_"))
#    }
#  }


plotSmartMeter(area=area,hy=hy,KWH_estimates=KWH_estimates,hy.split=NULL,time.step="HOUR",time.split=NULL)
