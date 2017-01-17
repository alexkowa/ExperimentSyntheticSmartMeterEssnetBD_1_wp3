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

# read SmartMeter data
load("/mnt/meth/BIGDATA/Smartmeter/smr.RData")
# half hourly energy consumption per household over a time period of ~2 years 
smr[,DAY:=substr(READING_DATETIME,1,10)]
smr[,HOUR:=substr(READING_DATETIME,11,13)]

# use GENERAL_SUPPLY_KWH
smr[,KWH_sum:=sum(GENERAL_SUPPLY_KWH),by=list(CUSTOMER_KEY,DAY,HOUR)]
setkeyv(smr,c("CUSTOMER_KEY","DAY","HOUR"))
smr <- unique(subset(smr,select=c("CUSTOMER_KEY","DAY","HOUR","KWH_sum")))

smr[,WDAYS:=wday(DAY)]
smr[,MONTH:=substr(DAY,6,7)]
#smr[,YEAR:=substr(READING_DATETIME,1,4)]
smr[,QUARTER:=if(any(MONTH%in%c("12","01","02"))){"Q1"}else if(any(MONTH%in%c("03","04","05"))){"Q2"}else if(any(MONTH%in%c("06","07","08"))){"Q3"}else{"Q4"},by=list(YEAR,MONTH)]

smr[,KWH_mean:=mean.default(KWH_sum),by=list(CUSTOMER_KEY,QUARTER,WDAYS,HOUR)]
setkeyv(smr,c("CUSTOMER_KEY","QUARTER","WDAYS","HOUR"))
smr <- unique(subset(smr,select=c("CUSTOMER_KEY","QUARTER","WDAYS","HOUR","KWH_mean")))

save(smr,file="SmartMeter_transformed.RData",compress=TRUE)

# specify day during week and weekend
smr[,DAY_TYPE:=if(any(WDAYS%in%c("1","2","3","4","5"))){"WORKDAY"}else{"WEEKEND"},by="WDAYS"]
# specify morning, noon, and night
smr[,HOUR_TYPE:=if(any(HOUR%in%c("06","07","08","09","10","11","12"))){"morning"}else if(any(HOUR%in%c("23","00","01","02","03","04","05"))){"night"}else{"noon"},by="HOUR"]

smr[,KWH_mean:=mean(KWH_mean),by=list(QUARTER,DAY_TYPE,HOUR_TYPE)]
smr <- unique(subset(smr,select=c("CUSTOMER_KEY","QUARTER","DAY_TYPE","HOUR_TYPE")))


# read shape file
area <- readOGR(dsn="/mnt/meth/Gussenbauer/ESSNet BigData/data",layer="STATISTIK_AUSTRIA_GEM_20160101")
id.levels <- levels(area@data$ID)
area <- data.table(fortify(area))
area[,id_index:=as.numeric(id)+1]
area[,id:=id.levels[id_index]]
area[,id_index:=NULL]


# write function to plot SmartMeter data
plotSmartMeter <- function(area,hy,smr,hy.split=NULL,time.step="HOUR",time.split=NULL){
  
  # use only part of hy for plotting (should boost performance)
  hy.plot <- subset(hy,select=c("db030","id","CUSTOMER_KEY",hy.split))
  
  # get time steps
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
