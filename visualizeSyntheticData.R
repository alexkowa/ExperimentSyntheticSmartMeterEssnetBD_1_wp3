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
library(broom)

# read synthetic population from simPop for testing porpuses
load("M:/Gussenbauer/ESSNet BigData/data/synthetic_population.RData")
setnames(p,"db040","state")
p[,State.Size:=.N,by="state"]

# read residence sizes per region
pop.size <- data.table(read.csv("M:/Gussenbauer/ESSNet BigData/data/Einwohnerzahl_01012016.csv", sep=";"))
pop.size[,Size:=as.numeric(gsub("\\.","",Size))]
pop.size[,state.n:=substr(Number,1,1)]
pop.size[,state:=if(state.n==1){"Burgenland"}else if(state.n==2){"Carinthia"}else if(state.n==3){"Lower Austria"}else if(state.n==4){"Upper Austria"
}else if(state.n==5){"Salzburg"}else if(state.n==6){"Styria"}else if(state.n==7){"Tyrol"}else if(state.n==8){"Vorarlberg"}else{"Vienna"},by=1:nrow(pop.size)]
pop.size[,Status:=NULL]
pop.size[,prob:=Size/sum(Size),by="state"]

# assign households randomly to communities (per State)
p.sub <- unique(p[,.(state,State.Size)])
setkey(p.sub,state)
setkey(pop.size,state)
pop.size <- p.sub[pop.size]
pop.size[,Number:=as.character(Number)]

pop.size <- pop.size[,sample(Number,size=unique(State.Size),prob=prob,replace=TRUE),by="state"]
setnames(pop.size,"V1","id")

setkey(pop.size,state)
setkey(p,state)

p <- cbind(p,pop.size[,.(id)])
rm(pop.size,p.sub)

# read shape file
area <- readOGR(dsn="data",layer="STATISTIK_AUSTRIA_GEM_20160101")
area <- data.table(fortify(area,region="ID"))


# calculate variable of interest
# for example mean income per community
p[,c("Mean.income","Median.income"):=.(mean(netIncome,na.rm=TRUE),median(netIncome,na.rm=TRUE)),by=id]
p.var <- unique(p[,.(id,Mean.income,Median.income)])
setkey(p.var,id)
setkey(area,id)
area <- p.var[area]


p1 <- ggplot() + geom_polygon( data=area, aes(x=long, y=lat, group = group, fill=Median.income),colour="black",size=0.25)
p1 <- p1 +  theme_bw()+xlab("")+ylab("")+
  scale_fill_distiller(name="Median Income")+
  theme(axis.line=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank())
p1


