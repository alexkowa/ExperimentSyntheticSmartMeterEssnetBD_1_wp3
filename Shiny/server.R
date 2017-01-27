# Shiny App for KWH estimates for ESSNet BigData Project
# 

library(data.table)
library(ggplot2)
library(shiny)
# load data
load("../hhWithCustomerKey.RData")
load("../KWH_estimates.RData")

# help-function to estimate mean of KWH per time.vars and hy.split
selectKWH <- function(hy,KWH_estimates,time.vars="HOUR",hy.split="HHOLD_INCOME_GROUP_CD"){
  
  # select collumns from KWH_estimates
  KWH_cn <- colnames(KWH_estimates)
  # number of time variables
  time.n <- unlist(strsplit(time.vars,"\\_"))
  if(length(time.n)==2){
    select_cn <- KWH_cn[(unlist(lapply(strsplit(KWH_cn,"\\_"),length))==4)&grepl(time.vars, substring(KWH_cn,nchar(KWH_cn)-nchar(time.vars)+1))]
  }else{
    select_cn <- KWH_cn[(unlist(lapply(strsplit(KWH_cn,"\\_"),length))==2)&grepl(time.vars, substring(KWH_cn,nchar(KWH_cn)-nchar(time.vars)+1))]
  }
  
  KWH_estimates_i <- subset(KWH_estimates,select=c("CUSTOMER_KEY",select_cn))
  
  # use only part of hy for plotting (should boost performance)
  hy.sub <- subset(hy,select=c("db030","CUSTOMER_KEY",hy.split))
  
  # set key on KWH_estimates_i in order to not join hy.sub with KWH_estimates_i (large memory allocation)
  setkey(KWH_estimates_i,CUSTOMER_KEY)
  
  # calculate mean over KWH estimates per hy.split
  # select in each hy.split all rows in KWH_estimates_i - one row for each CUSTOMER_KEY in hy.sub 
  # then use colmeans on all collumns of intereset (~select_cn) of this "subsample"
  KWH_out <- hy.sub[,.(KWH=colMeans(KWH_estimates_i[.(.SD[,CUSTOMER_KEY]),select_cn,with=FALSE],na.rm=TRUE),TIME1=select_cn),by=c(hy.split)]
  #KWH_out[,c(hy.split):=levels(get(hy.split))[get(hy.split)]]
  if(length(time.n)==2){
    KWH_out[,c("TIME1","TIME2","TIME.TYPE1","TIME.TYPE2"):=tstrsplit(TIME1,"\\_")]
  }else{
    KWH_out[,c("TIME1","TIME.TYPE1"):=tstrsplit(TIME1,"\\_")]
  }
  
  
  # rename time-collumn
  if("HOUR"%in%time.n){
    wcol <- paste("TIME",which(time.n%in%"HOUR"),sep="")
    KWH_out[,c(wcol):=paste(get(wcol),":00",sep="")]
  }
  if("WEEKDAY"%in%time.n){
    days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
    wcol <- paste("TIME",which(time.n%in%"WEEKDAY"),sep="")
    KWH_out[,c(wcol):=factor(get(wcol),labels=days)]
  }
  if("MONTH"%in%time.n){
    mon <- c("January","February","March","April","May","June","July","August","September","October","November","Dezember")
    mcol <- paste("TIME",which(time.n%in%"MONTH"),sep="")
    KWH_out[,c(mcol):=factor(get(mcol),labels=mon)]
  }
  if("DAYTIME"%in%time.n){
    dtcol <- paste("TIME",which(time.n%in%"DAYTIME"),sep="")
    KWH_out[,c(dtcol):=factor(get(dtcol),levels=c("morning","noon","night"))]
  }
  if("DAYTYPE"%in%time.n){
    dtcol <- paste("TIME",which(time.n%in%"DAYTYPE"),sep="")
    KWH_out[,c(dtcol):=factor(get(dtcol),levels=c("WORKDAY","WEEKEND"))]
  }
  return(KWH_out)  
}

# Estimate mean KWH per sociodemographic criteria and time intervall
shinyServer(
  
  
  function(input, output) {
    
    dataInput <- reactive({
      if(!(input$time2!="EMPTY"&input$time=="QUARTER")){
        selectKWH(hy,KWH_estimates,time.vars=input$time,hy.split=input$socio)
      }else{
        selectKWH(hy,KWH_estimates,time.vars=paste(input$time,input$time2,sep="_"),hy.split=input$socio) 
      }
    })
    
    
    # create plot for output
    output$Plot <- renderPlot({
      
      ## plot KWH_out
      if("TIME2"%in%colnames(dataInput())){
        #if(input$time=="QUARTER"){
        p1 <- ggplot(dataInput(),aes(TIME2,KWH,group=get(input$socio)))+
          geom_line(aes(colour=get(input$socio)),size = .75)+
          xlab(input$time2)+ylab("mean KWH")+
          scale_colour_discrete(name=input$socio)+facet_grid(TIME1~.)
        #}else{
        #  p1 <- ggplot(dataInput(),aes(TIME1,KWH,group=get(input$socio)))+
        #    geom_line(aes(colour=get(input$socio)),size = .75)+
        #    xlab(input$time)+ylab("mean KWH")+
        #    scale_colour_discrete(name=input$socio)+facet_grid(TIME2~.)
        #}
        
      }else{
        p1 <- ggplot(dataInput(),aes(TIME1,KWH,group=get(input$socio)))+
          geom_line(aes(colour=get(input$socio)),size = .75)+
          xlab(input$time)+ylab("mean KWH")+
          scale_colour_discrete(name=input$socio)
      }
      plot(p1)
      
    })
    
    # create table for output
    output$Table <- renderTable({
      cnames <- colnames(dataInput())
      if("TIME2"%in%cnames){
        out <- dcast(dataInput(),formula(paste(paste(cnames[1],cnames[3],sep="+"),cnames[4],sep="~")),value.var="KWH")
        setnames(out,cnames[3],input$time)
        out
      }else{
        dcast(dataInput(),formula(paste(cnames[1],cnames[3],sep="~")),value.var="KWH")
      }
      
    })
    
  }
)