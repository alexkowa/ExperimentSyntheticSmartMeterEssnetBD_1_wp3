vacant <- function(kwh,ret="pred"){
  reg <- abs(sin(cbind(
    rep(seq(-pi,0,l=3),8),
    rep(seq(-pi,0,l=6),4),
    rep(seq(-pi,0,l=12),2)
  )))
  reg <- rbind(reg,reg)
  regx <- reg[1:length(kwh),]
  x <- try(m <- lm(kwh~regx-1),silent=TRUE)
  if(class(x)=="try-error")
    return(NA_real_)
  if(ret=="pred")
    return(predict(m))
  else
    return(summary(m)$adj.r.squared)
}
vacantArima <- function(kwh,ret="x"){
  x <- try(aa <- auto.arima(ts(kwh,start=1,frequency=1)))
  if(class(x)[1]=="try-error")
    return(NA_real_)
  else{
    if(ret=="pred")
      return(ts(kwh,start=1,frequency=1)-aa$residuals)
    else
      return(cor(ts(kwh,start=1,frequency=1)-aa$residuals,ts(kwh,start=1,frequency=1)))
  }
}