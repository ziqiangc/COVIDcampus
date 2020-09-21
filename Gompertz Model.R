library(tidyverse)
library(MASS)
library(ggplot2)
# library(easynls)
library(nlstools)

set.seed(4)

fit.gompertz <- function(data, time){
  d <- data.frame(y=data, t=time)
  
  # Must have at least 3 datapoints at different times
  if (length(unique(d$t)) < 3) stop("too few data points to fit curve")
  
  # Pick starting values ###
  i <- which.min(diff(d$y))
  starting.values <- c(a=600,#
                       mu=4,#max(diff(d$y))/(d[i+1,"t"]-d[i, "t"]), 
                       lambda=0.06)#0.05)#i)
  print("Starting Values for Optimization: ")
  print(starting.values)
  ##########################
  formula.gompertz <- "y ~ a*exp(-mu*exp(-lambda*t))"
  nls(formula.gompertz, d, starting.values)
}

gompertz <- function(time, a, mu, lambda){
  y <-a*exp(-mu*exp(-lambda*time)) 
  return(data.frame(time=time, y=y))
}


y <- c(280,340,430,480,550,580,590,600,590,600)
time <- c(7,21,35,49,63,77,91,105,119,133)
dat <- data.frame(time, y)

Gompertz.model <- function(dat){
  
  # fit model
  #fit <- nlsfit(dat, model=10, start=c(600,4,0.05))#
  fit <- fit.gompertz(data = dat$y, time = dat$time)
  # predict curve
  newtime <- seq(from =7, to = 287, by = 14)
  # pred <- predict(fit, newdata= data.frame(t = newtime), se.fit = TRUE)
  
  # parameter extraction
  #param <- fit$Parameters[1:3,1]
  param <- coef(fit)
  param.upr <- confint2(fit)[,2]
  param.lwr <- confint2(fit)[,1]
  # confidence band
  upr <- gompertz(time = newtime, a = param.upr[1], mu = param.upr[2],
                  lambda = param.upr[3])$y
  lwr <- gompertz(time = newtime, a = param.lwr[1], mu = param.lwr[2],
                  lambda = param.lwr[3])$y
  fitted <- gompertz(time = newtime, a = param[1], mu = param[2],
                     lambda = param[3])$y
  
  # find the value at 95% of 8
  #time5000<- param[3]-((param[2]*exp(1))/(log(log(param[1]/8)))-1)/param[1]
  time.set <- (-1/param[3])*log((-1/param[2])*log(450/param[1]))
  time.upr <- (-1/param.upr[3])*log((-1/param.upr[2])*log(450/param.upr[1]))
  time.lwr <- (-1/param.lwr[3])*log((-1/param.lwr[2])*log(450/param.lwr[1]))
  
  with(dat, plot(time, y,  xlim=c(0, 287), ylim=c(280, 750), ylab="Number of positive cases", xlab="Time"))
  title(main=paste("Gompertz Model, Time Threshold =", round(time.set, 3)))
  lines(newtime, fitted)
  lines(newtime, lwr, lty = 2)
  lines(newtime, upr, lty =2)
  abline(h=450, lty=2, col="red")
  abline(v=time.set, lty=2, col="red")
  
  abline(v=time.upr, lty=3, col="royalblue")
  abline(v=time.lwr, lty=3, col="royalblue")
  
  axis(2, 450, labels = "450", col.axis="red", cex.axis=0.8)
  axis(1, time.set, labels = as.character(round(time.set,3)), col.axis="red", cex.axis=0.8)
}


res <- Gompertz.model(dat)

