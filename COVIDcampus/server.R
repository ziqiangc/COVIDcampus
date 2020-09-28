library(shiny)
library(datasets)
library(readxl)
library(dplyr)
library(ggplot2)
library(nlstools)
library(MASS)

# source("VisFun.R")


shinyServer(function(input, output){
  
  # preload data
  
  y <- c(280,340,430,480,550,580,590,600,590,600)
  time <- c(7,21,35,49,63,77,91,105,119,133)
  preload_dat <- data.frame(time, y)
  
  # intermediate function setup
  
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
  
  

  ## function set up
  
  QuickPlot1 <- function(dat, cutoff=100) {
    
    # Must have at least 3 datapoints at different times
    if (length(unique(dat$positive)) < 3) stop("too few data points to fit curve")
    
    # fit model
    mod = glm(formula = positive ~ time, family = "poisson", data = dat)
    
    # predict the curve
    grid <- seq(0, max(10, dat$time*2), length.out=100)
    preds <- predict(mod, newdata=data.frame(time=grid), type="link", se.fit=TRUE)
    
    # confidence band
    # se <- 1.96 * preds$se.fit
    # upr <- unlist(mod$family$linkinv(preds$fit + se))
    # lwr <- unlist(mod$family$linkinv(preds$fit - se))
    # fit <- unlist(mod$family$linkinv(preds$fit))
    
    param <- coef(mod)
    param.lwr <- confint(mod)[,1]
    param.upr <- confint(mod)[,2]
    
    
    inverse.predict <- function(y, param) {
      x = (log(y)-param[1]) / param[2]
      return(x)
    }
    
    predict.fun <- function(x, param) {
      y = exp(param[1] + param[2]*x)
    }
    
    fit = predict.fun(grid, param)
    upr = predict.fun(grid, param.lwr)
    lwr = predict.fun(grid, param.upr)
    
    time.set <- (mod$family$linkfun(cutoff)-param[1]) / param[2] # 95% LOD Estimate
    time.lwr <- (mod$family$linkfun(cutoff)-param.lwr[1]) / param.lwr[2]
    time.upr <- (mod$family$linkfun(cutoff)-param.upr[1]) / param.upr[2]
    
    
    ## Plot
    with(dat, plot(time, positive,ylab="Count of positives", xlab="Time period", pch=19, 
                   xlim = c(1,max(10, dat$time*2, time.upr,time.lwr)), ylim=c(0,max(c(upr,lwr)))))
    title(main=paste("Poisson Model, Time Threshold =", round(time.set, 3)))
    
    lines(grid, fit)
    lines(grid, lwr, lty=2)
    lines(grid, upr, lty=2)
    abline(h=cutoff, lty=2, col="red")
    
    abline(v=time.set, lty=2, col="red")
    
    abline(v=time.upr, lty=3, col="royalblue")
    abline(v=time.lwr, lty=3, col="royalblue")
    
    
    axis(2, cutoff, labels = cutoff, col.axis="red", cex.axis=0.8)
    axis(1, time.set, labels = as.character(round(time.set,3)), col.axis="red", cex.axis=0.8, outer =F)
  }
  
  
  
  QuickPlot2 <- function(dat){
    
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
  
  

  output$plot1 <- renderPlot({
    # If missing input, return to avoid error later in function
    if(is.null(input$choose_source)) {
      # dat <- input_dat
      # dat <- as.data.frame(read_excel("Sample data for Zack.xlsx"))
      period <- length(c(input$pos1, input$pos2, input$pos3, input$pos4, input$pos5))
      dat <- data.frame(time=c(7,7+14*c(1:c(period-1))),
                        y=c(input$pos1, input$pos2, input$pos3, input$pos4, input$pos5))
      
    } else {
      dat <- as.data.frame(read_excel(input$choose_source$datapath))
    }
    
    QuickPlot1(dat=dat)
  })
  
  output$plot2 <- renderPlot({
    # If missing input, return to avoid error later in function
    if(is.null(input$choose_source)) {
      dat <- preload_dat
      # dat <- as.data.frame(read_excel("Sample data for Zack.xlsx"))
    } else {
      dat <- as.data.frame(read_excel(input$choose_source$datapath))

    }
    
    QuickPlot2(dat=dat)
  })
  

  
  
  
})
