library(chemCal)

dat <- data.frame(time=c(1:7),
                  positive=c(0, 46, 58, 64, 64, 67, 70))


lod.model <- function(dat, cutoff=100) {

  dat <- dat[complete.cases(dat),]
  
  # Must have at least 3 datapoints at different times
  if (length(unique(dat$positive)) < 3) stop("too few data points to fit curve")
  
  # fit model
  mod = glm(formula = positive ~ time, family = "poisson", data = dat)
  
  # predict the curve
  grid <- seq(0, max(10, dat$time*2), length.out=100)
  preds <- predict(mod, newdata=data.frame(time=grid), type="link", se.fit=TRUE)
  
  # confidence band
  se <- 1.96 * preds$se.fit
  upr <- unlist(mod$family$linkinv(preds$fit + se))
  lwr <- unlist(mod$family$linkinv(preds$fit - se))
  fit <- unlist(mod$family$linkinv(preds$fit))
  
  param <- coef(mod)
  param.lwr <- confint(mod)[,1]
  param.upr <- confint(mod)[,2]
  
  
  # inverse.predict <- function(y, param) {
  #   x = (log(y)-param[1]) / param[2]
  #   return(x)
  # }
  # 
  # predict.fun <- function(x, param) {
  #   y = exp(param[1] + param[2]*x)
  # }
  
  # fit = predict.fun(grid, param)
  # upr = predict.fun(grid, param.lwr)
  # lwr = predict.fun(grid, param.upr)
  
  time.set <- (mod$family$linkfun(cutoff)-param[1]) / param[2] # 95% LOD Estimate
  # time.lwr <- (mod$family$linkfun(cutoff)-param.lwr[1]) / param.lwr[2]
  # time.upr <- (mod$family$linkfun(cutoff)-param.upr[1]) / param.upr[2]
  
  
  ## Plot
  with(dat, plot(time, positive,ylab="Count of positives", xlab="Time period", pch=19, 
                 xlim = c(1,max(10, sum(!is.na(dat$positive))*2)), ylim=c(0,max(c(upr,lwr)))))
  title(main=paste("Poisson Model, Time Threshold =", round(time.set, 3)))
  
  lines(grid, fit)
  lines(grid, lwr, lty=2)
  lines(grid, upr, lty=2)
  abline(h=cutoff, lty=2, col="red")
  
  abline(v=time.set, lty=2, col="red")
  
  # abline(v=time.upr, lty=3, col="royalblue")
  # abline(v=time.lwr, lty=3, col="royalblue")
  
  
  axis(2, cutoff, labels = cutoff, col.axis="red", cex.axis=0.8)
  axis(1, time.set, labels = as.character(round(time.set,3)), col.axis="red", cex.axis=0.8, outer =F)
}






