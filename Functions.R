# displaying residuals
display.resid <- function(fit,nbreaks=10){
  z <- rstudent(fit)
  hist(z,breaks = nbreaks,freq = FALSE,col="yellow",
       xlab = "Student's residuals",main = "")
  rug(jitter(z),col="brown")
  curve(dnorm(x,mean = mean(z),sd=sd(z)),add=TRUE,
        col="blue",lwd = 2)
  lines(density(z)$x,density(z)$y,col="red",lwd=2,lty=2)
#  legend("topright",legend=c("1","2"),
#         lty = 1:2,col=c("blue","red"),cex=.7)
}
#
normality3.extend<-function(df,p=.05,...)
{
  car::qqPlot(df)
  sht<<-shapiro.test(df)
  if(sht$p.value>p){
    print("Shapiro: norm")
  }
  else{
   print("Shapiro: no norm")
  }
  lil<<-lillie.test(df)
  ifelse(lil$p.value>p,"Lilliefors: norm","Lilliefors: no norm")
}
#
my.transform <- function(x){
  my.tr <- powerTransform(x)#Likelihood estimate lambda
  #(summary(my.tr))
  if(my.tr$lambda == 0){
    x1 <<- log(x+1)
  }
  else{
    x1 <<- x^my.tr$lambda
    print(paste("Optimal lambda  transformation: ",my.tr$lambda))
    ### doing Box-Cox's transformance  
  }
  return (x1)
}
#

my.trans.box_cox <- function(xXX){
  m.null <- lm(xXX~1)
  bc.null <- boxCox(xXX~1)
  bc.null.opt <- bc.null$x[which.max(bc.null$y)]
  print(paste("Optimal lambda Box-Cox's transformation: ",bc.null.opt))
### doing Box-Cox's transformance  
  XXX <- bcPower(xXX,bc.null.opt)
  return (XXX)
}

######### Grapics image for hat-statistic #####
hat.plot <- function(fit){
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit),main="Hat statistics")
  abline(h=c(2,3)*p/n,col="red",lty=1)
  identify(1:n,hatvalues(fit),names(hatvalues(fit)))
}
#
####### Cook's distance ##################
#
cook.dist.my <- function(fit,...){
  elips <- list(...)
  if(length(elips) == 0)
     stop("Name of date is absent!\nAdding date = <name_date>")
  cutoff <- 4/(nrow(elips$data)-length(fit$coefficients)-2)
  print(paste("Cook's critical distance = ",cutoff))
  if(cutoff>1){
        cutoff <- 1
    }
  plot(fit,which=4,cook.levels=cutoff)
  abline(h=cutoff,lty=1, col="red")
}
#
#######Cross-Validation #################
#
shrinkage <- function(fito,k=10){
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predikt <- function(fito,x){cbind(1,x)%*%fito$coef}
  
  x <- fito$model[,2:ncol(fito$model)]
  y <- fito$model[,1]
  
  results <- bootstrap::crossval(x,y,theta.fit,theta.predikt,ngroup = k)
  r2 <- cor(y,fito$fitted.values)^2
  r2cv <- cor(y,results$cv.fit)^2
  cat("Original R-square = ",r2,"\n")
  cat(k,"Fold Cross-Validated R-square = ",r2cv,"\n")
  cat("Change = ",r2-r2cv,"\n")
}
#
######## Relative weights ####
#
relweights <- function(fito,...){
  R <- cor(fito$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar,2:nvar]
  rxy <- R[2:nvar,1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda^2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta^2)
  rawwgt <- lambdasq %*% beta^2
  import <- (rawwgt/rsquare)*100
  lbls <- names(fito$model[2:nvar])
  rownames(import) <- lbls
  colnames(import) <- "Weights"
  barplot(t(import),names.arg = lbls,
          ylab = "% predicted variance",
          xlab = "Independence variables",
          main = "Relation weights",
          sub=paste("R-square=",round(rsquare,digits=3)),...)
  return(import)
}
######
################   TS - regressions #######################
my.regform <- function(tt,xa,ng=6,s=12,np=3,...){
  ll <- list(...)
  if(length(ll) == 0){
        SIN <- COS <- matrix(nr=length(tt),nc=ng)
        xnam <- paste0("COS[,", 1:ng,"]+SIN[,",1:ng,"]")
        for(i in 1:ng){
          COS[,i] <- cos(2*pi*i*tt/s)
          SIN[,i] <- sin(2*pi*i*tt/s)
          if(i == 1)
            xn1 <- xnam[1]
          else
          {
            xn1 <- paste(xn1,"+",xnam[i])
          }
          
        }
        #########  Attention!!!! ###############
        nn <- deparse(substitute(tt))
        ###        print (nn)
        xn0 <- nn
        for(i in 2:np)
          xn0 <- paste0(xn0,"+I(",nn,"^",i,")")
        print(xn0)  
        xn1 <- paste(xn0,"+",xn1)
  }
  else
  {
          print(paste("This part while in development!"))
          print(paste("But your call is very important to us !"))
          xn1 <- ll$my.fl
  }
  ############################################
  print(xn1)
  return (xa.lm <- lm(as.formula(paste("xa~",xn1))))
}
