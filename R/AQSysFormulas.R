require(rootSolve)
#
mrchk <- function(XYdt,P1=10,P2=1,P3=0,...){
  #
  names(XYdt)<-c("XC","YC")
  #
  FFn <- nls(
    YC~ P1*exp(P2*(XC^(0.5))-P3*(XC^3)),
    start=list(P1=P1,P2=P2,P3=P3),
    data=XYdt,na.exclude)    
  FFn
}

mrgsn <- function(XYdt,...){
  #
  names(XYdt)<-c("XC","YC")
  ##  controll<-nls.control(maxiter=50,
  ##  tol=1e-10, minFactor = 1/1024,
  ##  printEval = FALSE, warnOnly = FALSE)
  FFn <- nls(
    YC~ P1+P2*(XC)^0.5+P3*XC,
    start=list(P1=50,P2=1,P3=0),
    data=XYdt,na.exclude)    
  FFn
}

tello <- function(XYdt,...){
  #
  S <- diff(XYdt[,1])/diff(XYdt[,2])
  nX <- nrow(XYdt)
  XC <- XYdt[1:nX-1,1]
  #
  A1 <- LLSRxy(XC,S)
  names(A1)<-c("XC","S")
  #
  FFnEst <- nls(
    S ~ (P2/P1)+XC/P1,
    start=list(P1=-.1,P2=.001),
    data=A1,na.exclude)
  #
  coefEst <- summary(FFnEst)
  coef.1 <- coefEst$coefficients[1]
  coef.2 <- coefEst$coefficients[2]
  coef.3 <- mean(log(abs(exp(XYdt[,2])*(-exp(coef.1)-exp(coef.2 + XYdt[,1])))))
  #
  names(XYdt)<-c("XC","YC")
  FFn <- nls(
    XC~ exp((YC - P3)/P1) - P2,
    start=list(P1=coef.1,P2=coef.2,P3=coef.3),
    #start=list(P1=-.1,P2=.001,P3=-.1),
    data=XYdt,na.exclude)    
  FFn
}
