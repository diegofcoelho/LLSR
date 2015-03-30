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