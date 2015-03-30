AQSys.mathDesc <- function(mathDesc){
  switch(mathDesc,
         "merchuk"={
           Fn<-function(CoefSET,XC){
             P1<-CoefSET[1]
             P2<-CoefSET[2]
             P3<-CoefSET[3]
             P1*exp(P2*(XC^(0.5))-P3*(XC^3))
           }
         },
         "murugesan"={
           Fn <- function(CoefSET,XC){
             P1<-CoefSET[1]
             P2<-CoefSET[2]
             P3<-CoefSET[3]
             P1+P2*(XC)^0.5+P3*XC
           }
         },
         AQSys.err("0")
  )
  return(Fn)
}