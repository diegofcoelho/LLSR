library(LLSR)
library(ggplot2)

dataSET2 <- AQSearch.Binodal(db.uid='3776a5b13e2322c5443e8bbdcef9734e')
dataSET1 <- AQSearch.Binodal(db.uid='f93e74cf9f0dfd843c129594a63c4c35')

ans <- AQSys(dataSET1[6:100, ])
ans <- AQSys(dataSET2[6:100, ])
dataSET <- dataSET2[6:100, ]
dataSETB <- dataSET1[6:100, ]

Xs <- as.numeric(na.exclude(dataSET[, 1]))
Ys <- as.numeric(na.exclude(dataSET[, 2]))
PARs <- summary(ans)$coefficients[, 1]
P1 <- PARs["P1"]
P2 <- PARs["P2"]
P3 <- PARs["P3"]


eval1 <- AQSysEval(dataSET1)
eval1$data$CriticalPoint
eval2 <- AQSysEval(dataSET2)
eval2$data$CriticalPoint

D1 <- function(x){P1*exp(P2*sqrt(x) - P3*x^3)*(P2/(2*sqrt(x)) - 3*P3*x^2)}
xy1 <- data.frame(c(Xs), c(D1(Xs)))
D2 <- function(x){P1*(exp(P2*sqrt(x) - P3*x^3)*(-P2/(4*x^(3/2)) - 6*P3*x) + exp(P2*sqrt(x) - P3*x^3)*(P2/(2*sqrt(x)) - 3*P3*x^2)^2)}
xy2 <- data.frame(c(Xs), c(D2(Xs)))

block <- setNames(data.frame(Xs, Ys, D1(Xs), D2(Xs)), c("x", "y1", "y2", "y3"))

names(xy1) <- c("x", "y2")
names(xy2) <- c("x", "y3")

ggplot(block, aes(x)) + 
  geom_point(aes(y = y1, colour = "y1")) + 
  geom_point(aes(y = y2, colour = "y2")) + 
  geom_point(aes(y = y3, colour = "y3"))
#
ggplot(xy2, aes(x,y3)) + geom_line()

d <- 10^-100
m <- AQSearch.Slope(db.CompA = "Ammonium Sulphate", db.CompB = "Poly(ethylene glycol) 2000", db.Temp = 278.15)$TLSlope
m <- AQSearch.Slope(db.CompA = "Ammonium Sulphate", db.CompB = "Poly(ethylene glycol) 2000", db.Temp = 298.15)$TLSlope

#
sys <- function(x) {
  F1 <- P1 * exp(P2 * x[2] ^ 0.5 - P3 * x[2] ^ 3) - x[1]
  F2 <- P1 * exp(P2 * x[4] ^ 0.5 - P3 * x[4] ^ 3) - x[3]
  F3 <- (((0 - x[4]) ^ 2) + ((0 - x[3]) ^ 2)) - (((x[2] - 0) ^ 2) + ((x[1] - 0) ^ 2))
  F4 <- m * (x[2] - x[4]) - (x[1] - x[3])
  # F5 <- sqrt(((x[2] - x[4]) ^ 2) + ((x[1] - x[3]) ^ 2)) - d
  
  c(F1 = F1,
    F2 = F2,
    F3 = F3,
    F4 = F4)
}
#
(sysres <- multiroot(
  f = sys,
  start = c(10, 10, 10^-10, 30),
  positive = TRUE
))
#
ggplot(block, aes(x,y1)) + geom_point() + 
  annotate(
    "point",
    x = sysres$root[2],
    y = sysres$root[1],
    colour = "black",
    bg = "gold",
    shape = 23,
    size = 2
  )


#############################################################################################################################################
EqSys <- function(x) {
  F1 <- P2 * x[2] ^ 0.5 - P3 * x[2] ^ 3 - log(x[1]/P1)
  F2 <- m * (x[2] - x[4]) - (x[1] - x[3])
  F3 <- ((x[2] - x[4]) ^ 2) + ((x[1] - x[3]) ^ 2) - d^2
  F4 <- (((0 - x[4]) ^ 2) + ((0 - x[3]) ^ 2)) - (((x[2] - 0) ^ 2) + ((x[1] - 0) ^ 2))
  #
  return(c(F1 = F1,
           F2 = F2,
           F3 = F3,
           F4 = F4))
}
#
sysres <- nleqslv(c(10, 10, 10 ^ -10, 30), EqSys)$x
#
ggplot(block, aes(x, y1)) + geom_point() +
  annotate(
    "point",
    x = sysres[2],
    y = sysres[1],
    colour = "black",
    bg = "gold",
    shape = 23,
    size = 2
  )
#############################################################################################################################################

