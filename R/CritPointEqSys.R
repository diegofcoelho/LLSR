####################################################################################################################
options(digits = 14)
####################################################################################################################
#' @import ggplot2
#' @import nleqslv
#' @import rootSolve
####################################################################################################################
crit_point_eqsys <- function(dataSET,
                            modelName,
                            xmax,
                            xlbl,
                            ylbl,
                            Order,
                            ext) {
  ###
  dataSET <- toNumeric(dataSET, Order)
  PARs <- summary(AQSys(dataSET))$coefficients[, 1]
  BnFn <- mathDescPair(modelName)
  d <- 10^-100
  ###
  EqSys <- function(x) {
    F1 <- eval(parse(text = gsub("[$]", "", BnFn)))
    F2 <- (x[2] - x[4]) - (x[1] - x[3]) - d
    F3 <- ((x[2] - x[4]) ^ 2) + ((x[1] - x[3]) ^ 2) - d^2
    F4 <- (((0 - x[4]) ^ 2) + ((0 - x[3]) ^ 2)) - (((x[2] - 0) ^ 2) + ((x[1] - 0) ^ 2))
    #
    return(c(F1 = F1,
             F2 = F2,
             F3 = F3,
             F4 = F4))
  }
  #
  sysres <- nleqslv(c(10, 10, 10 ^ -10, 10), EqSys)$x
  OUTPUT <- setNames(sysres[1:2], c("YC", "XC"))
  #
  #
  #
  if (ext) {
    #
    OUTPUT_PLOT <- AQSys.plot(dataSET = dataSET,
                         silent = TRUE,
                         xmax = xmax,
                         xlbl = xlbl,
                         ylbl = ylbl) + annotate(
                           "point",
                           x = OUTPUT[2],
                           y = OUTPUT[1],
                           colour = "black",
                           bg = "gold",
                           shape = 23,
                           size = 2
                         )
    return(list(CriticalPoint=OUTPUT[2:1], Plot=OUTPUT_PLOT))
  }
  return(OUTPUT[2:1])
}
####################################################################################################################
