
#' @rdname AQSys.crpt
#' @title Merchuk's Method - Critical Point Calculation
#' @description Merchuk et al. described a very straightforward method to calculate the critical composition of a given Binodal
#' curve and its Tielines.
#' @details Using the binodal data, tieline's Slopes (S), the composition of bottom-rich component in the bottom phase (XB)
#' and an equation which stablish a relatioship between them, this function returns the critical composition of the binodal
#' under study. When used within a iterative function, mrchk.tielines() can be used to obtain TLL and S and therefore
#' calculate the critical composition.
#' @method AQSys crpt
#' @export AQSys.crpt
#' @export
#' @param tldata - A data.frame with two columns containing a set of Tieline's Slopes (S)
#' and its bottom-rich component composition in the bottom phase (XB).
#' @param dataSET - Binodal Experimental data that will be used in the nonlinear fit
#' @param ... Additional optional arguments. None are used at present.
#' @return (XCrit,YCrit) - The function returns Tieline's Critical Point Composition
#' @examples
#' \dontrun{
#' AQSys.crpt(dataSET,tldata)
#' }
AQSys.crpt <- function(dataSET, tldata,...) {
  # Fit dataSET data to Merchuk's equation and store it in Smmry
  Smmry <- summary(merchuk(dataSET))
  # store tieline data into a dataframe variable. It might be a better approach check if
  # user stored it in a dataframe and if not trigger an error.
  tldata <- as.data.frame(tldata)
  # calculate the regression coefficient of a polynomial equation of order 2
  resTLLfit <- lm(S ~ poly(XB, 2, raw = TRUE), data = tldata)
  # solve a system of equations using Merchuk's equation and the
  # polynomial from tieline regression
  fitC <- function(x) {
    F1 = -x[1] + Smmry$coefficients[1] * exp(Smmry$coefficients[2] * x[2] ^ 0.5 - Smmry$coefficients[3] * x[2] ^ 3) * ((Smmry$coefficients[2] / (2 * x[2] ^ 0.5)) - 3 * Smmry$coefficients[3] * x[2] ^ 2)
    F2 = -x[1] + resTLLfit$coefficients[1] + resTLLfit$coefficients[2] * x[2] + resTLLfit$coefficients[3] * x[2] ^ 2
    return(c(F1 = F1,F2 = F2))
  }
  # solve the system of equation for a given set of guess
  (sres <- multiroot(f = fitC, start = c(.1,.1)))
  # store calculated heavy-component concentration at global composition
  XCrit <- sres$root[2]
  # store calculated light-component concentration at global composition
  YCrit <- Smmry$coefficients[1] * exp(Smmry$coefficients[2] * (XCrit ^ (0.5)) - Smmry$coefficients[3] * (XCrit ^ 3))
  # include critical concentration in the output of the system of equations result
  sres$XCrit <- XCrit
  sres$YCrit <- YCrit
  # return all calculated parameters
  sres
}
