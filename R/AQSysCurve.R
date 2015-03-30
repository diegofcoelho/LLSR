#' @rdname AQSysCurve
#' @title This functions plot a curve based in the chosen model and its parameters.
#' @description The function returns a plot after using the parameters and model given by the user.
#' @details The function owns predefined set of equations that can be seen below and must be used, with adequated parameters,
#' to return a plot which represent the chosen model.
#' @export AQSysCurve
#' @param mathDesc - Equation to be used: merchuk, murugesan [type:string]
#' @param param - Model's parameters [type::data.frame]
#' @param xlbl - Plot's Horizontal axis label. 
#' @param ylbl - Plot's Vertical axis label.
#' @param main - Legacy from plot package. For more details, see \code{\link{plot.default}}
#' @param col - Legacy from plot package. For more details, see \code{\link{plot.default}}
#' @param type - Legacy from plot package. For more details, see \code{\link{plot.default}}
#' @param cex - Legacy from plot package. For more details, see \code{\link{plot.default}}
#' @param cexlab - Legacy from plot package. For more details, see \code{\link{plot.default}}
#' @param cexaxis - Legacy from plot package. For more details, see \code{\link{plot.default}}
#' @param cexmain - Legacy from plot package. For more details, see \code{\link{plot.default}}
#' @param cexsub - Legacy from plot package. For more details, see \code{\link{plot.default}}
#' @param xmax - Maximum value for the Horizontal axis' value (bottom-rich component)  [type:double]
#' @param mpl -Multiples curves overlayed in a single plot. Default is FALSE. [type::LOGIC]
#' @param HR - Magnify Plot's text to be compatible with High Resolution size [type:Boulean]
#' @param NP - Number of points used to build the fitted curve. Default is 100. [type:Integer]
#' @return A plot using the input model within the chosen interval and the curve's raw XY data. If no interval is selected, xmax=0.4.
#' @examples
#' \dontrun{
#' AQSysCurve("murugesan",as.data.frame(c(0.90,-3.48,2.92)),mpl=TRUE,col="red")
#' }
AQSysCurve <- function  (mathDesc, param, xlbl = "", ylbl = "", main = NULL, col = "black", type = "p",
                     cex = 1, cexlab = 1, cexaxis = 1, cexmain = 1, cexsub = 1, xmax = 0.4, mpl = FALSE, HR = FALSE, NP = 100)
{
  #
  AQSysHR(TRUE)
  #
  param<-as.double(unlist(param))
  #
  x<-sort(runif(NP,0.001,xmax))
  #
  switch(mathDesc,
         merchuk={
           Fn <- AQSys.mathDesc("merchuk")
         },
         murugesan={
           Fn <- AQSys.mathDesc("murugesan")
         },
         AQSys.err("0")
  )
  #
  rawdt <- curve( Fn(param, x), col = col, add = mpl,
                 xlim = c(0, xmax), xlab = xlbl, ylab = ylbl)
  #
  names(rawdt)<-c("XC","YC")
  rawdt<-as.data.frame(rawdt)
  invisible(rawdt)
}