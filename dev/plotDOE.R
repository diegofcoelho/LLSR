library(ggplot2)

plotIt <- function(DOE, AnalysisData){
  nSys <- length(unique(factor(DOE$System)))
  models_npars <- AQSysList(TRUE)
  data <- AnalysisData
  #
  for (sys in seq(1, nSys)) {
    xmax <- 22.5 #ceiling(max(data[[sys]]$maxTL$X)*1.05)
    xmin <- ceiling(min(data[[sys]]$maxTL$X)*0.95)
    ymax <- ceiling(max(data[[sys]]$maxTL$Y)*1.25)
    x <- seq(0, xmax)
    #
    CoefSET <- data[[sys]]$PARs
    modelName <- data[[sys]]$modelName
    Fn <- ifelse(modelName %in% names(models_npars), LLSR:::AQSys.mathDesc(modelName), AQSys.err("0"))
    rawdt <- data.frame(x, Fn(CoefSET, x))
    #
    names(rawdt) <- c("XC", "YC")
    #
    plot_image <- ggplot(data = rawdt, aes_string(x = "XC", y = "YC")) + 
      geom_line(data = rawdt, aes_string(x = "XC", y = "YC"), color = "black", linetype = 2, size = 1.1) +
      theme_light() + # xlab(paste(xlbl, "(%, m/m)")) + ylab(paste(ylbl, "(%, m/m)")) + 
      theme(
        validate = FALSE,
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        text = element_text(size = 16),
        legend.position = "top",
        axis.title.y = element_text(vjust = 5),
        axis.title.x = element_text(vjust = -2),
        panel.grid.major = element_line(size = .70, colour = "black"),
        panel.grid.minor = element_line(size = .70),
        panel.border = element_rect(size = .5, colour = "white"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.line = element_line(colour = 'black', size = 1.25),
        legend.title = element_text(
          colour = "black",
          size = 12,
          face = "bold",
          angle = 0
        ),
        legend.text = element_text(
          colour = "black",
          size = 12,
          face = "plain"
        )
      ) +
      scale_y_continuous(
        expand = c(0, 0),
        limits = c(-2.5, ymax),
        breaks = seq(0, ymax, by = 5),
        labels = seq(0, ymax, by = 5)
      ) +
      scale_x_continuous(
        expand = c(0, 0),
        limits = c(0, 25),
        breaks = seq(0, 22.5, by = 2.5),
        labels = seq(0, 22.5, by = 2.5)
      ) +
      annotate(
        "point",
        x = data[[sys]]$ConvergencePoint$XC,
        y = data[[sys]]$ConvergencePoint$YC,
        colour = "red",
        bg = "red",
        shape = 23,
        size = 2
      )
    #
    #
    #
    plot_image <- plot_image +
      geom_point(
        data = subset(DOE, DOE$System == sys),
        aes_string(x = "X", y = "Y"),
        colour = "red",
        size = 2,
        alpha = 0.4
      ) +
      geom_line(
        data = subset(DOE, (DOE$System == sys)),
        aes_string(x = "X", y = "Y", group = "TLL"),
        colour = "blue"
      )
    #
    print(plot_image)
  }
}
#data <- AQSysDOE(dataSET, nTL = 3, nPoints = 3)
#data <- AQSysDOE(dataSET, slope = vllsr_data[["db.tielines"]]$TLSlope, nTL = 2, nPoints = 2, tol = 1e-3)
#plotIt(data$DOE, data$data)
