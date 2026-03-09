library(LLSR)

test_that("AQSys.plot uses xmax for x-axis scale", {
  dataSET <- llsr_data$db.data[6:23, 1:2]
  p <- AQSys.plot(dataSET, xmax = 50, ymax = 30, silent = TRUE)
  # AQSys.plot returns the plot object when silent = TRUE
  expect_s3_class(p, "ggplot")
  x_scale <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x
  # For ggplot2 >= 3.3.0, x_scale has 'range$range' and 'breaks'
  expect_equal(x_scale$range$range, c(0, 50))
  expect_true(all(x_scale$breaks >= 0 & x_scale$breaks <= 50))
})

test_that("AQSysEval returns matching data and plot structures", {
  dataSET <- llsr_data$db.data[6:23, 1:2]
  eval_res <- AQSysEval(dataSET, xlbl = names(dataSET)[1], ylbl = names(dataSET)[2])
  # Single-system call should return a list for 'data' and a ggplot for 'plot'
  expect_true(is.list(eval_res$data))
  expect_s3_class(eval_res$plot, "ggplot")
})

