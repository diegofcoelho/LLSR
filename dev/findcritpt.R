
P1 <- 95.1049705911
P2 <- -0.4792224785
P3 <- 0.0002190497
g <- deriv( ~ P1 * exp(P2 * x ^ 0.5 - P3 * x ^ 3) - x, "x", func = TRUE)
uniroot(function(x) (-1.936187 - g(x)), c(0, 45), lower = 0.1, upper = 45, tol = .Machine$double.eps^0.25, maxiter = 1000)

