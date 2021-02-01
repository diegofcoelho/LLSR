library(LLSR)
library(rootSolve)
library(dplyr)
library(ggplot2)

source("E:/Projetos/GitHub/LLSR/dev/plotDOE.R")

path <- "E:/Dropbox/Natália Melani/R/data_template.xlsx"

vllsr_data <- AQSysDB(path)
dataSET <- vllsr_data[["db.data"]][, 1:6]
PARS <- vllsr_data[["db.sys"]][["merchuk"]]
SN <- c("PEG4000 300.15K","PEG6000 300.15K", "PEG8000 278.15K")

EvalData <-
  AQSysEval(
    dataSET,
    slope = as.numeric(vllsr_data[["db.tielines"]]$slopes$TLSlope),
    tol = 1e-3,
    silent = FALSE,
    xmax = 40,convrgnceLines = TRUE
  )
DOE <-
  AQSysDOE(
    dataSET,
    slope = as.numeric(vllsr_data[["db.tielines"]]$slopes$TLSlope),
    tol = 1e-3,
    nPoints = 5,
    nTL = 5,
    xmax = 40
  )
plotIt(DOE$DOE, DOE$data)
LLSR:::saveDATA(path, DOE$DOE)
