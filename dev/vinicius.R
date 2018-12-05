library(LLSR)
library(XLConnect)
path <- "E:\\Dropbox\\LABAM\\Vinicius\\Mestrado\\data_template.xlsx"

vllsr_data <- AQSysDB(path)
dataSET <- vllsr_data[["db.data"]][, 1:6]
PARS <- vllsr_data[["db.sys"]][["merchuk"]]
SN <- c("PEG4000 300.15K","PEG6000 300.15K", "PEG8000 278.15K")
EvalData <- AQSysEval(dataSET, slope = vllsr_data[["db.tielines"]]$TLSlope, tol = 1e-3)
DOE <- AQSysDOE(dataSET, slope = vllsr_data[["db.tielines"]]$TLSlope, tol = 1e-3, nPoints = 2, nTL = 2)
plotIt(DOE$DOE, DOE$data)
LLSR:::saveDATA(path, DOE$DOE)


