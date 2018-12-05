library(LLSR)
library(XLConnect)
path <- "E:\\Dropbox\\LABAM\\Artigos - Produção\\D7 - GitHUB - LLSR\\Dados.xlsx"
path <- "E:\\Dropbox\\Diego\\data.xlsx"


workBook <- loadWorkbook(path)
sheets <- getSheets(workBook)

llsr_data <- AQSysDB(path)

math_model <- "merchuk"

dataSET <- cllsr_data[["db.data"]][, 93:104]
PARS <- cllsr_data[["db.sys"]][["merchuk"]][44:49,]

subDS <- MANUSCRIPT[6:nrow(MANUSCRIPT),]
subDS <- as.data.frame(sapply(subDS, function(x) as.numeric(sub(",", ".", x))))

SN <- c("PEG2000 278.15K","PEG2000 298.15K", "PEG4000 278.15K","PEG4000 298.15K","PEG6000 278.15K","PEG6000 298.15K")

# ALL PLOTS AND REGRESSION SHOWN INDIVIDUALLY IN AN ORTHOGONAL PLOT
for (i in seq(1, ncol(MANUSCRIPT)/2)){
  Arr <- LLSRxy(MANUSCRIPT[6:nrow(MANUSCRIPT), i*2 -1 ], MANUSCRIPT[6:nrow(MANUSCRIPT), i*2], "XY")
  names(Arr) <- c("Ammonium Sulphate", paste("Poly(ethylene Glycol)", ceiling(i / 2) * 2000))
  AQSys.plot(Arr)
}
# ALL experimental data plotted altogether
AQSysPlot(subDS, "Ammonium Sulfate", "PEG Concentration", SN)
AQSysPlot(subDS)
#
#
#
AQSysCurve("merchuk", PARS[1, 8:10], seriesNames = c("PEG2000 278.15"), xlbl = "Ammonium Sulfate", ylbl = "PEG Concentration")
AQSysCurve("merchuk", PARS[, 8:10], seriesNames = SN, xlbl = "Ammonium Sulfate", ylbl = "PEG Concentration")
#
#
EvalData <- AQSysEval(MANUSCRIPT, seriesNames = SN, xlbl = "Ammonium Sulfate", ylbl = "PEG Concentration")
#
setwd("data")
save(llsr_data, file = "llsr_data.rda")
save(peg4kslt, file = "temp_data.rda")
