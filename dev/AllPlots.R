library(LLSR)
library(XLConnect)
path <- "E:\\Dropbox\\LABAM\\Artigos - ProduC'C#o\\D7 - GitHUB - LLSR\\Dados.xlsx"
path <- "E:\\Dropbox\\Diego\\data.xlsx"
path <- gsub("/", "\\", capture.output(cat(getwd(), "dev/data.xlsx", sep = "/")), fixed=TRUE)

workBook <- loadWorkbook(path)
sheets <- getSheets(workBook)

cllsr_data <- AQSysDB(path)
llsr_data <- AQSysDB(path)

math_model <- "merchuk"

dataSET <- cllsr_data[["db.data"]][, 93:104]
PARS <- cllsr_data[["db.sys"]][["merchuk"]][44:49, ]

subDS <- dataSET[6:nrow(dataSET), ]
subDS <- as.data.frame(sapply(subDS, function(x) as.numeric(sub(",", ".", x))))

SN <- c("PEG2000 278.15K","PEG2000 298.15K", "PEG4000 278.15K","PEG4000 298.15K","PEG6000 278.15K","PEG6000 298.15K")

# ALL PLOTS AND REGRESSION SHOWN INDIVIDUALLY IN AN ORTHOGONAL PLOT
for (i in seq(1, ncol(dataSET)/2)){
  Arr <- LLSRxy(dataSET[6:nrow(dataSET), i*2 -1 ], dataSET[6:nrow(dataSET), i*2], "XY")
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
EvalData <- AQSysEval(dataSET, seriesNames = SN, xlbl = "Ammonium Sulfate", ylbl = "PEG Concentration")
#
setwd("data")
save(llsr_data, file = "llsr_data.rda")
save(peg4kslt, file = "temp_data.rda")
