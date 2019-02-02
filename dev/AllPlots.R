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
#
#
#
#
llsr_path <- system.file("extdata", package = "LLSR") 
path <- gsub("/", "\\", capture.output(cat(llsr_path, "data.xlsx", sep = "/")), fixed=TRUE)
path2 <- file.path(llsr_path, "template.xlsx")
llsr_data <- AQSysDB(path2)

dataSET2 <- AQSearch.Binodal(db.uid='3776a5b13e2322c5443e8bbdcef9734e')
AQSysAnima(dataSET2, xlbl = "Ammonium Sulphate", ylbl = "Poly(ethylene glycol) 2000", xmax = 30)

# convert -loop 0 -delay 100 in1.png in2.png out.gif
system('"D:\\Program Files\\ImageMagick-7.0.8-Q16\\convert.exe" *.png image.gif')
file.remove(list.files(pattern=".png"))
# ffmpeg.exe -framerate 3 -i plot_(%d).png -r 15 -f mp4 out2.mp4
