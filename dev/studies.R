library(LLSR)
dataSET1 <- AQSearch.Binodal(db.uid='f93e74cf9f0dfd843c129594a63c4c35')
uid1 <- '908bb9e6a0f868201a8c739ac9189e9e'
uid2 <- '71791f6073da25cf25cca7472a0943bb'
uids <- c(uid1, uid2)
dataSET3 <- AQSearch.Tieline(db.uid = uids) 
subSET2 <- dataSET3[1, 12:13] 
Tm <- 4.181
Bm <- 5.184
subSET1 <- na.exclude(dataSET1[6:nrow(dataSET1), ])
AQSys.LevArmRule(subSET1, Xm=subSET2[[1]], Ym=subSET2[[2]], byW=TRUE, WT=Tm, WB=Bm, modelName = "tang")

# eval(parse(text = sprintf(
#   "P%d <- Smmry$coefficients[%d]",
#   seq(1, PARNumber),
#   seq(1, PARNumber)
# )))
sprintf(gsub("\\$2", "%d", sprintf(gsub("\\$1", "%d", FN), seq(1, 3, 2))), seq(2, 4, 2), seq(2, 4, 2))
# ans <- AQSearch.Tieline(db.uid = '908bb9e6a0f868201a8c739ac9189e9e')

dataSET1 <- AQSearch(db.CompA = "Ammonium Sulphate", db.CompB = "Poly(ethylene glycol) 2000", db.Temp = 278.15)
dataSET1_T <- subset(dataSET1$TieLines, REF.MD5=="6503fe74290ee9b972a39030a6b99a89")
dataSET1_B <- dataSET1$Binodal
subSET1 <- na.exclude(dataSET1_B[6:nrow(dataSET1_B),])

dataSET2 <- AQSearch(db.CompA = "Ammonium Sulphate", db.CompB = "Poly(ethylene glycol) 2000", db.Temp = 298.15)
dataSET2_T <- subset(dataSET2$TieLines, REF.MD5=="6503fe74290ee9b972a39030a6b99a89")
dataSET2_B <- dataSET2$Binodal[, 3:4]
subSET2 <- na.exclude(dataSET2_B[6:nrow(dataSET2_B),])

dataSET3 <- AQSearch(db.CompA = "Ammonium Sulphate", db.CompB = "Poly(ethylene glycol) 4000", db.Temp = 278.15)
dataSET3_T <- subset(dataSET3$TieLines, REF.MD5=="6503fe74290ee9b972a39030a6b99a89")
dataSET3_B <- dataSET3$Binodal
subSET3 <- na.exclude(dataSET3_B[6:nrow(dataSET3_B),])

dataSET4 <- AQSearch(db.CompA = "Ammonium Sulphate", db.CompB = "Poly(ethylene glycol) 4000", db.Temp = 298.15)
dataSET4_T <- subset(dataSET4$TieLines, REF.MD5=="6503fe74290ee9b972a39030a6b99a89")
dataSET4_B <- dataSET4$Binodal
subSET4 <- na.exclude(dataSET4_B[6:nrow(dataSET4_B),])

dataSET5 <- AQSearch(db.CompA = "Ammonium Sulphate", db.CompB = "Poly(ethylene glycol) 6000", db.Temp = 278.15)
dataSET5_T <- subset(dataSET5$TieLines, REF.MD5=="6503fe74290ee9b972a39030a6b99a89")
dataSET5_B <- dataSET5$Binodal
subSET5 <- na.exclude(dataSET5_B[6:nrow(dataSET5_B),])

dataSET6 <- AQSearch(db.CompA = "Ammonium Sulphate", db.CompB = "Poly(ethylene glycol) 6000", db.Temp = 298.15)
dataSET6_T <- subset(dataSET6$TieLines, REF.MD5=="6503fe74290ee9b972a39030a6b99a89")
dataSET6_B <- dataSET6$Binodal
subSET6 <- na.exclude(dataSET6_B[6:nrow(dataSET6_B),])
#
dataSET1 <- AQSearch.Binodal(db.uid = "f93e74cf9f0dfd843c129594a63c4c35")
subSET1 <- na.exclude(dataSET1[6:nrow(dataSET1),])
AQSys.CritPoint(subSET1, dataSET4, method="polynomial", xlbl = "Ammonium Sulphate", ylbl = "Poly(ethylene glycol) 2000")


AQSys.CritPoint(dataSET1, dataSET1_T, method = "numerical")
AQSys.CritPoint(subSET1, dataSET1_T, method = "polynomial")
AQSys.CritPoint(subSET1, dataSET4, method="algebraic")

suppressWarnings(AQSys.CritPoint(dataSET1_B, dataSET1_T, method = "polynomial", ext=TRUE))


crit_point_seq(dataSET1, ext = TRUE, xmax = 35, xlbl = "Ammonium Sulphate", ylbl = "Poly(ethylene glycol) 2000")
crit_point_eqsys(dataSET, ext = TRUE, xmax = 35, xlbl = "Ammonium Sulphate", ylbl = "Poly(ethylene glycol) 2000")


findPoint <- function(dataSET, Point, order, modelName = "merchuk", xMAX = 35){
  options(digits = 14)
  library(LLSR)
  library(rootSolve)
  if ((order == "xy")) {
    X <- unlist(unname(Point[1]))
    Y <- unlist(unname(Point[2]))
  } else {
    X <- unlist(unname(Point[2]))
    Y <- unlist(unname(Point[1]))
  }
  #
  models_npars <- AQSysList(TRUE)
  # Analyse data and return parameters
  model_pars <- summary(AQSys(dataSET,  modelName))$parameters[, 1]
  # Select Model based on the user choice or standard value
  Fn <- ifelse(
    modelName %in% names(models_npars),
    LLSR:::AQSys.mathDesc(modelName),
    LLSR:::AQSys.err("0")
  )
  # define a straight line EQUATION
  Gn <- function (yMin, AngCoeff, xMAX, x) {
    yMin + AngCoeff * (x - xMAX)
  }
  # Add constant variable values to the equations
  modelFn <- function(x) Fn(model_pars, x)
  modelTl <- function(x) Gn(Y, unlist(unname(Point[3])), X, x)
  #
  Xs <- uniroot.all(function(x)(modelFn(x) - modelTl(x)), c(0, xMAX), tol = 0.1) 
  Ys <- Fn(model_pars, Xs)
  c(Ys[1], Xs[1], Ys[2], Xs[2])
}



suppressWarnings(findPoint(dataSET1_B, data.frame(16.02276477346520,	10.00904670562970, -2.05757696045200), "yx"))
suppressWarnings(findPoint(dataSET1_B, data.frame(15.98103255758110,	12.00958797750790,	-2.03532667339990), "yx"))
suppressWarnings(findPoint(dataSET1_B, data.frame(9.99773010311342,	17.97289938932830,	-2.04646524063654), "yx"))
suppressWarnings(findPoint(dataSET1_B, data.frame(17.99267334669340,	12.00227865731460,	 -2.0408964989483), "yx"))


suppressWarnings(findPoint(dataSET3_B, data.frame(25.03840483982350,	6.98763042803815,	-2.10544193418820), "yx"))
suppressWarnings(findPoint(dataSET3_B, data.frame(15.99016792333800,	10.00073297485660,	-2.32451793497850), "yx"))
suppressWarnings(findPoint(dataSET3_B, data.frame(18.04997430077100,	9.97139635810926,	-2.23018461632230), "yx"))
suppressWarnings(findPoint(dataSET3_B, data.frame(15.01013918169560,	14.98589498925910,	-2.04892369831580), "yx"))

  
suppressWarnings(findPoint(dataSET4_B, data.frame(13.05149386288790,	12.98855802814090,	-2.41086111002750), "yx"))
suppressWarnings(findPoint(dataSET4_B, data.frame(20.00976660682230,	14.98066527029720,	-2.04223985025280), "yx"))
suppressWarnings(findPoint(dataSET4_B, data.frame(10.00644256588760,	11.94301143709600,	-2.47860149150490), "yx"))
suppressWarnings(findPoint(dataSET4_B, data.frame(15.02139820359280,	15.00010578842320,	-2.22700756600180), "yx"))
suppressWarnings(findPoint(dataSET4_B, data.frame(8.96354916067146,	10.99626199040770,	-2.77678935974050), "yx"))


suppressWarnings(findPoint(dataSET5_B, data.frame(19.59750691027800,	13.45983800306440,	-1.45657400413320), "yx"))
suppressWarnings(findPoint(dataSET5_B, data.frame(16.27424820978620,	9.99048323324071,	-1.86426119658780), "yx"))
suppressWarnings(findPoint(dataSET5_B, data.frame(10.19308319738990,	12.94556479528910,	-1.88389341956190), "yx"))
suppressWarnings(findPoint(dataSET5_B, data.frame(18.25103754082150,	10.00020952552160,	-2.17760418030860), "yx"))
suppressWarnings(findPoint(dataSET5_B, data.frame(18.21453363549620,	11.94307808524170,	-1.76811302175690), "yx"))


suppressWarnings(findPoint(dataSET6_B, data.frame(24.38431396045540,	12.99414519672460,	-1.48521150183440), "yx"))
suppressWarnings(findPoint(dataSET6_B, data.frame(20.31466426858510,	12.01307553956830,	-1.86426119658780), "yx"))
suppressWarnings(findPoint(dataSET6_B, data.frame(13.60345850813470,	11.30003683618130,	-2.14861757927065), "yx"))
suppressWarnings(findPoint(dataSET6_B, data.frame(10.99270625747310,	9.98645077720207,	-2.43297396195350), "yx"))
suppressWarnings(findPoint(dataSET6_B, data.frame(29.45935638616830,	14.01805616630020,	-1.72549932338050), "yx"))
