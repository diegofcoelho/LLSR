#setwd("/Users/dfcoelho/R HUB/db")
#rfile <<- "/Users/dfcoelho/R HUB/db/othersDATA.xlsx"
#
#"C:/Users/dfcoelho/Downloads/concept.xls"
#
options(digits=14)
require(XLConnect)
#
#' @import XLConnect
#
#' @rdname AQSysDB
#' @name AQSysDB
#' @title AQSysDB
#' @export
AQSysDB <- function(path,order="xy"){
  if (grepl(".xlsx",path) | grepl(".xls",path)){
    #
    wrbk <-loadWorkbook(path)
    sheets <- getSheets(wrbk)
    nSh <-length(sheets)  
    wsdt <-readWorksheet(wrbk,1,header = FALSE)
    #
    nSys <- as.numeric(wsdt[1,1])
    llsrdb <- data.frame()
    infodb <- data.frame()
    rowINFO <-3
    rowSYS <- 7 
    #
  } else {
    AQSys.err("1")
  }
  #
  for (i in AQSys.List()){
    #
    cat(i,": ")
    cat(paste("Analysing ",nSys," systems. ", collapse=NULL))
    #
    db.n.row <- nrow(llsrdb)
    if(db.n.row==0){
      db.first.row <- 1
      db.last.row <- nSys
    }else{
      db.first.row <- db.n.row+1
      db.last.row <- db.n.row+nSys
    }
    #
    for (j in db.first.row:db.last.row){
      #
      c1<-2*(j - db.n.row)-1
      c2<-c1+1
      lSys<-length(wsdt[,c1])
      #
      llsrdb[j,1] <- "REF"
      llsrdb[j,2] <- wsdt[5,c1]
      llsrdb[j,3] <- wsdt[5,c2]
      llsrdb[j,4] <- wsdt[3,c1]
      llsrdb[j,5] <- wsdt[3,c2]
      llsrdb[j,6] <- wsdt[4,c2]
      llsrdb[j,7] <- wsdt[4,c1]
      #
      rawSys<-wsdt[rowSYS:lSys,c1:c2]
      #
      dataSys <- as.data.frame(na.exclude(rawSys))
      #
      if (is.numeric(dataSys[1,1])){
      } else{
        dataSys[,1] <- as.numeric(sub(",", ".", dataSys[,1], fixed = TRUE))
        dataSys[,2] <- as.numeric(sub(",", ".", dataSys[,2], fixed = TRUE))
      }
      #
      if (order=="xy") resSys<-summary(AQSys(dataSys,mathDesc=i))
      else resSys<-summary(AQSys(dataSys[2:1],mathDesc=i))
      #
      llsrdb[j,8] <- resSys$parameters[1,1]
      llsrdb[j,9] <- resSys$parameters[2,1]
      llsrdb[j,10] <- resSys$parameters[3,1]
      llsrdb[j,11] <- resSys$sigma
      llsrdb[j,12] <- sum(resSys$residuals^2)
      llsrdb[j,13] <- resSys$parameters[1,2]
      llsrdb[j,14] <- resSys$parameters[2,2]
      llsrdb[j,15] <- resSys$parameters[3,2]
      llsrdb[j,16] <- resSys$parameters[1,3]
      llsrdb[j,17] <- resSys$parameters[2,3]
      llsrdb[j,18] <- resSys$parameters[3,3]
      llsrdb[j,19] <- resSys$convInfo$finTol
      llsrdb[j,20] <- length(dataSys[,1])
      llsrdb[j,21] <- i
    }
    #
    cat("[OK] \n")
    #
  }
  #
  #rSqrd
  names(llsrdb) <- c("REF", "UpperRich", "BottomRich", "pHSys", "addSys",
                     "addSysC", "tSys","P1", "P2", "P3",  "ResStdErr",
                     "SSR",	"P1_StdErr",	"P2_StdErr",	"P3_StdErr",
                    "P1_tValue",	"P2_tValue",	"P3_tValue", "AchConvTol",
                    "nPoints", "mathDesc")
  invisible(llsrdb)
}



