options(digits=14)
require(XLConnect)
#
#' @import XLConnect
#
#' @rdname AQSysDB
#' @name AQSysDB
#' @title AQSysDB
#' @export
#' @param path 1
#' @param order 2
AQSysDB <- function(path, order = "xy"){
  if (grepl(".xlsx",path) | grepl(".xls", path)){
    #
    wrbk <- loadWorkbook(path)
    sheets <- getSheets(wrbk)
    nSh <- length(sheets)  
    wsdt <- readWorksheet(wrbk, 1, header = FALSE)
    #
    if (is.odd(ncol(wsdt))) AQSys.err("2")
    #
    nSys <- ncol(wsdt)/2
    #
    llsrdb <- data.frame()
    db.info <- 1
    db.data <- 6 
    #
  } else {
    AQSys.err("1")
  }
  #
  cat('\014')
  cat(paste("Analysing ",nSys," systems. \n\n", collapse = NULL))
  #
  for (i in AQSys.List()){
    #
    cat(i,": ", sep = "")
    #
    db.n.row <- nrow(llsrdb)
    if(db.n.row == 0){
      db.first.row <- 1
      db.last.row <- nSys
    }else{
      db.first.row <- db.n.row + 1
      db.last.row <- db.n.row + nSys
    }
    #
    for (j in db.first.row:db.last.row){
      #
      c1 <- 2*(j - db.n.row) - 1
      c2 <- c1 + 1
      lSys<-length(wsdt[,c1])
      #
      llsrdb[j,1] <- wsdt[db.info + 3,c1]
      llsrdb[j,2] <- wsdt[db.info + 2,c1]
      llsrdb[j,3] <- wsdt[db.info + 2,c2]
      llsrdb[j,4] <- wsdt[db.info,c1]
      llsrdb[j,5] <- wsdt[db.info,c2]
      llsrdb[j,6] <- wsdt[db.info + 1,c2]
      llsrdb[j,7] <- wsdt[db.info + 1,c1]
      #
      rawSys <- wsdt[db.data:lSys, c1:c2]
      #
      db.Sys <- as.data.frame(na.exclude(rawSys))
      #
      if (is.numeric(db.Sys[1,1])){
      } else{
        db.first.col <- as.numeric(sub(",", ".", db.Sys[,1], fixed = TRUE))
        db.second.col <- as.numeric(sub(",", ".", db.Sys[,2], fixed = TRUE))
      }
      #
      if (order=="xy"){
        resSys<-summary(AQSys(LLSRxy(db.first.col,db.second.col),mathDesc=i))
      }
      else{
        resSys<-summary(AQSys(LLSRxy(db.second.col,db.first.col),mathDesc=i))
      }
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
      llsrdb[j,20] <- length(db.first.col)
      llsrdb[j,21] <- i
    }
    #
    cat("[OK] \n")
    #
  }
  #
  names(llsrdb) <- c("REF", "UpperRich", "BottomRich", "pHSys", "addSys",
                     "addSysC", "tSys","P1", "P2", "P3",  "ResStdErr",
                     "SSR",	"P1_StdErr",	"P2_StdErr",	"P3_StdErr",
                    "P1_tValue",	"P2_tValue",	"P3_tValue", "AchConvTol",
                    "nPoints", "mathDesc")
  invisible(llsrdb)
}



