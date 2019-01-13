####################################################################################################################
#' @rdname AQSearch.Tieline
#' @name AQSearch.Tieline
#' @description This function allow the user to search the LLSR database to find any ATPS that matches the used criteria.
#' @export
####################################################################################################################
AQSearch.Tieline <-
  function(db = LLSR::llsr_data,
           db.ph = NULL,
           db.CompA = NULL,
           db.CompB = NULL,
           db.Temp = NULL,
           db.CompC = NULL,
           db.uid = NULL,
           ...)
    UseMethod("AQSearch.Tieline")
####################################################################################################################
#' @rdname AQSearch.Tieline
#' @title Search function for ATPS Systems data
#' @description This function allow the user to search the package database to find any ATPS that matches the used criteria.
#' @details The function return the systems that matches the criteria submit by the user.
#' @param db A highly structure db containing data from previously analised data. LLSR database is used by default but user may input his own db if formatted properly.
#' @param db.CompA A String variable containing either the CAS, chemical formula or name of the upper phase enriched component..
#' @param db.CompB A String variable containing either the CAS, chemical formula or name of the lower phase component.
#' @param db.CompC A String variable containing either the CAS, chemical formula or name of the additive component.
#' @param db.Temp A numeric variable containing the Temperature (in Kelvin) to be searched within DB.
#' @param db.ph A numeric variable containing the pH to be searched within DB.
#' @param db.uid An Unique md5 hash Identification. User can retrieve data for a specific system if in possesion of its UID.
#' @param ... Additional optional arguments. None are used at present.
#' @method AQSearch.Tieline default
#' @export
#' @return Returns a data.frame containing system's parameters which match searched conditions
#' @examples
#' \dontrun{
#' AQSearch.Tieline(db.CompA="Ammonium")
#'}
####################################################################################################################
AQSearch.Tieline.default <-
  function(db = LLSR::llsr_data,
           db.ph = NULL,
           db.CompA = NULL,
           db.CompB = NULL,
           db.Temp = NULL,
           db.CompC = NULL,
           db.uid = NULL,
           ...) {
    cat("  [Tieline]\n")
    # initialize db.ans
    db.ans <- list()
    # create and initialise a list using the function's parameters
    db.params <- c(db.ph, db.CompA, db.CompB, db.Temp, db.CompC, db.uid)
    # if all parameters are null, the search is not valid and it triggers an error (check AQSys.err.R for details)
    if (all(unlist(lapply(db.params, is.null))))
      AQSys.err("6")
    # output variable is initialised with data from db.
    db.grep <- db$db.tielines$data
    if (!is.null(db.CompA)) {
      db.CompA.names <- db$db.cas[grep(db.CompA, db$db.cas$CHEM.NAME, ignore.case = TRUE), "CHEM.NAME"]
      db.CompA.altNames <- db$db.cas[grep(db.CompA, db$db.cas$CHEM.COMMON, ignore.case = TRUE), "CHEM.NAME"]
      db.CompA.cas <- db$db.cas[grep(db.CompA, db$db.cas$CAS.CODE, ignore.case = TRUE), "CHEM.NAME"]
      #
      db.chem.names <- c(db.CompA.names, db.CompA.altNames, db.CompA.cas)
      db.grep <- db.grep[which(db.chem.names == db.grep, TRUE)[, "row"],]
    }
    # search a system that matchs the lower-phase component, if search parameter is not null.
    if (!is.null(db.CompB)) {
      db.CompB.names <- db$db.cas[grep(db.CompB, db$db.cas$CHEM.NAME, ignore.case = TRUE), "CHEM.NAME"]
      db.CompB.altNames <- db$db.cas[grep(db.CompB, db$db.cas$CHEM.COMMON, ignore.case = TRUE), "CHEM.NAME"]
      db.CompB.cas <- db$db.cas[grep(db.CompB, db$db.cas$CAS.CODE, ignore.case = TRUE), "CHEM.NAME"]
      #
      db.chem.names <- c(db.CompB.names, db.CompB.altNames, db.CompB.cas)
      db.grep <- db.grep[which(db.chem.names == db.grep, TRUE)[, "row"],]
    }
    # search a system that matchs the additive component, if search parameter is not null.
    if (!is.null(db.CompC)) {
      db.CompC.names <- db$db.cas[grep(db.CompC, db$db.cas$CHEM.NAME, ignore.case = TRUE), "CHEM.NAME"]
      db.CompC.altNames <- db$db.cas[grep(db.CompC, db$db.cas$CHEM.COMMON, ignore.case = TRUE), "CHEM.NAME"]
      db.CompC.cas <- db$db.cas[grep(db.CompC, db$db.cas$CAS.CODE, ignore.case = TRUE), "CHEM.NAME"]
      #
      db.chem.names <- c(db.CompC.names, db.CompC.altNames, db.CompC.cas)
      db.grep <- db.grep[which(db.chem.names == db.grep, TRUE)[, "row"],]
    }
    # search a system that matchs the system's temperature, if search parameter is not null.
    if (!is.null(db.Temp)) {
      db.grep <- db.grep[db.grep$TEMP == db.Temp, ]
    }
    # search a system that matchs the system's pH, if search parameter is not null.
    if (!is.null(db.ph)) {
      db.grep <- db.grep[db.grep$PH == db.ph, ]
    }
    # search a system that matchs the system's UID, if search parameter is not null.
    if (!is.null(db.uid)) {
      db.grep <- db.grep[matchUID(db.uid, db.grep),]
    }
    #
    if (ncol(db.grep) != 0) {
      cat(paste("    Your search had [", nrow(db.grep), "] results.", "\n",sep = ""))
      db.ans[["TieLines"]] <- db.grep
      db.ans[["Slopes"]] <- matchSlope(db.grep$UID, db)
      invisible(db.ans)
    } else {
      # Triggers an "no results" error
      AQSys.err("5")
      invisible(NULL)
    }
  }
