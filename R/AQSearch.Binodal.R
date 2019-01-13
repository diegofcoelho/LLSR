####################################################################################################################
#' @rdname AQSearch.Binodal
#' @name AQSearch.Binodal
#' @description This function allow the user to search the LLSR database to find any ATPS that matches the used criteria.
#' @export
####################################################################################################################
AQSearch.Binodal <-
  function(db = LLSR::llsr_data,
           db.ph = NULL,
           db.upper = NULL,
           db.lower = NULL,
           db.temp = NULL,
           db.addtl = NULL,
           ...)
    UseMethod("AQSearch.Binodal")
####################################################################################################################
#' @rdname AQSearch.Binodal
#' @title Search function for ATPS Systems data
#' @description This function allow the user to search the package database to find any ATPS that matches the used criteria.
#' @details The function return the systems that matches the criteria submit by the user.
#' @param db A highly structure db containing data from previously analised data. LLSR database is used by default but user may input his own db if formatted properly.
#' @param db.ph A numeric variable containing the pH to be searched within DB.
#' @param db.upper A String variable containing either the CAS, chemical formula or name of the upper phase enriched component..
#' @param db.lower A String variable containing either the CAS, chemical formula or name of the lower phase component.
#' @param db.temp A numeric variable containing the Temperature (in Kelvin) to be searched within DB.
#' @param db.addtl A String variable containing either the CAS, chemical formula or name of the additive component.
#' @param db.uid An Unique md5 hash Identification. User can retrieve data for a specific system if in possesion of its UID.
#' @param ... Additional optional arguments. None are used at present.
#' @method AQSearch.Binodal default
#' @export
#' @return Returns a data.frame containing system's parameters which match searched conditions
#' @examples
#' \dontrun{
#' AQSearch.Binodal(db.upper="Ammonium")
#'}
####################################################################################################################
AQSearch.Binodal.default <-
  function(db = LLSR::llsr_data,
           db.ph = NULL,
           db.upper = NULL,
           db.lower = NULL,
           db.temp = NULL,
           db.addtl = NULL,
           db.uid = NULL,
           ...) {
    # db <- LLSR::llsr_data
    # initialize db.ans
    db.ans <- list()
    # create and initialise a list using the function's parameters
    db.params <-
      c(db.ph, db.upper, db.lower, db.temp, db.addtl, db.uid)
    # if all parameters are null, the search is not valid and it triggers an error (check AQSys.err.R for details)
    if (all(unlist(lapply(db.params, is.null))))
      AQSys.err("6")
    # output variable is initialised with data from db.
    db.grep <- db$db.data
    if (!is.null(db.upper)) {
      db.upper.names <- db$db.cas[grep(db.upper, db$db.cas$CHEM.NAME, ignore.case = TRUE), "CHEM.NAME"]
      db.upper.altNames <- db$db.cas[grep(db.upper, db$db.cas$CHEM.COMMON, ignore.case = TRUE), "CHEM.NAME"]
      db.upper.cas <- db$db.cas[grep(db.upper, db$db.cas$CAS.CODE, ignore.case = TRUE), "CHEM.NAME"]
      #
      db.chem.names <- c(db.upper.names, db.upper.altNames, db.upper.cas)
      db.grep <- db.grep[, matchBNDL2(db.chem.names, db.grep)]
    }
    # search a system that matchs the lower-phase component, if search parameter is not null.
    if (!is.null(db.lower)) {
      db.lower.names <- db$db.cas[grep(db.lower, db$db.cas$CHEM.NAME, ignore.case = TRUE), "CHEM.NAME"]
      db.lower.altNames <- db$db.cas[grep(db.lower, db$db.cas$CHEM.COMMON, ignore.case = TRUE), "CHEM.NAME"]
      db.lower.cas <- db$db.cas[grep(db.lower, db$db.cas$CAS.CODE, ignore.case = TRUE), "CHEM.NAME"]
      #
      db.chem.names <- c(db.lower.names, db.lower.altNames, db.lower.cas)
      db.grep <- db.grep[, matchBNDL2(db.chem.names, db.grep)]
    }
    # search a system that matchs the additive component, if search parameter is not null.
    if (!is.null(db.addtl)) {
      db.addtl.names <- db$db.cas[grep(db.addtl, db$db.cas$CHEM.NAME, ignore.case = TRUE), "CHEM.NAME"]
      db.addtl.altNames <- db$db.cas[grep(db.addtl, db$db.cas$CHEM.COMMON, ignore.case = TRUE), "CHEM.NAME"]
      db.addtl.cas <- db$db.cas[grep(db.addtl, db$db.cas$CAS.CODE, ignore.case = TRUE), "CHEM.NAME"]
      #
      db.chem.names <- c(db.addtl.names, db.addtl.altNames, db.addtl.cas)
      db.grep <- db.grep[, matchBNDL2(db.chem.names, db.grep)]
    }
    # search a system that matchs the system's temperature, if search parameter is not null.
    if (!is.null(db.temp)) {
      db.grep <- db.grep[, matchTpH(db.temp, db.grep, FALSE)]
    }
    # search a system that matchs the system's pH, if search parameter is not null.
    if (!is.null(db.ph)) {
      db.grep <- db.grep[, matchTpH(db.ph, db.grep, TRUE)]
    }
    # search a system that matchs the system's UID, if search parameter is not null.
    if (!is.null(db.uid)) {
      db.grep <- db.grep[, matchBNDL2(db.uid, db.grep)]
    }
    if (ncol(db.grep) != 0) {
      print(paste("Your search had [", ncol(db.grep) / 2, "] results.", sep = ""))
      db.ans <- db.grep
    } else {
      print(paste("Your search had no results.", sep = ""))
    }
    #
    # If search isn't null, evaluate data
    if (length(db.ans) != 0)  {
      invisible(db.ans)
      #
    } else{
      # Triggers an "no results" error
      AQSys.err("5")
    }
    
  }
