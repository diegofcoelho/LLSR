AQSys.err <- function (err){
  switch(err,
         "0"={
           errmsg<-"The selected Equation doesn't exist."
           stop(errmsg, call. = FALSE)
         },
         "1"={
           errmsg<-"The path must point to a '.xls' or '.xlsx' Microsoft Excel worksheet."
           stop(errmsg, call. = FALSE)
         },
         "2"={
           errmsg<-"There was a problem when calculating the number of systems to be analysed. Please make sure the data is formatted as the standard provided."
           stop(errmsg, call. = FALSE)
         },
         {
           errmsg<-"An Unknown error ocourred."
           stop(errmsg, call. = FALSE)
         }
  )

}
