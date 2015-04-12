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
         {
           errmsg<-"An Unknown error ocourred."
           stop(errmsg, call. = FALSE)
         }
  )

}