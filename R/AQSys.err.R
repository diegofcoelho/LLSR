AQSys.err <- function (err){
  switch(err,
         "0"={
           errmsg<-"The selected Equation doesn't exist."
           stop(errmsg, call. = FALSE)
         },{
           errmsg<-"An Unknown error ocourred."
           stop(errmsg, call. = FALSE)
         }
  )

}