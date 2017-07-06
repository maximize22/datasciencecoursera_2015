count <- function(cause="default") {
     if(cause=="default") {
          stop("no cause specified")
     }
     
     setwd("c:/devl/data-analysis/pa3")
     homicides <- readLines("homicides.txt")
     
     if(!cause %in% c("asphyxiation", "blunt force", "other", "shooting",
          "stabbing", "unknown")) {
               stop("bad cause specified")
     }
     if(cause=="asphyxiation") {
          cause.ct <- length(grep("[cC]ause: [Aa]sphyxiation", homicides))
     }
     if(cause=="blunt force") {
          cause.ct <- length(grep("[cC]ause: [Bb]lunt [Ff]orce", homicides))
     }
     if(cause=="other") {
          cause.ct <- length(grep("[cC]ause: [Oo]ther", homicides))
     }
     if(cause=="shooting") {
          cause.ct <- length(grep("[cC]ause: [Ss]hooting", homicides))
     }
     if(cause=="stabbing") {
          cause.ct <- length(grep("[cC]ause: [Ss]tabbing", homicides))
     }
     if(cause=="unknown") {
          cause.ct <- length(grep("[cC]ause: [Uu]nknown", homicides))
     }
     
     return(cause.ct)
}