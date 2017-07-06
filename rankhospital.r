setwd("c:/devl/data-analysis/pa3/")


best <- function(state="AL", outcome.name="best"){
     
     outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     outcome[,23] <- as.numeric(outcome[,23])
     df <- subset(outcome, outcome$State == state) 
     if(nrow(df)==0) stop("invalid state")
     if(!outcome.name %in% c('heart attack', 'heart failure', 'pneumonia')) stop("invalid outcome")
     if(outcome.name == 'heart attack')
          dff <- df[order(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, df$Hospital.Name),] 
     if(outcome.name == 'heart failure')
          dff <- df[order(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, df$Hospital.Name),] 
     if(outcome.name == 'pneumonia')
          dff <- df[order(df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, df$Hospital.Name),] 
     return(dff[1,2])     
}

rankhospital <- function(state="AL", outcome.name="best", rank="1"){
     
     sort.pos <- TRUE
     if(rank=="best") rank <- as.numeric(1)
          else if(rank=="worst") {
                    rank <- as.numeric(1)
                    sort.pos <- FALSE
          }
     outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     outcome[,11] <- as.numeric(outcome[,11])
     outcome[,17] <- as.numeric(outcome[,17])
     outcome[,23] <- as.numeric(outcome[,23])
     df <- subset(outcome, outcome$State == state) 
     if(nrow(df)==0) stop("invalid state")
     if(!outcome.name %in% c('heart attack', 'heart failure', 'pneumonia')) stop("invalid outcome")
     if(outcome.name == 'heart attack') cnum <- 11
     if(outcome.name == 'heart failure') cnum <- 17
     if(outcome.name == 'pneumonia') cnum <- 23
     if(sort.pos) {
     if(outcome.name == 'heart attack')
          dff <- df[order(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, df$Hospital.Name),] 
     if(outcome.name == 'heart failure')
          dff <- df[order(df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, df$Hospital.Name),] 
     if(outcome.name == 'pneumonia')
          dff <- df[order(df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, df$Hospital.Name),] 
     } else {
          if(outcome.name == 'heart attack')
               dff <- df[order(-df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, df$Hospital.Name),] 
          if(outcome.name == 'heart failure')
               dff <- df[order(-df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, df$Hospital.Name),] 
          if(outcome.name == 'pneumonia')
               dff <- df[order(-df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, df$Hospital.Name),] 
          
     }
     if(is.na(dff[rank,cnum])) {
          return(NA)
     } else { return(dff[rank,2])}
}

