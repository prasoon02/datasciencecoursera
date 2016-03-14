best <- function(state, outcome) {
     ## Read outcome data
     data <-read.csv("outcome-of-care-measures.csv",colClasses="character")
     ## Check that state and outcome are valid
     if(!state %in% unique(data[,7]))
     {
	stop("invalid state")
     }
     coln <- if (outcome == "heart attack")
     {11}
     else if (outcome == "heart failure")
     {17}
     else if (outcome == "pneumonia")
     {23}
     else
     {stop("invalid outcome")
     }
     ## Return hospital name in that state with lowest 30-day death
     deathcol=data[data$State==state,c(2,coln)]
     ## rate
     deathcol[which.min(deathcol[,2]),1]
}