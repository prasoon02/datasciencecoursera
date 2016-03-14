rankhospital <- function(state, outcome, num = "best") {
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
	     data[,coln]=as.numeric(data[,coln])
	     deathr=data[data[,7]==state,c(2,coln)]
	     deathr=na.omit(deathr)
	     nhos=nrow(deathr)
	     if(num=="best")
	     {num=1}
	     else if (num=="worst")
	     {num=nhos}
	     if(num>nhos){return(NA)}
	     ## Return hospital name in that state with the given rank
	     ord=order(deathr[,2],deathr[,1])
	     deathr[ord,][num,1]
	     ## 30-day death rate
}
