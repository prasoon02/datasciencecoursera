rankall <- function(outcome, num = "best") {
## Read outcome data
	     data <-read.csv("outcome-of-care-measures.csv",colClasses="character")
	     ## Check that state and outcome are valid
	     states<-unique(data[,7])
	     coln <- if (outcome == "heart attack")
	     {11}	     
	     else if (outcome == "heart failure")
	     {17}
	     else if (outcome == "pneumonia")
	     {23}
	     else
	     {stop("invalid outcome")
	     }
	     ## For each state, find the hospital of the given rank
	     data[,coln]=as.numeric(data[,coln])
	     data=data[,c(2,7,coln)]
	     data=na.omit(data)
	     ## Return a data frame with the hospital names and the
	     ## (abbreviated) state name
	     rank_in_state<-function(state){
		deathr=data[data[,2]==state,]
	     	nhos=nrow(deathr)
	     	if(num=="best")
	     	{num=1}
	     	else if (num=="worst")
	     	{num=nhos}
	     	if(num>nhos){return(NA)}
	     	ord=order(deathr[,3],deathr[,1])
	     	res=deathr[ord,][num,1]
		c(res,state)
	     }
	     output = do.call(rbind, lapply(states, rank_in_state))
    	     output = output[order(output[, 2]), ]
    	     rownames(output) = output[, 2]
    	     colnames(output) = c("hospital", "state")
    	     data.frame(output)
}
