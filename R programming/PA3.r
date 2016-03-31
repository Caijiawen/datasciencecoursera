file <- read.csv("C:/Users/Cai Jiawen/Desktop/outcome-of-care-measures.csv",stringsAsFactors=FALSE,na.strings="Not Available")

#### part1


minindex <- function(vector) {
  return(vector == min(vector,na.rm=TRUE))
}

best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  StateList <- unique(file$State)
  DiagnosisList <- c("heart attack","heart failure","pneumonia")
  if (any(state==StateList) & any(outcome==DiagnosisList)) {
    if (outcome=="heart attack") {
      statefile <- file[file$State==state,]
      statefile[,11] = as.numeric(statefile[,11])
      return (sort(statefile[,2][minindex(statefile[,11])])[1])
    }
    else if (outcome=="heart failure") {
      statefile <- file[file$State==state,]
      statefile[,17] = as.numeric(statefile[,17])
      return (sort(statefile[,2][minindex(statefile[,17])])[1])
    }
    else {
      statefile <- file[file$State==state,]
      statefile[,23] = as.numeric(statefile[,23])
      return (sort(statefile[,2][minindex(statefile[,23])])[1])
    }
  }
  else if (!any(state==StateList)) {
    stop("invalid state")
  }
  else {
    stop("invalid outcome")
  }
}



#### Part2


createsubset <- function(state, outcome) {
  OutcomeList <- c("heart attack","heart failure","pneumonia")
  # heart attack in column 11,heart failure in column 17 and so on
  OutcomeNum <- 6*which(OutcomeList==outcome)+5
  # using state to choose rows , and choose three columns will be in use
  subset <- file[file$State==state,c(2,7,OutcomeNum)]
  # rename the columns
  names(subset) <- c("hospital", "state", "outcome")
  # omit the rows with NA's in third column
  subset <- na.omit(subset)
  # sort the outcome then hospital name 
  # the order is crucial, to more important variable we order it later
  subset <- subset[order(subset$hospital),]
  subset <- subset[order(subset$outcome),]
  return (subset)
}


rankhospital <- function(state, outcome,num="best") {
  StateList <- unique(file$State)
  OutcomeList <- c("heart attack","heart failure","pneumonia")
  if (!state%in%StateList) stop("invalid state")
  else if (!outcome%in%OutcomeList) stop("invalid outcome")
  else if (!num%in%c("best","worst") & class(num)!="numeric") stop("invalid num")
  else {
    subset <- createsubset(state, outcome)
    if (num=="best") num = 1
    else if (num=="worst") num = nrow(subset)
    return (subset$hospital[num])
  }
}



#### part3


createallset <- function(outcome) {
  OutcomeList <- c("heart attack","heart failure","pneumonia")
  # heart attack in column 11,heart failure in column 17 and so on
  OutcomeNum <- 6*which(OutcomeList==outcome)+5
  # using state to choose rows , and choose three columns will be in use
  subset <- file[,c(2,7,OutcomeNum)]
  # rename the columns
  names(subset) <- c("hospital", "state", "outcome")
  # omit the rows with NA's in third column
  subset <- subset[!is.na(subset$outcome),]
  # sort the outcome then hospital name 
  # the order is crucial, to more important variable we order it later
  subset <- subset[order(subset$state),]
  subset <- subset[order(subset$hospital),]
  subset <- subset[order(subset$outcome),]
  return (subset)
}

rankall <- function(outcome,num="best") {
  OutcomeList <- c("heart attack","heart failure","pneumonia")
  if (!outcome%in%OutcomeList) stop("invalid outcome")
  else if (!num%in%c("best","worst") & class(num)!="numeric") stop("invalid num")
  else {
    subset <- createallset(outcome)[,c(1,2)]
    statelist <- split(subset,subset$state)
    if (num=="worst") {
      finallist <- lapply(statelist,function(x) x[nrow(x),1])
    }
    else if (num=="best") {
      finallist <- lapply(statelist,function(x) x[1,1])
    }
    else {
      finallist <- lapply(statelist,function(x) x[num,1])
    }
    rowname <- names(finallist)
    hospitallist <- as.vector(unlist(finallist))
    return (data.frame(hospital=hospitallist, state=rowname, row.names=rowname))
  }
}

