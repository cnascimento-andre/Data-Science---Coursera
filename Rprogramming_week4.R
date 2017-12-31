ex1<-function(ex=1,directory="D:/0_backup/pessoal/cursos/Data_science_MOOC/R_programming/week4"){
    setwd(directory)
    outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
    
    if (ex==1) {
        outcome[,11]<-as.numeric(outcome[,11])
        print("1 - Plot the 30-day mortality rates for heart attack")
        hist(outcome[,11],main = "30-day mortality rates for heart attack", xlab = "Probability [%]")
    }
    
}

best<-function(state,ocm, directory ="D:/0_backup/pessoal/cursos/Data_science_MOOC/R_programming/week4"){
    setwd(directory)
    outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
    # Check the state and outcome are valid
    if (!state%in%outcome$State){
        stop("Invalid state")
    }
    possiblesOC<-c("heart attack","heart failure","pneumonia")
    if (!ocm%in%possiblesOC){
        stop("Invalid outcome")
    }
    colnames(outcome)[11]<-"heart attack"
    colnames(outcome)[17]<-"heart failure"    
    colnames(outcome)[23]<-"pneumonia"
    outcome[,ocm]<-as.numeric(outcome[,ocm])
    s<-split(outcome,outcome$State)
    minimo<-min(s[[state]][[ocm]],na.rm = TRUE)
    posi<-match(minimo,s[[state]][[ocm]])
    s[[state]][["Hospital.Name"]][[posi]]
}

rankhospital <-function(state, ocm, num="best",directory ="D:/0_backup/pessoal/cursos/Data_science_MOOC/R_programming/week4"){
    setwd(directory)
    outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
    # Check the state and outcome are valid
    if (!state%in%outcome$State){
        stop("Invalid state")
    }
    possiblesOC<-c("heart attack","heart failure","pneumonia")
    if (!ocm%in%possiblesOC){
        stop("Invalid outcome")
    }
    colnames(outcome)[11]<-"heart attack"
    colnames(outcome)[17]<-"heart failure"    
    colnames(outcome)[23]<-"pneumonia"
    # create matrix with list of ranked hospital for given state and outcome
    m<-data.frame(Hospital.Name=outcome$Hospital.Name,State=outcome$State,outcome=as.numeric(outcome[[ocm]]))
    m<-m[m$State==state,]
    m<-m[complete.cases(m) , ]
    # sorting the data frame
    m<-m[with(m,order(m$outcome,m$Hospital.Name)),]
    if (num=="best"){
        print(m$Hospital.Name[1])
    } else if (num=="worst"){
        print(m$Hospital.Name[length(m$Hospital.Name)])
    } else if (class(num)=="numeric" & num<=length(m$outcome)){
        print(m$Hospital.Name[num])   
    } else {
        print(NA)
    }
}

rankall<-function(ocm,num="best",directory="D:/0_backup/pessoal/cursos/Data_science_MOOC/R_programming/week4"){
    setwd(directory)
    outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")

    possiblesOC<-c("heart attack","heart failure","pneumonia")
    if (!ocm%in%possiblesOC){
        stop("Invalid outcome")
    }
    colnames(outcome)[11]<-"heart attack"
    colnames(outcome)[17]<-"heart failure"    
    colnames(outcome)[23]<-"pneumonia"
    stateslist<-sort(unique(outcome$State))
    hospitals<-c()
    for (i in 1:length(stateslist)){
        m<-data.frame(Hospital.Name=outcome$Hospital.Name,State=outcome$State,outcome=as.numeric(outcome[[ocm]]))
        state<-stateslist[i]
        m<-m[m$State==state,]
        m<-m[complete.cases(m) , ]
        # sorting the data frame
        m<-m[with(m,order(m$outcome,m$Hospital.Name)),]
        if (num=="best"){
            hospitals[i]<-as.character(m$Hospital.Name[1])
        } else if (num=="worst"){
            hospitals[i]<-as.character(m$Hospital.Name[length(m$Hospital.Name)])
        } else if (class(num)=="numeric" & num<=length(m$outcome)){
            hospitals[i]<-as.character(m$Hospital.Name[num])   
        } else {
            hospitals[i]<-"NA"
        }
    }
    result<-data.frame(State=stateslist,Hospital=hospitals)
    result
}   
