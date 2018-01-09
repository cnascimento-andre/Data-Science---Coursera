#02 - Rprogramming - Week2 excercises
pollutantmean <-function(directory, pollutant, id){
    #pollutant can be sulfate or nitrate
    wd <- getwd()
    pasta <- file.path(wd,directory)
    medias <- list(sulfate = c(),nitrate = c())
    for (i in 1:length(id)){
        case<-nchar(id[i])
        if (case == 1){
            arquivo <- paste(pasta,"/","00",id[i],".csv",sep="")
            a<-read.csv(arquivo)
            medias$nitrate <-c(medias$nitrate, a$nitrate[!is.na(a$nitrate)])
            medias$sulfate <-c(medias$sulfate, a$sulfate[!is.na(a$sulfate)])
        } 
        if (case == 2){
            arquivo <- paste(pasta,"/","0",id[i],".csv",sep="")
            a<-read.csv(arquivo)
            medias$nitrate <-c(medias$nitrate, a$nitrate[!is.na(a$nitrate)])
            medias$sulfate <-c(medias$sulfate, a$sulfate[!is.na(a$sulfate)])
        }
        if (case == 3){
            arquivo <- paste(pasta,"/",id[i],".csv",sep="")
            a<-read.csv(arquivo)
            medias$nitrate <-c(medias$nitrate, a$nitrate[!is.na(a$nitrate)])
            medias$sulfate <-c(medias$sulfate, a$sulfate[!is.na(a$sulfate)])
        }
    }
    mean(medias[[pollutant]])
}

complete<-function(directory,id=1:32){
    wd <- getwd()
    pasta <- file.path(wd,directory)
    nobs<-c()
    for (i in 1:length(id)){
        case<-nchar(id[i])
        if (case == 1){
            arquivo <- paste(pasta,"/","00",id[i],".csv",sep="")
            a<-read.csv(arquivo)
            nobs[i]<-sum(complete.cases(a))
        }
        if (case == 2){
            arquivo <- paste(pasta,"/","0",id[i],".csv",sep="")
            a<-read.csv(arquivo)
            nobs[i]<-sum(complete.cases(a))
        }
        if (case == 3){
            arquivo <- paste(pasta,"/",id[i],".csv",sep="")
            a<-read.csv(arquivo)
            nobs[i]<-sum(complete.cases(a))
        }
    }
    cbind(id,nobs)
}

corr<-function(directory,thr=0){
    wd <- getwd()
    pasta <- file.path(wd,directory)
    files<-list.files(pasta)
    cr<-c()
    id<-1
    for (i in 1:length(files)){
        arquivo <- paste(pasta,"/",files[i],sep="")
        a<-read.csv(arquivo)
        gate<-sum(complete.cases(a))
        if (gate>thr) {
            cc<-complete.cases(a)
            nit<-a$nitrate[cc]
            sulf<-a$sulfate[cc]
            cr[id]<-cor(nit,sulf)
            id<-id+1
        }
    }
    print(length(cr))
    print(summary(cr))
    cr
}
