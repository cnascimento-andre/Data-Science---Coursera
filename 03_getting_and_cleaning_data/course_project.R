# Getting and cleaning data - course project
run_analysis<-function(tidy = "FALSE", directory="D:/0_backup/pessoal/cursos/Data_science_JHU_MOOC/03_getting_and_cleaning_data/week4/UCI HAR Dataset"){
    setwd(directory) #location of UCI HAR Dataset folder
    
    #loading data
    aux<-c("test","train")
    aux2<-c("x","y","z")
    body_acc<-list(test=list(x=c(),y=c(),z=c()),train=list(x=c(),y=c(),z=c()))
    body_gyro<-list(test=list(x=c(),y=c(),z=c()),train=list(x=c(),y=c(),z=c()))
    total_acc<-list(test=list(x=c(),y=c(),z=c()),train=list(x=c(),y=c(),z=c()))
    for (i in 1:2) {
        for (j in 1:3){
            body_acc[[aux[i]]][[aux2[j]]]<-read.table(paste(directory,"/",aux[i],"/Inertial Signals/body_acc_",aux2[j],"_",aux[i],".txt",sep = ""))
            body_acc[[aux[i]]][[aux2[j]]]<-as.data.frame(t(body_acc[[aux[i]]][[aux2[j]]]))
            body_gyro[[aux[i]]][[aux2[j]]]<-read.table(paste(directory,"/",aux[i],"/Inertial Signals/body_gyro_",aux2[j],"_",aux[i],".txt",sep = ""))
            body_gyro[[aux[i]]][[aux2[j]]]<-as.data.frame(t(body_gyro[[aux[i]]][[aux2[j]]]))
            total_acc[[aux[i]]][[aux2[j]]]<-read.table(paste(directory,"/",aux[i],"/Inertial Signals/total_acc_",aux2[j],"_",aux[i],".txt",sep = ""))
            total_acc[[aux[i]]][[aux2[j]]]<-as.data.frame(t(total_acc[[aux[i]]][[aux2[j]]]))
            j<-j+1
        }
        i<-i+1
    }
    activities_test<-read.table(paste(directory,"/test/y_test.txt",sep = ""))
    activities_test<-activities_test$V1
    subject_test<-read.table(paste(directory,"/test/subject_test.txt",sep = ""))
    subject_test<-subject_test$V1
    activities_train<-read.table(paste(directory,"/train/y_train.txt",sep = ""))
    activities_train<-activities_train$V1
    subject_train<-read.table(paste(directory,"/train/subject_train.txt",sep = ""))
    subject_train<-subject_train$V1
    
    #setting big data frame
    subject<-as.factor(c(subject_test,subject_train))
    activities<-as.character(c(activities_test,activities_train))
    aux3<-as.character(c(1:6))
    activitiesvector<-as.character(c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))
    for (i in 1:6) {
        activities[activities==aux3[i]]<-activitiesvector[i]
        i<-i+1
    }
    Dbody_acc<-list(mean=list(x=c(),y=c(),z=c()),sd=list(x=c(),y=c(),z=c()))
    Dbody_gyro<-list(mean=list(x=c(),y=c(),z=c()),sd=list(x=c(),y=c(),z=c()))
    Dtotal_acc<-list(mean=list(x=c(),y=c(),z=c()),sd=list(x=c(),y=c(),z=c()))
    aux4<-c("mean","sd")
    for (j in 1:3){
        for (k in 1:2){
            Dbody_acc[[aux4[k]]][[aux2[j]]]<-c(unlist(lapply(body_acc$test[[aux2[j]]],aux4[k]), use.names = FALSE),
                                              unlist(lapply(body_acc$train[[aux2[j]]],aux4[k]), use.names = FALSE))
            Dbody_gyro[[aux4[k]]][[aux2[j]]]<-c(unlist(lapply(body_gyro$test[[aux2[j]]],aux4[k]), use.names = FALSE),
                                              unlist(lapply(body_gyro$train[[aux2[j]]],aux4[k]), use.names = FALSE))
            Dtotal_acc[[aux4[k]]][[aux2[j]]]<-c(unlist(lapply(total_acc$test[[aux2[j]]],aux4[k]), use.names = FALSE),
                                              unlist(lapply(total_acc$train[[aux2[j]]],aux4[k]), use.names = FALSE))
            k<-k+1
        }
        j<-j+1
    }
    data<-data.frame(subject,test_train = c(rep("test",length(subject_test)),rep("train",length(subject_train))),activities,
                     bd_acc_x_mean = Dbody_acc$mean$x, bd_acc_x_sd = Dbody_acc$sd$x,
                     bd_acc_y_mean = Dbody_acc$mean$y, bd_acc_y_sd = Dbody_acc$sd$y,
                     bd_acc_z_mean = Dbody_acc$mean$z, bd_acc_z_sd = Dbody_acc$sd$z,
                     bd_gyr_x_mean = Dbody_gyro$mean$x, bd_gyr_x_sd = Dbody_gyro$sd$x,
                     bd_gyr_y_mean = Dbody_gyro$mean$y, bd_gyr_y_sd = Dbody_gyro$sd$y,
                     bd_gyr_z_mean = Dbody_gyro$mean$z, bd_gyr_z_sd = Dbody_gyro$sd$z,
                     tt_acc_x_mean = Dtotal_acc$mean$x, tt_acc_x_sd = Dtotal_acc$sd$x,
                     tt_acc_y_mean = Dtotal_acc$mean$y, tt_acc_y_sd = Dtotal_acc$sd$y,
                     tt_acc_z_mean = Dtotal_acc$mean$z, tt_acc_z_sd = Dtotal_acc$sd$z)

    #setting tidy data
    Tdata<-c()
    tidydata<-split(data,data$activities)
    for (i in 1:6) {
        tidydata[[i]]<-split(tidydata[[i]],tidydata[[i]][["subject"]])
        for (j in 1:30) {
            tidydata[[i]][[j]]<-tidydata[[i]][[j]][,!names(tidydata[[i]][[j]])%in%c("subject","test_train","activities")]
            tidydata[[i]][[j]]<-apply(tidydata[[i]][[j]],2,mean)
            Tdata<-rbind(Tdata,tidydata[[i]][[j]])
        }
    }
    subject<-rep(as.character(c(1:30)),6)
    activities<-c(rep(activitiesvector[1],30),rep(activitiesvector[2],30),rep(activitiesvector[3],30),
                      rep(activitiesvector[4],30),rep(activitiesvector[5],30),rep(activitiesvector[6],30))
    Tdata<-data.frame(activities,subject,Tdata)
    if(tidy == "TRUE"){
        return(Tdata)
    }
    else {
        return(data)
    }
}
