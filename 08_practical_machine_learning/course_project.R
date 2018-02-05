# machine learning - course project
library(ggplot2)
library(caret)

directory<-"D:/0_backup/pessoal/cursos/Data_science_JHU_MOOC/08_machine_learning"
setwd(directory)
train <- read.csv("pml-training.csv", na.strings = c("", "NA"))
test <- read.csv("pml-testing.csv")

# cleaning train data
# remove columuns which number of NA is too large
numNA <- apply(apply(train,2,is.na),2,sum)
numNA <- numNA[numNA>0.9*length(train$X)]
training <- train[ , !(names(train) %in% names(numNA))]
# remove non predictors
training <- subset(training, select = -c(X,user_name,cvtd_timestamp,new_window, num_window, raw_timestamp_part_1, raw_timestamp_part_2))

# Boosting with trees GBM
# may take a while to run
inTrain <- createDataPartition(y=training$classe, p=0.7, list = FALSE)
trainingGBM<- training[inTrain,]
testingGBM<-training[-inTrain,]

modfitGBM <- train(classe ~., method = "gbm", data = trainingGBM, verbose = FALSE)
print(modfitGBM)
predGBM<-predict(modfitGBM, testingGBM)
table(predGBM,testingGBM$classe)

df<-as.data.frame(table(predGBM,testingGBM$classe))
df<-data.frame(Predicted = df$predGBM, Actual = df$Var2, Freq = df$Freq)

# normalizing frequency
freqsum <- tapply(df$Freq, df$Actual, sum)
freqN <- c()
for (i in 1:5) {
    df.split <- df[df$Actual == levels(df$Actual)[i] ,]
    dfN <- df.split$Freq/freqsum[i]
    freqN <- c(freqN, dfN)
}
df <- cbind(df,freqN)

# Plotting confusion matrix
ggplot(data =  df, mapping = aes(x = df$Actual, y = df$Predicted)) +
    geom_tile(aes(fill = df$freqN), colour = "white") +
    geom_text(aes(label = sprintf("%0.2f", df$freqN*100)), vjust = 1) +
    scale_fill_gradient(low = "blue", high = "red") +
    theme_bw() + theme(legend.position = "none") +
    ggtitle("Boosting with trees - GBM - prediction [%]") + xlab("Actual") + ylab("Predicted")

# Plotting importance of predictors
# summary(modfitGBM$finalModel)

# prediction for test vector
predGBM.test<-predict(modfitGBM, test)

# Principal components ...... did not work well, see pic1 and 2, more usefull for linear models
## prComp <- prcomp(subset(training, select = -c(X,user_name,cvtd_timestamp, classe, new_window,raw_timestamp_part_1,raw_timestamp_part_2)))
## plot(prComp$x[,1],prComp$x[,2],xlab="PC1", ylab="PC2", col = training$classe, pch = 19)
## legend(x = 'right',legend = unique(training$classe),col=1:length(training$classe),pch=19)
