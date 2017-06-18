
if(!file.exists("data")){
  dir.create("data")
}

library(dplyr)
install.packages("data.table")
library(data.table)


TableYTrain<-read.table("./Train/Y_train.txt")


TableXTrain<-read.table("./Train/X_train.txt")
#head(TableXTrain,5)
#tail(TableXTrain,5)

TableXTest<-read.table("./Test/x_test.txt")
TableYTest<-read.table("./Test/Y_test.txt")

TableX<- rbind(TableXTrain,TableXTest)
TableY<- rbind(TableYTrain,TableYTest)

# Data set TableX only with  mean and standart deviation

Ftures<-read.table("features.txt")
col<-grep("std|mean[()]",as.character(Ftures$V2))
TableXMS<-TableX[,col]


# Activities of data set  are named by mapping names with number in TableY
# and name rows to TableXMS. No se puede usar rownames por uniqueness

ActLabels<-read.table("./activity_labels.txt")
ActN<-function(TableY){ActLabels[TableY,2]}
AcTN<-sapply(TableY,ActN)
TableN<-cbind(AcTN,TableXMS)


# Variable names. Columns are named with he number from col and 
# names from features.txt

varName<-function(col) {Ftures[col,2]}
varName<-sapply(col,varName)
colnames(TableN)<-c("Activity",as.character(varName))

# independent data set with average of each variable,each
# activity and subject

# The Subject data is added to the data set. Test and train are integrated in one. 
# Strucuture is preserved

SubjTrain<-read.table("./test/Subject_test.txt")
SubjTest<-read.table("./train/subject_train.txt")
Subj<- rbind(SubjTrain,SubjTest)

Ntable<-cbind(Subject=Subj,TableN)
Ntable<-rename(Ntable,Subject=V1)

# mean values per activiity  are obtain in a ActiG file

  ActiG<-group_by(Ntable,Activity)%>% 
  summarise_each(funs(mean), -c(Subject,Activity))

# Mean values of all variables  per Subject  are obtain in the file SubjG

  SubjG<-group_by(Ntable,Subject)%>% 
  summarise_each(funs(mean), -c(Subject,Activity))
  
## DS are saved in files
  
write.table(TableN,file= "./data/TableN.txt",row.name=FALSE)
write.table(ActiG,file="./data/ActivityGMeans.txt",row.names=FALSE)
write.table(SubjG,file="./data/SubjectGMeans.txt",row.names = FALSE)
