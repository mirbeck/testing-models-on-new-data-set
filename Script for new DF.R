
#Testing models in blind dataset which was given as a bonus to the task #3
library('readr')

testData <- read.csv("Desktop/Data Science/ubiqum/projects/DS3/Task#3/test data set/testData.csv")
testData <- testData[, -which(names(testData) %in% c("SPACEID", "RELATIVEPOSITION", "USERID", "PHONEID", "TIMESTAMP", "WAP248", "WAP046","WAP113", "WAP172", "WAP173", "WAP175", "WAP180", "WAP181", "WAP189", "WAP369", "WAP503"))]

testData$IsTrainSet <- FALSE

#Change BUILDINGID and FLOOR to factors
testData$BUILDINGID <- factor(testData$BUILDINGID)
testData$FLOOR <- factor(testData$FLOOR)

# Put all WAP's in WAPS  
WAPS<-grep("WAP", names(testData), value=T)

#Change all WAPS with value 100 (which is no signal) to -110 (no signal too)
testData[,WAPS] <- sapply(testData[,WAPS],function(x) ifelse(x==100,-110,x))

#Function to find 1st, 2nd, 3rd highest WAPs and RSSI values 
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]

# Set Highest WAP number
testData<-testData %>% 
  mutate(HighWAP=colnames(testData[WAPS])[apply(testData[WAPS],1,which.max)])
#Set 2nd highest WAP number
testData<-testData %>% 
  mutate(HighWAP2=colnames(testData[WAPS])[apply(testData[WAPS],1,maxn(2))])
#Set 3rd highest WAP number
testData<-testData %>% 
  mutate(HighWAP3=colnames(testData[WAPS])[apply(testData[WAPS],1,maxn(3))])
# Set highest RSSI value
testData<-testData %>% 
  mutate(HighRSSI=apply(testData[WAPS], 1, max))

# Transforming to factor some columns as we are going to use them as predictors in classification
testData$HighWAP<-as.factor(testData$HighWAP)
testData$HighWAP2<-as.factor(testData$HighWAP2)
testData$HighWAP3<-as.factor(testData$HighWAP3)

#----------------------------BUILDING ID-------------------------------
saveRDS(Building_SVM01, file="Building_SVM01")
Building_SVM01 <- readRDS("Building_SVM01")
#SVM
SVM_BUILD<-predict(Building_SVM01,testData[,c(1:509)])
summary(SVM_BUILD)
SVM_BUILD<-unfactor(SVM_BUILD)

#KNN
saveRDS(KNN_BUILD, file="KNN_BUILD")
KNN_BUILD <- readRDS("KNN_BUILD")
system.time(KNN_BUILD <- knn(train=trainingData[,c(1:509)], test=testData[,c(1:509)],
                            cl=trainingData$BUILDINGID, k=5))
summary(KNN_BUILD)
KNN_BUILD<-unfactor(KNN_BUILD)
#RF
saveRDS(RF, file="RF")
RF <- readRDS("RF")
RF_BUILD<-predict(RF, testData)
summary(RF_BUILD)

testData<-testData %>% 
  mutate(BUILIDNGID_RF=RF_BUILD)

#--------------------------------FLOOR---------------------------

#SVM
saveRDS(Floor_HighWAP_SVM, file="Floor_HighWAP_SVM")
Floor_HighWAP_SVM <- readRDS("Floor_HighWAP_SVM")
SVM_FLOOR<-predict(Floor_HighWAP_SVM, testData[,c(1:509)])
SVM_FLOOR<-unfactor(SVM_FLOOR)

#KNN

saveRDS(KNN_FLOOR, file="KNN_FLOOR")
KNN_FLOOR <- readRDS("KNN_FLOOR")
system.time(KNN_FLOOR <- knn(trainingData[,c((1:509),(519))], test=testData[,c((1:509),(519))],
                                 cl=trainingData$FLOOR, k=5))
KNN_FLOOR<-unfactor(KNN_FLOOR)

#-------------------------Predicting LONGITUDE---------------------
saveRDS(RF_LONG, file="RF_LONG")
RF_LONG <- readRDS("RF_LONG")
#RF
RF_LONGITUDE<-predict(RF_LONG, testData)
RF_LONGITUDE

#KNN
saveRDS(KNN_LONGITUDE, file="KNN_LONGITUDE")
KNN_LONGITUDE <- readRDS("KNN_LONGITUDE")
set.seed(123)
system.time(KNN_LONGITUDE <- knn(trainingData[,c((1:509),(519))], test=testData[,c((1:509),(519))],
                            cl=trainingData$LONGITUDE, k=5))
KNN_LONGITUDE <- unfactor(KNN_LONGITUDE)


#--------------------------Predicting LATITUDE----------------------

#RF
saveRDS(RF_LAT, file="RF_LAT")
RF_LAT <- readRDS("RF_LAT")
RF_LATITUDE<-predict(RF_LAT, testData)
RF_LATITUDE

#KNN
saveRDS(KNN_LATITUDE, file="KNN_LATITUDE")
KNN_LATITUDE <- readRDS("KNN_LATITUDE")
set.seed(123)
system.time(KNN_LATITUDE <- knn(trainingData[,c((1:509),(519))], test=testData[,c((1:509),(519))],
                           cl=trainingData$LATITUDE, k=5))

KNN_LATITUDE <- unfactor(KNN_LATITUDE)

#Saving all predictions in csv file
KNN_TOTAL <-cbind(KNN_LATITUDE,KNN_LONGITUDE,KNN_FLOOR,KNN_BUILD)
View(KNN_TOTAL)
write.csv(KNN_TOTAL,'KNN_TOTAL.csv', row.names = FALSE)
MULT_TOTAL <-cbind(RF_LATITUDE,RF_LONGITUDE,SVM_FLOOR,SVM_BUILD) 
View(MULT_TOTAL)
write.csv(MULT_TOTAL,'MULT_TOTAL.csv', row.names = FALSE)

#------------------Visualization of predictions-----------------------
KNN_TOTAL<-as.data.frame(KNN_TOTAL)
plot_ly(KNN_TOTAL) %>%
  add_markers(x = ~KNN_LATITUDE, y = ~KNN_LONGITUDE, size = 1, color = ~KNN_BUILD, colors = c('#1f77b4', '#ff7f0e','#2ca02c')) %>%
  layout(title = "PREDICTED BUILDINGS")

plot_ly(KNN_TOTAL) %>%
  add_markers(x = ~KNN_LONGITUDE, y = ~KNN_LATITUDE, z =~KNN_FLOOR, size = 5, color = ~KNN_FLOOR, colors = c('#1f77b4', '#ff7f0e','#2ca02c', '#d62728', '#9467bd')) %>%
  layout(title = "PREDICTED FLOORS IN BUILDINGS")



