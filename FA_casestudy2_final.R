install.packages("party")
install.packages("mobForest")
library(party)
library(mobForest)
library(caret)
data<-read.csv("case2.csv")
colnames(data)
train<-sample(1:1000,700,replace = FALSE)
traindata<-data[train,]
testdata<-data[-train,]
model_pd<-ctree(Probability.of.Default ~Exposure.at.Default+City+ Annual.Income+Total.Loan.Approved.Amount+Education+Profession+Internal.Bank.Rating,data = traindata,controls = ctree_control(mtry=3,maxdepth=15))
model_pd
plot(model_pd)
model_ead<-ctree(Exposure.at.Default ~ Annual.Income+City+Total.Loan.Approved.Amount+Education+Profession+Internal.Bank.Rating,data = traindata,controls = ctree_control(mtry=4,maxdepth=2))
model_ead
plot(model_ead)
df1<-split(traindata,traindata$City,drop = T)
df2<-split(testdata,testdata$City,drop = T)
train_pd1<-df1$Bangalore
test_pd1<-df2$Bangalore
train_pd2<-rbind(df1$Chennai,df1$Kolkata,df1$Mumbai,df1$Pune)
test_pd2<-rbind(df2$Chennai,df2$Kolkata,df2$Mumbai,df2$Pune)
colnames(data_pd1)
#model for Banglore city
pd1_cntrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)
model_svm_pd1 <- train(Probability.of.Default~Annual.Income+Education+Profession+Total.Loan.Approved.Amount, data = train_pd1, method = "svmRadial",
                    trControl=pd1_cntrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
prd_pd1<-predict(model_svm_pd1,newdata = test_pd1)
#model for Other cities
model_svm_pd2 <- train(Probability.of.Default~City+Annual.Income+Education+Profession+Total.Loan.Approved.Amount, data = train_pd2, method = "svmRadial",
                       trControl=pd1_cntrl,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
prd_pd2<-predict(model_svm_pd2,newdata = test_pd2)

#model for EAD

train_ead1<-split(traindata,traindata$Total.Loan.Approved.Amount<33328,drop=T)$'TRUE'
test_ead1<-split(testdata,testdata$Total.Loan.Approved.Amount<33328,drop=T)$'TRUE'
train_ead2<-split(traindata,traindata$Total.Loan.Approved.Amount>33328 & traindata$Total.Loan.Approved.Amount<56384)$'TRUE'
test_ead2<-split(testdata,testdata$Total.Loan.Approved.Amount>33328 & testdata$Total.Loan.Approved.Amount<56384)$'TRUE'
train_ead3<-split(traindata,traindata$Total.Loan.Approved.Amount>56384 & traindata$Total.Loan.Approved.Amount<79208)$'TRUE'
test_ead3<-split(testdata,testdata$Total.Loan.Approved.Amount>56384 & testdata$Total.Loan.Approved.Amount<79208)$'TRUE'
train_ead4<-split(traindata,traindata$Total.Loan.Approved.Amount>79208)$'TRUE'
test_ead4<-split(testdata,testdata$Total.Loan.Approved.Amount>79208)$'TRUE'

#model EAD1
model_svm_ead1 <- train(Exposure.at.Default~City+Annual.Income+Education+Profession+Total.Loan.Approved.Amount, data = train_ead1, method = "svmRadial",
                       trControl=pd1_cntrl,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
prd_ead1<-predict(model_svm_ead1,newdata = test_ead1)

#model EAD2
model_svm_ead2 <- train(Exposure.at.Default~City+Annual.Income+Education+Profession+Total.Loan.Approved.Amount+Internal.Bank.Rating, data = train_ead2, method = "svmRadial",
                       trControl=pd1_cntrl,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
prd_ead2<-predict(model_svm_ead2,newdata = test_ead2)

#model EAD4
model_svm_ead4 <- train(Exposure.at.Default~City+Annual.Income+Education+Profession+Total.Loan.Approved.Amount+Internal.Bank.Rating, data = train_ead4, method = "svmRadial",
                       trControl=pd1_cntrl,
                       preProcess = c("center", "scale"),
                       tuneLength = 10)
prd_ead4<-predict(model_svm_ead4,newdata = test_ead4)

#modelEAD3
model_svm_ead3 <- train(Exposure.at.Default~City+Annual.Income+Education+Profession+Total.Loan.Approved.Amount+Internal.Bank.Rating, data = train_ead3, method = "svmRadial",
                        trControl=pd1_cntrl,
                        preProcess = c("center", "scale"),
                        tuneLength = 10)
prd_ead3<-predict(model_svm_ead3,newdata = test_ead3)

#model LGD
model_lgd<-model_pd<-ctree(LGD ~City+ Annual.Income+Total.Loan.Approved.Amount+Education+Profession+Internal.Bank.Rating,data = traindata,controls = ctree_control(mtry=3,maxdepth=15))
plot(model_lgd)

train_lgd1<-split(traindata,traindata$Total.Loan.Approved.Amount<30687)$'TRUE'
test_lgd1<-split(testdata,testdata$Total.Loan.Approved.Amount<30687)$'TRUE'
train_lgd2<-split(traindata,traindata$Total.Loan.Approved.Amount>30687)$'TRUE'
test_lgd2<-split(testdata,testdata$Total.Loan.Approved.Amount>30687)$'TRUE'

#model_LGD1
model_svm_lgd1<-train(LGD~City+Annual.Income+Education+Profession+Total.Loan.Approved.Amount, data = train_lgd1, method = "svmRadial",
                      trControl=pd1_cntrl,
                      preProcess = c("center", "scale"),
                      tuneLength = 10)
prd_lgd1<-predict(model_svm_lgd1,newdata = test_lgd1)

#model_LGD2
model_svm_lgd2<-train(LGD~City+Annual.Income+Education+Profession+Total.Loan.Approved.Amount, data = train_lgd2, method = "svmRadial",
                      trControl=pd1_cntrl,
                      preProcess = c("center", "scale"),
                      tuneLength = 10)
prd_lgd2<-predict(model_svm_lgd2,newdata = test_lgd2)

#Final expected loss
prd_pd<-c(prd_pd1,prd_pd2)
prd_ead<-c(prd_ead1,prd_ead2,prd_ead3,prd_ead4)
prd_lgd<-c(prd_lgd1,prd_lgd2)
prd_el<-prd_pd/100*prd_ead*prd_lgd/100
actual_prd<-data$EL[-train]
cor(prd_el,actual_prd)
