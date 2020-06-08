# Libraries used
library(tidyverse)
library(Amelia)
library(caret)
library(MASS) #For boxcox transformations
library(corrplot)
library(psych)
library(corrgram)
library(mlbench)
library(e1071) # For finding skewness
library(car) #For symbox func
library(pls)
library(glmnet) #For ridge regression
library(elasticnet)
library(caret)
library(hqreg)
library(xgboost)
library(gbm)
library(nnet)


#Loading data
crimeTrain <- read.csv("crimeTrain.csv")
crimeTest <- read.csv("crimeTest.csv")
View(crimeTest)


# Data Wrangling
cTest <- read.csv("crimeTest.csv")

crimeTest$ViolentCrimesPerPop <- NA
crimeTest$Id <- NULL

crimeTrain <- rbind(crimeTrain,crimeTest)

Train <- crimeTrain
View(crimeTrain)

myfun<-function(x) mean(is.na(x))
apply(crimeTrain,2,myfun) #Getting percentage of missing values in each variable

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


plot(Train$FTPoliceFieldPerPop, Train$ViolentCrimesPerPop) 
plot(Train$county, Train$ViolentCrimesPerPop) #Target variable doesn't seem to be affected by county
str(Train$county) 
unique(Train$communityname) #has 1135 unique values. So, can be removed.
unique(Train$county) 


crimeTrain$county <- NULL

crimeTrain <- crimeTrain[,c(1:99)]

crimeTrain$LandArea <- Train$LandArea
crimeTrain$PopDens <- Train$PopDens
crimeTrain$PctUsePubTrans <- Train$PctUsePubTrans
crimeTrain$ViolentCrimesPerPop <- Train$ViolentCrimesPerPop
crimeTrain$communityname <- NULL
crimeTrain$X <- NULL
#OtherPerCap
sum(is.na(crimeTrain$OtherPerCap))
str(crimeTrain$OtherPerCap)
hist(crimeTrain$OtherPerCap)
unique(crimeTrain$OtherPerCap)
crimeTrain$OtherPerCap[is.na(crimeTrain$OtherPerCap)] <- getmode(crimeTrain$OtherPerCap)
#Replacing one missing value with mode value


summary(crimeTrain$householdsize)
summary(crimeTrain$PersPerOccupHous)
#As these both variables mean the same and also have similarl value, householdsize is removed
crimeTrain$householdsize <- NULL


hist(crimeTrain$ViolentCrimesPerPop)
hist(crimeTrain$pctUrban)
?apply
skew <- apply(crimeTrain, 2, skewness)
print(skew[skew > 3])

#Let us look into these variables with high skewness and transform them
str(crimeTrain$population)
crimeTrain$population <- crimeTrain$population + 0.01
symbox(crimeTrain$population, data = crimeTrain, powers=c(3,2,1,0,-0.5,-1,-2))
crimeTrain$population <- crimeTrain$population^(-0.5)

crimeTrain$numbUrban <- crimeTrain$numbUrban + 0.01
symbox(crimeTrain$numbUrban, data = crimeTrain, powers=c(3,2,1,0,-0.5,-1,-2))
crimeTrain$numbUrban <- log(crimeTrain$numbUrban)

crimeTrain$NumUnderPov <- crimeTrain$NumUnderPov + 0.01
symbox(crimeTrain$NumUnderPov, data = crimeTrain, powers=c(3,2,1,0,-0.5,-1,-2))
crimeTrain$NumUnderPov <- crimeTrain$NumUnderPov^(-0.5)

crimeTrain$NumIlleg <- crimeTrain$NumIlleg + 0.01
symbox(crimeTrain$NumIlleg, data = crimeTrain, powers=c(3,2,1,0,-0.5,-1,-2))
crimeTrain$NumIlleg <- log(crimeTrain$NumIlleg)

crimeTrain$NumImmig <- crimeTrain$NumImmig + 0.01
symbox(crimeTrain$NumImmig, data = crimeTrain, powers=c(3,2,1,0,-0.5,-1,-2))
crimeTrain$NumImmig <- crimeTrain$NumImmig^(-0.5)

crimeTrain$HousVacant <- crimeTrain$HousVacant + 0.01
symbox(crimeTrain$HousVacant, data = crimeTrain, powers=c(3,2,1,0,-0.5,-1,-2))
crimeTrain$HousVacant <- crimeTrain$HousVacant^(-0.5)

crimeTrain$NumInShelters <- crimeTrain$NumInShelters + 0.01
symbox(crimeTrain$NumInShelters, data = crimeTrain, powers=c(3,2,1,0,-0.5,-1,-2))
crimeTrain$NumInShelters <- crimeTrain$NumInShelters^(-1)

crimeTrain$state <- as.factor(crimeTrain$state)

ggplot(crimeTrain, aes(x = crimeTrain$medIncome, y = crimeTrain$ViolentCrimesPerPop)) + geom_point()
ggplot(crimeTrain, aes(x = crimeTrain$medFamInc, y = crimeTrain$ViolentCrimesPerPop)) + geom_point()
ggplot(crimeTrain, aes(x = crimeTrain$perCapInc, y = crimeTrain$ViolentCrimesPerPop)) + geom_point()
#From the above two plots we can remoce medIncome and medFamInc
crimeTrain$medIncome <- NULL
crimeTrain$medFamInc <- NULL
crimeTrain$numbUrban <- NULL
crimeTrain$NumUnderPov <- NULL

ggplot(crimeTrain, aes(x = crimeTrain$PctLess9thGrade, y = crimeTrain$ViolentCrimesPerPop)) + geom_point()
ggplot(crimeTrain, aes(x = crimeTrain$PctNotHSGrad, y = crimeTrain$ViolentCrimesPerPop)) + geom_point()
ggplot(crimeTrain, aes(x = crimeTrain$PctBSorMore, y = crimeTrain$ViolentCrimesPerPop)) + geom_point()
crimeTrain$PctLess9thGrade <- NULL

ggplot(crimeTrain, aes(x = crimeTrain$PctUnemployed, y = crimeTrain$ViolentCrimesPerPop)) + geom_point()
ggplot(crimeTrain, aes(x = crimeTrain$PctEmploy, y = crimeTrain$ViolentCrimesPerPop)) + geom_point()
cor(crimeTrain$PctNotHSGrad, crimeTrain$PctUnemployed)
#From the above correlation value and plots, Unemployed and PctNotHSGrad are correlated and similarly affecting target variable.
#So, we'll remove PctNotHSGrad to avoid overfitting.
crimeTrain$PctNotHSGrad <- NULL
cor(crimeTrain$PctEmploy, crimeTrain$PctUnemployed)

crimeTrain$PctEmplManu <- NULL
crimeTrain$PctEmplProfServ <- NULL

cor(crimeTrain$MalePctDivorce, crimeTrain$FemalePctDiv)
cor(crimeTrain$MalePctDivorce, crimeTrain$TotalPctDiv)
#From above two we can remove MalePctDivorce and FemalePctDiv
crimeTrain$MalePctDivorce <- NULL
crimeTrain$FemalePctDiv <- NULL
crimeTrain$NumIlleg <- NULL
crimeTrain$PctKids2Par <- NULL

cor(crimeTrain$PctYoungKids2Par,crimeTrain$PctTeen2Par)
cor(crimeTrain$PctYoungKids2Par,crimeTrain$ViolentCrimesPerPop)
cor(crimeTrain$PctTeen2Par,crimeTrain$ViolentCrimesPerPop)
crimeTrain$PctTeen2Par <- NULL
crimeTrain$PctWorkMomYoungKids <- NULL


cor(crimeTrain$PctRecImmig10, crimeTrain$PctRecentImmig)
cor(crimeTrain$PctImmigRecent, crimeTrain$PctImmigRec10)
cor(crimeTrain$PctRecentImmig, crimeTrain$ViolentCrimesPerPop)
cor(crimeTrain$PctImmigRec10, crimeTrain$ViolentCrimesPerPop)

crimeTrain$PctRecImmig5 <- NULL
crimeTrain$PctRecImmig8 <- NULL
crimeTrain$PctImmigRec5 <- NULL
crimeTrain$PctImmigRec8 <- NULL

cor(crimeTrain$PctSpeakEnglOnly,crimeTrain$PctNotSpeakEnglWell)
crimeTrain$PctLargHouseOccup <- NULL
crimeTrain$PersPerOccupHous <- NULL
crimeTrain$PersPerOwnOccHous <- NULL
crimeTrain$PersPerRentOccHous <- NULL
crimeTrain$MedYrHousBuilt <- NULL

ggplot(crimeTrain, aes(x = crimeTrain$MedOwnCostPctInc, y = crimeTrain$ViolentCrimesPerPop)) + geom_point()
ggplot(crimeTrain, aes(x = crimeTrain$MedOwnCostPctIncNoMtg, y = crimeTrain$ViolentCrimesPerPop)) + geom_point()
cor(crimeTrain$MedOwnCostPctInc, crimeTrain$MedOwnCostPctIncNoMtg)
cor(crimeTrain$MedOwnCostPctInc, crimeTrain$ViolentCrimesPerPop)
cor(crimeTrain$ViolentCrimesPerPop, crimeTrain$MedOwnCostPctIncNoMtg)

cor(crimeTrain$OwnOccMedVal, crimeTrain$OwnOccHiQuart)
cor(crimeTrain$OwnOccMedVal, crimeTrain$OwnOccLowQuart)
cor(crimeTrain$RentMedian, crimeTrain$RentLowQ)
cor(crimeTrain$RentMedian, crimeTrain$RentHighQ)
crimeTrain$OwnOccHiQuart <- NULL
crimeTrain$OwnOccLowQuart <- NULL
crimeTrain$RentLowQ <- NULL
crimeTrain$RentHighQ <- NULL
crimeTrain$MedRent <- NULL

cor(crimeTrain$RentMedian, crimeTrain$MedRent)

cor(crimeTrain$PctSameCity85, crimeTrain$PctSameHouse85)
crimeTrain$PctSameHouse85 <- NULL

par(mar = rep(0,4))
nums <- unlist(lapply(crimeTrain, is.numeric))
crime_corr <- crimeTrain[,nums]
corr_c <- cor(crime_corr)
corrplot(corr_c, method = "square", tl.cex = 0.6, tl.offset = 0.4,tl.srt = 90, cl.ratio = 0.3)
#The following observations can be made from the above correlation plot.
#1. age12t21 and age12t29 have correlation. This may be due to overlapping in data. So, we,ll make age12t29 to age21t29
#2. age65up and pctWSocSec are highly correlated and pctWSocSec is more related to ViolentCrimesPerPop.
#SO, we'll remove age65up to avoid overfitting.
#3. perCapInc and whitePerCap are highly correlated and seems to be similarly correlated with other variables. 
#So, wel
#4. PctBSorMore and PctOccupMgmtProf are highly correlated and PctOccupMgmtProf seems to be more related to other variables.
#So, we'll remove PctBSorMore. 
#5. PctFam2Par and PctYoungKids2Par are highly correlated. PctYoungKids2Par is removed to avoid multicollinearity.
#6. PctPersOwnOccup and PctHousOwnOcc are highly correlated and are similarly related to other variables.
# So, we'll remove PctPersOwnOccup
#7. PctRecentImmig and PctForeignBorn are highly correlated. PctForeignBorn is removed to avoid multicollinearity.
#8. OwnOccMedVal and RentMedian are highly correlated. OwnOccMedVal is removed to avoid multicollinearity.
#9. Also, other variables having correlation with ViolentCrimesPerPop are racepctblack, racepctwhite,pctWInvInc, pctWPubAsst,PctPopUnderPov,
# PctUnemployed,PctFam2Par, PersPerOwnOccHous, HousVacant.


crimeTrain$agePct12t21 <- NULL
cor(crimeTrain$perCapInc, crimeTrain$whitePerCap)
crimeTrain$agePct65up <- NULL
crimeTrain$PctBSorMore <- NULL
crimeTrain$PctYoungKids2Par <- NULL
crimeTrain$PctPersOwnOccup <- NULL
crimeTrain$PctForeignBorn <- NULL
crimeTrain$OwnOccMedVal <- NULL
crimeTrain$agePct16t24 <- NULL

dev.off()
cor(crimeTrain$PctFam2Par, crimeTrain$PctYoungKids2Par)
cor(crimeTrain$PctFam2Par, crimeTrain$ViolentCrimesPerPop)
cor(crimeTrain$ViolentCrimesPerPop, crimeTrain$OwnOccMedVal)
cor(crimeTrain$RentMedian, crimeTrain$ViolentCrimesPerPop)
cor(crimeTrain$agePct12t21, crimeTrain$agePct16t24)


#######################################################  PLS ###########################################
my_full <- crimeTrain
crimeTrain1 <- crimeTrain[1:1200,]
crimeTest1 <- crimeTrain[1201:1850,]
View(crimeTest1)
View(crimeTrain1)

set.seed(100)

train_index <- sample(1:nrow(crimeTrain1), 1000)
crimeTrain_train <- crimeTrain1[train_index,]
crimeTrain_test <- crimeTrain1[-train_index,]

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
PLS_fit <- train(ViolentCrimesPerPop~., data = crimeTrain_train, method = "pls", tuneLength = 20, trcontrol = train_control)
?train
plot(PLS_fit ,main = "RMSE PLS Crime Prediction", xlab="components")

PLS_fit$results

summary(PLS_fit)
??RMSEP

predict_pls <- predict(PLS_fit, crimeTrain_test, ncomp = 7)
plot(predict_pls)
postResample(pred = predict_pls,obs = crimeTrain_test$ViolentCrimesPerPop)

predict_pls1 <- predict(PLS_fit, crimeTest1,ncomp = 7)
PLS_submission <- cbind(cTest$Id, predict_pls1)
write.csv(PLS_submission, file = "PLS_submission1.csv")


predict_pls2 <- predict(PLS_fit, crimeTrain_train, ncomp = 7)
plot(predict_pls)
postResample(pred = predict_pls2,obs = crimeTrain_train$ViolentCrimesPerPop)

######################################### RIDGE ###############################################

x <- model.matrix(ViolentCrimesPerPop~., crimeTrain_train)   # converting into Matrix
y <- crimeTrain_train$ViolentCrimesPerPop

grid <- 10^seq(10,-2, length = 100)
ridge_fit <- glmnet(x, y, alpha = 0, lambda = grid)

plot(ridge_fit)
set.seed(1)
cv.out <- cv.glmnet(x, y, alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min #0.03745824
ridge_test <- crimeTrain_test
ridge_test$ViolentCrimesPerPop <-NULL

bestlam


newX <- model.matrix(~.-ViolentCrimesPerPop,data=crimeTrain_test)
predict_ridge <- predict(ridge_fit, s = bestlam, newx = newX)
postResample(pred = predict_ridge,obs = crimeTrain_test$ViolentCrimesPerPop)

predict_ridge2 <- predict(ridge_fit, s = bestlam, newx = x)
postResample(pred = predict_ridge2,obs = crimeTrain_train$ViolentCrimesPerPop)


crimeTest1$ViolentCrimesPerPop <-NULL
?model.matrix
newx1 <- model.matrix(~.,data = crimeTest1)
predict_ridge1 <- predict(ridge_fit, s = bestlam, newx = newx1)
ridge_submission <- cbind(cTest$Id, predict_ridge1)
write.csv(ridge_submission, file = "ridge_submission.csv")

############################################################## LASSO ############################################

lasso_fit <- glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso_fit)

set.seed(2)
cv.out1 <- cv.glmnet(x, y, alpha = 1)
plot(cv.out1)
bestlam1 <- cv.out1$lambda.min
bestlam1 #

predict_lasso <- p0.001547779redict(lasso_fit, s = bestlam1, newx = newX)
postResample(pred = predict_lasso,obs = crimeTrain_test$ViolentCrimesPerPop)

predict_lasso2<- predict(lasso_fit, s = bestlam1, newx = x)
postResample(pred = predict_lasso2,obs = crimeTrain_train$ViolentCrimesPerPop)

predict_lasso1 <- predict(lasso_fit, s = bestlam1, newx =  newx1)
lasso_submission <- cbind(cTest$Id, predict_lasso1)
write.csv(lasso_submission, file = "lasso_submission1.csv")

################################################################### ELASTIC NET ###############################################

enetGrid <- expand.grid(lambda=seq(0,0.001,length=100),fraction=seq(0.1,0.9,length=30))


Enet_fit <- train(ViolentCrimesPerPop~.,
                  data=crimeTrain_train,
                  method="enet",
                  trControl=train_control,
                  tuneGrid=enetGrid)


Enet_fit$bestTune
summary(Enet_fit)
  plot(Enet_fit$results)

plot.enet(Enet_fit, xvar = "fraction")


predict_Enet <- predict(Enet_fit, crimeTrain_test)
plot(predict_Enet)
postResample(pred = predict_Enet,obs = crimeTrain_test$ViolentCrimesPerPop)

predict_Enet1 <- predict(Enet_fit, crimeTest1)
Enet_submission <- cbind(cTest$Id, predict_Enet1)
write.csv(Enet_submission, file = "ENET_submission.csv")

predict_Enet2 <- predict(Enet_fit,crimeTrain1)
plot(predict_Enet)
postResample(pred = predict_Enet2,obs = crimeTrain1$ViolentCrimesPerPop)

########################################################### SVM LINEAR #######################################

train_control <- trainControl(method = "repeatedcv", number = 5, repeats = 5)  # 5 fold cross validation

grid2 <- expand.grid(C = seq(1,10,length=15))

SVM_fit1 <- train(ViolentCrimesPerPop~.,
                  data=crimeTrain_train,
                  method="svmLinear",        #linear Kernal
                   trControl=train_control,
                  tuneGrid = grid2)

SVM_fit1  
plot(SVM_fit1)


predict_SVM1 <- predict(SVM_fit1, crimeTrain_test)
plot(predict_SVM1)
postResample(pred = predict_SVM1,obs = crimeTrain_test$ViolentCrimesPerPop)

predict_SVM1 <- predict(SVM_fit1, crimeTest1)

postResample(pred = predict_SVM1,obs = crimeTrain1$ViolentCrimesPerPop)

SVM_submission1 <- cbind(cTest$Id, predict_SVM1)
write.csv(SVM_submission1, file = "SVM_submission.csv")


svm_fit <- train(ViolentCrimesPerPop~., data = crimeTrain_train, method = "svmLinear",
                 preProc = c("center", "scale"), tuneLength = 10,
                 trControl = train_control)

print(svm_fit)

plot(svm_fit, 
     plotType = "scatter",
     metric = "RMSE")

#################################### Neural Network ##############################################################

ggplot(data = crimeTrain_train, aes(x = crimeTrain_train$state, y = crimeTrain_train$ViolentCrimesPerPop)) + 
  geom_point() + geom_boxplot()

crimeTrain$state[crimeTrain$state == '10'|crimeTrain$state == '11'|crimeTrain$state == '20'|crimeTrain$state == '32' ] <- '37'
unique(crimeTrain_train$state)

nnet_fit <- nnet(ViolentCrimesPerPop~., data = crimeTrain1, size = 3,
                 decay = 0.01, linout = TRUE, trace = F, maxit = 600,
                 MaxNWts = 5*(ncol(crimeTrain1)+1)+ 5+ 1)

nnet_predict <- predict(nnet_fit, crimeTrain_test)
nnet_predict
postResample(nnet_predict, crimeTrain_test$ViolentCrimesPerPop)

nnet_predict2 <- predict(nnet_fit, crimeTest1)
nnet_submission <- data.frame(cTest$Id, nnet_predict2)
write.csv(nnet_submission, "submission.csv", row.names = F)

nnet_fit1 <- avNNet(ViolentCrimesPerPop~., data = crimeTrain1, size = 3,
                    decay = 0.01, linout = TRUE, trace = F, maxit = 600,
                    MaxNWts = 5*(ncol(crimeTrain1)+1)+ 5+ 1)

nnet_predict3 <- predict(nnet_fit1, crimeTrain_test)
postResample(nnet_predict3, crimeTrain_test$ViolentCrimesPerPop)

nnet_predict4 <- predict(nnet_fit1, crimeTest1)
nnet_submission1 <- data.frame(cTest$Id, nnet_predict4)
write.csv(nnet_submission1, "submission1.csv", row.names = F)

fit_lm <- lm(ViolentCrimesPerPop~., data = crimeTrain_train)
summary(fit_lm)
vif(fit_lm)

RSS <- c(crossprod(fit_lm$residuals)) 
MSE <- RSS / length(fit_lm$residuals) #Mean squared error:
RMSE <- sqrt(MSE) #0.07785005 #Root Mean Squared Error
RMSE

fit_lm1 <- stepAIC(fit_lm, direction = "both")

summary(fit_lm1)
vif(fit_lm1)
RSS <- c(crossprod(fit_lm1$residuals)) 
MSE <- RSS / length(fit_lm1$residuals) #Mean squared error:
RMSE <- sqrt(MSE) # 0.07964134 #Root Mean Squared Error
RMSE


par(mfrow = c(2,2))
plot(fit_lm1)
#The first plot clearly shows that residuals of our model are spread equally along the horizontal line, indicating that our model do not have non-linear relationship. There are few variables which are far from the horizontal line.
#The QQ Plot indicates residuals are clearly normally distributed as they have all the values held on the dashed line. 
#The Standardized and fitted values plot shows few of the variables are not transformed but many of them are falls I straight line.
#Standardized residuals vs leverage indicates that 402 observation is outside cooks distance.  617 and 754 have high leverage.
dev.off()
plot(cooks.distance(fit_lm1), rstudent(fit_lm1))


influencePlot(fit_lm1)

ncvTest(fit_lm1)

plot(fit_lm1$fitted.values, fit_lm1$residuals)



