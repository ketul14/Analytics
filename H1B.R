
#Read the CSV
H1B <- read.csv("H1B_Disclosure_Clean_1.6.csv", header = T)
str(H1B)
summary(H1B)


H1B$DECISION_DAYS <- NULL

str(H1B)
#Dummy Variable
#H1B.dmy <- dummyVars( "~ ." , H1B, fullRank = T)
#H1B.d <- data.frame(predict(H1B.dmy, newdata = H1B))
#head(H1B)
#str(H1B)

#Correlation between predictors and dependent variable
#cor(H1B.d, H1B.d$CASE_STATUS)


#str(H1B)
#str(H1B.d)

## Data Splitting for categorical 
set.seed(99)
trainIndex <- createDataPartition(H1B$CASE_STATUS, p=.7, list=F)
nrow(trainIndex)
H1B.imbal.train <- H1B[trainIndex,]
H1B.imbal.test <- H1B[-trainIndex,]
str(H1B.imbal.train)
str(H1B.imbal.test)

#checking the class imbalance
table(H1B.imbal.train$CASE_STATUS)

## Create train data split for DV and Predictors
x.H1B.train <- H1B.imbal.train [,-ncol(H1B.imbal.train)]
y.H1B.train <- H1B.imbal.train [, ncol(H1B.imbal.train)]

## Create test data split for DV and Predictors
y.H1B.test <- H1B.imbal.test$CASE_STATUS
x.H1B.test <- H1B.imbal.test [,-ncol(H1B.imbal.test)]
head(y.H1B.test)
colnames(x.H1B.test)
nrow(x.H1B.test)

# SMOTE Imbalancing
library(grid)
library(DMwR)
set.seed(99)
nrow(H1B.imbal.train)
H1B.smote.train <- SMOTE(CASE_STATUS ~ ., H1B.imbal.train, perc.over = 5020, perc.under=102)
table(H1B.smote.train$CASE_STATUS)
prop.table(table(H1B.smote.train$CASE_STATUS))

## Create test data split for DV and Predictors
y.H1B.smote.train <- H1B.smote.train$CASE_STATUS
x.H1B.smote.train <- H1B.smote.train [,-ncol(H1B.smote.train)]
head(y.H1B.smote.train)
colnames(x.H1B.smote.train)


#Dummy Variable
H1B.dmy <- dummyVars( "~ ." , H1B[,-24], fullRank = T)
H1B.d <- data.frame(predict(H1B.dmy, newdata = H1B[,-24]),H1B$CASE_STATUS)
head(H1B)
str(H1B.d)


# Data Exploration and ploting pair
library(car)
library(hexbin)

subset <- data.frame(H1B$NEW_EMPLOYMENT, H1B$PREVAILING_WAGE..YEARLY., H1B$SOC,
                     H1B$NAICS, H1B$CASE_STATUS) 

bitmap("Pairs.tiff", height = 4, width = 4, units = 'in', type="tifflzw", res=300)
pairs(subset)
dev.off()


#####Data splitting for numerical 
set.seed(99)
trainIndex.d <- createDataPartition(H1B.d$H1B.CASE_STATUS, p=.7, list=F)
nrow(trainIndex.d)
H1B.imbal.train.d <- H1B.d[trainIndex.d,]
H1B.imbal.test.d <- H1B.d[-trainIndex.d,]
str(H1B.imbal.train.d)
str(H1B.imbal.test.d)
colnames(H1B.imbal.train.d)
colnames(H1B.imbal.test.d)

#checking the class imbalance
table(H1B.imbal.train.d$H1B.CASE_STATUS)

## Create train data split for DV and Predictors
x.H1B.train.d <- H1B.imbal.train.d [,-ncol(H1B.imbal.train.d)]
y.H1B.train.d <- H1B.imbal.train.d [, ncol(H1B.imbal.train.d)]
str(x.H1B.train.d)
str(y.H1B.train.d)
colnames(x.H1B.train.d)
names(y.H1B.train.d)

## Create test data split for DV and Predictors
y.H1B.test.d <- H1B.imbal.test.d [, ncol(H1B.imbal.train.d)]
x.H1B.test.d <- H1B.imbal.test.d [,-ncol(H1B.imbal.test.d)]
head(y.H1B.test.d)
colnames(x.H1B.test.d)


# SMOTE Imbalancing
library(DMwR)
library(grid)
set.seed(99)
nrow(H1B.imbal.train.d)
H1B.smote.train.d <- SMOTE(H1B.CASE_STATUS ~ ., H1B.imbal.train.d, perc.over = 5020, perc.under=102)
table(H1B.smote.train.d$H1B.CASE_STATUS)
prop.table(table(H1B.smote.train.d$H1B.CASE_STATUS))

## Create test data split for DV and Predictors
y.H1B.smote.train.d <- H1B.smote.train.d$H1B.CASE_STATUS
x.H1B.smote.train.d <- H1B.smote.train.d [,-ncol(H1B.smote.train.d)]
head(y.H1B.smote.train.d)
colnames(x.H1B.smote.train.d)

###### Principal Component Analysis (PCA)
H1B.smote.prin_comp <- prcomp(x.H1B.smote.train.d, scale. = T)
names(H1B.smote.prin_comp)
H1B.smote.prin_comp$center
biplot(H1B.smote.prin_comp, scale = 0)

#compute standard deviation of each principal component
H1B.smote.std_dev <- H1B.smote.prin_comp$sdev

#compute variance
H1B.smote.pr_var <- H1B.smote.std_dev^2

#check variance of first 10 components
H1B.smote.pr_var[1:10]

#proportion of variance explained
H1B.smote.prop_varex <- H1B.smote.pr_var/sum(H1B.smote.pr_var)
H1B.smote.prop_varex[1:20]

#screen plot
plot(H1B.smote.prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
#cumulative scree plot
plot(cumsum(H1B.smote.prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#add a training set with principal components
H1B.smote.pcr.train <- data.frame(CASE_STATUS = y.H1B.smote.train.d, H1B.smote.prin_comp$x)

#we are interested in first 30 PCAs
H1B.smote.pcr.train <- H1B.smote.pcr.train[,1:51]

y.H1B.smote.pcr.train <- H1B.smote.pcr.train[, 1]
x.H1B.smote.pcr.train <- H1B.smote.pcr.train[, -1]

#transform test into PCA
H1B.smote.pcr.test <- predict(H1B.smote.prin_comp, newdata = H1B.imbal.test.d)
H1B.smote.pcr.test <- as.data.frame(H1B.smote.pcr.test)

#select the first 50 components
H1B.smote.pcr.test <- H1B.smote.pcr.test[,1:50]

## to run parallel with 4 cores
cl <- makeCluster(4)
registerDoParallel(cl)

#some parameters to control the sampling during parameter tuning and testing
#10 fold crossvalidation, using 10-folds instead of 10 to reduce computation time in class demo, use 10 and with more computation to spare use
#repeated cv
set.seed(99)
ctrl <- trainControl(method="cv", number=10,
                     classProbs=TRUE,
                     #function used to measure performance
                     summaryFunction = twoClassSummary, #multiClassSummary for non binary
                     allowParallel =  TRUE) 


#####Logistic without PCA    
set.seed(99)
m.H1B.smote.glm <- readRDS("m.H1B.smote.glm.rdata")
m.H1B.smote.glm
getTrainPerf(m.H1B.smote.glm)
varImp(m.H1B.smote.glm)

m.H1B.smote.glm

#Making prediction on test data
p.H1B.smote.glm <- predict(m.H1B.smote.glm, x.H1B.test)
p.H1B.smote.glm.prob <- predict(m.H1B.smote.glm, x.H1B.test, type="prob")

confusionMatrix(p.H1B.smote.glm,y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.glm.newthresh <- factor(ifelse(p.H1B.smote.glm.prob[[1]]>0.45, "CERTIFIED", "DENIED"))
confusionMatrix(p.H1B.smote.glm.newthresh, y.H1B.test)

test.H1B.smote.glm.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.glm.prob[[1]])
plot(test.H1B.smote.glm.roc)

newthresh<- coords(test.H1B.smote.glm.roc, x="best", best.method="closest.topleft")

auc(test.H1B.smote.glm.roc)

#Logistic with PCA 
set.seed(99)
m.H1B.smote.pca.glm <- train(y=y.H1B.smote.pcr.train, x=x.H1B.smote.pcr.train,  
                             method = "glm",
                             metric = "ROC", tuneLength = 7,
                             trControl = ctrl)

m.H1B.smote.pca.glm
getTrainPerf(m.H1B.smote.pca.glm)

varImp(m.H1B.smote.pca.glm)
#Making prediction on test data
p.H1B.smote.pca.glm <- predict(m.H1B.smote.pca.glm,H1B.smote.pcr.test)
p.H1B.smote.pca.glm.prob <- predict(m.H1B.smote.pca.glm, H1B.smote.pcr.test, type="prob")

confusionMatrix(p.H1B.smote.pca.glm,y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.pca.glm.newthresh <- factor(ifelse(p.H1B.smote.pca.glm.prob[[1]]>0.40, "CERTIFIED", "DENIED"))
p.H1B.smote.pca.glm.newthresh 
confusionMatrix(p.H1B.smote.pca.glm.newthresh, y.H1B.test)

test.H1B.smote.pca.glm.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.pca.glm.prob[[1]])
plot(test.H1B.smote.pca.glm.roc)

newthresh<- coords(test.H1B.smote.pca.glm.roc, x="best", best.method="closest.topleft")
plot(p.H1B.smote.pca.glm.newthresh)
auc(test.H1B.smote.pca.glm.roc)


######Linear Discriminate Analysis without PCA
m.H1B.smote.lda <- readRDS("m.H1B.smote.lda.rdata")
m.H1B.smote.lda
getTrainPerf(m.H1B.smote.lda)
varImp(m.H1B.smote.lda)

p.H1B.smote.lda <- predict(m.H1B.smote.lda,x.H1B.test.d)
p.H1B.smote.lda.prob <- predict(m.H1B.smote.lda, x.H1B.test.d, type="prob")

confusionMatrix(p.H1B.smote.lda,y.H1B.test.d) #calc accuracies with confuction matrix on test set

p.H1B.smote.lda.newthresh <- factor(ifelse(p.H1B.smote.lda.prob[[1]]>0.40, "CERTIFIED", "DENIED"))
levels(p.H1B.smote.lda.newthresh) 
confusionMatrix(p.H1B.smote.lda.newthresh, y.H1B.test.d)

test.H1B.smote.lda.roc<- roc(response= y.H1B.test.d, predictor= p.H1B.smote.lda.prob[[1]])
plot(test.H1B.smote.lda.roc)

newthresh<- coords(test.H1B.smote.lda.roc, x="best", best.method="closest.topleft")

p.H1B.smote.lda.prob [[1]]
levels(p.H1B.smote.lda.newthresh)

auc(test.H1B.smote.lda.roc)


#Linear Discriminate Analysis with PCA
m.H1B.smote.pca.lda <- readRDS("m.H1B.smote.pca.lda.rdata")
m.H1B.smote.pca.lda 
getTrainPerf(m.H1B.smote.pca.lda)
varImp(m.H1B.smote.pca.lda)

#Making prediction on test data
p.H1B.smote.pca.lda <- predict(m.H1B.smote.pca.lda,H1B.smote.pcr.test)
p.H1B.smote.pca.lda.prob <- predict(m.H1B.smote.pca.lda, H1B.smote.pcr.test, type="prob")

confusionMatrix(p.H1B.smote.pca.lda,y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.pca.lda.newthresh <- factor(ifelse(p.H1B.smote.pca.lda.prob[[1]]>0.45, "CERTIFIED", "DENIED"))
p.H1B.smote.pca.lda.newthresh 
confusionMatrix(p.H1B.smote.pca.lda.newthresh, y.H1B.test)

test.H1B.smote.pca.lda.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.pca.lda.prob[[1]])
plot(test.H1B.smote.pca.lda.roc)

newthresh<- coords(test.H1B.smote.pca.lda.roc, x="best", best.method="closest.topleft")
plot(p.H1B.smote.pca.lda.newthresh)
auc(test.H1B.smote.pca.lda.roc)


##Decision Tree without PCA
m.H1B.smote.rpart <- readRDS("m.H1B.smote.rpart.rdata")
m.H1B.smote.rpart

getTrainPerf(m.H1B.smote.rpart)
varImp(m.H1B.smote.rpart)

#Making prediction on test data
p.H1B.smote.rpart <- predict(m.H1B.smote.rpart,x.H1B.test)
p.H1B.smote.rpart.prob <- predict(m.H1B.smote.rpart, x.H1B.test, type="prob")

confusionMatrix(p.H1B.smote.rpart,y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.rpart.newthresh <- factor(ifelse(p.H1B.smote.rpart.prob[[1]]>0.69, "CERTIFIED", "DENIED"))
p.H1B.smote.rpart.newthresh 
confusionMatrix(p.H1B.smote.rpart.newthresh, y.H1B.test)

test.H1B.smote.rpart.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.rpart.prob[[1]])
plot(test.H1B.smote.rpart.roc)

newthresh<- coords(test.H1B.smote.rpart.roc, x="best", best.method="closest.topleft")
plot(p.H1B.smote.rpart.newthresh)
auc(test.H1B.smote.rpart.roc)

#Plotting DT using rpart
library(rpart)
set.seed(99)
smote.fit <- rpart(y.H1B.smote.train~. ,data=x.H1B.smote.train,  
                   method="class",
                   control=rpart.control(minsplit=1),
                   parms=list(split='information'))
plot(smote.fit)
library(rpart.plot)
rpart.plot(smote.fit, type=4, extra=2,clip.right.labs=FALSE, varlen=0, faclen=3)
rpart.plot(smote.fit)


printcp(smote.fit) #display crossvalidated error for each tree size
plotcp(smote.fit) #plot cv error


#we can grab this from the plotcp table automatically with 
Smote.opt.cp <- smote.fit$cptable[which.min(smote.fit$cptable[,"xerror"]),"CP"]

#lets prune the tree
Smote.fit.pruned <- prune(smote.fit,cp=0.025553)

#lets review the final tree
rpart.plot(Smote.fit.pruned)

##Decision Tree with PCA
m.H1B.smote.pca.rpart <- readRDS("m.H1B.smote.pca.rpart.rdata")
m.H1B.smote.pca.rpart 
getTrainPerf(m.H1B.smote.pca.rpart)
varImp(m.H1B.smote.pca.rpart)

#Making prediction on test data
p.H1B.smote.pca.rpart <- predict(m.H1B.smote.pca.rpart,H1B.smote.pcr.test)
p.H1B.smote.pca.rpart.prob <- predict(m.H1B.smote.pca.rpart, H1B.smote.pcr.test, type="prob")

confusionMatrix(p.H1B.smote.pca.rpart,y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.pca.rpart.newthresh <- factor(ifelse(p.H1B.smote.pca.rpart.prob[[1]]>0.33, "CERTIFIED", "DENIED"))
p.H1B.smote.pca.rpart.newthresh 
confusionMatrix(p.H1B.smote.pca.rpart.newthresh, y.H1B.test)

test.H1B.smote.pca.rpart.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.pca.rpart.prob[[1]])
plot(test.H1B.smote.pca.rpart.roc)

newthresh<- coords(test.H1B.smote.pca.rpart.roc, x="best", best.method="closest.topleft")
plot(p.H1B.smote.pca.rpart.newthresh)
auc(test.H1B.smote.pca.rpart.roc)

##Naive Bayes without PCA
library(klaR)
m.H1B.smote.nb <- readRDS("m.H1B.smote.nb.rdata")
m.H1B.smote.nb
getTrainPerf(m.H1B.smote.nb)
varImp(m.H1B.smote.nb)

#Making prediction on test data
p.H1B.smote.nb <- predict(m.H1B.smote.nb,x.H1B.test)
p.H1B.smote.nb.prob <- predict(m.H1B.smote.nb, x.H1B.test, type="prob")

confusionMatrix(p.H1B.smote.nb,y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.nb.newthresh <- factor(ifelse(p.H1B.smote.nb.prob[[1]]>0.30, "CERTIFIED", "DENIED"))
p.H1B.smote.nb.newthresh 
confusionMatrix(p.H1B.smote.nb.newthresh, y.H1B.test)

test.H1B.smote.nb.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.nb.prob[[1]])
plot(test.H1B.smote.nb.roc)

newthresh<- coords(test.H1B.smote.nb.roc, x="best", best.method="closest.topleft")
plot(p.H1B.smote.nb.newthresh)
auc(test.H1B.smote.nb.roc)


##Naive Bayes with PCA
m.H1B.smote.pca.nb <- readRDS("m.H1B.smote.pca.nb.rdata")
m.H1B.smote.pca.nb
getTrainPerf(m.H1B.smote.pca.nb)
varImp(m.H1B.smote.pca.nb)

#Making prediction on test data
p.H1B.smote.pca.nb <- predict(m.H1B.smote.pca.nb,H1B.smote.pcr.test)
p.H1B.smote.pca.nb.prob <- predict(m.H1B.smote.pca.nb, H1B.smote.pcr.test, type="prob")

confusionMatrix(p.H1B.smote.pca.nb,y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.pca.nb.newthresh <- factor(ifelse(p.H1B.smote.pca.nb.prob[[1]]>0.35, "CERTIFIED", "DENIED"))
p.H1B.smote.pca.nb.newthresh 
confusionMatrix(p.H1B.smote.pca.nb.newthresh, y.H1B.test)

test.H1B.smote.pca.nb.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.pca.nb.prob[[1]])
plot(test.H1B.smote.pca.nb.roc)

newthresh<- coords(test.H1B.smote.pca.nb.roc, x="best", best.method="closest.topleft")
plot(p.H1B.smote.pca.nb.newthresh)
auc(test.H1B.smote.pca.nb.roc)

####GAM without PCA
m.H1B.smote.gam <- library(splines)
library(gam)
set.seed(99)
m.H1B.smote.gam <- readRDS("m.H1B.smote.gam.rdata")
#saveRDS(m.H1B.smote.gam, file = "m.H1B.smote.gam.rdata")
m.H1B.smote.gam
getTrainPerf(m.H1B.smote.gam)
varImp(m.H1B.smote.gam)

p.H1B.smote.gam <- predict(m.H1B.smote.gam,x.H1B.test.d)
p.H1B.smote.gam.prob <- predict(m.H1B.smote.gam, x.H1B.test.d, type="prob")

confusionMatrix(p.H1B.smote.gam,y.H1B.test.d) #calc accuracies with confuction matrix on test set

p.H1B.smote.gam.newthresh <- factor(ifelse(p.H1B.smote.gam.prob[[1]]>0.60, "CERTIFIED", "DENIED"))
confusionMatrix(p.H1B.smote.gam.newthresh, y.H1B.test.d)

test.H1B.smote.gam.roc<- roc(response= y.H1B.test.d, predictor= p.H1B.smote.gam.prob[[1]])
plot(test.H1B.smote.gam.roc)

newthresh<- coords(test.H1B.smote.gam.roc, x="best", best.method="closest.topleft")

p.H1B.smote.gam.prob[[1]]
levels(p.H1B.smote.gam.newthresh)
plot(p.H1B.smote.gam)

auc(test.H1B.smote.gam.roc)

##GAM with PCA
m.H1B.smote.pca.gam <- readRDS("m.H1B.smote.pca.gam.rdata")
m.H1B.smote.pca.gam
getTrainPerf(m.H1B.smote.pca.gam)
varImp(m.H1B.smote.pca.gam)

#Making prediction on test data
p.H1B.smote.pca.gam <- predict(m.H1B.smote.pca.gam,H1B.smote.pcr.test)
p.H1B.smote.pca.gam.prob <- predict(m.H1B.smote.pca.gam, H1B.smote.pcr.test, type="prob")

confusionMatrix(p.H1B.smote.pca.gam,y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.pca.gam.newthresh <- factor(ifelse(p.H1B.smote.pca.gam.prob[[1]]>0.40, "CERTIFIED", "DENIED"))
confusionMatrix(p.H1B.smote.pca.gam.newthresh, y.H1B.test)

test.H1B.smote.pca.gam.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.pca.gam.prob[[1]])
plot(test.H1B.smote.pca.gam.roc)

newthresh<- coords(test.H1B.smote.pca.gam.roc, x="best", best.method="closest.topleft")
plot(p.H1B.smote.pca.gam.newthresh)
auc(test.H1B.smote.pca.gam.roc)

##PLS without PCA
m.H1B.smote.pls <- readRDS("m.H1B.smote.pls.rdata")
m.H1B.smote.pls
getTrainPerf(m.H1B.smote.pls)
varImp(m.H1B.smote.pls)

p.H1B.smote.pls <- predict(m.H1B.smote.pls,x.H1B.test.d)
p.H1B.smote.pls.prob <- predict(m.H1B.smote.pls, x.H1B.test.d, type="prob")

confusionMatrix(p.H1B.smote.pls,y.H1B.test.d) #calc accuracies with confuction matrix on test set

p.H1B.smote.pls.newthresh <- factor(ifelse(p.H1B.smote.pls.prob[[1]]>0.45, "CERTIFIED", "DENIED"))
levels(p.smote.pls.newthresh) 
confusionMatrix(p.H1B.smote.pls.newthresh, y.H1B.test.d)

test.smote.pls.roc<- roc(response= y.H1B.test.d, predictor= p.H1B.smote.pls.prob[[1]])
plot(test.smote.pls.roc)

newthresh<- coords(test.smote.pls.roc, x="best", best.method="closest.topleft")

p.smote.pls.prob[[1]]
levels(p.smote.pls.newthresh)
plot(p.smote.pls)

auc(test.smote.pls.roc)

##PLS with PCA
set.seed(99)
m.H1B.smote.pca.pls <- readRDS("m.H1B.smote.pca.pls.rdata")

saveRDS(m.H1B.smote.pca.pls, file = "m.H1B.smote.pca.pls.rdata")
m.H1B.smote.pca.pls
getTrainPerf(m.H1B.smote.pca.pls)
varImp(m.H1B.smote.pca.pls)

#Making prediction on test data
p.H1B.smote.pca.pls <- predict(m.H1B.smote.pca.pls,H1B.smote.pcr.test)
p.H1B.smote.pca.pls.prob <- predict(m.H1B.smote.pca.pls, H1B.smote.pcr.test, type="prob")

confusionMatrix(p.H1B.smote.pca.pls, y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.pca.pls.newthresh <- factor(ifelse(p.H1B.smote.pca.pls.prob[[1]]>0.45, "CERTIFIED", "DENIED"))
confusionMatrix(p.H1B.smote.pca.pls.newthresh, y.H1B.test)

test.H1B.smote.pca.pls.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.pca.pls.prob[[1]])
plot(test.H1B.smote.pca.pls.roc)

newthresh<- coords(test.H1B.smote.pca.pls.roc, x="best", best.method="closest.topleft")
plot(p.H1B.smote.pca.pls.newthresh)
auc(test.H1B.smote.pca.pls.roc)


##Glmnet without PCA
m.H1B.smote.glmnet <- readRDS("m.H1B.smote.glmnet.rdata")
m.H1B.smote.glmnet
getTrainPerf(m.H1B.smote.glmnet)
varImp(m.H1B.smote.glmnet)
#Making prediction on test data
p.H1B.smote.glmnet <- predict(m.H1B.smote.glmnet,x.H1B.test.d)
p.H1B.smote.glmnet.prob <- predict(m.H1B.smote.glmnet, x.H1B.test.d, type="prob")

confusionMatrix(p.H1B.smote.glmnet,y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.glmnet.newthresh <- factor(ifelse(p.H1B.smote.glmnet.prob[[1]]>0.40, "CERTIFIED", "DENIED"))
p.H1B.smote.glmnet.newthresh 
confusionMatrix(p.H1B.smote.glmnet.newthresh, y.H1B.test)

test.H1B.smote.glmnet.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.glmnet.prob[[1]])
plot(test.H1B.smote.glmnet.roc)

newthresh<- coords(test.H1B.smote.glmnet.roc, x="best", best.method="closest.topleft")
plot(p.H1B.smote.glmnet.newthresh)
auc(test.H1B.smote.glmnet.roc)

##Glmnet with PCA
m.H1B.smote.pca.glmnet <- readRDS("m.H1B.smote.pca.glm.rdata")
m.H1B.smote.pca.glmnet
getTrainPerf(m.H1B.smote.pca.glmnet)
varImp(m.H1B.smote.pca.glmnet)

#Making prediction on test data
p.H1B.smote.pca.glmnet <- predict(m.H1B.smote.pca.glmnet,H1B.smote.pcr.test)
p.H1B.smote.pca.glmnet.prob <- predict(m.H1B.smote.pca.glmnet, H1B.smote.pcr.test, type="prob")

confusionMatrix(p.H1B.smote.pca.glmnet,y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.pca.glmnet.newthresh <- factor(ifelse(p.H1B.smote.pca.glmnet.prob[[1]]>0.42, "CERTIFIED", "DENIED"))
confusionMatrix(p.H1B.smote.pca.glmnet.newthresh, y.H1B.test)

test.H1B.smote.pca.glmnet.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.pca.glmnet.prob[[1]])
plot(test.H1B.smote.pca.glmnet.roc)

newthresh<- coords(test.H1B.smote.pca.glmnet.roc, x="best", best.method="closest.topleft")
plot(p.H1B.smote.pca.glmnet.newthresh)
auc(test.H1B.smote.pca.glmnet.roc)


##Random Forest without PCA
m.H1B.smote.rf <- readRDS("m.H1B.smote.rf.rdata")
m.H1B.smote.rf

getTrainPerf(m.H1B.smote.rf)
varImp(m.H1B.smote.rf)
plot(m.H1B.smote.rf)

p.H1B.smote.rf <- predict(m.H1B.smote.rf ,x.H1B.test)
plot(p.H1B.smote.rf)
confusionMatrix(p.H1B.smote.rf ,y.H1B.test)

p.H1B.smote.rf.prob<- predict(m.H1B.smote.rf , x.H1B.test ,type="prob")
p.H1B.smote.rf.newthresh <- factor(ifelse(p.H1B.smote.rf.prob[[1]]>0.87, "CERTIFIED", "DENIED"))
p.H1B.smote.rf.newthresh
confusionMatrix(p.H1B.smote.rf.newthresh, y.H1B.test )

test.H1B.smote.rf.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.rf.prob [[1]])
plot(test.H1B.smote.rf.roc)

p.H1B.smote.rf.prob [[1]]
levels(p.H1B.smote.rf.newthresh)

auc(test.H1B.smote.rf.roc)

##Random Forest with PCA
m.H1B.smote.pca.rf <- readRDS("m.H1B.smote.pca.rf.rdata")
m.H1B.smote.pca.rf
getTrainPerf(m.H1B.smote.pca.rf)
varImp(m.H1B.smote.pca.rf)

#Making prediction on test data
p.H1B.smote.pca.rf <- predict(m.H1B.smote.pca.rf,H1B.smote.pcr.test)
p.H1B.smote.pca.rf.prob <- predict(m.H1B.smote.pca.rf, H1B.smote.pcr.test, type="prob")

confusionMatrix(p.H1B.smote.pca.rf,y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.pca.rf.newthresh <- factor(ifelse(p.H1B.smote.pca.rf.prob[[1]]>0.85, "CERTIFIED", "DENIED"))
confusionMatrix(p.H1B.smote.pca.rf.newthresh, y.H1B.test)

test.H1B.smote.pca.rf.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.pca.rf.prob[[1]])
plot(test.H1B.smote.pca.rf.roc)

newthresh<- coords(test.H1B.smote.pca.rf.roc, x="best", best.method="closest.topleft")
plot(p.H1B.smote.pca.rf.newthresh)
auc(test.H1B.smote.pca.rf.roc)


##Bagging without PCA
library(ipred)
set.seed(99)
m.H1B.smote.bag <- readRDS("m.H1B.smote.bag.rdata")
#saveRDS(m.H1B.smote.bag, file = "m.H1B.smote.bag.rdata")
m.H1B.smote.bag

getTrainPerf(m.H1B.smote.bag)
varImp(m.H1B.smote.bag)
plot(m.H1B.smote.bag)

p.H1B.smote.bag<- predict(m.H1B.smote.bag,x.H1B.test)
confusionMatrix(p.H1B.smote.bag,y.H1B.test)

p.H1B.smote.bag.prob <- predict(m.H1B.smote.bag,x.H1B.test,type="prob")
p.H1B.smote.bag.newthresh <- factor(ifelse(p.H1B.smote.bag.prob[[1]]>0.70, "CERTIFIED", "DENIED"))
p.H1B.smote.bag.newthresh 
confusionMatrix(p.H1B.smote.bag.newthresh, y.H1B.test)

test.H1B.smote.bag.roc<- roc(response= y.H1B.test, predictor=p.H1B.smote.bag.prob[[1]])
plot(test.H1B.smote.bag.roc)

auc(test.H1B.smote.bag.roc)

##Bagging with PCA
set.seed(99)
m.H1B.smote.pca.bag <- readRDS("m.H1B.smote.pca.bag.rdata")
#saveRDS(m.H1B.smote.pca.bag, file = "m.H1B.smote.pca.bag.rdata")
m.H1B.smote.pca.bag
getTrainPerf(m.H1B.smote.pca.bag)
varImp(m.H1B.smote.pca.bag)

#Making prediction on test data
p.H1B.smote.pca.bag <- predict(m.H1B.smote.pca.bag,H1B.smote.pcr.test)
p.H1B.smote.pca.bag.prob <- predict(m.H1B.smote.pca.bag, H1B.smote.pcr.test, type="prob")

confusionMatrix(p.H1B.smote.pca.bag,y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.pca.bag.newthresh <- factor(ifelse(p.H1B.smote.pca.bag.prob[[1]]>0.70, "CERTIFIED", "DENIED"))
p.H1B.smote.pca.bag.newthresh 
confusionMatrix(p.H1B.smote.pca.bag.newthresh, y.H1B.test)

test.H1B.smote.pca.bag.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.pca.bag.prob[[1]])
plot(test.H1B.smote.pca.bag.roc)

newthresh<- coords(test.H1B.smote.pca.bag.roc, x="best", best.method="closest.topleft")

auc(test.H1B.smote.pca.bag.roc)

##Boosting without PCA
m.H1B.smote.ada <- readRDS("m.H1B.smote.ada.rdata")

m.H1B.smote.ada
getTrainPerf(m.H1B.smote.ada)
varImp(m.H1B.smote.ada)

#Making prediction on test data
p.H1B.smote.ada <- predict(m.H1B.smote.ada,x.H1B.test)
p.H1B.smote.ada.prob <- predict(m.H1B.smote.ada, x.H1B.test, type="prob")

confusionMatrix(p.H1B.smote.ada,y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.ada.newthresh <- factor(ifelse(p.H1B.smote.ada.prob[[1]]>0.90, "CERTIFIED", "DENIED"))
confusionMatrix(p.H1B.smote.ada.newthresh, y.H1B.test)

test.H1B.smote.ada.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.ada.prob[[1]])
plot(test.H1B.smote.ada.roc)

newthresh<- coords(test.H1B.smote.ada.roc, x="best", best.method="closest.topleft")
plot(p.H1B.smote.ada.newthresh)
auc(test.H1B.smote.ada.roc)

##Boosting with PCA
m.H1B.smote.pca.ada <- readRDS("m.H1B.smote.pca.ada.rdata")
m.H1B.smote.pca.ada
getTrainPerf(m.H1B.smote.pca.ada)
varImp(m.H1B.smote.pca.ada)

#Making prediction on test data
p.H1B.smote.pca.ada <- predict(m.H1B.smote.pca.ada,H1B.smote.pcr.test)
p.H1B.smote.pca.ada.prob <- predict(m.H1B.smote.pca.ada, H1B.smote.pcr.test, type="prob")

confusionMatrix(p.H1B.smote.pca.ada,y.H1B.test) #calc accuracies with confuction matrix on test set

p.H1B.smote.pca.ada.newthresh <- factor(ifelse(p.H1B.smote.pca.ada.prob[[1]]>0.90, "CERTIFIED", "DENIED"))
confusionMatrix(p.H1B.smote.pca.ada.newthresh, y.H1B.test)

test.H1B.pca.smote.ada.roc<- roc(response= y.H1B.test, predictor= p.H1B.smote.pca.ada.prob[[1]])
plot(test.H1B.pca.smote.ada.roc)

newthresh<- coords(test.H1B.pca.smote.ada.roc, x="best", best.method="closest.topleft")
plot(p.H1B.smote.pca.ada.newthresh)
auc(test.H1B.pca.smote.ada.roc)





#compare training performance 
#create list of cross validation runs (resamples) for without PCA models
rValues <- resamples(list(Logit=m.H1B.smote.glm, LDA=m.H1B.smote.lda, Rpart=m.H1B.smote.rpart, 
                          Naivebayes=m.H1B.smote.nb, GAM=m.H1B.smote.gam, PLS=m.H1B.smote.pls,
                          Glmnet=m.H1B.smote.glmnet, RandomForest=m.H1B.smote.rf, 
                          Bagging=m.H1B.smote.bag, Boosting=m.H1B.smote.ada))


#create list of cross validation runs (resamples) for with PCA models
rValues_PCA <- resamples(list(Logit_PCA=m.H1B.smote.pca.glm, LDA_PCA=m.H1B.smote.pca.lda, 
                              Rpart_PCA=m.H1B.smote.pca.rpart, Naivebayes_PCA=m.H1B.smote.pca.nb,
                              GAM_PCA=m.H1B.smote.pca.gam, PLS_PCA=m.H1B.smote.pca.pls,
                              Glmnet_PCA=m.H1B.smote.pca.glmnet, RandomForest_PCA=m.H1B.smote.pca.rf, 
                              Bagging_PCA=m.H1B.smote.pca.bag, Boosting_PCA=m.H1B.smote.pca.ada))

##create list of cross validation runs (resamples) for all PCA models
rvalues_All <- resamples(list(Logit=m.H1B.smote.glm, LDA=m.H1B.smote.lda, Rpart=m.H1B.smote.rpart, 
                              Naivebayes=m.H1B.smote.nb, GAM=m.H1B.smote.gam, PLS=m.H1B.smote.pls,
                              Glmnet=m.H1B.smote.glmnet, RandomForest=m.H1B.smote.rf, 
                              Bagging=m.H1B.smote.bag, Boosting=m.H1B.smote.ada,
                              Logit_PCA=m.H1B.smote.pca.glm, LDA_PCA=m.H1B.smote.pca.lda, 
                              Rpart_PCA=m.H1B.smote.pca.rpart, Naivebayes_PCA=m.H1B.smote.pca.nb,
                              GAM_PCA=m.H1B.smote.pca.gam, PLS_PCA=m.H1B.smote.pca.pls,
                              Glmnet_PCA=m.H1B.smote.pca.glmnet, RandomForest_PCA=m.H1B.smote.pca.rf, 
                              Bagging_PCA=m.H1B.smote.pca.bag, Boosting_PCA=m.H1B.smote.pca.ada))

#create plot for without PCA models comparing them
bwplot(rValues, metric="ROC")
bwplot(rValues, metric="Sens") #Sensitvity
bwplot(rValues, metric="Spec")

#create dot plot comparing them
dotplot(rValues, metric="ROC")
dotplot(rValues, metric="Sens") #Sensitvity
dotplot(rValues, metric="Spec")

xyplot(rValues, metric="ROC")
xyplot(rValues, metric="Sens") #Sensitvity
xyplot(rValues, metric="Spec")

##create plot for with PCA models comparing them
bwplot(rValues_PCA, metric="ROC")
bwplot(rValues_PCA, metric="Sens") #Sensitvity
bwplot(rValues_PCA, metric="Spec")

#create dot plot comparing them
dotplot(rValues_PCA, metric="ROC")
dotplot(rValues_PCA, metric="Sens") #Sensitvity
dotplot(rValues_PCA, metric="Spec")

xyplot(rValues_PCA, metric="ROC")
xyplot(rValues_PCA, metric="Sens") #Sensitvity
xyplot(rValues_PCA, metric="Spec")

#create plot for all the models comparing them
bwplot(rvalues_All, metric="ROC")
bwplot(rvalues_All, metric="Sens") #Sensitvity
bwplot(rvalues_All, metric="Spec")

#create dot plot comparing them
dotplot(rvalues_All, metric="ROC")
dotplot(rvalues_All, metric="Sens") #Sensitvity
dotplot(rvalues_All, metric="Spec")

xyplot(rvalues_All, metric="ROC")
xyplot(rvalues_All, metric="Sens") #Sensitvity
xyplot(rvalues_All, metric="Spec")

summary(rvalues_All)

##build to combined ROC plot with resampled ROC curves
plot(test.H1B.smote.glm.roc, legacy.axes=T, ylim=c(0:1))
plot(test.H1B.smote.lda.roc, add=T, col="Blue")
plot(test.H1B.smote.rpart.roc, add=T, col="Green")
plot(test.H1B.smote.nb.roc, add=T, col="Red")
plot(test.smote.pls.roc, add=T, col="Pink")
plot(test.H1B.smote.glmnet.roc, add=T, col="seagreen4")
plot(test.H1B.smote.rf.roc, add=T, col="Orange")
plot(test.H1B.smote.bag.roc, add=T, col="darkmagenta")
plot(test.H1B.smote.ada.roc, add=T, col="coral2")
plot(test.H1B.smote.gam.roc, add=T, col="cyan")
legend(x=0.1, y=1, legend=c("Glm", "LDA", "DT", "NB", "PLS", "Glmnet", "RF", "Bag", "Boosting", "GAM"), 
       col=c("black","blue","green","red", "pink", "yellow", "Orange", "darkmagenta", "coral2", "cyan"),lty=1)

###PRECISION RECALL

# Precision, Recall and F1 Score for GLM without PCA
precision_glm <- posPredValue(p.H1B.smote.glm, y.H1B.test, positive="CERTIFIED")
recall_glm <- sensitivity(p.H1B.smote.glm, y.H1B.test, positive="CERTIFIED")
F1-glm <- (2 * precision * recall) / (precision + recall)

# Precision, Recall and F1 Score for Glm with PCA
precision_pca_glm <- posPredValue(p.H1B.smote.pca.glm, y.H1B.test, positive="CERTIFIED")
recall_pca_glm <- sensitivity(p.H1B.smote.pca.glm, y.H1B.test, positive="CERTIFIED")
F1-pca_glm <- (2 * precision_pca_glm * recall_pca_glm) / (precision_pca_glm + recall_pca_glm)

# Precision, Recall and F1 Score for LDA without PCA
precision_lda <- posPredValue(p.H1B.smote.lda, y.H1B.test, positive="CERTIFIED")
recall_lda <- sensitivity(p.H1B.smote.lda, y.H1B.test, positive="CERTIFIED")
F1-lda <- (2 * precision_lda * recall_lda) / (precision_lda + recall_lda)

# Precision, Recall and F1 Score for LDA with PCA
precision_pca_lda <- posPredValue(p.H1B.smote.pca.lda, y.H1B.test, positive="CERTIFIED")
recall_pca_lda <- sensitivity(p.H1B.smote.pca.lda, y.H1B.test, positive="CERTIFIED")
F1-pca_lda <- (2 * precision_pca_lda * recall_pca_lda) / (precision_pca_lda + recall_pca_lda)


# Precision, Recall and F1 Score for DT without PCA
precision_rpart <- posPredValue(p.H1B.smote.rpart, y.H1B.test, positive="CERTIFIED")
recall_rpart <- sensitivity(p.H1B.smote.rpart, y.H1B.test, positive="CERTIFIED")
F1-rpart <- (2 * precision_rpart * recall_rpart) / (precision_rpart + recall_rpart)

# Precision, Recall and F1 Score for DT with PCA
precision_pca_rpart <- posPredValue(p.H1B.smote.pca.rpart, y.H1B.test, positive="CERTIFIED")
recall_pca_rpart <- sensitivity(p.H1B.smote.pca.rpart, y.H1B.test, positive="CERTIFIED")
F1-pca_rpart <- (2 * precision_pca_rpart * recall_pca_rpart) / (precision_pca_rpart + recall_pca_rpart)

# Precision, Recall and F1 Score for NB without PCA
precision_nb <- posPredValue(p.H1B.smote.nb, y.H1B.test, positive="CERTIFIED")
recall_nb <- sensitivity(p.H1B.smote.nb, y.H1B.test, positive="CERTIFIED")
F1-nb <- (2 * precision_nb * recall_nb) / (precision_nb + recall_nb)

# Precision, Recall and F1 Score for NB with PCA
precision_pca_nb <- posPredValue(p.H1B.smote.pca.nb, y.H1B.test, positive="CERTIFIED")
recall_pca_nb <- sensitivity(p.H1B.smote.pca.nb, y.H1B.test, positive="CERTIFIED")
F1-pca_nb <- (2 * precision_pca_nb * recall_pca_nb) / (precision_pca_nb + recall_pca_nb)

# Precision, Recall and F1 Score for GAM without PCA
precision_gam <- posPredValue(p.H1B.smote.gam, y.H1B.test, positive="CERTIFIED")
recall_gam <- sensitivity(p.H1B.smote.gam, y.H1B.test, positive="CERTIFIED")
F1-gam <- (2 * precision_gam * recall_gam) / (precision_gam + recall_gam)

# Precision, Recall and F1 Score for GAM with PCA
precision_pca_gam <- posPredValue(p.H1B.smote.pca.gam, y.H1B.test, positive="CERTIFIED")
recall_pca_gam <- sensitivity(p.H1B.smote.pca.gam, y.H1B.test, positive="CERTIFIED")
F1-pca_gam <- (2 * precision_pca_gam * recall_pca_gam) / (precision_pca_gam + recall_pca_gam)

# Precision, Recall and F1 Score for PLS without PCA
precision_pls <- posPredValue(p.H1B.smote.pls, y.H1B.test, positive="CERTIFIED")
recall_pls <- sensitivity(p.H1B.smote.pls, y.H1B.test, positive="CERTIFIED")
F1-pls <- (2 * precision_pls * recall_pls) / (precision_pls + recall_pls)

# Precision, Recall and F1 Score for PLS with PCA
precision_pca_pls <- posPredValue(p.H1B.smote.pca.pls, y.H1B.test, positive="CERTIFIED")
recall_pca_pls <- sensitivity(p.H1B.smote.pca.pls, y.H1B.test, positive="CERTIFIED")
F1-pca_pls <- (2 * precision_pca_pls * recall_pca_pls) / (precision_pca_pls + recall_pca_pls)

# Precision, Recall and F1 Score for GLMNET without PCA
precision_glmnet <- posPredValue(p.H1B.smote.glmnet, y.H1B.test, positive="CERTIFIED")
recall_glmnet <- sensitivity(p.H1B.smote.glmnet, y.H1B.test, positive="CERTIFIED")
F1-glmnet <- (2 * precision_glmnet * recall_glmnet) / (precision_glmnet + recall_glmnet)

# Precision, Recall and F1 Score for GLMNET with PCA
precision_pca_glmnet <- posPredValue(p.H1B.smote.pca.glmnet, y.H1B.test, positive="CERTIFIED")
recall_pca_glmnet <- sensitivity(p.H1B.smote.pca.glmnet, y.H1B.test, positive="CERTIFIED")
F1-pca_glmnet <- (2 * precision_pca_glmnet * recall_pca_glmnet) / (precision_pca_glmnet + recall_pca_glmnet)

# Precision, Recall and F1 Score for Random Forest without PCA
precision_rf <- posPredValue(p.H1B.smote.rf, y.H1B.test, positive="CERTIFIED")
recall_rf <- sensitivity(p.H1B.smote.rf, y.H1B.test, positive="CERTIFIED")
F1-rf <- (2 * precision_rf * recall_rf) / (precision_rf + recall_rf)

# Precision, Recall and F1 Score for Random Forest with PCA
precision_pca_rf <- posPredValue(p.H1B.smote.pca.rf, y.H1B.test, positive="CERTIFIED")
recall_pca_rf <- sensitivity(p.H1B.smote.pca.rf, y.H1B.test, positive="CERTIFIED")
F1-pca_rf <- (2 * precision_pca_rf * recall_pca_rf) / (precision_pca_rf + recall_pca_rf)

# Precision, Recall and F1 Score for Bagging without PCA
precision_bag <- posPredValue(p.H1B.smote.bag, y.H1B.test, positive="CERTIFIED")
recall_bag <- sensitivity(p.H1B.smote.bag, y.H1B.test, positive="CERTIFIED")
F1-bag <- (2 * precision_bag * recall_bag) / (precision_bag + recall_bag)

# Precision, Recall and F1 Score for Bagging with PCA
precision_pca_bag <- posPredValue(p.H1B.smote.pca.bag, y.H1B.test, positive="CERTIFIED")
recall_pca_bag <- sensitivity(p.H1B.smote.pca.bag, y.H1B.test, positive="CERTIFIED")
F1-pca_bag <- (2 * precision_pca_bag * recall_pca_bag) / (precision_pca_bag + recall_pca_bag)

# Precision, Recall and F1 Score for Boosting without PCA
precision_ada <- posPredValue(p.H1B.smote.ada, y.H1B.test, positive="CERTIFIED")
recall_ada <- sensitivity(p.H1B.smote.ada, y.H1B.test, positive="CERTIFIED")
F1-ada <- (2 * precision_ada * recall_ada) / (precision_ada + recall_ada)

# Precision, Recall and F1 Score for Boosting with PCA
precision_pca_ada <- posPredValue(p.H1B.smote.pca.ada, y.H1B.test, positive="CERTIFIED")
recall_pca_ada <- sensitivity(p.H1B.smote.pca.ada, y.H1B.test, positive="CERTIFIED")
F1-pca_ada <- (2 * precision_pca_ada * recall_pca_ada) / (precision_pca_ada + recall_pca_ada)

