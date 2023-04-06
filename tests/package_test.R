## package test
library(dplyr)
library(corf)
?corf
rm(list=ls())
## check documentation is working
?init_orf
?predict.orf
?get_importance
?corf #need to fix - the package documentation needs to be fixed.




### initialize a model
orfmod <- init_orf(numClasses = 2, numFeatures = 10, numRandomTests = 2, counterThreshold = 10, maxDepth = 5, numTrees = 10, numEpochs = 1,
                   method = 'gini')
orfmod$forest$tree0[,grep("feature",colnames(orfmod$forest$tree0))]

orfmod$forest$tree0

orfmod$hyperparameters

### create some data
x <- matrix(runif(10), nrow=1)

### check predictions
p <- predict(orfmod, x)
names(p)
p

### train the model with the data 
orfmod2 <- train_orf(model = orfmod, x = x, y=as.matrix(1))
orfmod2
p <- predict(orfmod2, x)
p

orfmod2$oobe

orfmod2$forest$tree0

orfmod$forest$tree0[,grep("feature",colnames(orfmod$forest$tree0))]
orfmod$forest$tree1[,grep("feature",colnames(orfmod$forest$tree0))]


### run data through a  hundred times
for(i in 1:100)
  orfmod2 <- train_orf(model = orfmod2, x = x, y=as.matrix(0))

orfmod2$oobe
orfmod2$n

p <- predict(orfmod2, x)
p

#testing causal
cmod <- init_orf(numClasses = 2, numFeatures = 10, numRandomTests = 10, counterThreshold = 10, 
                 maxDepth = 5, numTrees = 10, numEpochs = 1, 
                 numTreatments = 2, causal = TRUE, type="classification", method = 'mse')
cmod$hyperparameters
cmod$forest$tree0
cmod <- train_orf(model = cmod, x = x, y=as.matrix(0), w=as.matrix(0))
cmod <- train_orf(model = cmod, x = x, y=as.matrix(1), w=as.matrix(1))
cmod$forest$tree0[,c("nodeNumber", 
                     "randomTest0_treatTrueStats0", "randomTest0_treatTrueStats1",
                     "randomTest0_controlTrueStats0", "randomTest0_controlTrueStats1"
)]

i <- 1
for(i in 1:10) {
  cmod <- train_orf(model = cmod, x = x, y=as.matrix(0), w=as.matrix(0))
  cmod <- train_orf(model = cmod, x = x, y=as.matrix(1), w=as.matrix(1))
}
for(i in 1:2) {
  cmod <- train_orf(model = cmod, x = x, y=as.matrix(1), w=as.matrix(0))
}
for(i in 1:8) {
  cmod <- train_orf(model = cmod, x = x, y=as.matrix(0), w=as.matrix(1))
}
cmod$forest$tree0

# cmod$forest$tree0[,c("isLeaf", "treatCounter","controlCounter",
#                      "treatLabelStats_0", "treatLabelStats_1",
#                      "controlLabelStats_0", "controlLabelStats_1",
#                      "ite_0","ite_1")] %>% as_data_frame(.) %>%
#   mutate(check_ite_1 = treatLabelStats_1 / treatCounter - controlLabelStats_1 / controlCounter) -> tree0

#tree0 %>% select(isLeaf, ite_1, check_ite_1)

cmod$forest$tree0[,c(grep("feature",colnames(cmod$forest$tree0)))]


cmod$forest$tree0[1,grep("randomTest0",colnames(cmod$forest$tree0))]


cbind(t(x), 0:(ncol(x)-1), round(get_importance(cmod),3))


df <- as.data.frame(cmod$forest$tree0)
df %>%
  filter(nodeNumber==0) %>%
  mutate(treatP0 = bestTest_treatTrueStats0/(bestTest_treatTrueStats1+bestTest_treatTrueStats0),
         controlP0 = bestTest_controlTrueStats0 / (bestTest_controlTrueStats1+bestTest_controlTrueStats0),
         treatP1 = bestTest_treatTrueStats1/(bestTest_treatTrueStats1+bestTest_treatTrueStats0),
         controlP1 = bestTest_controlTrueStats1 / (bestTest_controlTrueStats1+bestTest_controlTrueStats0)) %>%
  select(nodeNumber, isLeaf, bestTest_feature, treatP1, controlP1, treatP0, controlP0) %>%
  mutate(SS=(treatP1-controlP1)^2 + (treatP0-controlP0)^2)

### stress test ###
I <- 1000
numTrees <- 100
counterThreshold <- 10
numFeatures <- 50
numClasses <- 2
numRandomTests <- round(sqrt(numFeatures),0)
maxDepth = 10

X <- matrix(rnorm(I * numFeatures), ncol=numFeatures, nrow=I)
#betas <- rnorm(numFeatures, 0, 1)
betas <- c(rnorm(10, 0, 1), rep(0, numFeatures-10))
z <- X %*% betas 
p <- 1/exp(-z)
y <- ifelse(runif(I) < p/100, 1, 0)
table(y)


orf_mod <- init_orf(numClasses = numClasses, numFeatures = numFeatures, numRandomTests = numRandomTests, counterThreshold = counterThreshold, maxDepth = maxDepth, numTrees = numTrees, numEpochs = 1, method='hellinger')
orf_mod <- train_orf(model = orf_mod, x = as.matrix(X), y = matrix(y, ncol=1))
orf_mod$oobe / orf_mod$n


p <- predict(orf_mod, as.matrix(X))
p$confidence[1:3,]

predict(orf_mod, matrix(X[1,], nrow=1))
predict(orf_mod, matrix(X[2,], nrow=1))
predict(orf_mod, matrix(X[3,], nrow=1))


#how many nodes do the trees have?
table(sapply(orf_mod$forest, function(x) {nrow(x)}))

#what were identified as features
orf_mod$forest$tree0[,c(1,grep("feature",colnames(orf_mod$forest$tree0)))]

orf_mod$forest$tree0[,c(1,grep("best",colnames(orf_mod$forest$tree0)))]


#importance
cbind(0:(ncol(X)-1), round(get_importance(orf_mod),3), betas)

#replicate
for(i in 1:length(orf_mod$forest)) {
  if(i == 1) {
    df <- as.data.frame(orf_mod$forest[[i]])
  } else {
    df <- rbind(df, orf_mod$forest[[i]])
  }
}

length(df)
dim(df)
names(df)
summary(df)

df %>%
  filter(isLeaf==0) %>%
  mutate(counter = counter + parentCounter,
         p1_self = ifelse(counter>0,labelStats_1 / counter,0), 
         p0_self = ifelse(counter>0,labelStats_0 / counter),
         score1_self = p1_self * (1 - p1_self), score0_self = p0_self * (1 - p0_self),
         self_score = score1_self + score0_self,
         trueCounter = bestTest_trueStats0 + bestTest_trueStats1,
         falseCounter = bestTest_falseStats0 + bestTest_falseStats1,
         p1_true = ifelse(trueCounter>0,bestTest_trueStats1 / trueCounter,0),
         p0_true = ifelse(trueCounter>0,bestTest_trueStats0 / trueCounter,0),
         p1_false = ifelse(falseCounter>0,bestTest_falseStats1 / falseCounter,0),
         p0_false = ifelse(falseCounter>0,bestTest_falseStats0 / falseCounter,0),
         score1_true = p1_true * (1 - p1_true), score0_true = p0_true * (1 - p0_true),
         score_true = score1_true + score0_true,
         score1_false = p1_false * (1 - p1_false), score0_false = p0_false * (1 - p0_false),
         score_false = score1_false + score0_false,
         split_score = (score_true * trueCounter + score_false * falseCounter)/(trueCounter + falseCounter),
         mdg = (self_score - split_score) * counter
  ) %>% 
  group_by(bestTest_feature) %>% 
  summarise(n=n(), mdg = sum(mdg)/sum(counter)) %>%
  #arrange(desc(mdg)) %>% 
  print(n=100)



### process row by row ###
orf_mod <- init_orf(numClasses = numClasses, numFeatures = numFeatures, numRandomTests = numRandomTests, counterThreshold = counterThreshold, maxDepth = maxDepth, numTrees = numTrees, numEpochs = 1)

for(i in 1:I) {
  orf_mod <- train_orf(model = orf_mod, x = matrix(X[i,],nrow=1), y = matrix(y[i], ncol=1))
  if(i %% 100 == 0) {print(i)}
}

get_importance(orf_mod)


### test importance ###
library(corf)
I <- 1000 
numTrees <- 100
counterThreshold <- 10
numFeatures <- 10
numClasses <- 2
numRandomTests <- round(sqrt(numFeatures),0)
maxDepth = 10

X <- matrix(rnorm(I * numFeatures), ncol=numFeatures, nrow=I)
#betas <- rnorm(numFeatures, 0, 1)
#only first 3 columns related to the outcome, the rest are noise
betas <- c(rnorm(3, 0, 1), rep(0, numFeatures-3))
z <- X %*% betas 
p <- 1/exp(-z)
y <- ifelse(runif(I) < p, 1, 0)
table(y)


orf_mod <- init_orf(numClasses = numClasses, numFeatures = numFeatures, numRandomTests = numRandomTests, counterThreshold = counterThreshold, maxDepth = maxDepth, numTrees = numTrees, numEpochs = 1, method='hellinger')
orf_mod <- train_orf(model = orf_mod, x = as.matrix(X), y = matrix(y, ncol=1))
orf_mod$oobe / orf_mod$n


cbind(0:(ncol(X)-1), round(get_importance(orf_mod),3), betas)
sum(get_importance(orf_mod))
plot(get_importance(orf_mod), abs(betas))


### test the generic methods ###
#is.orf <- function(x) {inherits(x, "orf")}

orf_mod <- init_orf(numClasses = numClasses, numFeatures = numFeatures, numRandomTests = numRandomTests, counterThreshold = counterThreshold, maxDepth = maxDepth, numTrees = numTrees, numEpochs = 1, method='hellinger')
#class(orf_mod) <- "orf"
is.orf(orf_mod)

orf_mod <- train_orf(model = orf_mod, x = as.matrix(X), y = matrix(y, ncol=1))
#class(orf_mod) <- "orf"

is.orf(orf_mod)
orf_mod$oobe / orf_mod$n


###


print(orf_mod)
summary(orf_mod)

#cross validation
n <- 10000
k <- 10
x <- matrix(rnorm(n*k), ncol=k, nrow=n)
betas <- runif(k)
w <- ifelse(runif(n) > .5, 1, 0)
te <- rnorm(n, .5, .05)
y <- ifelse(x %*% betas + te * w> rnorm(n), 1, 0)
table(y)
table(y, w)

cv <- causal_orf_cv(x=x, y=as.matrix(y), treat = as.matrix(w), numClasses = 2, numRandomTests = 3, counterThreshold = 2, maxDepth = 10, numTrees = 500, numEpochs = 1, nfolds = 10, type = "gini")
summary(cv)

################################################################################################
#### test regression methods
library(dplyr) 
library(corf)

I <- 100
numTrees <- 2
counterThreshold <- 1
numFeatures <- 3
numRandomTests <- 3
maxDepth = 10

X <- matrix(rnorm(I * numFeatures), ncol=numFeatures, nrow=I)
#only first 3 columns related to the outcome, the rest are noise
betas <- c(rnorm(3, 0, 1), rep(0, numFeatures-3))
y <- X %*% betas 
summary(y)
summary(X)

### check initialization
orfreg <- init_orf(numFeatures = numFeatures, numRandomTests = numRandomTests, 
                   counterThreshold = counterThreshold, maxDepth = maxDepth, 
                   numTrees = numTrees, numEpochs = 1,
                   type="regression")

names(orfreg)
orfreg$hyperparameters

orfreg$forest$tree0 %>% as.data.frame(.) %>%
  select(nodeNumber)


# ### check if the import/export features work properly
# orfreg2 <- train_orf(model = orfreg, x = as.matrix(X), y = matrix(y, ncol=1), trainModel = FALSE)
# names(orfreg)
# 
# for(i in names(orfreg$hyperparameters)) {
#   print(orfreg$hyperparameters[[i]] == orfreg2$hyperparameters[[i]])
# }
# 
# names(orfreg$hyperparameters)
# names(orfreg2$hyperparameters)
# 
# dim(orfreg$forest[[1]])
# dim(orfreg2$forest[[1]])
# for(i in 1:ncol(orfreg$forest[[1]])) {
#   if(orfreg$forest[[1]][1,i] != orfreg2$forest[[1]][1,i]) {
#     print(colnames(orfreg$forest[[1]])[i])
#     print(orfreg$forest[[1]][1,i] )
#     print(orfreg2$forest[[1]][1,i] )
#   }
# }



# ### check training the model
# s <- 1
# predict(orfreg, matrix(X[s,], ncol=numFeatures), allTrees = TRUE)
# 
# 
# orfreg2 <- train_orf(model = orfreg, x = as.matrix(X), y = matrix(y, ncol=1), trainModel = TRUE)
# orfreg2$oobe 
# sqrt(orfreg2$oobe / orfreg2$n)
# orfreg2$forest$tree0

### check prediction calculations
s <- c(1:4)
orfreg2 <- train_orf(model = orfreg, x = matrix(X[s,], ncol=numFeatures), y = matrix(y[s], ncol=1), trainModel = TRUE)
orfreg2$forest$tree0 %>% as.data.frame(.) %>%
  select(nodeNumber, rightChildNodeNumber, leftChildNodeNumber, counter,
         bestTest_feature, bestTest_threshold,
         yMean, yVar)
orfreg2$forest$tree1 %>% as.data.frame(.) %>%
  select(nodeNumber, rightChildNodeNumber, leftChildNodeNumber, counter,
         bestTest_feature, bestTest_threshold,
         yMean, yVar)

mean(y[s])

p <- predict(orfreg2, matrix(X[s,],ncol=numFeatures))

plot(p$prediction, y[s], xlim=c(-2,2),ylim=c(-2,2))
abline(a=0,b=1)
# 
# x_test <- matrix(c(rep(-1,10)), nrow=1)
# predict(orfreg2, x_test)
# 
# ### check predicting
# ### create some data
# x <- matrix(runif(10), nrow=1)
# p2 <- predict(orfreg2, x)
# p2
p <- predict(orfreg2, matrix(X,nrow=nrow(X)))
p

predict(orfreg2, matrix(X[1,],nrow=1))
p$prediction[1]
predict(orfreg2, matrix(X[2,],nrow=1))
p$prediction[2]
predict(orfreg2, matrix(X[3,],nrow=1))
p$prediction[3]

x1_10 <- X[20:30,]
p2 <- predict(orfreg2, x1_10)
p$variance[20:30]
p2$variance

### getting different results when run with multiple rows vs single row

plot(p$prediction, y)


sqrt(sum((p$prediction - y)^2)/length(y))

### feature importance
### failing here - looking for number of classes
imps <- get_importance(orfreg2) #not calculating correctly...
imps 

cbind(0:(numFeatures-1), round(imps,3), betas)
# is this being calculated right?

### fit with a larger forest and test out the variances
library(dplyr)
library(corf)
I <- 1000
numTrees <- 200
counterThreshold <- 5
numFeatures <- 30
numRandomTests <- 10
maxDepth = 20

X <- matrix(rnorm(I * numFeatures), ncol=numFeatures, nrow=I)
#only first 3 columns related to the outcome, the rest are noise
betas <- c(rnorm(10, 0, 1), rep(0, numFeatures-10))
y <- X %*% betas 
summary(y)
summary(X)

### check initialization
orfreg <- init_orf(numFeatures = numFeatures, numRandomTests = numRandomTests, 
                   counterThreshold = counterThreshold, maxDepth = maxDepth, 
                   numTrees = numTrees, numEpochs = 1,
                   type="regression", minFeatRange = rep(-4,numFeatures),
                    maxFeatRange = rep(4,numFeatures),
                   method = "mse")

### train the model
orfreg <- train_orf(model = orfreg, x = as.matrix(X), y = matrix(y, ncol=1), trainModel = TRUE)
orfreg$oobe 
sqrt(orfreg$oobe / orfreg$n)

#predictions including all 
p <- predict(orfreg, as.matrix(X), allTrees = TRUE, biasCorrect = TRUE)
names(p)
summary(p$prediction)
summary(y)
summary(p$variance)

table(p$yHatAllTrees == 0) #0.6% coming up as 0. is this causing the compression?


# predict(orfreg, matrix(X[which(y==max(y)),], nrow=1), allTrees = TRUE)

# orfreg$forest$tree4 %>% as.data.frame(.) %>%
#   select(nodeNumber, isLeaf, counter, parentCounter, bestTest_feature, bestTest_threshold, yMean)

#check predictions against values from every tree
pred_check <- apply(p$yHatAllTrees, 1, mean)
length(pred_check)
summary(pred_check - p$prediction)
#

var_check <- apply(p$yHatAllTrees, 1, var)
summary(var_check - p$variance)

#from variance estimate
p_025 = p$prediction - 1.96 * sqrt(p$variance)
p_975 = p$prediction + 1.96 * sqrt(p$variance)

table(y > p_025 & y < p_975)
sum((y > p_025 & y < p_975)==FALSE)/I

#from all trees
qtiles <- t(apply(p$yHatAllTrees, 1, quantile, probs=c(0.025, 0.975)))
dim(qtiles)

p_025 = qtiles[,1]
p_975 = qtiles[,2]

table(y > p_025 & y < p_975)
sum((y > p_025 & y < p_975)==FALSE)/I

#plot
l <- 9
plot(p$prediction, y, xlim=c(-l,l), ylim=c(-l,l), xlab="Prediction", ylab="Actual")
abline(a=0, b=1, col='red')
# segments(x0 = p$prediction, x1=p$prediction, 
#          y0 = p_025, y1=p_975)
lines(lowess(x =p_025, y=y), col='blue', lty=2)
lines(lowess(x =p_975, y=y), col='blue', lty=2)

library(randomForest)
rf <- randomForest(x = as.matrix(X), y=y, ntree = numTrees, mtry=numRandomTests)
summary(rf)
points(rf$predicted, y, col='green')
abline(a=0,b=1,col='red')

legend('bottomright', inset=.01, legend=c('OnlineRF','RF'), pch=1, col=c("black","green"))

rmse <- function(a,p) {sqrt(mean((a-p)^2))}

#check on a hold out dataset
X2 <- matrix(rnorm(I * numFeatures), ncol=numFeatures, nrow=I)
#only first 3 columns related to the outcome, the rest are noise
y2 <- X2 %*% betas 
summary(y2)
summary(X2)

#predictions including all 
#p2 <- predict(orfreg, as.matrix(X2), allTrees = TRUE, biasCorrect = TRUE)
p2 <- predict(orfreg, as.matrix(X2), allTrees = TRUE)

#from all trees
qtiles2 <- t(apply(p2$yHatAllTrees, 1, quantile, probs=c(0.025, 0.975)))
dim(qtiles2)

p2_025 = qtiles2[,1]
p2_975 = qtiles2[,2]

table(y2 > p2_025 & y < p2_975)

cor(p2$prediction, y2)^2
plot(p2$prediction, y2, xlim=c(-5,5), ylim=c(-5,5), ylab='Actual', xlab='Prediction')
abline(a=0,b=1,col='red')
lines(lowess(x=p2_025, y=y2), col='blue', lty=2)
lines(lowess(x=p2_975, y=y2), col='blue', lty=2)

rf.p <- predict(rf, X2)
points(rf.p, y2, col='green')
abline(a=0,b=1,col='red')

orf.rmse <- rmse(p2$prediction, y2)
rf.rmse <- rmse(rf.p, y2)

legend('bottomright', inset=.01, 
       legend=c(paste('OnlineRF: ', round(orf.rmse,3)),
                paste('RF: ', round(rf.rmse,3))), 
       pch=1, col=c("black","green"))



### compare to grf ###
library(grf)
grf_rf <- regression_forest(X=X, Y=y, num.trees = numTrees, sample.fraction = .25, mtry = numFeatures, min.node.size = 1, honesty = FALSE)
print(grf_rf)
grf_rf.p <- predict(grf_rf, X2)
head(grf_rf.p)

points(grf_rf.p$predictions, y2, col='forestgreen')
abline(a=0,b=1,col='red')

print(grf_rf.rmse <- rmse(grf_rf.p$prediction, y2))



#hist(unlist(p2$yHatAllTrees))

### compare other methods of splitting ###

### Adjusted MSE - from Athey/Imbens
orfreg_amse <- init_orf(numFeatures = numFeatures, numRandomTests = numRandomTests, 
                        counterThreshold = counterThreshold, maxDepth = maxDepth, 
                        numTrees = numTrees, numEpochs = 1,
                        type="regression", minFeatRange = rep(-4,numFeatures),
                        maxFeatRange = rep(4,numFeatures),
                        method = "amse")


### train the model
orfreg_amse <- train_orf(model = orfreg_amse, x = as.matrix(X), y = matrix(y, ncol=1), trainModel = TRUE)
#sqrt(orfreg$oobe / orfreg$n)
sqrt(orfreg_amse$oobe / orfreg_amse$n)

p2_amse <- predict(orfreg_amse, as.matrix(X2))
orfreg_amse.rmse <- rmse(p2_amse$prediction, y2)
points(p2_amse$prediction, y2, col='orange')

### Bias Corrected MSE - from Athey/Imbens
orfreg_et <- init_orf(numFeatures = numFeatures, numRandomTests = numRandomTests, 
                        counterThreshold = counterThreshold, maxDepth = maxDepth, 
                        numTrees = numTrees, numEpochs = 1,
                        type="regression", minFeatRange = rep(-4,numFeatures),
                        maxFeatRange = rep(4,numFeatures),
                        method = "et")


### train the model
orfreg_et <- train_orf(model = orfreg_et, x = as.matrix(X), y = matrix(y, ncol=1), trainModel = TRUE)
#sqrt(orfreg$oobe / orfreg$n)
sqrt(orfreg_et$oobe / orfreg_et$n)

p2_et <- predict(orfreg_et, as.matrix(X2))
print(orfreg_et.rmse <- rmse(p2_et$prediction, y2))
points(p2_et$prediction, y2, col='blue')

### Difference between left and right split nodes
orfreg_diff <- init_orf(numFeatures = numFeatures, numRandomTests = numRandomTests, 
                         counterThreshold = counterThreshold, maxDepth = maxDepth, 
                         numTrees = numTrees, numEpochs = 1,
                         type="regression", minFeatRange = rep(-4,numFeatures),
                         maxFeatRange = rep(4,numFeatures),
                         method = "diff")


### train the model
orfreg_diff <- train_orf(model = orfreg_diff, x = as.matrix(X), y = matrix(y, ncol=1), trainModel = TRUE)
#sqrt(orfreg$oobe / orfreg$n)
sqrt(orfreg_diff$oobe / orfreg_diff$n)

p2_diff <- predict(orfreg_diff, as.matrix(X2))
print(orfreg_diff.rmse <- rmse(p2_diff$prediction, y2))
points(p2_diff$prediction, y2, col='deeppink')

### what about a completely randomized forest?
orfreg_cr <- init_orf(numFeatures = numFeatures, numRandomTests = 1, 
                        counterThreshold = counterThreshold, maxDepth = maxDepth, 
                        numTrees = numTrees, numEpochs = 1,
                        type="regression", minFeatRange = rep(-4,numFeatures),
                        maxFeatRange = rep(4,numFeatures),
                        method = "mse")


### train the model
orfreg_cr <- train_orf(model = orfreg_cr, x = as.matrix(X), y = matrix(y, ncol=1), trainModel = TRUE)
#sqrt(orfreg$oobe / orfreg$n)
sqrt(orfreg_cr$oobe / orfreg_cr$n)

p2_cr <- predict(orfreg_cr, as.matrix(X2))
print(orfreg_cr.rmse <- rmse(p2_cr$prediction, y2))
points(p2_cr$prediction, y2, col='darkgreen')


### Bias Corrected MSE - from Athey/Imbens
orfreg_gini <- init_orf(numFeatures = numFeatures, numRandomTests = numRandomTests, 
                         counterThreshold = counterThreshold, maxDepth = maxDepth, 
                         numTrees = numTrees, numEpochs = 1,
                         type="regression", minFeatRange = rep(-4,numFeatures),
                         maxFeatRange = rep(4,numFeatures),
                         method = "gini")


### train the model
orfreg_gini <- train_orf(model = orfreg_gini, x = as.matrix(X), y = matrix(y, ncol=1), trainModel = TRUE)
#sqrt(orfreg$oobe / orfreg$n)
sqrt(orfreg_gini$oobe / orfreg_gini$n)

p2_gini <- predict(orfreg_gini, as.matrix(X2))
print(orfreg_gini.rmse <- rmse(p2_gini$prediction, y2))
points(p2_gini$prediction, y2, col='blue')



### using a higher splitting counter
orfreg_100 <- init_orf(numFeatures = numFeatures, numRandomTests = numRandomTests, 
                      counterThreshold = 100, maxDepth = maxDepth, 
                      numTrees = numTrees, numEpochs = 1,
                      type="regression", minFeatRange = rep(-4,numFeatures),
                      maxFeatRange = rep(4,numFeatures),
                      method = "gini")


### train the model
orfreg_100 <- train_orf(model = orfreg_100, x = as.matrix(X), y = matrix(y, ncol=1), trainModel = TRUE)
#sqrt(orfreg$oobe / orfreg$n)
sqrt(orfreg_100$oobe / orfreg_100$n)

p2_100 <- predict(orfreg_100, as.matrix(X2))
print(orfreg_100.rmse <- rmse(p2_100$prediction, y2))
points(p2_100$prediction, y2, col='brown')

### more epochs
orfreg_e10 <- init_orf(numFeatures = numFeatures, numRandomTests = numRandomTests, 
                       counterThreshold = counterThreshold, maxDepth = maxDepth, 
                       numTrees = numTrees, numEpochs = 10,
                       type="regression", minFeatRange = rep(-4,numFeatures),
                       maxFeatRange = rep(4,numFeatures),
                       method = "mse")


### train the model
orfreg_e10 <- train_orf(model = orfreg_e10, x = as.matrix(X), y = matrix(y, ncol=1), trainModel = TRUE)
#sqrt(orfreg$oobe / orfreg$n)
sqrt(orfreg_e10$oobe / orfreg_e10$n)

p2_e10 <- predict(orfreg_e10, as.matrix(X2))
print(orfreg_e10.rmse <- rmse(p2_e10$prediction, y2))
points(p2_e10$prediction, y2, col='dodgerblue')


### more epochs
orfreg_max <- init_orf(numFeatures = numFeatures, numRandomTests = numFeatures, 
                       counterThreshold = counterThreshold, maxDepth = maxDepth, 
                       numTrees = numTrees, numEpochs = 10,
                       type="regression", minFeatRange = rep(-5,numFeatures),
                       maxFeatRange = rep(5,numFeatures),
                       method = "amse")


### train the model
orfreg_max <- train_orf(model = orfreg_max, x = as.matrix(X), y = matrix(y, ncol=1), trainModel = TRUE)
#sqrt(orfreg$oobe / orfreg$n)
sqrt(orfreg_max$oobe / orfreg_max$n)

p2_max <- predict(orfreg_max, as.matrix(X2))
print(orfreg_max.rmse <- rmse(p2_max$prediction, y2))
points(p2_max$prediction, y2, col='darkred')


### test causal regression
library(dplyr)
library(corf)

I <- 1000
numTrees <- 100
counterThreshold <- 2
numFeatures <- 10
numRandomTests <- 3
maxDepth = 10

W <- ifelse(runif(I) > .5, 1, 0)

X <- matrix(rnorm(I * numFeatures), ncol=numFeatures, nrow=I)
#first column drives the pick from betas or alphas
betas <- c(rnorm(3, 0, 1), rep(0, numFeatures-3))
alphas <- c(rep(0,3),rnorm(3, 0, 1), rep(0, numFeatures-6))

y1 <- X %*% betas + rnorm(I, 0, 1)
y2 <- X %*% alphas + rnorm(I, 0, 1)
y <- ifelse(W==1,y1,y2)

summary(lm(y1~X))
betas
summary(lm(y2~X))
alphas
summary(lm(y~X))
summary(lm(y~X:I(W==1)))

summary(y)
summary(X)

### initialize non-causal version
orfreg <- init_orf(numFeatures = numFeatures, numRandomTests = numRandomTests, 
                     counterThreshold = counterThreshold, maxDepth = maxDepth, 
                     numTrees = numTrees, numEpochs = 10,
                     type="regression", method = "mse", causal = FALSE)

orfreg <- train_orf(model = orfreg, x = as.matrix(X), y = matrix(y, ncol=1), trainModel = TRUE)
sqrt(orfreg$oobe / orfreg$n)


### causal version
orfreg_c <- init_orf(numFeatures = numFeatures, numRandomTests = numRandomTests, 
                     counterThreshold = counterThreshold, maxDepth = maxDepth, 
                     numTrees = numTrees, numEpochs = 10,
                     type="regression", method = "mse", causal = TRUE, numTreatments = 2)

orfreg_c <- train_orf(model = orfreg_c, x = as.matrix(X), y = matrix(y, ncol=1), 
                      w = matrix(W,ncol=1), trainModel = TRUE)
sqrt(orfreg_c$oobe / orfreg_c$n)

p <- predict(orfreg_c, matrix(X,nrow=nrow(X))) 
names(p)
summary(as.data.frame(p))


### check treatment effects against those generated
TE <- y1 - y2
summary(TE)
TE_hat = p$tauHat[,2]
l <- 6
plot(TE_hat, TE, xlim=c(-l,l), ylim=c(-l,l))
abline(a=0,b=1, col='red')
cor(TE, TE_hat)

### how many are within prediction variance
p <- predict(orfreg_c, matrix(X,nrow=nrow(X)), allTrees = TRUE) 
qtiles <- t(apply(p$tauHat_all$treatment1, 1, quantile, probs=c(0.025, 0.975)))
head(qtiles)
table(TE > qtiles[,1] & TE < qtiles[,2])


### create separate dataset and score those
W.2 <- ifelse(runif(I) > .5, 1, 0)

X.2 <- matrix(rnorm(I * numFeatures), ncol=numFeatures, nrow=I)
#first column drives the pick from betas or alphas

y1.2 <- X.2 %*% betas + rnorm(I, 0, 1)
y2.2 <- X.2 %*% alphas + rnorm(I, 0, 1)
y.2 <- ifelse(W.2==1,y1.2,y2.2)

p.2 <- predict(orfreg_c, matrix(X.2,nrow=nrow(X.2))) 
summary(as.data.frame(p.2))


### check treatment effects against those generated
TE.2 <- y1.2 - y2.2
summary(TE.2)
TE_hat.2 = p.2$tauHat[,2]
plot(TE_hat.2, TE.2)
cor(TE.2, TE_hat.2)


### check implementation of getting tauhat for all trees
numTrees <- 1000

orfreg_c <- init_orf(numFeatures = numFeatures, numRandomTests = numRandomTests, 
                     counterThreshold = counterThreshold, maxDepth = maxDepth, 
                     numTrees = numTrees, numEpochs = 1,
                     type="regression", method = "mse", causal = TRUE, numTreatments = 2)

orfreg_c <- train_orf(model = orfreg_c, x = as.matrix(X), y = matrix(y, ncol=1), 
                      w = matrix(W,ncol=1), trainModel = TRUE)

p <- predict(orfreg_c, matrix(X.2,nrow=nrow(X.2)), allTrees=TRUE) 
names(p)
dim(p$tauHat_all[[2]])

mean_check <- rowMeans(p$tauHat_all[[2]])
head(mean_check)
head(p$tauHat[,2])
summary(mean_check - p$tauHat[,2])

#95% confidence interval for each row
tauHatAll <- as.data.frame(p$tauHat_all[[2]])
dim(tauHatAll)

qtiles <- t(apply(tauHatAll, 1, quantile, c(.025, .975)))
head(qtiles)
dim(qtiles)


TE.2 <- (y1.2 - y2.2)
table(TE.2 < qtiles[,2] & TE > qtiles[,1])

dat <- as.data.frame(cbind(te=TE.2, tauHat=p$tauHat[,2], qtiles))
colnames(dat) <- c('tau','tauHat', 'tauHat_025', 'tauHat_975')
head(dat)

table(dat$tauHat > dat$tauHat_025)
table(dat$tauHat < dat$tauHat_975)
table(dat$tau < dat$tauHat_975)
table(dat$tau > dat$tauHat_025)
table(dat$tau > dat$tauHat_025 & dat$tau < dat$tauHat_975)


col <- ifelse(dat$tau > dat$tauHat_025 & dat$tau < dat$tauHat_975, 'black','red')
plot(x=dat$tauHat, y=dat$tau, col=col)
segments(x0 = dat$tauHat, x1 = dat$tauHat, y0=dat$tauHat_025, y1=dat$tauHat_975)

#estimate 
tauHat_025 <- p$prediction - sqrt(p$variance)*1.96
tauHat_975 <- p$prediction + sqrt(p$variance)*1.96

head(tauHat_025)
head(p$prediction)
head(sqrt(p$variance))

table(dat$tau > tauHat_025 & dat$tau < tauHat_975)
#393/607

col <- ifelse(dat$tau > tauHat_025 & dat$tau < tauHat_975, 'black','red')
plot(x=dat$tauHat, y=dat$tau, col=col)
segments(x0 = dat$tauHat, x1 = dat$tauHat, tauHat_025, tauHat_975)

### seems to be working for all trees - but not achieving coverage. 65% should be 95%
### 2 standard deviations definitely not working


### compare to wager/athey
library(grf)

grf.mod <- causal_forest(X=X, Y=y, W=W)
print(grf.mod)

grf.preds <- predict(grf.mod, newdata = X.2)
head(grf.preds)

plot(grf.preds$predictions, TE.2, xlim=c(-l,l), ylim=c(-l,l))
points(TE_hat.2, TE.2, col='green')
abline(a=0, b=1, col='red')
rmse(grf.preds$predictions, TE.2)
rmse(TE_hat.2, TE.2)

cor(cbind(GRF=grf.preds$predictions, ORF=TE_hat.2, Actuals=c(TE.2)))


### train on large number of data points
library(dplyr)
library(corf)
#I <- 1e6
I <- 1e4
numTrees <- 1
counterThreshold <- 20
numFeatures <- 30
numRandomTests <- 10
maxDepth = 20

X <- matrix(rnorm(I * numFeatures), ncol=numFeatures, nrow=I)
#only first 3 columns related to the outcome, the rest are noise
betas <- c(rnorm(10, 0, 1), rep(0, numFeatures-10))
y <- X %*% betas 
summary(y)
summary(X)

### check initialization
orfreg <- init_orf(numFeatures = numFeatures, numRandomTests = numRandomTests, 
                   counterThreshold = counterThreshold, maxDepth = maxDepth, 
                   numTrees = numTrees, numEpochs = 1,
                   type="regression", minFeatRange = rep(-4,numFeatures),
                   maxFeatRange = rep(4,numFeatures),
                   method = "mse")

### train the model
orfreg <- train_orf(model = orfreg, x = as.matrix(X), y = matrix(y, ncol=1), trainModel = TRUE)
orfreg$oobe 
sqrt(orfreg$oobe / orfreg$n)



### check predicted values
library(dplyr)
library(corf)
#I <- 1e6
I <- 1000
numTrees <- 100
counterThreshold <- 10
numFeatures <- 20
numRandomTests <- 10
maxDepth = 20

X <- matrix(rnorm(I * numFeatures), ncol=numFeatures, nrow=I)
#only first 3 columns related to the outcome, the rest are noise
betas <- c(rnorm(3, 0, 1), rep(0, numFeatures-3))
y <- X %*% betas 
summary(y)
summary(X)

### check initialization
orfreg <- init_orf(numFeatures = numFeatures, numRandomTests = numRandomTests, 
                   counterThreshold = counterThreshold, maxDepth = maxDepth, 
                   numTrees = numTrees, numEpochs = 1,
                   type="regression", minFeatRange = rep(-4,numFeatures),
                   maxFeatRange = rep(4,numFeatures),
                   method = "mse")

### train the model
orfreg <- train_orf(model = orfreg, x = as.matrix(X), y = matrix(y, ncol=1), trainModel = TRUE)
orfreg$oobe 
sqrt(orfreg$oobe / orfreg$n)

orfreg$forest$tree0 %>% as.data.frame(.) %>%
  select(nodeNumber, rightChildNodeNumber, leftChildNodeNumber, 
         isLeaf, counter, parentCounter, yMean, yVar, 
         bestTest_feature, bestTest_threshold)


predict(orfreg, matrix(nrow=1, data=c(2,0,0)))

#unweighted
p <- predict(orfreg, as.matrix(X), allTrees = TRUE)
#weighted
p <- predict(orfreg, as.matrix(X), allTrees = TRUE, treeWeight = TRUE)


hist(p$yHatAllTrees[1,])
head(p$wgtAllTrees)

#check on weighed average
m <- p$yHatAllTrees * p$wgtAllTrees
dim(m)
v <- rowSums(m) / rowSums(p$wgtAllTrees)
length(v)
head(v)
head(p$prediction)

m1 <- p$yHatAllTrees[1,] * p$wgtAllTrees[1,]
sum(m1)/sum(p$wgtAllTrees[1,])


l <- 2
plot(p$prediction, y, xlim=c(-l,l), ylim=c(-l,l))
abline(a=0, b=1, col='red')
abline(lm(y~p$prediction))

t <- 1
plot(x=p$yHatAllTrees[,t], y=y, xlim=c(-l,l), ylim=c(-l,l))
abline(a=0, b=1, col='red')
abline(lm(y~p$yHatAllTrees[,t]))

for(t in 1:100) {
#  points(p$yHatAllTrees[,t], y)
  abline(lm(y~p$yHatAllTrees[,t]))
}

head(p$yHatAllTrees)

p_check <- rowMeans(p$yHatAllTrees)
points(p_check, y, col='green')
abline(lm(y~p_check),col='green')




errs <- c()
for(i in 1:numTrees) {
  errs <- c(errs, rmse(p$yHatAllTrees[,i],y))
}
hist(errs, xlim=c(0,1))
abline(v=rmse(p$prediction,y), col='red')

# l <- 4
# par(mfrow=c(3,3))
# for(nTree in c(1:9)) {
#   plot(p$yHatAllTrees[,nTree], y, xlim=c(-l,l), ylim=c(-l,l))
#   abline(a=0, b=1, col='red')
# }
# par(mfrow=c(1,1))
# 
# 

#### replicate simulation test
I <- 1000
numTrees <- 100
counterThreshold <- 2
numFeatures <- 10
numRandomTests <- 3
maxDepth = 10

W <- ifelse(runif(I) > .5, 1, 0)

X <- matrix(rnorm(I * numFeatures), ncol=numFeatures, nrow=I)
#first column drives the pick from betas or alphas
betas <- c(rnorm(3, 0, 1), rep(0, numFeatures-3))
alphas <- c(rep(0,3),rnorm(3, 0, 1), rep(0, numFeatures-6))

y1 <- X %*% betas + rnorm(I, 0, 1)
y2 <- X %*% alphas + rnorm(I, 0, 1)
y <- ifelse(W==1,y1,y2)

summary(lm(y1~X))
betas
summary(lm(y2~X))
alphas
summary(lm(y~X))
summary(lm(y~X:I(W==1)))

summary(y)
summary(X)

orfreg_c <- init_orf(numFeatures = numFeatures, numRandomTests = numRandomTests, 
                     counterThreshold = counterThreshold, maxDepth = maxDepth, 
                     numTrees = numTrees, numEpochs = 1,
                     type="regression", method = "mse", causal = TRUE, numTreatments = 2)

orfreg_c <- train_orf(model = orfreg_c, x = as.matrix(X), y = matrix(y, ncol=1), 
                      w = matrix(W,ncol=1), trainModel = TRUE)
