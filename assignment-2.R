library(plotly)
require(corrplot)
require(glmnet)
library(dplyr)
library(aod)

library(tidyr)
library(factoextra)
library('MASS')
library('heplots')
library('cluster')
library("NbClust")
library('class')
library('e1071')	#first install class and then this one
library('tree')
library('caret')
library('mclust')

setwd("/Users/stavros/Documents/BI/Statistics II/Project II")
voteData = read.csv("combined_data.csv", sep = ",")

str(voteData)
summary(voteData)
sum(apply(voteData,2, is.na))

# -----------------------------------------
# Code for removing state - Mentioned below - include to remove swing states
# -----------------------------------------

swingStates = c("Wisconsin", "Pennsylvania", "New Hampshire", "Minnesota", "Arizona", "Georgia", "Virginia", "Florida", "Michigan", "Nevada", "Colorado", "North Carolina", "Maine")
voteData = dplyr::filter(voteData, !grepl(paste(swingStates, collapse= "|"), state))

# -----------------------------------------
# -----------------------------------------
voteData = voteData[, -c(1,2,4, 6:12)]
countiesAsFactors = voteData$county
voteData$county = as.integer(as.factor(voteData$county))
str(voteData)

my.statistics = function(Actual,Predicted) {
  confusion.table = table(Actual=Actual,Predicted=Predicted)
  output = list(confusion.table=confusion.table)
  TN = confusion.table[1]
  FN = confusion.table[2]
  FP = confusion.table[3]
  TP = confusion.table[4]
  output$accuracy = (TP+TN)/sum(confusion.table)
  output$precission = (TP)/(TP+FP)
  output$sensitivity = (TP)/(TP+FN)
  output$specificity = (TN)/(TN+FP)
  output$FPR = (FP)/(TN+FP)
  output$ARI = adjustedRandIndex(Predicted, Actual)
  
  return(output)
}

# Get some good predictive covariates from the first project's step

mylogit = glm(hilary_elected ~ . , data = voteData, family = "binomial")

summary(mylogit)

X = model.matrix(mylogit)[]
lasso = cv.glmnet(X, voteData$hilary_elected, alpha = 1, family="binomial")

plot(lasso)

min = coef(lasso, s = "lambda.min")
lse = coef(lasso, s = "lambda.1se")

plot(lasso$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso$lambda.min, lasso$lambda.1se)), lty =2)

selected = lse[lse[,1]!=0,]
selectedNames = c(names(selected)[-1],"hilary_elected", 'county')

# selectedNames = c(names(selected)[-1],"hilary_elected") -> Try removing all states
lassoVoteData = voteData[, which(names(voteData) %in% selectedNames)]

summary(lassoVoteData)

# Try without Using Lasso - Simply use the VoteData - Include code to do so
# lassoVoteData = voteData[,-1]
# str(lassoVoteData)

# We fist divide our dataframe to a train and test split

set.seed(123)
smp_size = floor(0.75 * nrow(lassoVoteData))
train_ind = sample(seq_len(nrow(lassoVoteData)), size = smp_size)

trainData = lassoVoteData[train_ind, ]
testData = lassoVoteData[-train_ind, ]


# K - nn 
km1 = knn(train = trainData[,-2], test = testData[,-2], cl = trainData[,2], k = 4)
km2 = knn(train = trainData[,-2], test = testData[,-2], cl = trainData[,2], k = 50)

my.statistics(testData[,2], km1)
my.statistics(testData[,2], km2)

plot(testData[,1],testData[,3],col=as.numeric(testData[,2]+2),pch=19,cex=.8, ylim = c(0,100000), xlab="County Number", ylab="Population estimate", main="State population and votes Prediction")
points(testData[,1],testData[,3],col=as.numeric(km2)+2,lwd=2)

# knn.cv(trainData[,-2], cl = trainData[,2], k=7)

# So we clearly see the issue in this graph, where there are no clear distinctions between the votes (even in 2D)
# We expect similar behavior with all other classification methods

# We notice slightly better performance after using Lasso (initially 57%, jumped to 60%)

###################################################################
####  tree
###################################################################
fit1 = tree(as.factor(hilary_elected) ~ ., data = trainData)
tr = predict(fit1,testData[,-2],type='class')
my.statistics(testData[,2], tr)

# Our variable is categorical, so reduction in Variance is not an option for splitting. Let us assess different options

# Gini is the probability of correctly labeling a randomly chosen element if it was randomly labeled according to the distribution
# of labels in the node. Lower the Gini Impurity, higher is the homogeneity of the node
fit2 = tree(as.factor(hilary_elected) ~ ., data = trainData, split="gini")
tr2 = predict(fit1,testData[,-2],type='class')
my.statistics(testData[,2], tr2)
plot(fit2)
text(fit2) # Unclear to see what's going on

# Our data is not an extremely big frame. So we may also use information gain and see if we get a better result
# Let us also see chi-squared

boxM(Y = lassoVoteData[,-2],  group = lassoVoteData$hilary_elected)
# We avoid FDA since we don't assume multivariate Distribution w/ eq. variances

mq1=qda(hilary_elected ~ ., data = trainData)
mq2=predict(mq1, newdata = as.data.frame(testData[,-2]))$class
table(testData$hilary_elected,mq2)
my.statistics(testData[,2], mq2) # Lower accuracy from tree


# Naive Bayes
nbm <- naiveBayes(y = trainData$hilary_elected, x = trainData[,-2])
nbclass <- predict(nbm, testData[,-2])
table(testData$hilary_elected, nbclass)
my.statistics(testData[,2],nbclass)

# We Notice that all our models are at 57% accuracy levels.
# Historically we know that US elections have some of the states that could be either party's to take, called Swing states
# Would it make sense to see if it improves the model's performance if we exclude these states ?

# ***** In order to avoid duplicating code I'll have a commented out part in the top ******
# Removing the swing states we got an accuracy of 68%, which is a good jump. 
# After removing the county from the equation we get

# K-fold
trControl <- trainControl(method  = "cv",
                          number  = 5)

forFit = voteData
forFit$hilary_elected = as.factor(forFit$hilary_elected)
str(forFit)
kFoldFit <- train(hilary_elected ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = forFit)

kFoldFit

# PART 2 

colsToBeRemoved = c("PST045214", "PST040210", "PST120214", "POP010210","AGE135214", "AGE295214", "AGE775214", "SEX255214",
                "RHI125214", "RHI225214", "RHI325214", "RHI425214","RHI525214", "RHI625214", "RHI725214", "RHI825214",
                "POP715213", "POP645213", "POP815213", "EDU635213","EDU685213", "VET605213", "hilary_elected")

# moreColsToNotInclude = c("AGE135214","AGE295214","POP010210","PST120214","PST040210")
# clusterData = clusterData[,-which(names(clusterData) %in% moreColsToNotInclude)]
# head(clusterData)

clusterData = voteData[, -which(names(voteData) %in% colsToBeRemoved)]
head(clusterData)

# As we don’t want the k-means algorithm to depend to an arbitrary variable unit, we start by scaling the data. We know Kmeans is suspectible to outliers

scaledClusterData = scale(clusterData[,-c(1,2)])
scaledClusterData = data.frame(countiesAsFactors, scaledClusterData)
head(scaledClusterData, n=3)

scaledClusterData$countiesAsFactors = as.factor(scaledClusterData$countiesAsFactors)
pairs(scaledClusterData[, c(1,3:6)], col=as.numeric(scaledClusterData$countiesAsFactors)+1)
str(scaledClusterData)
ogData = scaledClusterData

scaledClusterData$countiesAsFactors = as.integer(scaledClusterData$countiesAsFactors)

# Use Sum of Squares
kmean_withinss = function(k) {
  cluster = kmeans(scaledClusterData, k)
  return (cluster$tot.withinss)
}

max_k <- 50 
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)
elbow <-data.frame(2:max_k, wss)

ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, max_k, by = 1))

# Also check with NbClust
# nbc = NbClust(data = scaledClusterData[,c(2:4)], distance = "euclidean", min.nc = 2, max.nc = 30, method = "ward.D", index = "alllong")
nbc = NbClust(data = scaledClusterData,
               distance = "euclidean",
               min.nc = 2, max.nc = 5,
               method = "ward.D",
               index = "alllong")

# From our graph it seems that we start to get diminishing results at k = 6

kmeansCluster = kmeans(scaledClusterData[,-1], centers = 6, nstart = 25)
kmeansCluster$size
# I notice some variance on the size of some clusters


# Using Mahalanobis distance


# Validation
# Hierarchical clustering 
# Silhouette graph - Dunn index - Elbow method

avg_sil <- function(k) {
  km.res <- kmeans(scaledClusterData, centers = k)
  ss <- silhouette(km.res$cluster, dist(scaledClusterData))
  mean(ss[, 3])
}

avg_sil_values <- sapply(2:max_k, avg_sil)

plot(2:max_k, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes",
     main = "Average Silhouettes per number of Clusters")

# plot(silhouette(kmeansCluster$cluster, dist(scaledClusterData[-1])), col=2:6, main ="ward")



# MANOVA





