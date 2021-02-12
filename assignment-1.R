library(plotly)
require(corrplot)
require(glmnet)
library(dplyr)
library(aod)

existsIn = function (item, array, arrayAsString){
  if(item %in% array | grepl(item, arrayAsString)){
    return(item)
  }
}

setwd("/Users/stavros/Documents/BI/Statistics II/Project I")
voteData = read.csv("combined_data.csv", sep = ",")

str(voteData)
summary(voteData)
sum(apply(voteData,2, is.na))

# White alone
plot_ly(x=voteData$state, y=voteData$hilary_prob, z=voteData$RHI825214, type="scatter3d", mode="markers", color=voteData$state, main = " Hilary success percentage per state based on White population percentage")

voteData = voteData[, -c(1:4, 6:12)]

# Attempting to run based on a binomial model since there is a vote on either Sanders or Hilary

popInsight= voteData[,-c( 5, 8:17, 19:23, 33 )]

par(mfrow = c(1,1))
corrplot(cor(popInsight))
# Corrplot shows us there is strong correlation between some of the covariates, that would essentially be removed using Lass

# Starting GLM
mylogit = glm(hilary_elected ~ . , data = voteData, family = "binomial")

summary(mylogit)

X = model.matrix(mylogit)[]
lasso = cv.glmnet(X, voteData$hilary_elected, alpha = 1, family="binomial")

plot(lasso)

min = coef(lasso, s = "lambda.min")
lse = coef(lasso, s = "lambda.1se")

plot(lasso$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso$lambda.min, lasso$lambda.1se)), lty =2)

selected = min[min[,1]!=0,]
selectedNames = c(names(selected)[-1],"hilary_elected")

collapsedNames = paste(selectedNames, collapse= " ")
newSelectedNames = sapply(colnames(voteData), existsIn, selectedNames, collapsedNames)
newSelectedNames = newSelectedNames[!sapply(newSelectedNames, is.null)]

lassoModel = glm(hilary_elected ~ ., data = select(voteData,names(newSelectedNames)), family = "binomial")
summary(lassoModel)

aicModel = step(lassoModel, direction = 'both')
summary(aicModel)

# Acting as our base model

finalModel1 = glm(hilary_elected ~ AGE295214 + RHI325214 + RHI825214 + HSG445213 + HSG096213 + HSD310213 + PVY020213 + SBO415207, data = voteData, family='binomial')

summary(finalModel1)

vcov(finalModel1)

wald.test(b = coef(finalModel1), Sigma = vcov(finalModel1), Terms = c(2:4)) # Low p-value, statistically important but not a great pick probably

# Goodness of Fit

with(finalModel1, pchisq(deviance, df.residual, lower.tail = FALSE)) # just checking that it's btter than the null 

confint(finalModel1) # With a 95% confidence we don't necesserliy reject H0 if 0 is included 

with(finalModel1, null.deviance - deviance)
with(finalModel1, df.null - df.residual)
with(finalModel1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) # Significant diff between our model and null model

par(mfrow = c(2,2), mar = c(5,5,1,1))

plot(voteData$RHI825214, resid(finalModel1, type = 'pearson'), ylab = 'Residuals (Pearson)', xlab = 'White alone', cex.lab = 1.5, cex.axis = 1.5, pch = 16, col = 'blue', cex = 1.5)
plot(voteData$PVY020213, resid(finalModel1, type = 'pearson'), ylab = 'Residuals (Pearson)', xlab = 'Persons below poverty level', cex.lab = 1.5, cex.axis = 1.5, pch = 16, col = 'blue', cex = 1.5)
plot(voteData$RHI825214, resid(finalModel1, type = 'deviance'), ylab = 'Residuals (Deviance)', xlab = 'White alone', cex.lab = 1.5, cex.axis = 1.5, pch = 16, col = 'red', cex = 1.5)
plot(voteData$PVY020213, resid(finalModel1, type = 'deviance'), ylab = 'Residuals (Deviance)', xlab = 'Persons below poverty level', cex.lab = 1.5, cex.axis = 1.5, pch = 16, col = 'red', cex = 1.5)


finalModel2 = glm(hilary_elected ~ RHI325214 + RHI825214 + HSD310213 + PVY020213, data = voteData, family='binomial')

with(finalModel2, pchisq(deviance, df.residual, lower.tail = FALSE))
with(finalModel1, pchisq(deviance, df.residual, lower.tail = FALSE))

interactionModel = glm(hilary_elected ~  PVY020213 + RHI825214 + HSD310213 + PVY020213 * PVY020213 * RHI825214, data = voteData, family='binomial')
summary(interactionModel)


with(interactionModel, null.deviance - deviance)
with(interactionModel, df.null - df.residual)
with(interactionModel, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)) # actually worsens ? 
