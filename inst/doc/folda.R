## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(folda)
mpg <- as.data.frame(ggplot2::mpg) # Prepare the data
datX <- mpg[, -5] # All predictors without Y
response <- mpg[, 5] # we try to predict "cyl" (number of cylinders)

## -----------------------------------------------------------------------------
fit <- folda(datX = datX, response = response, subsetMethod = "all")

## -----------------------------------------------------------------------------
fit <- folda(datX = datX, response = response, subsetMethod = "forward", testStat = "Pillai")
print(fit) # 6 out of 11 variables are selected, displ is the most important among them

## ----fig.asp=0.618,out.width = "70%",fig.align = "center"---------------------
plot(fit, datX = datX, response = response)

## ----fig.asp=0.618,out.width = "70%",fig.align = "center"---------------------
# A 1D plot is created when there is only one feature 
# or for binary classification problems.
mpgSmall <- mpg[, c("cyl", "displ")]
fitSmall <- folda(mpgSmall[, -1, drop = FALSE], mpgSmall[, 1])
plot(fitSmall, mpgSmall, mpgSmall[, 1])

## -----------------------------------------------------------------------------
head(predict(fit, datX, type = "response"))
head(predict(fit, datX, type = "prob")) # Posterior probabilities

## -----------------------------------------------------------------------------
fitW <- folda(mpg[, -2], mpg[, 2], testStat = "Wilks")
fitW$forwardInfo

## -----------------------------------------------------------------------------
fitP <- folda(mpg[, -2], mpg[, 2], testStat = "Pillai")
fitP$forwardInfo

## -----------------------------------------------------------------------------
# MASS::lda(model~., data = mpg)

#> Error in lda.default(x, grouping, ...) : 
#>   variables  1  2  3  4  5  6  7  8  9 10 11 12 13 14 27 28 37 38 40 appear to be constant within groups

## -----------------------------------------------------------------------------
# Create a dataset with missing values
(datNA <- data.frame(X1 = rep(NA, 5), # All values are NA
                     X2 = factor(rep(NA, 5), levels = LETTERS[1:3]), # Factor with all NA values
                     X3 = 1:5, # Numeric column with no missing values
                     X4 = LETTERS[1:5], # Character column
                     X5 = c(NA, 2, 3, 10, NA), # Numeric column with missing values
                     X6 = factor(c("A", NA, NA, "B", "B"), levels = LETTERS[1:3]))) # Factor with missing values

## -----------------------------------------------------------------------------
(imputedSummary <- missingFix(datNA))

## -----------------------------------------------------------------------------
(datNAnew <- data.frame(X1 = 1:3, # New column not in the reference
                        X3 = 1:3, # Matching column with no NAs
                        X4 = as.factor(c("E", "F", NA)), # Factor with a new level "F" and missing values
                        X5 = c(NA, 2, 3))) # Numeric column with a missing value

## -----------------------------------------------------------------------------
getDataInShape(datNAnew, imputedSummary$ref)

## -----------------------------------------------------------------------------
sapply(airquality, anyNA) # Ozone and Solar.R have NAs

## -----------------------------------------------------------------------------
fitAir <- folda(airquality[, -5], airquality[, 5])

## -----------------------------------------------------------------------------
fitAir$misReference

## -----------------------------------------------------------------------------
predict(fitAir, data.frame(rep(NA, 4)))

## -----------------------------------------------------------------------------
table(mpg$cyl)

## -----------------------------------------------------------------------------
set.seed(443)
fitCyl <- folda(mpg[, -5], mpg[, 5], downSampling = TRUE)
fitCyl$confusionMatrix

## -----------------------------------------------------------------------------
fitCyl30 <- folda(mpg[, -5], mpg[, 5], downSampling = TRUE, kSample = 30)
fitCyl30$confusionMatrix

## -----------------------------------------------------------------------------
fitCylWithPrior <- folda(mpg[, -5], mpg[, 5], downSampling = TRUE, prior = table(mpg[, 5]))
fitCylWithPrior$confusionMatrix

## -----------------------------------------------------------------------------
table(iris$Species, dnn = NULL)

## -----------------------------------------------------------------------------
misClassCost <- matrix(c(0, 100, 1,
                         1, 0, 1,
                         1, 100, 0), 3, 3, byrow = TRUE)

## -----------------------------------------------------------------------------
fitEqualCost <- folda(iris[, -5], response = iris[, 5])
fitNewCost <- folda(iris[, -5], response = iris[, 5], misClassCost = misClassCost)

## -----------------------------------------------------------------------------
table(predict(fitEqualCost, iris), dnn = NULL)

## -----------------------------------------------------------------------------
table(predict(fitNewCost, iris), dnn = NULL)

