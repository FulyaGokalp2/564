# KNN

library(ISLR)
library(class)
data(Default)
head(Default)

set.seed(42)
Default$student <- as.numeric(Default$student) - 1
head(Default)
str(Default)

summary(Default)
default_idx <- sample(nrow(Default), 5000)
default_trn <- Default[default_idx, ]
default_tst <- Default[-default_idx, ]

# training data
X_default_trn <- default_trn[ ,-1]
y_default_trn <- default_trn$default
head(X_default_trn)
head(y_default_trn)

# testing data
X_default_tst <- default_tst[, -1]
y_default_tst <- default_tst$default

head(knn(train = X_default_trn,
    test = X_default_tst,
    cl = y_default_trn,
    k = 3))

calc_class_err = function(actual, predicted){
  mean(actual != predicted)
}

calc_class_err(actual = y_default_tst,
               predicted = knn(train = X_default_trn,
                               test = X_default_tst,
                               cl = y_default_trn,
                               k = 5))

calc_class_err(actual = y_default_tst,
               predicted = knn(train = scale(X_default_trn),
                               test = scale(X_default_tst),
                               cl = y_default_trn,
                               k = 5))

set.seed(42)
k_to_try = 1:100
err_k = rep(x = 0, times = length(k_to_try))

for (i in seq_along(k_to_try)) {
  pred = knn(train = scale(X_default_trn),
             test = scale(X_default_tst),
             cl = y_default_trn,
             k = k_to_try[i])
  err_k[i] = calc_class_err(y_default_tst, pred)
}

plot(err_k, type = "b", col="dodgerblue", cex=1, pch=20,
     xlab = "k, number of n.",
     ylab = "classification error",
     main = "(Test) Error Rate vs N.")
abline(h=min(err_k), col="darkorange", lty=3)
abline(h=mean(y_default_tst=="Yes"), col="grey", 
       lty=2)

dev.print(pdf, "kPlot.pdf")

min(err_k)
which(err_k == min(err_k))

predicted = knn(train = scale(X_default_trn),
                   test = scale(X_default_tst),
                   cl = y_default_trn,
                   k = 24)
table(predicted)
table(y_default_tst)

tab <- table(predicted, y_default_tst)
tab

accuracy <- function(x){sum(diag(x)/sum(rowSums(x)))*100}
accuracy(tab)


library(caret)

confusionMatrix(tab)
