setwd("/Users/peterzhang/Desktop/CLU/R")

#--------------------------------------  EastWestAirlinesCluster.csv  --------------------------------------------------------

ewa = read.csv("/Users/peterzhang/Downloads/EastWestAirlinesCluster.csv",stringsAsFactors = TRUE)
View(ewa)
missval = sapply(ewa,function(x) sum(is.na(x)))
missval
#A. Apply hierarchical clustering with Euclidian distance. Make sure to normalize the data first. How
#many clusters appear? Choose a cutoff distance and discuss how many clusters you selected.

ewa = ewa[,-1]
ewa.norm = scale(ewa)
ewa.norm.dist = dist(ewa.norm,method = "euclidean")
ewa.hclust = hclust(ewa.norm.dist,method = "ward.D")
plot(ewa.hclust)


#B. What would happen if data were not normalized?
# Answer: 
#   It is important that a database is normalized to minimize redundancy (duplicate data) and to ensure only related data is stored in each table. 
#   It also prevents any issues stemming from database modifications such as insertions, deletions, and updates 
#   If we don't normalize the data, the machine learning algorithm will be dominated by the variables that use a larger scale, 
#   adversely affecting model performance. This makes it imperative to normalize the data


#C. Compare the cluster centroids to characterize the different clusters using the aggregate()
#function and try to label the clusters based on all or any of the variables in the dataset. Describe
#the characteristics of each cluster.
memb = cutree(ewa.hclust,k = 3)
table(memb)
memb = cutree(ewa.hclust,k = 5)
table(memb)

ewa.norm = as.data.frame(ewa.norm)
centers.unnorm = aggregate(. ~ memb, data = ewa.norm, FUN = mean)
centers.unnorm

#D. Use k-means clustering with k = the number of selected above. Use a visual approach
#(fviz_cluster) to see your clusters. Discuss the results.

install.packages("factoextra")
library(ggplot2)
library(factoextra)

k2 <- kmeans(ewa.norm, 2)
k3 <- kmeans(ewa.norm, 3)
k4 <- kmeans(ewa.norm, 4)
k5 <- kmeans(ewa.norm, 5)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = ewa) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = ewa) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = ewa) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = ewa) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

# Base from the visualization showing,cluster center  = 3 is the best visualization


#E. Plot an Elbow chart. Based on the results, how many clusters should you choose? Re-run kmeans with k = the number of clusters you selected based on the Elbow chart. Use a visual
#approach (fviz_cluster) to see your clusters. Discuss the results. 

set.seed(123)
ewa.kmeans = function(k) {
  mean(kmeans(ewa.norm,k)$withinss)
}
k.values <- 1:8
library(purrr)
wss_values <- map_dbl(k.values, ewa.kmeans)
plot(k.values, wss_values, type="b", pch = 10,
     xlab="Number of clusters K", 
     ylab="Average within-clusters sum of squared distance")

# Base from the the Elbow Chart, cluster  = 3 is the best option
# so the elbow Chart and the fviz_cluster visualization result are match.




#--------------------------------------   UniversalBank(1).csv  --------------------------------------------------------

banks.df = read.csv("/Users/peterzhang/Downloads/UniversalBank(1).csv",stringsAsFactors = TRUE)
View(banks.df)
missval = sapply(banks.df,function(x) sum(is.na(x)))
missval
t(t(names(banks.df)))
#drops <- c("ID","ZIP.Code","Personal.Loan")
#banks.df = as.data.frame(banks.df[ , !(names(banks.df) %in% drops)])


#A. Partition the data into training (60%) and validation (40%) sets. Consider the following
#customer: Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2, Education = 2,
#Mortgage = 0, Securities Account = 0, CD Account = 0, Online = 1, and Credit Card = 1.
#Perform a k-NN classification with all predictors (except ID and ZIP code) using k = 1. How would
#this customer be classified?
set.seed(60)

train.rows <- sample(row.names(banks.df), 0.6*dim(banks.df)[1])  
valid.rows <- setdiff(row.names(banks.df), train.rows)  
train.df <- banks.df[train.rows, -c(1,5)]
valid.df <- banks.df[valid.rows, -c(1,5)]


# initialize normalized training, validation data, complete data frames to originals
#train.norm.df <- train.df
#valid.norm.df <- valid.df
#banks.norm.df <- banks.df

#normalize
norm.values <- preProcess(train.df[-8], method=c("center", "scale"))
train.norm.df <- predict(norm.values, train.df[-8])
valid.norm.df<- predict(norm.values, valid.df[-8])
banks.norm.df<- predict(norm.values, banks.df[-c(1,5,10)])

#New Data
new.df <- data.frame(Age=40,Experience=10,Income=84,Family=2,CCAvg=2,Education=2,Mortgage=0,Securities.Account=0,CD.Account=0,Online=1,CreditCard=1) 
new.norm.df <- predict(norm.values, new.df)

#Prediction with k = 1 
knn.pred <- class::knn(train = banks.norm.df,
                       test = new.norm.df,
                       cl = banks.df$Personal.Loan, k = 1)
knn.pred
# This specific person will not get a personal loan from the bank considering all the aspects 


#B. What is a choice of k that balances between overfitting (k too small) and ignoring the predictor
#information (k too large)? Write R code to derive this information and interpret the results.

library(caret)
accuracy.df <- data.frame(k = seq(1, 10, 1), RMSE = rep(0, 5))
for (i in 1:10) {
  knn.pred <- class::knn(train = train.norm.df, 
                         test = valid.norm.df, 
                         cl = train.df$Personal.Loan, k = i)
  class(knn.pred)
  accuracy.df[i, 2] <- RMSE(as.numeric(as.character(knn.pred)), valid.df$Personal.Loan)
}
accuracy.df
#> accuracy.df
#k      RMSE
#1   1 0.2085665
#2   2 0.2202272
#3   3 0.2037155
#4   4 0.2049390
#5   5 0.2097618
#6   6 0.2073644
#7   7 0.2144761
#8   8 0.2179449
#9   9 0.2213594
#10 10 0.2213594


#C. Consider the following customer: Age = 40, Experience = 10, Income = 84, Family = 2, CCAvg = 2,
#Education = 2, Mortgage = 0, Securities Account = 0, CD Account = 0, Online = 1 and Credit Card= 1. 
#Classify the customer using the best k you found above.

# I would say k = 4 would be the best way to classify this customer 

#D. Repartition the data, this time into training (80%) and validation sets (20%). Apply the k-NN
#method with the k chosen above. And re-classify the customer at C based on these other
#partitions. Discuss the results. 

set.seed(80)

train.rows <- sample(row.names(banks.df), 0.8*dim(banks.df)[1])  
valid.rows <- setdiff(row.names(banks.df), train.rows)  
train.df <- banks.df[train.rows, -c(1,5)]
valid.df <- banks.df[valid.rows, -c(1,5)]


# initialize normalized training, validation data, complete data frames to originals
#train.norm.df <- train.df
#valid.norm.df <- valid.df
#banks.norm.df <- banks.df

#normalize
norm.values <- preProcess(train.df[-8], method=c("center", "scale"))
train.norm.df <- predict(norm.values, train.df[-8])
valid.norm.df<- predict(norm.values, valid.df[-8])
banks.norm.df<- predict(norm.values, banks.df[-c(1,5,10)])

#New Data
new.df <- data.frame(Age=40,Experience=10,Income=84,Family=2,CCAvg=2,Education=2,Mortgage=0,Securities.Account=0,CD.Account=0,Online=1,CreditCard=1) 
new.norm.df <- predict(norm.values, new.df)

#Prediction with k = 1 
knn.pred <- class::knn(train = banks.norm.df,
                       test = new.norm.df,
                       cl = banks.df$Personal.Loan, k = 1)
knn.pred

accuracy.df <- data.frame(k = seq(1, 5, 1), RMSE = rep(0, 5))
for (i in 1:5) {
  knn.pred <- class::knn(train = train.norm.df, 
                         test = valid.norm.df, 
                         cl = train.df$Personal.Loan, k = i)
  class(knn.pred)
  accuracy.df[i, 2] <- RMSE(as.numeric(as.character(knn.pred)), valid.df$Personal.Loan)
}
accuracy.df
#k      RMSE
#1   1 0.2024846
#2   2 0.2000000
#3   3 0.1760682
#4   4 0.1870829
#5   5 0.1870829
#6   6 0.1974842
#7   7 0.2000000
#8   8 0.2024846
#9   9 0.2024846
#10 10 0.2024846

# Since 4 and 5 are the same I could say both k = 4 and k = 5 would be the best way to classify this customer 
