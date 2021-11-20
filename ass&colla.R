#Problem#1
  setwd("~/Desktop/R/")
  cereal = read.csv("~/Downloads/Cereals.csv", stringsAsFactors = TRUE)
  View(cereal)

#1
#Explore your data and create a list showing all variables and their type
  str(cereal)

  # Discuss which variables are numeric and which ones are categorical
  # Categorical variables { /factor/ name, mfr, type}
  # Numerical variables   { /int/num/ calories, protein, fat, sodium, fiber, carbo, sugars, potass, vitamins, shelf, weight, cups,rating}

#converting all categorical variables to "dummy" variables 
  cerealname <- model.matrix(~ 0 + name, data = cereal)
  cerealname <- as.data.frame(cerealname)
  t(t(names(cerealname)))  
  
  mfr <- model.matrix(~ 0 + mfr, data = cereal)
  mfr <- as.data.frame(mfr)
  t(t(names(mfr)))  
  
  type <- model.matrix(~ 0 + type, data = cereal)
  type <- as.data.frame(type)
  t(t(names(type)))  

#2
#Compute the mean, median, min, max, and standard deviation for all numerical variables. Show your results with 2 decimal points only
  num_cereal = cereal[4:16]
  summary<-data.frame(
                      mean=sapply(num_cereal, mean, na.rm=TRUE),
                      median=sapply(num_cereal, median, na.rm=TRUE), 
                      max=sapply(num_cereal, max, na.rm=TRUE),
                      min=sapply(num_cereal, min, na.rm=TRUE),
                      standard_deviation=sapply(num_cereal, sd, na.rm=TRUE))
  cer_summary<-format(round(summary, 2), nsmall = 2)
  View(cer_summary)
# Which pairs of variables are most strongly correlated? 
  #Fiber and Potass correlation is about positive 0.91 which is the most positive 
  #Rating and Sugars correlation is about negative 0.75 which is the most negative
  
# What do you recommend to do with the variables that are strongly correlated? Why?
  # we should remove one of independent variables that strongly correlated.
  # because you run into the multicollinearity conundrum and your regression model's 
  # regression coefficients related to the two highly correlated variables will be unreliable.
  
#3
# histograms for sodium and calories
  install.packages("ggplot2")
  library(ggplot2)
  set.seed(1)
  par(mfrow =c(1,2))
  calh = hist(cereal$calories,ylim=c(0,30),main = "Histogram for Calories", xlab = "Calories", ylab = "Freq")
  text(calh$mids,calh$counts,labels=calh$counts, adj=c(0.5, -0.5))
  sodh = hist(cereal$sodium, ylim=c(0,25),main = "Histogram for sodium", xlab = "Sodium", ylab = "Freq")
  text(sodh$mids,sodh$counts,labels=sodh$counts, adj=c(0.5, -0.5))

# How many cereals have the highest sodium content?
  #  1 cereals have highest sodium
# How many cereals have the highest calories?
  #  1 cereals have highest calories

#4
  #Side by side Box plot
  boxplot(cereal$calories ~ cereal$type ,xlab = "type", ylab = "calories")
  # In this plot we are know in C type has 7 outliers
  # 1st quartile equal to 100 calories
  # 3rd quartile same as median equal to 110 calories
  # There is no outlier in H type


#----------------------------------------------------------------------------------------------------------


#Problem#2
  cosmetic = read.csv("~/Downloads/Cosmetics(1).csv", stringsAsFactors = TRUE)
  View(cosmetic)
#1
#Explain what the first row of the binary transaction matrix means:
  # The 0 means no purchase, the 1 is meaning purchased, for example under the bag is 0 meaning this item is not purchased, under the blush has 1 meaning this item has purchased.

#2
#: support = 10% and confidence = 50%. Show some rules sorted by lift
  install.packages("arules")
  library(arules)
  c.mat <- as.matrix(cosmetic[,-1])
  c.trans <- as(c.mat, "transactions")
  inspect(c.trans)
  # get rules; when running apriori(), include minimum support & confidence, & target as arguments.
  c.rules <- apriori(c.trans, 
                   parameter = list(supp = 0.1, conf = 0.5, target = "rules"))
  # inspect the first six rules, sorted by their lift
  inspect(head(sort(c.rules, by = "lift"), n = 10))
  
#3
#Comment on the first association rule emerging from your output
  # There are 1000 of transactions in this data sets, about 15% which is 149 transaction people about Brushes and Nail.Polish.
  # There are also higher percentage in this list, such as 
  # {Concealer,Eye.shadow} has 18% purchase with {Mascara}
  # {Blush,Eye.shadow} has 17% purchase with {Mascara}
  # so can we see {Mascara} has higher level purchase chance in most transactions

#4
  #Support: 15% of all transactions in which {Brushes, Nail.Polish} are purchased

#5
  #Confidence: if {Brushes] were purchased, there is high rate percent of the time {Nail.Polish} was purchased.

#6
  # Use lift ratio to Indicates how efficient the rule is in finding consequences
  # lift ratio we are 3.6 times more likely to find a transaction with {Brushes} 
  # IF we only look at those transactions where {Nail.Polish} are purchased 
  # compared to searching randomly in all transactions

#----------------------------------------------------------------------------------------------------------

#Problem#3  Collaborative Filtering
  course = read.csv("~/Downloads/courserating.csv", stringsAsFactors = TRUE)
  View(course)
#1
  # LN and DS is possible to compute correlations with E.N
  
#2
  # The single nearest student to EN is LN, so the single course we should recommend to EN is Python
  # Because in the LN courses,the additonal to the same as EN, and Python has higher rating, thats why we should recommend Python to EN
  
#3
  install.packages("recommenderlab")
  library(recommenderlab)
  course = course[,-1]
  m1 <- as.matrix(course)
  r1 <- as(m1, "realRatingMatrix")
  similarity(r1, method = "cosine", which = "users",min_matching = 2)
  UB.Rec <- Recommender(r1, "UBCF", param=list(normalize = NULL, method="cosine"))
  pred <- predict(UB.Rec, r1[1,])
  as(pred, "matrix")
  pred <- predict(UB.Rec, r1[2,])
  as(pred, "matrix")
  pred <- predict(UB.Rec, r1[3,])
  as(pred, "matrix")
  pred <- predict(UB.Rec, r1[4,])
  as(pred, "matrix")
  pred <- predict(UB.Rec, r1[5,])
  as(pred, "matrix")
  # Base from the R code testing, we should recommend Python to EN

#4
#What is the conceptual difference between using the correlation as opposed to cosine similarities? 
  # Correlation is only able to caculate between two items when they have two or more common items, if some data missing it is not possible to assess the correlation in the usual way,
  # In this case, in order to access the correlation between two variables in the presence of missing data, one way is to carry out "complete case analysis" which involves
  # excluding all cases in  which either one or both of the values being correlated are missing.
  # However, the missing value in the data set will not affect the result if use similarities, we dont need to worry about that.
  # This is the different between using correlation and similarities,one needs to consider missing value and another don't need to.
  
#5
  # With out computing calculation, beside Python, SQL should be another recommendation course
  
#6
  UB.Rec <- Recommender(r1, "UBCF", param=list(normalize = NULL, method="cosine"))
  pred <- predict(UB.Rec, r1[4,],n = 1,type="topNList")
  as(pred, "matrix")
  # According the result, Python is the recommendation for EN







