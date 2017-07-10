library(lattice)
library(boot)
library(effects)
library(caTools)
library(rpart)
library(e1071)
library(nnet)
library(rpart.plot)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')


set.seed(100)
options(scipen=999)

#loading in the dataset and preprocessing data
rwanda                                                         <- read.csv("../survey.csv")
rwanda[rwanda$business_start == '########',]                   <- NA
rwanda$sell_food_assistance[rwanda$sell_food_assistance == ""] <- NA
rwanda$outside_job                                             <- as.numeric(rwanda$outside_job)
rwanda$business_start <- as.numeric(rwanda$business_start)



rwanda <- subset(rwanda, select=c("camp_name","competition","num_employee","market_condition","market_security","cash_food_local", 
                                  "outside_job", "income_compare","business_start", "customer_locations", "customer_locations_camp_change",
                                  "entrepreneurship_training", "training_grow", "business_leave_camp" ,"leave_camp_support_business", "id_problem_fequency",
                                  "key_good_demand_change","avg_customers","x", "y"))
mug <- rwanda[rwanda$camp_name == 'mugombwa',]
kim <- rwanda[rwanda$camp_name == 'kigeme',]

xyplot(avg_customers ~ business_start, data = rwanda,
       ylab = "Average Number of Customers",
       xlab = "Days Since Camp Opened",
       main = "Average Number of Customer VS Days",
       type = c("p","r"))

#linear regression, DV ~ IV, predicting business_start from avg_customer (not signficant)
linear.model <- lm(business_start ~ avg_customers, data = rwanda)
summary(linear.model)

#linear regression, DV ~ .(all variables), seeing if any other variables are signficant to business_start(none are signficant)
linear.model1 <- lm(business_start ~ ., data = rwanda)
summary(linear.model1)

#linear regression, DV ~ IV1 + IV2 + IV3 + IV4, key good demand change and customer change camp change are signficant to predicting num of employees
linear.model2 <- lm(num_employee ~ key_good_demand_change + customer_locations_camp_change + x +y, data = rwanda)
summary(linear.model2)

#linear regression, DV ~ IV, predicting average customers from x and y, not signficant
linear.model3 <- lm(avg_customers ~ x + y , data = rwanda)
summary(linear.model3)

#linear regression, Dv ~ IV, predicting average customer from num employee, not signficant
linear.model4 <- lm(avg_customers ~ num_employee , data = rwanda)
summary(linear.model4)

#linear regression, DV ~ IV, predicting business start from x and y, the variables x and y 
# are signficant but the complete model is not signficant
linear.model5 <- lm(business_start ~ x + y, data = rwanda)
summary(linear.model5)

#reloading the data for logistic regression analysis
rwanda  <- read.csv("../survey.csv")
rwanda$business_start <- as.numeric(rwanda$business_start)

#glm is general logistical models, when you specifict the family as binomial its becomes logistric regression
#outside job is a strong predictor of sell food assistance
log.model1 <- glm(sell_food_assistance ~ outside_job, data = rwanda, family=binomial)
summary(log.model1)


#logistic regression: avg customer is not a strong predictor of competition
log.model2 <- glm(competition ~ avg_customers, data = rwanda, family=binomial)
summary(log.model2)

#logistic regression: business start is not a strong predictor of fincances care
log.model3 <- glm(finances_care ~ business_start, data = rwanda, family=binomial)
summary(log.model3)


#machine learning: classfication trees, nuernets and support vector machines
rwanda                                                         <- read.csv("../survey.csv")
set.seed(100)
rwanda[rwanda$business_start == '########',]                   <- NA
rwanda$sell_food_assistance[rwanda$sell_food_assistance == ""] <- NA
rwanda$outside_job                                             <- as.numeric(rwanda$outside_job)
rwanda$business_start                                          <- as.numeric(rwanda$business_start)
rwanda                                                         <- subset(rwanda, select=c("camp_name","competition","num_employee","market_condition","market_security","cash_food_local", 
                                  "outside_job", "income_compare","business_start", "customer_locations", "customer_locations_camp_change",
                                  "entrepreneurship_training", "training_grow", "business_leave_camp" ,"leave_camp_support_business", "id_problem_fequency",
                                  "key_good_demand_change","avg_customers","x", "y"))
rwanda                                                        <- na.omit(rwanda)


#dividing up the data into training and testing
sample <- sample.int(n = nrow(rwanda), size = floor(.75*nrow(rwanda)), replace = F)
train  <- rwanda[sample, ]
test   <- rwanda[-sample, ]

#predicting competiton using all variables avaialable, 46% accuracy is not great
tree.model <- rpart(competition ~. , data = train, method= "class")
pred       <- predict(tree.model,test , type = "class")
predTable  <- table(pred, test$competition)
val <- sum(diag(predTable))/sum(predTable)
val <- round((val*100),2)
print(sprintf("Accuracy for classifcation decision tree is %s percent",val)) 
rpart.plot(tree.model)

#support vector machine of the same
svm.model <- svm(competition ~ . , data = train, method= "class")
pred = predict(svm.model,test , type = "class")
predTable <- table(pred, test$competition)
val <- sum(diag(predTable))/sum(predTable)
val <- round((val*100),2)
print(sprintf("Accuracy for svm is %s percent",val)) 

#trying nueral network
nn.model <- nnet(competition ~. , data = train,linout=TRUE, size=5, trace = FALSE)
pred = predict(nn.model,test , type = "class")
predTable <- table(pred, test$competition)
sum(diag(predTable))/sum(predTable)
plot.nnet(nn.model)


tree.model2 <- rpart(key_good_demand_change ~ . , data = train, method= "class")
pred = predict(tree.model2,test , type = "class")
predTable <- table(pred, test$key_good_demand_change)
val <- sum(diag(predTable))/sum(predTable)
val <- round((val*100),2)
print(sprintf("Classfication tree - predicting key_good_demand_change accuracy is %s percent",val)) 
rpart.plot(tree.model2)

tree.model3 <- rpart(camp_name ~ .-x -y , data = train, method= "class")
pred = predict(tree.model3,test , type = "class")
predTable <- table(pred, test$camp_name)
sum(diag(predTable))/sum(predTable)
predTable <- table(pred, test$key_good_demand_change)
val <- sum(diag(predTable))/sum(predTable)
val <- round((val*100),2)
print(sprintf("Classfication tree - predicting camp name without x and y accuracy is %s percent",val)) 
rpart.plot(tree.model3)


coln <- c()
for(ele in colnames(rwanda)){
  if (eval(parse(text=paste0("is.numeric(rwanda$",ele,")"))) == FALSE){
    coln <- c(ele, coln)
  }
}

#running through all non numeric columns with sufficient data through classifcation tress, neural netwokrks, and SVMs
for(ele in coln ){
  print(ele)
  tree.model <- eval(parse(text=paste0("rpart(",ele," ~ . , data = train, method= 'class')")))
  pred = predict(tree.model,test , type = "class")
  predTable <- table(pred, eval(parse(text=paste0("test$",ele))))
  val <- sum(diag(predTable))/sum(predTable)
  val <- val * 100
  val <- round(val,2)
  print(sprintf("CT Accuracy is %s percent",val))  
  

  svm.model <- eval(parse(text=paste0("svm(",ele," ~ . , data = train, method= 'class')")))
  pred <- predict(svm.model,test , type = "class")
  predTable <- table(pred, eval(parse(text=paste0("test$",ele))))
  val <- sum(diag(predTable))/sum(predTable)
  val <- val * 100
  val <- round(val,2)
  print(sprintf("SVM Accuracy is %s percent",val))  
  
  tryCatch({
  nn.model <- eval(parse(text=paste0("nnet(",ele," ~ . , data = train, ,linout=FALSE, size=5, trace = FALSE)")))
  pred = predict(nn.model,test , type = "class")
  predTable <- table(pred, test$competition)
  sum(diag(predTable))/sum(predTable)
  val <- sum(diag(predTable))/sum(predTable)
  val <- val * 100
  val <- round(val,2)
  print(sprintf("NN Accuracy is %s percent",val))
  }, warning = function(e) {
  })
  print("-----------------")
}












