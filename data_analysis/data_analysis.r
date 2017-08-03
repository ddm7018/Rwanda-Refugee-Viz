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
options(warn=0)

#loading in the dataset and preprocessing data
rwanda                                                         <- read.csv("../survey2.csv", header = TRUE)

rwanda$business_start[rwanda$business_start == '########']     <- NA
#rwanda$sell_food_assistance[rwanda$sell_food_assistance == ""] <- NA
rwanda$outside_job                                             <- as.numeric(rwanda$outside_job)
rwanda$business_start <- as.numeric(rwanda$business_start)

#sellfoodassistance is the issue

rwanda <- subset(rwanda, select=c("camp_name","competition","num_employee","market_condition","market_security","cash_food_local", 
                                  "outside_job", "income_compare","business_start", "customer_locations", "customer_locations_camp_change",
                                  "entrepreneurship_training", "business_leave_camp" ,"leave_camp_support_business", "id_problem_fequency",
                                  "key_good_demand_change","avg_customers","x", "y","finances_care_underbed","finances_care_remmitances","finances_care_mobile","finances_care_other_text", "training_grow_financing", "training_grow_training",
                                  "business_type_clothing","sell_food_assistance","business_type_grocery", "business_type_restaurant", "business_type_craft","ibs_comm", "ibs_saco", "ibs_mobile", "ibs_microloan","ibs_nobank"))
mug <- rwanda[rwanda$camp_name == 'mugombwa',]
kim <- rwanda[rwanda$camp_name == 'kigeme',]




run_linear_regression_models <- function(data){
#linear regression, DV ~ IV, predicting business_start from avg_customer (not signficant)
linear.model <- lm(business_start ~ avg_customers, data = data)
print(summary(linear.model))


#linear regression, DV ~ .(all variables), seeing if any other variables are signficant to business_start(none are signficant)
# linear.model1 <- lm(business_start ~ ., data = data)
# print(summary(linear.model1))

#linear regression, DV ~ IV1 + IV2 + IV3 + IV4, key good demand change and customer change camp change are signficant to predicting num of employees
linear.model2 <- lm(num_employee ~ key_good_demand_change + customer_locations_camp_change + market_security, data = data)
print(summary(linear.model2))
par(mfrow = c(2, 2)) 
plot(linear.model2)

#linear regression, DV ~ IV, predicting average customers from x and y, not signficant
linear.model3 <- lm(avg_customers ~ x + y , data = data)
print(summary(linear.model3))

#linear regression, Dv ~ IV, predicting average customer from num employee, not signficant
linear.model4 <- lm(avg_customers ~ num_employee , data = data)
print(summary(linear.model4))

#linear regression, DV ~ IV, predicting business start from x and y, the variables x and y 
# are signficant but the complete model is not signficant
linear.model5 <- lm(business_start ~ x + y, data = data)
print(summary(linear.model5))

}
run_linear_regression_models(mug)
run_linear_regression_models(kim)

#reloading the data for logistic regression analysis

#recheck
kim$sell_food_assistance <- NULL

log.model1 <- glm(sell_food_assistance ~ outside_job, data = mug, family=binomial)
print(summary(log.model1))

run_logistic_regression_models <- function(data){

  # #logistic regression: avg customer is not a strong predictor of competition
  log.model2 <- glm(competition ~ avg_customers, data = data, family=binomial)
  print(summary(log.model2))
  
  # #logistic regression: business start is not a strong predictor of fincances care
  log.model3 <- glm(finances_care_mobile ~ business_start, data = data, family=binomial)
  print(summary(log.model3))
  
}
run_logistic_regression_models(kim)
run_logistic_regression_models(mug)

run_classfication_models <- function(data, print = FALSE){

#dividing up the data into training and testing
set.seed(100)
sample <- sample.int(n = nrow(data), size = floor(.75*nrow(data)), replace = F)
train  <- data[sample, ]
test   <- data[-sample, ]

#predicting competiton using all variables avaialable, 46% accuracy is not great
tree.model <- rpart(competition ~. , data = train)
pred       <- predict(tree.model,test , type = "class")
predTable  <- table(pred, test$competition)
val <- sum(diag(predTable))/sum(predTable)
val <- round((val*100),2)
print(sprintf("Accuracy for classifcation decision tree is %s percent",val)) 
#rpart.plot(tree.model)

#support vector machine of the same
svm.model <- svm(competition ~ . , data = train)
test <- na.omit(test)
pred = predict(svm.model,test , type = "class")
predTable <- table(pred, test$competition)
val <- sum(diag(predTable))/sum(predTable)
val <- round((val*100),2)
print(sprintf("Accuracy for svm is %s percent",val)) 

#trying nueral network
nn.model <- nnet(competition ~. , data = train,linout=TRUE, size=5, trace = FALSE)
pred = predict(nn.model,test , type = "class")
predTable <- table(pred, test$competition)
val <- sum(diag(predTable))/sum(predTable)
val <- round((val*100),2)
print(sprintf("Accuracy for nueral network predicting competition is %s percent",val)) 
#plot.nnet(nn.model)


tree.model2 <- rpart(key_good_demand_change ~ . , data = train, method= "class")
pred = predict(tree.model2,test , type = "class")
predTable <- table(pred, test$key_good_demand_change)
val <- sum(diag(predTable))/sum(predTable)
val <- round((val*100),2)
print(sprintf("Classfication tree - predicting key_good_demand_change accuracy is %s percent",val)) 
#rpart.plot(tree.model2)
}
run_classfication_models(mug)
run_classfication_models(kim)

run_classfication_by_ele <- function(data, ele){
  set.seed(100);
  data$business_start <-NULL

  tune.model1 <- eval(parse(text=paste0("tune.svm(as.factor(",ele,") ~ . ,data=data)")))
  val <- round((1-tune.model1$best.performance),3)

  tune.model2 <- eval(parse(text=paste0("tune.nnet(as.factor(",ele,") ~ . , data = data, ,linout=FALSE, size=5, trace = FALSE)")))
  val1 <- round((1-tune.model2$best.performance),3)

  tune.model3 <- eval(parse(text=paste0("tune.rpart(as.factor(",ele,") ~ . ,data=data)")))
  val2 <- round((1-tune.model3$best.performance),3)


  print(sprintf("%s, %s, %s,  %s",ele,val ,val1, val2))
}

run_machine_learning_2 <- function(data, remove_camp = TRUE){
coln <- c()
for(ele in colnames(data)){
  if (eval(parse(text=paste0("is.numeric(data$",ele,")"))) == FALSE){
    coln <- c(ele, coln)
  }
}
if(remove_camp == TRUE){
coln <- coln[coln != "camp_name"]
}

#running through all non numeric columns with sufficient data through classifcation tress, neural netwokrks, and SVMs
for(ele in coln ){
  run_classfication_by_ele(data, ele)
}
}

mug$ibs_comm <- NULL
mug$ibs_microloan <- NULL
mug$business_type_craft <- NULL
mug$sell_food_assistance <- NULL
mug$market_security <- NULL

kim$cash_food_local <- NULL

suppressWarnings(run_machine_learning_2(mug))
suppressWarnings(run_machine_learning_2(kim))



