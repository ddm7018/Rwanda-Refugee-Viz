library(dplyr)


rwanda <- read.csv("survey.csv")
rwanda                                                         <- read.csv("survey.csv")
set.seed(100)
rwanda[rwanda$business_start == '########',]                   <- NA
rwanda$sell_food_assistance[rwanda$sell_food_assistance == ""] <- NA
rwanda$outside_job                                             <- as.numeric(rwanda$outside_job)
rwanda$business_start                                          <- as.numeric(rwanda$business_start)
rwanda                                                         <- subset(rwanda, select=c("object_id","camp_name","competition","num_employee","market_condition","market_security","cash_food_local", 
                                                                                          "outside_job", "income_compare","business_start", "customer_locations", "customer_locations_camp_change",
                                                                                          "entrepreneurship_training", "training_grow", "business_leave_camp" ,"leave_camp_support_business", "id_problem_fequency",
                                                                                          "key_good_demand_change","avg_customers","x", "y"))
rwanda                                                        <- na.omit(rwanda)

vars <- c("camp_name","num_employee","market_condition","market_security","cash_food_local", 
          "outside_job","competition", "income_compare","business_start", "customer_locations", "customer_locations_camp_change",
          "entrepreneurship_training", "training_grow", "business_leave_camp" ,"leave_camp_support_business", "id_problem_fequency",
          "key_good_demand_change","avg_customers","x", "y")


non_numeric_vals <- c()
numeric_vals <- c()
for(ele in vars){
  if (eval(parse(text=paste0("is.numeric(rwanda$",ele,")"))) == FALSE){
    non_numeric_vals <- c(ele, non_numeric_vals)
  }
  else{
    numeric_vals <- c(ele, numeric_vals)
  }
}


