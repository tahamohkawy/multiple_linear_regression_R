#import the dataset

dataset <- read.csv("50_Startups.csv")

#Encode the categorical data
dataset$State <- factor(dataset$State,
                        levels = c("New York","California","Florida"),
                        labels = c(1,2,3))


#Split the dataset into training set and test set
library(caTools)
split <- sample.split(dataset$Profit , SplitRatio = 0.8)
training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)


#No need to do feature scaling, since the library will take care of that

#Now we need to automate backward elimination multiple linear regression

SL <- 0.05
should_continue <- TRUE

independent_variables <- c("R.D.Spend", "Administration", "Marketing.Spend", "State")

while (should_continue){
  
  xnam <-  paste0(independent_variables, sep = "")
  fmla <- as.formula(paste("Profit ~ ", paste(xnam, collapse= "+")))
  
  regressor <- lm(formula = fmla, data = training_set )
  
  the_summary <- summary(regressor)
  coefficent <- the_summary$coefficients[2:nrow(the_summary$coefficients) , "Pr(>|t|)"]
  
  the_max <- max(the_summary$coefficients[,"Pr(>|t|)"])
  print("The max p-value ",the_max)
  
  if ( the_max > SL){
    coefficent <- coefficent[! coefficent == the_max]
    #ignore_dummy_variables <- names(coefficent)[names(coefficent) %in% independent_variables]
    independent_variables <- names(coefficent)[names(coefficent) %in% independent_variables]
  }
  else{
    should_continue <- FALSE
    break
  }
}

print("The final regressor is : ")
summary(regressor)

y_pred <- predict(regressor , test_set)

y_pred



