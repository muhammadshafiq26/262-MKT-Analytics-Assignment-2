#[1]
myData <- read.csv("marketing_conversion_data.csv")
myData
names(myData)
sum(is.na(myData))
str(myData)


#[2]
lpmModel <- lm(convert~ discount_pct + email_opens + income + loyalty_yrs + site_mins,
               data = myData)
flextable::as_flextable(lpmModel)


#[3]
logisticModel <- glm(convert~ discount_pct + email_opens + income + loyalty_yrs + site_mins,
                    data = myData,
                    family = binomial)
flextable::as_flextable(logisticModel)


#[4]
odds_ratio <- exp(coef(logisticModel))
odds_ratio
(1.089919456-1)*100
(1.599930641-1)*100
(0.984914244-1)*100
(0.982587189-1)*100
(1.090604237-1)*100


#[5]
new_data <- data.frame(
  discount_pct=c(10,28),
  email_opens=c(3,9),
  income=c(55,45),
  loyalty_yrs=c(1,2),
  site_mins=c(6,14)
)
new_data

predict(lpmModel, newdata = new_data, type = 'response')
predict(logisticModel, newdata = new_data, type = 'response')

pHatLin <- predict(lpmModel, type = 'response')
yHatLin <- ifelse(pHatLin >= .50, 1, 0)

pHatLog <- predict(logisticModel, type = 'response')
yHatLog <- ifelse(pHatLog >= .50, 1, 0)


#[6]
accuracy_lin <- mean(myData$convert == yHatLin)*100
accuracy_lin
accuracy_log <- mean(myData$convert == yHatLog)*100
accuracy_log

#[7]
simulation_data <- data.frame(
  discount_pct = mean(myData$discount_pct),
  email_opens  = seq(min(myData$email_opens), max(myData$email_opens), by = 1),
  income       = mean(myData$income),
  loyalty_yrs  = mean(myData$loyalty_yrs),
  site_mins    = mean(myData$site_mins))

simulation_data$probability_prediction <- predict(
  logisticModel, newdata = simulation_data, type = "response")
simulation_data

plot(simulation_data$email_opens, 
     simulation_data$probability_prediction, 
     type = "b", 
     pch = 19, 
     col = "skyblue",
     xlab = "Number of Emails Opened",
     ylab = "Conversion Probability Prediction",
     main = "Simulated Effect of email_opens on Predicted Conversion Probability")




#[8]
training_data <- myData[1:400, ]
validation_data <- myData[401:800, ]
Model_1 <- glm(convert ~ discount_pct + email_opens + income + loyalty_yrs + site_mins,
               data = training_data,
               family = binomial)
pHatModel_1 <- predict(Model_1,
                      newdata = validation_data,
                      type = 'response')
yHatModel_1 <- ifelse(pHatModel_1 >= .50, 1, 0)
flextable::as_flextable(Model_1)

Model_2 <- glm(convert ~ discount_pct + email_opens + income,
               data = training_data,
               family = binomial)
pHatModel_2 <- predict(Model_2,
                       newdata = validation_data,
                       type = 'response')
yHatModel_2 <- ifelse(pHatModel_2 >= .50, 1, 0)

accuracyModel_1 <- mean(validation_data$convert==yHatModel_1)*100
accuracyModel_1
accuracyModel_2 <- mean(validation_data$convert==yHatModel_2)*100
accuracyModel_2


#[9]
AIC(Model_1, Model_2)
