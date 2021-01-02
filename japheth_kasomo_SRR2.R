data_purchase_behaviour <- read.csv("C:/Users/Japheth/Desktop/Stats Regr - Homework2/data_purchase_behaviour.csv")
View(data_purchase_behaviour)
require(xtable)


names(data_purchase_behaviour) # Viewing the columns
head(data_purchase_behaviour)  # Eying the data by displaying first 6 rows
dim(data_purchase_behaviour)   # Checking the dimension of the data (rows and columns)
str(data_purchase_behaviour)



#Question # one Descriptive statistics

#Summary statistics for continuous variable
summa <-  summary(data.frame(data_purchase_behaviour$Stay_In_Current_City_Years,data_purchase_behaviour$Age_num,data_purchase_behaviour$Purchase))

# generating Latex code for table
print(xtable(summa),"latex")

#Frequancy table for Gender, checking number number of males and females
table(data_purchase_behaviour$Gender)

#Frequancy distribution of City category
table(data_purchase_behaviour$City_Category)

#frequency of Marital status
table(factor(data_purchase_behaviour$Marital_Status,levels = c("1","0"),labels = c("Married","Unmarried")))


#Question two # Bulding the model

# Using Back selection method to build the model
Model1 <-lm(Purchase~Gender+City_Category+Stay_In_Current_City_Years+Marital_Status+Age_num,data_purchase_behaviour)
summary(Model1)


#Eliminating insignificance variables 
Model2 <-lm(Purchase~Gender+City_Category+Age_num,data_purchase_behaviour)
summary(Model2)  # the final model with all varibles being significance


#getting the coefients of the model
Beta_hat  <- (coefficients(Model2))
Beta_hat

#Question three #Interpreating the model

#question four# the assumptions of model 

#Linear Association
par(mfrow=c(1,1))
plot(Model2, which =1)

#Homoskedasticity Assumtion
plot(Model2, which = 3)

#Normality - the error term should be normally distributed
plot(Model2, which = 2)

#Potential outliers
plot(Model2, which = 4)


#Question 7 Comparison of model

Model3 <-lm(Purchase~Gender+Age_num,data_purchase_behaviour)
summary(Model3)

anova(Model3,Model2)

