#script for logistic regression model creation
#martinrapilly@yahoo.fr

#load library
library(psych)
library(caret)

#load the csv file
csv<-read.csv2("C:/essai/Fire_stats_output/stats.txt")

#get basic information on the csv file
describe(csv)
summary(csv)
str(csv)

#convert columns to the correct type
is.factor(csv$uso)#check if column USO has been entered as a factor (i.e. categorical value for R)
csv['uso'] <-factor(csv$uso)#convert uso to a factor
is.factor(csv$uso)

csv['sum_burned'] <-as.double(csv$sum_burned)#convert sum_burned from text to double
csv['altitud'] <-as.double(csv$altitud)
csv['pendiente'] <-as.double(csv$pendiente)
csv['temperatura'] <-as.double(csv$temperatura)
csv['viento'] <-as.double(csv$viento)
csv['vapor'] <-as.double(csv$vapor)
csv['radiaciones'] <-as.double(csv$radiaciones)
csv['EVI'] <-as.double(csv$EVI)
csv['distancia_poblados'] <-as.double(csv$distancia_poblados)
csv['distancia_vias'] <-as.double(csv$distancia_vias)
str(csv)

#replace negative values of column viento to 0 (to avoid error during logistic regression)
table(csv$viento)#count occurrences
csv$viento[csv$viento ==-3.402823e+38] <- 0
table(csv$viento)#count occurrences
csv['viento'] <-as.double(csv$viento)
#same correction to vapor
table(csv$vapor)#count occurrences
csv$vapor[csv$vapor ==-3.402823e+38] <- 0
table(csv$vapor)#count occurrences
csv['vapor'] <-as.double(csv$vapor)

#recode sum_burned from 0,..,9 (a pixel detected 9 times as burned) to 0 and 1 (unburned and burned)
table(csv$sum_burned)#count occurrences
csv['sum_burned']<-ifelse(csv$sum_burned>=1,1,0)
table(csv$sum_burned)#count occurrences

#create a random sample for training data set
csvSample0<-csv[sample(which (csv$sum_burned<1) ,10000), ]
table(csvSample0$sum_burned)#count occurrences
csvSample0
csvSample1<-csv[sample(which (csv$sum_burned>0) ,10000), ]
table(csvSample1$sum_burned)#count occurrences
csvSample1

dim(csvSample0)#check number of rows and number of columns
dim(csvSample1)#check number of rows and number of columns

#combine the two sample dataframes
csvSampleAll <- rbind(csvSample0, csvSample1)
str(csvSampleAll)

#logistic regression
mylogit <- glm(sum_burned ~ altitud+pendiente+temperatura+viento+vapor+radiaciones+EVI+distancia_poblados+distancia_vias+uso, data = csvSampleAll, family = "binomial")
summary(mylogit)
##results are given in log-odds scale (logarithm of odds-ratio). If estimate is 1.2, it means that for an increase of 1 in a the coefficient
##of a variable (altitud for instance), the log-odds of a fire is increased by 1.2 

#calculate odds ratios (exp(log(odds)))
exp(coef(mylogit))

#predict values of sum_burned (dependent variable) based on model using test data set
#create a new random sample for the test data set
csvSample0B<-csv[sample(which (csv$sum_burned<1) ,10000), ]
table(csvSample0B$sum_burned)#count occurrences
csvSample0B
csvSample1B<-csv[sample(which (csv$sum_burned>0) ,10000), ]
table(csvSample1B$sum_burned)#count occurrences
csvSample1B
dim(csvSample0B)#check number of rows and number of columns
dim(csvSample1B)#check number of rows and number of columns
#combine the two sample dataframes
csvSampleAllB <- rbind(csvSample0B, csvSample1B)
str(csvSampleAllB)
#predict with the test data set
predict_reg <- predict(mylogit,csvSampleAllB, type = "response")
# Changing probabilities
predict_reg <- ifelse(predict_reg >0.5, 1, 0)

# Evaluating model accuracy
# using confusion matrix
table(csvTestAll$sum_burned, predict_reg)#confusion matrix
missing_classerr <- mean(predict_reg != csvSampleAllB$sum_burned)
print(paste('Accuracy =', 1 - missing_classerr))

#test the importance of each variable in the model
varImp(mylogit)


