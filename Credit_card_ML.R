#-----Section 01-------------------------------------------
# get the data

# set working directory
setwd(dirname(file.choose()))
getwd()

#-----Section 02-------------------------------------------
# import data file UCI_Credit_Card.csv and put relevant variables in a data frame

infile<-"UCI_Credit_Card.csv"
dataset=read.csv(infile, header = TRUE, sep = ",")
credit_ds = dataset
colnames(credit_ds)
attach(credit_ds)


#-----Section 03-------------------------------------------
# exploring and preparing the data

#----libraries needed for visualisation----

library(corrplot)
library(ggplot2)
library(naniar)
library(DataExplorer)


#-------------
# examine the structure, class and other attributes of the credit_ds data frame
str(credit_ds)
class(credit_ds)
names(credit_ds)
head(credit_ds)

# changing the last coloumn name to default 
names(credit_ds)[names(credit_ds) == "default.payment.next.month"] <- "defaulter"
names(credit_ds)


#Transform sex, education, marriage in char variables
credit_ds$SEX <- as.character(credit_ds$SEX)
credit_ds$EDUCATION <- as.character(credit_ds$EDUCATION)
credit_ds$MARRIAGE <- as.character(credit_ds$MARRIAGE)

#Transform default into factor variables
credit_ds$default <- as.factor(credit_ds$default)



#Transform variables to better understanding of database
#SEX: Gender (1=male, 2=female). 
credit_ds$SEX <- factor(credit_ds$SEX, levels = c("1", "2"),
                        labels = c("M", "F"))
table(credit_ds$SEX)


#MARRIAGE: Marital status (1=married, 2=single, 3&0=others)
credit_ds$MARRIAGE[credit_ds$MARRIAGE == "0"] <- "others"
credit_ds$MARRIAGE[credit_ds$MARRIAGE == "1"] <- "married"
credit_ds$MARRIAGE[credit_ds$MARRIAGE == "2"] <- "single"
credit_ds$MARRIAGE[credit_ds$MARRIAGE == "3"] <- "others"
table(credit_ds$MARRIAGE)


#EDUCATION: (1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown, 0=unknown)

credit_ds$EDUCATION[credit_ds$EDUCATION == "1"] <- "graduate school"
credit_ds$EDUCATION[credit_ds$EDUCATION == "2"] <- "university school"
credit_ds$EDUCATION[credit_ds$EDUCATION == "3"] <- "high school"
credit_ds$EDUCATION[credit_ds$EDUCATION == "4"] <- "others"
credit_ds$EDUCATION[credit_ds$EDUCATION == "5"] <- "unknown"
credit_ds$EDUCATION[credit_ds$EDUCATION == "6"] <- "unknown"
credit_ds$EDUCATION[credit_ds$EDUCATION == "0"] <- "unknown"
table(credit_ds$EDUCATION)

#Age being split into intervals 
credit_ds$Age_interval <-cut(credit_ds$AGE,breaks=c(0,10,20,30,40,50),
                             include.lowest=TRUE)



#-------Visualisation of variables-----

#Plotting coorelation matrix
r = cor(credit_ds)
corrplot(r, method = "circle")

#finding missing values
gg_miss_var(credit_ds)


#defaulter among gender
ggplot(credit_ds, aes(defaulter)) +
  geom_bar(aes(fill=factor(SEX))) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  scale_fill_discrete(labels = c("Male", "Female")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Default", y = "Count",
       title = "Default Payments Among Gender", fill = "Gender") +
  theme(title = element_text(family = "serif"))

#defaulter among Married
ggplot(credit_ds, aes(defaulter)) +
  geom_bar(aes(fill=factor(MARRIAGE))) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  scale_fill_discrete(labels = c("married", "others", "single")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Default", y = "Count",
       title = "Default Payments Among Marriage", fill = "Marital status") +
  theme(title = element_text(family = "serif"))

#defaulter among Education
ggplot(credit_ds, aes(x=defaulter)) + 
  geom_bar(aes(fill=factor(EDUCATION))) +
  scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  xlab("Default Payment Status")+ylab("Customer Count") + 
  #  facet_wrap(~SEX)+
  scale_fill_discrete(name="Education")




counts_age <- table(credit_ds$Age_interval, credit_ds$defaulter)
barplot(counts_age, main="Age - Default vs Non-Default",
        xlab="Number of Gears", col=c("#6600FF", "#9900FF", "#CC00FF", "#CC33CC", "#FF0099"),
        legend = rownames(counts_age))



#comparison of balance limit with variables-----
# Balance limits by sex and education
ggplot(credit_ds, aes(factor(MARRIAGE), (LIMIT_BAL/1000), fill=SEX)) + 
  geom_boxplot() +
  xlab("Marital Status") + 
  ylab("Balance Limit") + 
  coord_cartesian(ylim = c(0,350)) +
  scale_fill_brewer(palette = "Set3")

# Balance limits by sex and education
ggplot(credit_ds, aes(factor(EDUCATION), (LIMIT_BAL/1000), fill=SEX)) + 
  geom_boxplot() +
  xlab("Education") + 
  ylab("Balance Limit") +
  scale_fill_brewer(palette = "Accent")


# Balance limits by sex and age
ggplot(credit_ds, aes(factor(Age_interval), (LIMIT_BAL/1000), fill=SEX)) + 
  geom_boxplot() +
  xlab("Age interval") + 
  ylab("Balance Limit") +
  scale_fill_brewer(palette = "OrRd")

#box plotting for bill amounts
boxplot(credit_ds[13:18])

#box plotting for payment amounts
boxplot(credit_ds[19:24])

boxplot(credit_ds[7:24])

#--------------Scaling functions-------------
#*
#*#only these variables are required for modelling
default_ds = dataset[7:25]
# Encoding the target feature as factor
default_ds$default = factor(default_ds$default_ds, levels = c(0, 1))


# min-max scaling
credit_mm <- apply(default_ds, MARGIN = 2, 
                   FUN = function(x) (x - min(x))/diff(range(x)))

boxplot (credit_mm, main = "Min Max")



# z-score
credit.z1 <- apply(default_ds[-19], MARGIN = 2, 
                   FUN = function(x) (x - mean(x))/sd(x))
boxplot (credit.z1, main = "Z-score")


# z-score with sd=4
credit.z2 <- apply(default_ds, MARGIN = 2, 
                   FUN = function(x) (x - mean(x))/(4*sd(x)))
boxplot (credit.z2, main = "Z-score, 4 sd")




# soft max scaling
library(DMwR2)
credit.sm <- apply(default_ds, MARGIN = 2, FUN = function(x) 
  (SoftMax(x,lambda = 8, mean(x), sd(x))))
boxplot (credit.sm, main = "Soft Max, lambda = 8")

#confirming and taking min-max 
default_ds <-as.data.frame(credit_mm)



#---------splitting data set---------------
#*
training_set = default_ds[1:27000,]
test_set = default_ds[27001:30000,]

#checking if balanced dataset or not
prop.table(table(training_set$default))
prop.table(table(test_set$default))


#--------------Modelling starts here--------------
#*
#*********************KNN******************************
#*

#using K value as 25
library(class)
set.seed(12345)
knn_pred25 = knn(train = training_set[, -19], test = test_set[-19],
                 cl = training_set[, 19],k = 25)


library(caret)
confusionMatrix(as.factor(test_set[,19]),as.factor(knn_pred25), 
                positive = "1")


#*********************Logistic Regression******************************


# Fitting Logistic Regression to the Training set
library(caTools)
classifier = glm(formula = default.payment.next.month ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-19])
log_pred = ifelse(prob_pred > 0.5, 1, 0)

#confusion matrix
confusionMatrix(as.factor(test_set[,19]),as.factor(log_pred), 
                positive = "1")

#making ROC curve and calculating AUC value
library(pROC)
roc <- roc(test_set$default.payment.next.month, log_pred)
plot(roc, avg= "threshold", colorize=T, 
     lwd=3, main="Logistic Regression ROC curve")
auc(test_set$default.payment.next.month, log_pred)



#*********************SVM************************
#*
library(e1071)
classifier = svm(formula = default.payment.next.month ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
svm_pred = predict(classifier, newdata = test_set[-19])

# Making the Confusion Matrix
library(caret)
confusionMatrix(as.factor(test_set[,19]),as.factor(svm_pred), positive = "1")

