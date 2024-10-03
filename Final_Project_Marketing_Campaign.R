#Research Q1
#Importing Libraries
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(plyr)
library(gmodels)
library(ggplot2)
library(RVAideMemoire)
library(corrplot)
library(factoextra)
library(psych)
#Set working directory
setwd(
  "D:/DATA OLD SSD/ADMIN/Documents/Career/Academic/3_MSIM/Sem 1/Data Stats & Modelling/Final Project"
)
marketing_campaign <- read.csv("marketing_campaign.csv", sep = ";")
View(marketing_campaign)
# 2240x29
dim(marketing_campaign)
#checking for null
sum(is.na(marketing_campaign))
marketing_campaign <- na.omit(marketing_campaign)
dim(marketing_campaign)
sum(is.na(marketing_campaign))
#Numeric Data for PCA
pca_data <- marketing_campaign[, c(9:24)]
sum(is.na(pca_data))

colnames(pca_data)
#Identifying componenets using the knee method
pca <- prcomp(pca_data, scale. = TRUE)
pca
#Create scree plot
fviz_eig(pca, addlabels = TRUE, main = "Scree Plot")
# Identifying components using the eigenvalue method
eigen_comp <- prcomp(pca_data, center = T, scale. = T)
eigen_comp
#Plotting the result
plot(eigen_comp, main = "Scree Plot")
abline(1, 0, col = "red")
pca_1 <-
  principal(pca_data,
            rotate = "varimax",
            nfactors = 3,
            scores = TRUE)
pca_1
p1 <- print(pca_1$loadings, cutoff = .54, sort = T)
#Removing variables with no factor loading
pca_data_2 = subset(pca_data, select = -c(Recency, AcceptedCmp1, NumWebVisitsMonth))
pca_2 <-
  principal(pca_data_2,
            rotate = "varimax",
            nfactors = 3,
            scores = TRUE)
pca_2
p2 <- print(pca_2$loadings, cutoff = .54, sort = T)
#PCA Summary information
ls(pca_2)
pca_2$values
pca_2$communality
pca_2$rot.mat
#Normality Test
shapiro.test(pca_data$Recency)
shapiro.test(pca_data$MntWines)
shapiro.test(pca_data$MntFruits)
shapiro.test(pca_data$MntMeatProducts)
shapiro.test(pca_data$MntFishProducts)
shapiro.test(pca_data$MntSweetProducts)
shapiro.test(pca_data$MntGoldProds)
shapiro.test(pca_data$NumDealsPurchases)
shapiro.test(pca_data$NumWebPurchases)
shapiro.test(pca_data$NumCatalogPurchases)
shapiro.test(pca_data$NumStorePurchases)
shapiro.test(pca_data$NumWebVisitsMonth)
shapiro.test(pca_data$AcceptedCmp1)
shapiro.test(pca_data$AcceptedCmp2)
shapiro.test(pca_data$AcceptedCmp3)
shapiro.test(pca_data$AcceptedCmp4)
shapiro.test(pca_data$AcceptedCmp5)
#All values < 0.05
#Correlation Plot
cor_var = cor(pca_data[c(
  'Recency',
  'MntWines',
  'MntFruits',
  'MntMeatProducts',
  'MntFishProducts',
  'MntSweetProducts',
  'MntGoldProds',
  'NumDealsPurchases',
  'NumWebPurchases',
  'NumCatalogPurchases',
  'NumStorePurchases',
  'NumWebVisitsMonth',
  'AcceptedCmp1',
  'AcceptedCmp2',
  'AcceptedCmp3',
  'AcceptedCmp4',
  'AcceptedCmp5'
)], method = 'spearman')
cor_var_rounded <- round(cor_var, 1)
cor_plot = corrplot(cor_var_rounded,
                    addCoef.col = "black",
                    method = 'color')



#Research Q2
library(yacca)
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(plyr)
library(readxl)
library(gmodels)
library(ggplot2)
library(RVAideMemoire)
library(corrplot)
library(factoextra)
library(psych)
library(ggfortify)
library(DescTools)
library(yacca)
library(dplyr)
library(ggplot2)
library(rsample)
library(caret)
library(vip)

#setup working directory
setwd("C:\\Users\\riyan\\OneDrive\\Desktop\\Masters\\MSIM Courses\\Data Stats and Info")

#Reading File
marketing_data <- read_csv2('marketing_campaign.csv')

#Check Sample Size and Number of Variables
dim(marketing_data)

# Display the column names
colnames(marketing_data)

#Check Missing Data
sum(is.na(marketing_data))

#Remove Missing Data with listwise deletion
data <- na.omit(marketing_data)
dim(data)

#Creating dataset with numerical variables
subset <- data[, c(16:19, 21:25, 29)]
View(subset)

#Performing Logistic Regression to understand n the realm of customer behavior analysis in marketing,
#understanding the influence of customer purchasing patterns, on their responsiveness is paramount on Marketing Campaign Dataset

#Normality Test
shapiro.test(subset$NumDealsPurchases)
shapiro.test(subset$NumWebPurchases)
shapiro.test(subset$NumCatalogPurchases)
shapiro.test(subset$NumStorePurchases)
shapiro.test(subset$Response)
shapiro.test(subset$AcceptedCmp3)
shapiro.test(subset$AcceptedCmp4)
shapiro.test(subset$AcceptedCmp1)
shapiro.test(subset$AcceptedCmp2)
shapiro.test(subset$AcceptedCmp5)
#All values < 0.05

colnames(subset)

#Correlation Plot
cor_var = cor(subset[c(
  'NumDealsPurchases',
  'NumWebPurchases',
  'NumCatalogPurchases',
  'NumStorePurchases',
  'Response',
  'AcceptedCmp3',
  'AcceptedCmp2',
  'AcceptedCmp1',
  'AcceptedCmp4',
  'AcceptedCmp5'
)], method = 'spearman')
cor_plot = corrplot(cor_var, addCoef.col = "black", method = 'color')

#the correlation value magnitude for all the variables is less than 0.7 which means that multicollinearity is not strong

#Running the logistic regression model now
#dependent variable is Response

# Create training (70%) and test (30%) sets

set.seed(123)  # use a set seed point for reproducibility
split <- initial_split(subset, prop = .7, strata = "Response")
train <- training(split)
test  <- testing(split)

#Logistic Regression for explaining dependent variable

subset$Response <- as.factor(subset$Response)

logistic_reg <- glm(Response ~ .,
                    family = "binomial",
                    data = subset)

#Coefficients Not in exponential form
summary(logistic_reg)

logistic_reg = train(
  form = Response ~ .,
  data = train,
  method = "glm",
  family = "binomial"
)

predicted_classes <- predict(logistic_reg, test)
predicted_classes <-
  ifelse(predicted_classes > 0.5, 1, 0)  # Convert to 0 or 1
predicted_classes <- as.factor(predicted_classes)

confusionMatrix(predicted_classes, as.factor(test$Response))

#Variables of Importance

vip(logistic_reg, num_features = 5)

#Research Q3
library(readr)
library(stringr)
library(tidyr)
library(dplyr)
library(plyr)
library(gmodels)
library(ggplot2)
library(RVAideMemoire)
library(corrplot)
#Set working directory  setwd("/Users/milonishah/Downloads")
#Reading File
Marketing_data <- read_csv2('marketing_campaign.csv') 
#Check Sample Size and Number of Variables
dim(Marketing_data)  
# Display the column names in Personality_data 
colnames(Marketing_data)
#Check for missing data
sum(is.na(Marketing_data))
#Treat Missing Values 
#Listwise Deletion 
Marketing_data2 <- na.omit(Marketing_data)
#Check new data has no missing data 
sum(is.na(Marketing_data2))
# Create a contingency table for Recency_Group and AcceptedCmp2
contingency_table_cmp2 <- table(Marketing_data2$Recency_Group, Marketing_data2$AcceptedCmp2)

# Perform Fisher's exact test for AcceptedCmp1 
fisher_test_cmp2 <- fisher.test(contingency_table_cmp2)
# Print the result 
print(fisher_test_cmp2) 
########################################################################
contingency_table_cmp3 <- table(Marketing_data2$Recency_Group, Marketing_data2$AcceptedCmp3)
# Perform Fisher's exact test for AcceptedCmp3 
fisher_test_cmp3 <- fisher.test(contingency_table_cmp3)
# Print the result 
print(fisher_test_cmp3)
###################################################################### 
contingency_table_cmp4 <- table(Marketing_data2$Recency_Group, Marketing_data2$AcceptedCmp4) 
# Perform Fisher's exact test for AcceptedCmp4
fisher_test_cmp4 <- fisher.test(contingency_table_cmp4) 
# Print the result print(fisher_test_cmp4)  
##################################################################### 
contingency_table_cmp5 <- table(Marketing_data2$Recency_Group, Marketing_data2$AcceptedCmp5) 
# Perform Fisher's exact test for AcceptedCmp5 
fisher_test_cmp5 <- fisher.test(contingency_table_cmp5)
# Print the result 
print(fisher_test_cmp5)


#Research Q4
#chi sq test 
# Create a contingency table
contingency_table <- table(responsesdata1$Complain, responsesdata1$Response)
# Print the contingency table 
print("Contingency Table:") print(contingency_table) 
# Perform the Chi-square
test chi_square_test <- chisq.test(contingency_table) 
# Print the results 
print("Chi-square Test Results:") print(chi_square_test)



