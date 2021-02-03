# Author: Ronit Randhawa
# July 2020

###### Intro Code ####
library(corrplot)       # used for making correlation plot
library(cowplot)        # used for combining multiple plots
library(ggplot2)        # used for plotting
library(caret)          # used for modeling
library(dplyr)          # used for data manipulation and joining
library(data.table)
library(hydroGOF)
setwd("/Users/ronitrandhawa/Desktop/ /Code/R Studio/SupermarketSalesPrediction-MachineLearning")

#Read train and test files
train = read.csv("train.csv")
test = read.csv("test.csv")


##### Inspection of datasets #####
summary(train)
summary(test)

sum(is.na(train))
colSums(is.na(train))
sum(is.na(test))
colSums(is.na(test))

str(train)
str(test)

colnames(train)
colnames(test)

dim(train) 
dim(test) # one less column (predictor column)

head(train)
head(test)

tail(train)
tail(test)

# do for all non-numeric variables (maybe even some numeric variables)
unique(train$Item_Fat_Content) # fat content       **empty rows
table(train$Item_Fat_Content)
unique(train$Item_Type) # item type
table(train$Item_Type)
unique(train$Outlet_Establishment_Year) # outlet est 
table(train$Outlet_Establishment_Year)
unique(train$Outlet_Size) # outlet size      ** empty rows
table(train$Outlet_Size)
unique(train$Outlet_Location_Type)  # location type
table(train$Outlet_Location_Type)
unique(train$Outlet_Type) # outlet type
table(train$Outlet_Type)

#quick glance over the feature names of train and test datasets
names(train)
names(test)
#Item_Outlet_Sales is present in the train but not in the test dataset because this is the target variable that we have to predict. It is common to not have the target variable column in the test dataset

#summary of the dataset. make a note where you see NA’s
summary(train)
summary(test)


#use sum(is.na)
sum(is.na(train))
colSums(is.na(train))
sum(is.na(test))
colSums(is.na(test))


#Make a note of numeric and categorical variables
str(train)
str(test)
# categorical: Item_Identifier (ID type), Item_Fat_Content, Item_Type, Outlet_Identifier (ID type), Outlet_Establishment_Year (?), Outlet_Size, Outlet_Location_Type, Outlet_Type
# continuous: Item_Weight, Item_Visibility, Item_MRP, Item_Outlet_Sales

##### Create combined dataset for data wrangling #####

#combine both train and test data. It is needed for accurate visualization, address missing values, imputations, feature engineering on set of data than repeating it on two sets of datasets separately. 
# Once done we can split it again in test and train
#hint: use rbind to join the two datasets
combinedData = rbind(train, test)

#did you hit an error? what would you do? If you cannot resolve - textMe. 
#to keep going without resolving the above work on the train dataset.
test$Item_Outlet_Sales = NA
combinedData = rbind(train, test)

#validate that the combined dataset has correct number of columns.
nrow(combinedData)
sum(is.na(combinedData))


##########################Exploratory Data Analysis########################
######################## UNIVARIATE ANALYSIS OR ONE VARIABLE PLOTS ################################
# NUMERIC: plot all independent variables on a single variable graph

ItemWeightUni = ggplot(combinedData, aes(Item_Weight)) + geom_histogram() # had to remove 2439 missing rows
ItemWeightUni

ItemVisibilityUni = ggplot(combinedData, aes(Item_Visibility)) + geom_histogram()
ItemVisibilityUni

ItemMRPUni = ggplot(combinedData, aes(Item_MRP)) + geom_histogram()
ItemMRPUni

SalesUni = ggplot(combinedData, aes(Item_Outlet_Sales)) + geom_histogram() # removed rows from test data
SalesUni # lower price = more observations

#EXPECTATION: 4 plots for Numeric independent variable

# all plots
ContinuousAllUniPlots = plot_grid(ItemWeightUni, ItemVisibilityUni, ItemMRPUni, SalesUni)
ContinuousAllUniPlots


#CATEGORICAL VARIABLES

#EXPECTATION: 8 plots for categorical variables (includes Year as a factor variable)
#for categorical variables check for finiteness of values by using table(dataset$colName)

# plot for Item_Fat_Content. 
table(combinedData$Item_Fat_Content)
# LF low fat Low Fat     reg Regular 
# 522     178    8485     195    4824 

# DATA WRANGLING FOR ITEM_FAT_CONTENT
#reduce the number of fat content to 2
combinedData$Item_Fat_Content[combinedData$Item_Fat_Content == "LF"] = "Low Fat"
combinedData$Item_Fat_Content[combinedData$Item_Fat_Content == "low fat"] = "Low Fat"
combinedData$Item_Fat_Content[combinedData$Item_Fat_Content == "reg"] = "Regular"
table(combinedData$Item_Fat_Content)

ItemFatUni = ggplot(combinedData, aes(Item_Fat_Content)) + geom_bar()
ItemFatUni

ItemTypeUni = ggplot(combinedData, aes(Item_Type)) + geom_bar()
ItemTypeUni

combinedData$Outlet_Establishment_Year = as.factor(combinedData$Outlet_Establishment_Year)
OutletEstUni = ggplot(combinedData, aes(Outlet_Establishment_Year)) + geom_bar()
OutletEstUni

OutletSizeUni = ggplot(combinedData, aes(Outlet_Size)) + geom_bar()
OutletSizeUni # many missing values
ggplot(combinedData, aes(Outlet_Size, Item_Outlet_Sales)) + geom_point() # sales for missing size values are still performing well
ggplot(combinedData, aes(Outlet_Size, Item_Outlet_Sales)) + geom_violin() # very similar to small. convert empty values to small
combinedData$Outlet_Size[combinedData$Outlet_Size == ""] = "Small" # makes the empty rows = small
OutletSizeUni = ggplot(combinedData, aes(Outlet_Size)) + geom_bar()
OutletSizeUni

OutletLocationTypeUni = ggplot(combinedData, aes(Outlet_Location_Type)) + geom_bar()
OutletLocationTypeUni

OutletTypeUni = ggplot(combinedData, aes(Outlet_Type)) + geom_bar()
OutletTypeUni

# plotting all plots together 
CategoricalAllUniPlots = plot_grid(ItemFatUni, ItemTypeUni, OutletEstUni, OutletSizeUni, OutletLocationTypeUni, OutletTypeUni)
CategoricalAllUniPlots
# here, there are only 6 plots. do i need to put the identifiers as well?


########## BIVARIATE ANALYSIS 2 variable plots################################
#Target variable vs. independent continuous variables  
#to discover relationships and use those findings in missing data imputation and feature engineering 
#scatter plots for the continuous or numeric variables and violin plots for the categorical variables.

#split dataset into train and test.. since outlet sales is null for test data and cannot be used for plotting
newTest = combinedData[which(is.na(combinedData$Item_Outlet_Sales)),]
newTrain = combinedData[-which(is.na(combinedData$Item_Outlet_Sales)),]

#build scatter plots. Y will be what you are predicting. X will be each independent variable
# continuous: 
ItemWeightBi = ggplot(newTrain, aes(Item_Weight, Item_Outlet_Sales)) + geom_point()
ItemWeightBi # still need to remove empty rows. use mean method through for loop

ItemVisibilityBi = ggplot(newTrain, aes(Item_Visibility, Item_Outlet_Sales)) + geom_point()
ItemVisibilityBi

ItemMRPBi = ggplot(newTrain, aes(Item_MRP, Item_Outlet_Sales)) + geom_point()
ItemMRPBi # seems to be split into 4 groups???

#Plot all on the same plot
ContinuousAllBiPlots = plot_grid(ItemWeightBi, ItemVisibilityBi, ItemMRPBi)
ContinuousAllBiPlots

#Target Variable vs Independent Categorical Variables
#Visualise the categorical variables with respect to Item_Outlet_Sales. 
#We will try to check the distribution of the target variable across all the categories of each of the categorical variable.

# We could  use boxplots here, but instead we’ll use the violin plots as they show the full distribution of the data. 
# The width of a violin plot at a particular level indicates the concentration or density of data at that level. 
# The height of a violin tells us about the range of the target variable values.

# Item_Type vs Item_Outlet_Sales 
ItemFatContentBi = ggplot(newTrain, aes(Item_Fat_Content, Item_Outlet_Sales)) + geom_violin()
ItemFatContentBi

ItemTypeBi = ggplot(newTrain, aes(Item_Type, Item_Outlet_Sales)) + geom_violin()
ItemTypeBi

OutletEstBi = ggplot(newTrain, aes(Outlet_Establishment_Year, Item_Outlet_Sales)) + geom_violin()
OutletEstBi

OutletSizeBi = ggplot(newTrain, aes(Outlet_Size, Item_Outlet_Sales)) + geom_violin()
OutletSizeBi

OutletLocationTypeBi = ggplot(newTrain, aes(Outlet_Location_Type, Item_Outlet_Sales)) + geom_violin()
OutletLocationTypeBi

OutletTypeBi = ggplot(newTrain, aes(Outlet_Type, Item_Outlet_Sales)) + geom_violin()
OutletTypeBi

# all categorical bivariable plots
CategoricalAllBiPlots = plot_grid(ItemFatContentBi, ItemTypeBi, OutletEstBi, OutletLocationTypeBi, OutletTypeBi)
CategoricalAllBiPlots


#Observations?

################ANALYSIS TO IMPUTE VALUES ########
# already did outlet_size missing values

#ITEM WEIGHT MISSING VALUES
# Item_Weight has many missing values
# Missing data in Item_Outlet_Sales can be ignored since they belong to the test dataset.
sum(is.na(combinedData$Item_Weight))
#We’ll now impute [FEATURE] with mean  based on the Item_Identifier variable.
missing_index = which(is.na(combinedData$Item_Weight)) #2439
for(i in missing_index) {  
  item = combinedData$Item_Identifier
  combinedData$Item_Weight[i] = mean(combinedData$Item_Weight[combinedData$Item_Identifier == item], na.rm = T)
}
#Now let’s see if there is still any missing data in Item_Weight
sum(is.na(combinedData$Item_Weight))


## ITEM VISIBILITY MISSING VALUES
#Replacing 0’s in Item_Visibility variable. 
#zeroes in Item_Visibility variable can be replaced with Item_Identifier mean values of Item_Visibility. 
#visualize in the plot before changing
ggplot(combinedData) + geom_histogram(aes(Item_Visibility), bins = 100)
#replace the zeroes.
zero_index = which(combinedData$Item_Visibility == 0) 
for(i in zero_index) { 
  item = combinedData$Item_Identifier[i] 
  combinedData$Item_Visibility[i] = mean(combinedData$Item_Visibility[combinedData$Item_Identifier == item], na.rm = T)  
}
#After the replacement of zeroes, We’ll plot the histogram of Item_Visibility again. 
#In the histogram, we can see that the issue of zero item visibility has been resolved.
ggplot(combinedData) + geom_histogram(aes(Item_Visibility), bins = 100)

###### More Data Wrangling - the item type may change other variables #####

# for ex, if item type = hygiene, it shouldn't hava a fat content
# solution: change it to a new type - "none"
combinedData$Item_Fat_Content[combinedData$Item_Type == "Health and Hygiene"] = "None"  
combinedData$Item_Fat_Content[combinedData$Item_Type == "Household"] = "None"  
combinedData$Item_Fat_Content[combinedData$Item_Type == "Others"] = "None"
unique(combinedData$Item_Fat_Content)


###### Creating new feautures ######

# new variable for years of operation (who cares about est year) 
combinedData$Outlet_Establishment_Year = as.numeric(combinedData$Outlet_Establishment_Year) # make sure this works
combinedData$Outlet_Age= as.factor(2013 - combinedData$Outlet_Establishment_Year) # 2013 because that was when this data was taken
combinedData$Outlet_Establishment_Year=NULL


# Price
#lets create a new feature named "price" which is a categorical variable..
summary(combinedData) # will choose #s by quartile
#MRP <=80,  price =  "low"
#MRP >80 and MRP <= 130,  price = "medium"
#MRP >180,  price ="High"
combinedData$price = "Low"
combinedData$price[combinedData$Item_MRP >180] ="High"
combinedData$price[combinedData$Item_MRP>80 & combinedData$Item_MRP <=180] <- "Medium"
summary(combinedData)


# New Feature: Item_Type_new
#summary(cData) to categorize to perishable / non-perishable
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood", "Snack Foods" )
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")
# create a new feature 'Item_Type_new' 
combinedData$Item_Type_new = ifelse(combinedData$Item_Type %in% perishable, "perishable", ifelse(combinedData$Item_Type %in% non_perishable, "non_perishable", "not_sure"))
table(combinedData$Item_Type_new)
# #reset Item_Type to the new value and drop Item_Type_new
combinedData$Item_Type = combinedData$Item_Type_new
combinedData$Item_Type_new = NULL


# New Feature: Item_Category
#Let’s compare Item_Type with the first 2 characters of Item_Identifier, #i.e., ‘DR’, ‘FD’, and ‘NC’. These identifiers most probably stand for drinks, food, and non-consumable.
table(combinedData$Item_Type, substr(combinedData$Item_Identifier, 1, 2))
#create a new category with DR FD and NC item type
combinedData$Item_category = substr(combinedData$Item_Identifier, 1, 2)
table(combinedData$Item_category )


# New Feature: Price per unit weight 
combinedData$price_per_unit_wt = combinedData$Item_MRP/combinedData$Item_Weight


##### Label encoding #####
# Label encoding simply means converting each category in a variable to a number. 
# It is more suitable for ordinal variables — categorical variables with some order.
# Label encoding for the categorical variables. 
# We will label encode Outlet_Size and Outlet_Location_Type as these are ordinal variables.
table(combinedData$Outlet_Size)
combinedData$Outlet_Size_num = ifelse(combinedData$Outlet_Size == "Small", 0,ifelse(combinedData$Outlet_Size == "Medium", 1, 2))
# confirm correct substitution
table(combinedData$Outlet_Size_num)
table(combinedData$Outlet_Size) #later drop this or replace 
combinedData$Outlet_Size=combinedData$Outlet_Size_num
# Drop the temp column
combinedData$Outlet_Size_num=NULL

table(combinedData$Outlet_Location_Type)
combinedData$Outlet_Location_Type_num = ifelse(combinedData$Outlet_Location_Type == "Tier 3", 0,
                                           ifelse(combinedData$Outlet_Location_Type == "Tier 2", 1, 2))
table(combinedData$Outlet_Location_Type_num)
table(combinedData$Outlet_Location_Type)
combinedData$Outlet_Location_Type=combinedData$Outlet_Location_Type_num
combinedData$Outlet_Location_Type_num=NULL


######## Run One hot encoding #######
# One hot encoding ordinal variable/categorical variable, coverts to zero and one
# In One hot encoding, each category of a categorical variable is converted into a new binary column (1/0).
dt.data=data.table(combinedData)
# create dummy variable for all features other than the three inside the c()
ohe = dummyVars("~.", data = dt.data[,-c("Item_Identifier", "Outlet_Identifier")], fullRank = T) #encode all relevant variable except item id
ohe_df = data.table(predict(ohe, dt.data[,-c("Item_Identifier", "Outlet_Identifier")]))

########## FINAL DATASET - we add the item identifier column to the dataset which has been ohe-coded ######
### The ohe_df is you look closely is nothing but the whole of dataset minus the item identifier
### then we add to theis the item id column which is done below.
combinedData = cbind(dt.data[,c("Item_Identifier", "Outlet_Identifier")], ohe_df)

##### Model Creation #####

newDataset = data.table(combinedData[-which(is.na(combinedData$Item_Outlet_Sales)),]) # data.table for lm().

set.seed(620)
index=createDataPartition(newDataset$Item_Outlet_Sales, list=F, p=0.7)
newTrain=newDataset[index,]
newTest=newDataset[-index,]

#### train() model #####
model_train_lm1 = train(Item_Outlet_Sales~. - Item_Identifier - Outlet_Identifier, newTrain, method="lm")
summary(model_train_lm1)

accuracy_train_lm1 = model_train_lm1$results
accuracy_train_lm1
# predict = predict(model_train_lm1, newTest)

##### lm() - this model gives no warnings (but same-ish model) #####
model_lm1 = lm(Item_Outlet_Sales ~ ., data = newTrain[, -c("Item_Identifier", "Outlet_Identifier")])
summary(model_lm1)

predict.lm = predict(model_lm1, newTest)
RMSE(predict.lm, newTest$Item_Outlet_Sales) 
R2(predict.lm, newTest$Item_OutletSales)

# expected output
submissionTable = newTest[, c("Item_Identifier", "Outlet_Identifier", "Item_Outlet_Sales")]
submissionTable$PredictedValues = predict.lm
# add accuracy, rmse, to subTable

# fittedValues = model_lm$fitted.values # lm function gives you access to fitted values variable
