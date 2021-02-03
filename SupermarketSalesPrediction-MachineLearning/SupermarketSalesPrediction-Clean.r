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

# show all values for non-numeric variables
unique(train$Item_Fat_Content) # fat content       **empty rows
table(train$Item_Fat_Content)
unique(train$Item_Type) # item type
table(train$Item_Type)
unique(train$Outlet_Size) # outlet size      ** empty rows
table(train$Outlet_Size)
unique(train$Outlet_Location_Type)  # location type
table(train$Outlet_Location_Type)
unique(train$Outlet_Type) # outlet type
table(train$Outlet_Type)

#feature names of train and test datasets
names(train)
names(test) #Item_Outlet_Sales is present in the train but not in the test dataset.

# find na values in datasets
sum(is.na(train))
colSums(is.na(train))
sum(is.na(test))
colSums(is.na(test))

# Class of variable in each dataset
str(train)
str(test)
# categorical: Item_Identifier (ID type), Item_Fat_Content, Item_Type, Outlet_Identifier (ID type), Outlet_Establishment_Year (?), Outlet_Size, Outlet_Location_Type, Outlet_Type
# continuous: Item_Weight, Item_Visibility, Item_MRP, Item_Outlet_Sales

##### Create combined dataset for data wrangling #####
test$Item_Outlet_Sales = NA
combinedData = rbind(train, test)

# validation
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

# all plots
ContinuousAllUniPlots = plot_grid(ItemWeightUni, ItemVisibilityUni, ItemMRPUni, SalesUni)
ContinuousAllUniPlots


#CATEGORICAL VARIABLES

# plot for Item_Fat_Content. 
table(combinedData$Item_Fat_Content)
# LF low fat Low Fat     reg Regular 
# 522     178    8485     195    4824 

# DATA WRANGLING FOR ITEM_FAT_CONTENT
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

# all categorical univariable plots
CategoricalAllUniPlots = plot_grid(ItemFatUni, ItemTypeUni, OutletEstUni, OutletSizeUni, OutletLocationTypeUni, OutletTypeUni)
CategoricalAllUniPlots


########## BIVARIATE ANALYSIS 2 variable plots################################
# outlet sales is null for test data and cannot be used for plotting, so split
newTest = combinedData[which(is.na(combinedData$Item_Outlet_Sales)),]
newTrain = combinedData[-which(is.na(combinedData$Item_Outlet_Sales)),]

# CONTINUOUS: 
ItemWeightBi = ggplot(newTrain, aes(Item_Weight, Item_Outlet_Sales)) + geom_point()
ItemWeightBi # missing some values

ItemVisibilityBi = ggplot(newTrain, aes(Item_Visibility, Item_Outlet_Sales)) + geom_point()
ItemVisibilityBi # some 0 visibility (incorrect)

ItemMRPBi = ggplot(newTrain, aes(Item_MRP, Item_Outlet_Sales)) + geom_point()
ItemMRPBi # observation: seems to be split into 4 groups

# All continuous bivariable plots
ContinuousAllBiPlots = plot_grid(ItemWeightBi, ItemVisibilityBi, ItemMRPBi)
ContinuousAllBiPlots

# CATEGORICAL:

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
# already imputed outlet_size missing values

# ITEM WEIGHT MISSING VALUES
sum(is.na(combinedData$Item_Weight))

missing_index = which(is.na(combinedData$Item_Weight)) #2439
for(i in missing_index) {  
  item = combinedData$Item_Identifier
  combinedData$Item_Weight[i] = mean(combinedData$Item_Weight[combinedData$Item_Identifier == item], na.rm = T)
}
sum(is.na(combinedData$Item_Weight))


## ITEM VISIBILITY MISSING VALUES
ggplot(combinedData) + geom_histogram(aes(Item_Visibility), bins = 100) # not missing, 0 visibility

zero_index = which(combinedData$Item_Visibility == 0) 
for(i in zero_index) { 
  item = combinedData$Item_Identifier[i] 
  combinedData$Item_Visibility[i] = mean(combinedData$Item_Visibility[combinedData$Item_Identifier == item], na.rm = T)  
}
ggplot(combinedData) + geom_histogram(aes(Item_Visibility), bins = 100)


###### More Data Wrangling - the item type may change other variables #####

# non-edible items should not have fat content
combinedData$Item_Fat_Content[combinedData$Item_Type == "Health and Hygiene"] = "None"  
combinedData$Item_Fat_Content[combinedData$Item_Type == "Household"] = "None"  
combinedData$Item_Fat_Content[combinedData$Item_Type == "Others"] = "None"
unique(combinedData$Item_Fat_Content)


###### Creating new features ######

# Feature 1: Years of Operation (rather than est year)
combinedData$Outlet_Establishment_Year = as.numeric(combinedData$Outlet_Establishment_Year) # make sure this works
combinedData$Outlet_Age= as.factor(2013 - combinedData$Outlet_Establishment_Year) # 2013 because that was when this data was taken
combinedData$Outlet_Establishment_Year=NULL


# Feature 2: Price Range
summary(combinedData) # numbers chosen by quartile
#MRP <=80,  price =  "low"
#MRP >80 and MRP <= 130,  price = "medium"
#MRP >180,  price ="High"
combinedData$price = "Low"
combinedData$price[combinedData$Item_MRP >180] ="High"
combinedData$price[combinedData$Item_MRP>80 & combinedData$Item_MRP <=180] <- "Medium"
summary(combinedData)


# Feature 3: Item_Type_new - regroup item type by perishable/non-perishable (household items included)
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood", "Snack Foods" )
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

combinedData$Item_Type_new = ifelse(combinedData$Item_Type %in% perishable, "perishable", ifelse(combinedData$Item_Type %in% non_perishable, "non_perishable", "not_sure"))
table(combinedData$Item_Type_new)
combinedData$Item_Type = combinedData$Item_Type_new
combinedData$Item_Type_new = NULL


# Feature 4: Item_category Based on Item_Identifier
# ‘DR’, ‘FD’, and ‘NC’ in Item_Identifier stand for drinks, food, and non-consumable.
table(combinedData$Item_Type, substr(combinedData$Item_Identifier, 1, 2))

combinedData$Item_category = substr(combinedData$Item_Identifier, 1, 2)
table(combinedData$Item_category )


# Feature 5: Price per unit weight 
combinedData$price_per_unit_wt = combinedData$Item_MRP/combinedData$Item_Weight


##### Label encoding for ordinal variables#####
# Outlet_Size label encode
table(combinedData$Outlet_Size)
combinedData$Outlet_Size_num = ifelse(combinedData$Outlet_Size == "Small", 0,
                                      ifelse(combinedData$Outlet_Size == "Medium", 1, 2))
table(combinedData$Outlet_Size_num)
combinedData$Outlet_Size=combinedData$Outlet_Size_num
combinedData$Outlet_Size_num=NULL

# Outlet_Location_Type label encode
table(combinedData$Outlet_Location_Type)
combinedData$Outlet_Location_Type_num = ifelse(combinedData$Outlet_Location_Type == "Tier 3", 0,
                                           ifelse(combinedData$Outlet_Location_Type == "Tier 2", 1, 2))
table(combinedData$Outlet_Location_Type_num)
combinedData$Outlet_Location_Type=combinedData$Outlet_Location_Type_num
combinedData$Outlet_Location_Type_num=NULL


######## Run One hot encoding for nonordinal variables #######
dt.data=data.table(combinedData)
# create dummy variable for appropriate features
ohe = dummyVars("~.", data = dt.data[,-c("Item_Identifier", "Outlet_Identifier")], fullRank = T)
ohe_df = data.table(predict(ohe, dt.data[,-c("Item_Identifier", "Outlet_Identifier")]))

########## FINAL DATASET + add back Item/Outlet Identifier to ohe-coded dataset ######
combinedData = cbind(dt.data[,c("Item_Identifier", "Outlet_Identifier")], ohe_df)

newDataset = data.table(combinedData[-which(is.na(combinedData$Item_Outlet_Sales)),]) # data.table for lm().

set.seed(620)
index=createDataPartition(newDataset$Item_Outlet_Sales, list=F, p=0.7)
newTrain=newDataset[index,]
newTest=newDataset[-index,]

##### Model Creation #####
#### train() model #####
model_train_lm1 = train(Item_Outlet_Sales~. - Item_Identifier - Outlet_Identifier, newTrain, method="lm")
summary(model_train_lm1)

accuracy_train_lm1 = model_train_lm1$results
accuracy_train_lm1
# predict = predict(model_train_lm1, newTest)

##### lm() - same-ish model but no warnings #####
model_lm1 = lm(Item_Outlet_Sales ~ ., data = newTrain[, -c("Item_Identifier", "Outlet_Identifier")])
summary(model_lm1)

predict.lm = predict(model_lm1, newTest)
RMSE(predict.lm, newTest$Item_Outlet_Sales) 
# R2(predict.lm, newTest$Item_OutletSales)

# expected output
submissionTable = newTest[, c("Item_Identifier", "Outlet_Identifier", "Item_Outlet_Sales")]
submissionTable$PredictedValues = predict.lm
# add accuracy, rmse, to subTable

# fittedValues = model_lm$fitted.values # lm function gives you access to fitted values variable
