# Predicting_ViolentCrime_Data
Exploratory Data Analysis and Feature Engineering on Violent Crime dataset from Kaggle and performing regression analysis to predict the crime rate per 100k population. This project compared the performances if Linear Regression, PLS, regularized models like Ridge regression, LASSO and ElasticNet, and SVM regression models.

-------------
Author 
---

**Gowtham Teja Kanneganti**

Contact: gteja0410@gmail.com

Packages required for the project : tidyverse,Amelia,caret,MASS,corrplot,psych,corrgram,mlbench,e1071,car,pls,glmnet,elasticnet,xgboost

-----
Data Overview
----
The data combines data from a variety of sources to include socio-economic data, law enforcement data, and crime data. There are 1,200 observations of 117 possible predictors and 1 target variable (ViolentCrimesPerPop) -- each variable is described below (under Data Fields). Most of the data is numeric. There are several missing values in some variables.

-----
Data Details
----
The variables included in the dataset involve the community, such as the percent of the population considered urban, and the median family income, and involving law enforcement, such as per capita number of police officers, and percent of sworn full time police officers on patrol.

The per capita violent crimes variable, ViolentCrimesPerPop, is the target variable and is calculated using population and the sum of crime variables considered violent crimes in the United States: murder, rape, robbery, and assault.

All numeric data was normalized into the decimal range 0.00-1.00 using an equal-interval binning method. Attributes retain their distribution and skew (hence for example the population attribute has a mean value of 0.06 because most communities are small). E.g. An attribute described as 'mean people per household' is actually the normalized (0-1) version of that value.

The normalization preserves rough ratios of values WITHIN an attribute (e.g. double the value for double the population within the available precision - except for extreme values (all values more than 3 SD above the mean are normalized to 1.00; all values more than 3 SD below the mean are normalized to 0.00)).

However, the normalization does not preserve relationships between values BETWEEN attributes (e.g. it would not be meaningful to compare the value for whitePerCap with the value for blackPerCap for a community)

A limitation was that the LEMAS survey was of the police departments with at least 100 officers, plus a random sample of smaller departments. For our purposes, communities not found in both census and crime datasets were omitted. Many communities are missing LEMAS data.


-----
Exploratory Data Analysis details
----

"Target variable" doesn't seem to be affected by county. So Country is removed.

“Communityname” has 1135 unique values. So, can be removed.
