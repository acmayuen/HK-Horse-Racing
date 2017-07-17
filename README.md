# Purpose of project

The purpose of project is a stepping stone for myself to learn Machine Learning knowlege through a real - life case.  HKJC did a good job to maintain the data quality and transparency to public.

Make use of the public available information with data mining skills and predictive analysis tools in order to estimate the probability of win for each horse in a race.  The following is the procedures taken and different approach used after learning R or attempting new methodology.

# 1. Data
1.	Data Collection: Using Excel VBA to collect data from HKJC and newspaper web sites, including each race result, horse basic information, trackwork etc, approximately 8,000 races and 100,000 rows data
2.	Data Cleaning: Using R to check whether the data set contains NA / missing / Inf and imputing mean / max / min / 0 or being removed as necessary
3.	Feature Engineering: merge data tables and extract addition predictors such as speed rating, last phase speed, horse win percentage etc.
4.	Data Transformation: Normalize or scale the data for standardized unit, extracting predictors
5.	Preparing training and testing set data: stratified sampling the whole data set per race and splitting data into training and testing sets by 70% and 30% respectively (train: 60%, validate: 20%, test: 20% if applying two – steps analysis)
6.	Variable Importance:  Using Random Forrest to find the importance of variable to response
7.	Predictive model: 
a.  With two – steps conditional logistic regression, where using fundamental predictors to model the basic strength for each horse, then applying public odds for  to predict the probability of winning for each horse in the same race
b.	Using gradient descent method / lasso method on conditional logistic regression
c.  Adopting other ML method, such as Random Forrest, Support Vector Machine (Regression approach), neural network with tuned data
8.	Cross Validation: 10 fold cross – validation to check the accuracy of model
9.	Comparing Different Models: Comparing confusion matrix from different approaches together with the variable importance and cross validation, picking the best modelling method for analysis.  Fine tuning predictors, learning rate, training / testing data set splitting.
10.	Wagering strategy: Applying Kelly’s Criterion in Excel VBA to find an optimal strategy for wagering

The fundamental model’s accuracy (without applying odds as factor) for predicting winner is slightly lower than public favorite (around 28% accuracy) but the average odds return is higher than the public’s estimation.  In all, the return turns out with a marginal profit.  There is potential to increase the accuracy by enhancing feature engineering and applying more sophisticated algorithm.  However, as too much noise and mankind uncertainty involved in race, it is expected that the accuracy may be lift up by only a few percent as the existing public estimate is fairly efficient.
