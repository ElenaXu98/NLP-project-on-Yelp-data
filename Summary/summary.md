## Introduction
Yelp is a powerful platform for business owners to collect information about their business and data about customer satisfaction. 
In this project, we analyzed the customer's reviews and various other attributes of restaurants, trying to extract useful information and give suggestions to businesses. 
Our analysis focuses on all the pubs that have a full bar service in Wisconsin. 
Among these pubs, our specific goals are to analyze the most important facilities and services that customers prefer and then provide suggestions to pubs to improve their star ratings. 
The rating of a restaurant is measured in stars from 0 to 5. 
We want to answer the following questions:

1. What facilities will affect businesses’ rating? 
How will the rating be affected? How do the work hours influence the rating?
2. What additional services could improve the rating? For example, parking, valet, garage, etc.
3. How do the work hours influence the rating? 

For this project, we used a real data set from Yelp. We first conducted an exploratory data analysis. 
Then, we built a multiple regression model by stepwise selection to predict the rating of a business. 
In the following sections, we cover the details of our analysis and the model.

## Preprocessing 

### Data and Sample Size

The Yelp dataset is released by Yelp to encourage students to do research on it. 
All the data are stored in four json files which contains information about business, written reviews, which are longer comments and the tips, 
which are shorter comments, and the data about the user who wrote the review or the tip. We mainly focused on the following features: stars, 
open hours and facilities/attributes data in business json file, and the content of reviews in review json file.
After filtering all the open restaurants with a full bar service in Wisconsin, we got 466 pubs with 69 attributes and its stars ratings, 
50569 reviews, and the corresponding business id. 

### Clean Attributes Variables
To obtain attributes of each pub, we separated BusinessParking, GoodForMeal, and Ambience into several binary variables. 
Some redundant characters are deleted from levels of attributes factor(I don't know what this means). 
For example, “u’free’” is the same as “free” in attributes Wifi. 
We deleted the redundant “u’”. Also, missing values of both nominal and ordinal variables are interpolated by their mode. 

### Predictors

A pub's work hours are the predictors that calculated by hours.Monday-hours.Sunday attributes. (I don't understand this sentence)
To get the information about reviews’ sentiment of each pub, we created a new predictor called positive review ratio. 
We did this by dividing the number of positive reviews by the number of reviews of each pub. 
The sentiment analysis is used to evaluate the polarity score of a review. 
First, we parsed each review into single words and deleted stopwords like he, she, the.
Then, we counted the number of positive words and negative words in each review. 
Finally, we calculated the sentiment of each review by subtracting the number of negative words from the number of positive words. 
The sentiments of each word are defined and stored in a lexicon which is provided by the *tidytext* library in R. 
After conducting sentiment analysis of each review, we grouped the reviews by business id to get the number of positive reviews. 
The two separate datasets, the review dataset and the business dataset, were merged by business id column.

## EDA/Statistics Tests

### Plots

The plots in this section show the proportion of a specific attribute of a business to the stars rating. 
Note that most of attributes are discrete. 
We plotted a histogram showing the distribution of the stars for each attribute. 

## Model Diagnostics
After finalizing the EDA and after some statistical tests gave us the thorough insight in our data, we decided to use multiple regression model for answering our statistical questions. One of the main questions we wanted to answer was whether or not the work hours of a pub have any affect on the star rating. After standardizing work hours for each day of the week, we preceded to prepare other attributes for the regression model. These attributes were the predictors that we used in our model that we found that were significant to the star rating in our exploratory analysis. The predictors were work hours, restaurant attire, noise level, presence of WiFi, takeout and other atrtibutes provided in the business dataset, along with the sentiment analysis of the reviews. The results were: ..... someone add this part 

## Data-Drive Business Plan

## Conclusion


