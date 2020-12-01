# NLP-project-on-Yelp-data
STAT 628 course project about NLP on Yelp data.

## Repo structure

### Misc 
Contains first drafts, first passes, random code.

### Presentation
Containt presentation deck `Slides.pptx` and the `Readme.md` file with the link to Google Slides.

### Shiny
Here is the [link](https://xwang2443.shinyapps.io/shiny/) to the Shiny app. 

This directory contains all the files related to the app. 

### code
Contains the following files:
- `main.R` contains all of the code for the project
- `DataPreprocessing.ipybnb` exploratory notebook
- `scriptR.ipybn` exploratory notebook

### images
Contains all the images relevant for the summary and the presentation

### output
Contains the output datasets produced by the `main.R`. Following is the description of each file.
| Dataset | Description  |
|---|---|
|`regdata.csv` |  The data used to do the regression |
| `sentiment_review`  | Sentiment analysis output of the reviews  of the pubs in the original dataset.  |
|  `sentiment_review_WI.csv` | Sentiment analysis output of the reviews of all the Wisconsin pubs.  |
|  `sentiment_tip.csv` | Sentiment analysis output of the tips of the pubs in the original dataset.  |
|  `sentiment_tip_WI.csv` | Sentiment analysis output of the tips of all the Wisconsin pubs.  |
| `topic_NLP.csv`  | edit  |

## Memo on our process

### categories we will focus on

pubs in WI

### the main question we want to solve

What pubs can do to improve their business based on yelp reviews

1. Should they offer takeout service?
2. Should they extend opening hours?

3. ...


## Data preprocessing details

- User_city.json
  1. Keeping all tags.
  2. Delete missing values
- Tip_city.json
  1. Keeping all tags.
  2. Delete missing values
- Review_city.json
  1. Keeping all tags.
  2. Merge with user_city.json on user_id by left join
  3. Delete reviews duplicates that have exactly the same text
- Business_city.json
  1. Keeping tags which is useful in the analysis.
  2. filter:
     1.  Is_open == 1
     2. Alcohol variable with “full_bar” string
     3. state == "WI"
 

### Several aspects of pubs that we can analysis

##### atmosphere

Words: first detect: atmosphere, Atmosphere

Evaluation words: cozy, festive, amazing, great, free, inviting, chill, warm, welcoming, cool, great, nice, comfortable, casual

##### food()

Frequency of food will be found and be used to give suggestions.

##### alcohol drinks

###### Different kinds of alcohol drinks

- fermented drinks: beer, Ale, wine
- distilled drinks: Rum, Brandy, Gin, whiskey, Texas whiskey, Vodka, Absinthe, Tequila 
- cocktails

###### Detect words:	

beer, Ale, wine, Rum, Brandy, Gin, whiskey, Texas whiskey, Vodka, Absinthe, Tequila, cocktails, Cocktails

###### several questions that we want to answer

- What features can influence the rating of a pub?
- What can the owner do to increase the rating?
- What kinds of alcohol drinks are good for pubs' rating?
- ...

## Contributions
| Name  | Email  |
|---|---|
| Yinqiu Xu | add email  |
| Xiaofeng Wang |  xwang2443@wisc.edu |
|  Milica Cvetkovic |mcvetkovic@wisc.edu|
