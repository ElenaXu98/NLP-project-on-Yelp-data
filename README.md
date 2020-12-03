# NLP-project-on-Yelp-data
In this project, we give business suggestions to **pubs in Wisconsin** on how to improve ratings using sentiment analysis and regression model. Details are included in the folders.

### code

The 'code' folder contains the following files:

- `main.R` contains all of the code for the project
- `DataPreprocessing.ipybnb` exploratory notebook
- `scriptR.ipybn` exploratory notebook

*Please create a `NLP-project-on-Yelp-data/data/` folder and put all the four json files in it. Then set your working directory as `NLP-project-on-Yelp-data/code/` before run the `main.R`*

### output
Contains the output datasets produced by the `main.R`. Following is the description of each file.
| Dataset | Description  |
|---|---|
|`review_pubs.csv` | Merged table of business and reviews of all the Wisconsin pubs in the original dataset. |
| `tip_pubs.csv` | Merged table of business and tips of all the Wisconsin pubs in the original dataset. |
| `sentiment_review.csv` | Sentiment analysis output of the reviews of all the Wisconsin pubs.  |
|  `sentiment_tip.csv` | Sentiment analysis output of the tips of all the Wisconsin pubs. |
| `regdata.csv` | Sentiment analysis output of the tips of all the Wisconsin pubs.  |
| `high_freq.csv` | Word frequency of high-rating tips. |
| `low_freq.csv` | Word frequency of low-rating reviews. |
| `test.csv` | Output of statistical tests |

### Presentation

Containt presentation deck `Slides.pptx` 

### images

Contains all the images relevant for the summary and the presentation.

- QQplot, residualVSfitted,leverage.jpeg: images for model diagnostic
- reviewStars, pubStars,jpeg: images of stars distribution
- delivery, goodForDancing,Takeout, hasTV: images of histgram of star distribution under with and without the attributes
- cocktails, wine, beer,....,jpeg: images of stars distribution under reviews mentioned and not mentioned the alcohol beverage

### Shiny

Here is the [link](https://xwang2443.shinyapps.io/shiny/) to the Shiny app. 

This directory contains the code to create app and related files used in the app. 

### Summary

`Suggestions for Pubs In Wisconsin_Group16`: The four-page summary mainly introduces the whole process of this data analysis project and discusses about the business suggestions.

### Misc 

Contains first drafts, first passes, random code. It's a record of all the things we have tried. Some are useful  and some are not included in the final report.

### the main question we want to solve

1. *How different facilities affect the star ratings?*
2. How do the work hours affect the star ratings?

3. Which additional services could help improve the ratings?

## Contributors
| Name  | Email  |
|---|---|
| Yinqiu Xu | yxu475@wisc.edu |
| Xiaofeng Wang |  xwang2443@wisc.edu |
|  Milica Cvetkovic |mcvetkovic@wisc.edu|
