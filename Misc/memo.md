## categories we will focus on

pubs



## the main question we want to solve

What pubs can do to improve their business based on yelp reviews

1. Should they offer takeout service?
2. Should they extend opening hours?

3. ...



## Data preprocessing details



- User_city.json
  1. Keeping tags: 'average_stars','fans','review_count','useful','user_id','yelping_since'
  2. Delete missing values
- Tip_city.json
  1. Keeping tags: 'business_id','text','user_id'
  2. Delete missing values
- Review_city.json
  1. Keeping tags:'business_id','review_id','stars','text','user_id'
  2. Merge with user_city.json on user_id by left join
  3. Delete reviews duplicates that have exactly the same text
- Business_city.json
  1. Keeping tags: 
  2. filter:
     1.  Is_open == 1
     2. Alcohol variable with “full_bar” string

- Some futuer ideas:
  - convert reviews into a 

## Several aspects of pubs that we can analysis

#### atmosphere

Words: first detect: atmosphere, Atmosphere

Evaluation words: cozy, festive, amazing, great, free, inviting, chill, warm, welcoming, cool, great, nice, comfortable, casual

#### food()

Detect words: 



#### alcohol drinks

##### Different kinds of alcohol drinks

- fermented drinks: beer, Ale, wine
- distilled drinks: Rum, Brandy, Gin, Whisky/whiskey, Texas whiskey, Vodka, Absinthe, Tequila 
- cocktails

##### Detect words:	

beer, Ale, wine, Rum, Brandy, Gin, Whisky/whiskey, Texas whiskey, Vodka, Absinthe, Tequila, cocktails, Cocktails



##### several questions that we want to answer

- Does atmosphere matters for pubs rating? What kinds of atmoshpere are good?
- Does pubs suitable for family?
- What kinds of alcohol drinks are good for pubs' rating?
- ...



