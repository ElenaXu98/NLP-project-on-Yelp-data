if (!require("rjson")) {
  install.packages("rjson")
  stopifnot(require("rjson"))
}
if (!require("jsonlite")) {
  install.packages("jsonlite")
  stopifnot(require("jsonlite"))
}
if (!require("stringr")) {
  install.packages("stringr")
  stopifnot(require("stringr"))
}
if (!require("sqldf")) {
  install.packages("sqldf")
  stopifnot(require("sqldf"))
}
if (!require("kernlab")) {
  install.packages("kernlab")
  stopifnot(require("kernlab"))
}
if (!require("tidytext")) {
  install.packages("tidytext")
  stopifnot(require("tidytext"))
}
if (!require("dplyr")) {
  install.packages("dplyr")
  stopifnot(require("dplyr"))
}
if (!require("TSA")) {
  install.packages("TSA")
  stopifnot(require("TSA"))
}

business<-jsonlite::stream_in(file("data/business_city.json"))
review<-jsonlite::stream_in(file("data/review_city.json"))
tip<-jsonlite::stream_in(file("data/tip_city.json"))
user<-jsonlite::stream_in(file("data/user_city.json"))


#First, we need to find all the pubs by using the tag:alcohol in the business
#Then we need to decide what tags to stay.
all_pubs<-data.frame()
for (i in length(business$name):1)
{
  
  judgement_pre<-strsplit(business$attributes$Alcohol[i],"'")[[1]]
  judgement<-judgement_pre[2]
  if(is.na(business$attributes$Alcohol[i])==0 &is.na(judgement)==0)
  {
    if(judgement=="full_bar" | business$attributes$Alcohol[i]=="full_bar" )
    {
      #find the pub and record it.
      all_pubs_pre<-data.frame(name=business$name[i],business_id=business$business_id[i],review_count=business$review_count[i],is.open=business$is_open[i],stars=business$stars[i],attributes=business$attributes[i,],hours=business$hours[i,],categories=business$categories[i],state=business$state[i],city=business$city[i])
      all_pubs<-rbind(all_pubs,all_pubs_pre)
    }
  }
}


#Now we have all the data of pubs.
#Next we need to find all open pubs

all_pubs<-subset(all_pubs, is.open==1)

#Then we will find the reviews that are related to the pubs.(It will be a long process to run the code below)

all_review<-subset(review,business_id%in%all_pubs$business_id)


#Here we got all the related reviews.
#Then we will do the same thing to the tip and user

all_tip<-subset(tip,business_id%in%all_pubs$business_id)

user_id_pre<-unique(all_review$user_id)

all_user<-subset(user,user_id%in%user_id_pre)



#In case that there are same comments, we decide to delete all the repeated data.

sq1 <- sqldf("select text, count(text ) count from all_review group by text having count > 1")
all_review<-subset(all_review,!(text%in%sq1$text))
all_user<-subset(all_user,user_id%in%all_review$user_id)
all_tip<-subset(all_tip,user_id%in%all_user$user_id)

if (!require("rjson")) {
  install.packages("rjson")
  stopifnot(require("rjson"))
}
if (!require("jsonlite")) {
  install.packages("jsonlite")
  stopifnot(require("jsonlite"))
}
if (!require("stringr")) {
  install.packages("stringr")
  stopifnot(require("stringr"))
}
if (!require("sqldf")) {
  install.packages("sqldf")
  stopifnot(require("sqldf"))
}
if (!require("kernlab")) {
  install.packages("kernlab")
  stopifnot(require("kernlab"))
}
if (!require("tidytext")) {
  install.packages("tidytext")
  stopifnot(require("tidytext"))
}
if (!require("dplyr")) {
  install.packages("dplyr")
  stopifnot(require("dplyr"))
}
if (!require("TSA")) {
  install.packages("TSA")
  stopifnot(require("TSA"))
}
if (!require("tidyverse")) {
  install.packages("tidyverse")
  stopifnot(require("tidyverse"))
}

business<-jsonlite::stream_in(file("data/business_city.json"))
review<-jsonlite::stream_in(file("data/review_city.json"))
tip<-jsonlite::stream_in(file("data/tip_city.json"))
user<-jsonlite::stream_in(file("data/user_city.json"))


#First, we need to find all the pubs by using the tag:alcohol in the business
#Then we need to decide what tags to stay.
all_pubs<-data.frame()
for (i in length(business$name):1)
{
  
  judgement_pre<-strsplit(business$attributes$Alcohol[i],"'")[[1]]
  judgement<-judgement_pre[2]
  if(is.na(business$attributes$Alcohol[i])==0 &is.na(judgement)==0)
  {
    if(judgement=="full_bar" | business$attributes$Alcohol[i]=="full_bar" )
    {
      #find the pub and record it.
      all_pubs_pre<-data.frame(name=business$name[i],business_id=business$business_id[i],review_count=business$review_count[i],is.open=business$is_open[i],stars=business$stars[i],attributes=business$attributes[i,],hours=business$hours[i,],categories=business$categories[i],state=business$state[i],city=business$city[i])
      all_pubs<-rbind(all_pubs,all_pubs_pre)
    }
  }
}


#Now we have all the data of pubs.
#Next we need to find all open pubs

all_pubs<-subset(all_pubs, is.open==1)

#Then we will find the reviews that are related to the pubs.(It will be a long process to run the code below)

all_review<-subset(review,business_id%in%all_pubs$business_id)


#Here we got all the related reviews.
#Then we will do the same thing to the tip and user

all_tip<-subset(tip,business_id%in%all_pubs$business_id)

user_id_pre<-unique(all_review$user_id)

all_user<-subset(user,user_id%in%user_id_pre)

#In case that there are same comments, we decide to delete all the repeated data.

sq1 <- sqldf("select text, count(text ) count from all_review group by text having count > 1")
all_review<-subset(all_review,!(text%in%sq1$text))
all_user<-subset(all_user,user_id%in%all_review$user_id)
all_tip<-subset(all_tip,user_id%in%all_user$user_id)

#Remove the columns not related to the restaurants
drops <- c("attributes.HairSpecializesIn","attributes.AcceptsInsurance")
all_pubs = all_pubs[ , !(names(all_pubs) %in% drops)]

# Create a new dataset that is going to have 'name' 'business_id' 'review_count' 'is.open' 'stars' and each of of the categories columns. 
# The categories are going to have 1 or 0 values. We will do the same for the location. 
# This way when we build an app, a customer can filter by location and by category.
# Then extract this to a new dataframe pubs_categories
pubs_categories = all_pubs %>%
  select(name, business_id, review_count, stars, categories, state) %>% 
  filter(review_count>3) %>%
  group_by(business_id) %>%
  unnest_tokens(categories, categories, token = 'regex', pattern=", ") %>%
  mutate(value = 1) %>% 
  spread(categories, value, fill = 0)

