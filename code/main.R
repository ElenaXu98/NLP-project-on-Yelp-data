
#################### loading packages that we need ##########################
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

####################### data preprocessing ##################################################
business<-jsonlite::stream_in(file("../data/business_city.json"))
review<-jsonlite::stream_in(file("../data/review_city.json"))
tip<-jsonlite::stream_in(file("../data/tip_city.json"))
user<-jsonlite::stream_in(file("../data/user_city.json"))
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

#Below is the change to lower letter and some abbreviation in review.
all_review$text<-tolower(all_review$text)
all_review$text<-gsub("\'t"," not",all_review$text)
all_review$text<-gsub("\'d"," would",all_review$text)
all_review$text<-gsub("he\'s","he is",all_review$text)
all_review$text<-gsub("she\'s","she is",all_review$text)
all_review$text<-gsub("i\'m","i am",all_review$text)
all_review$text<-gsub("it\'s","it is",all_review$text)
all_review$text<-gsub("\\$","",all_review$text)

# same cleaning to tips text
all_tip$text<-tolower(all_tip$text)
all_tip$text<-gsub("\'t"," not",all_tip$text)
all_tip$text<-gsub("\'d"," would",all_tip$text)
all_tip$text<-gsub("he\'s","he is",all_tip$text)
all_tip$text<-gsub("she\'s","she is",all_tip$text)
all_tip$text<-gsub("i\'m","i am",all_tip$text)
all_tip$text<-gsub("it\'s","it is",all_tip$text)
all_tip$text<-gsub("\\$","",all_tip$text)

############## join review and pubs data frame and filter to get the reviews of all pubs in Wisconsin
review_pubs <- left_join(all_review,all_pubs,how="left",by="business_id") #314845 entries
sum(is.na(review_pubs$state))  # no NA's in state 
review_pubs_WI <- review_pubs[review_pubs$state == "WI",]  #50569 entries
tip_pubs <- left_join(all_tip,all_pubs,how="left",by="business_id")
tip_pubs_WI <- tip_pubs[tip_pubs$state == "WI",]  

write.csv(review_pubs,file = "../output/review_pubs.csv")
write.csv(tip_pubs,file = "../output/tip_pubs.csv")

######### tokenize and remove stop words, then get the most frequent nouns and adjectives in review text
Noun<-unnest_tokens(tibble(txt=all_review$text),word, txt)%>%anti_join(stop_words) %>%left_join(parts_of_speech) %>%filter(pos %in% c("Noun")) %>%count(word,sort = TRUE)
TopNoun <- Noun[1:100,] # select top 100 
#TopNoun <- Noun[Noun$n>quantile(Noun$n,0.99),] # select top 0.01
summary(Noun)
quantile(Noun$n,0.99) # frequency = 6715.44

Adj<-unnest_tokens(tibble(txt=all_review$text),word, txt)%>%anti_join(stop_words) %>%left_join(parts_of_speech) %>%filter(pos %in% c("Adjective")) %>%count(word,sort = TRUE)
TopAdj <- Adj[1:100,]  # select top 100 
summary(Adj)
quantile(Adj$n,0.99) # frequency = 6289
Adj[100,'n']

######### tokenize and remove stop words, then get the most frequent nouns and adjectives in tip text
Noun_tip<-unnest_tokens(tibble(txt=all_tip$text),word, txt)%>%anti_join(stop_words) %>%left_join(parts_of_speech) %>%filter(pos %in% c("Noun")) %>%count(word,sort = TRUE)
TopNoun_tip <- Noun_tip[1:100,] # select top 100 
summary(Noun_tip)
quantile(Noun_tip$n,0.99) # frequency = 279.29

Adj_tip<-unnest_tokens(tibble(txt=all_tip$text),word, txt)%>%anti_join(stop_words) %>%left_join(parts_of_speech) %>%filter(pos %in% c("Adjective")) %>%count(word,sort = TRUE)
TopAdj_tip <- Adj_tip[1:100,]
summary(Adj_tip)
quantile(Adj_tip$n,0.99) 

########### get sentiment of review and tip text
sentiment_review <- c()
for (i in 1:dim(all_review)[1]) {
  tidy_review <- unnest_tokens(tibble(txt=all_review$text[i]),word, txt)%>%
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing"))%>%
    count(word,sentiment)%>%
    spread(sentiment,n,fill = 0)
  sentiment <- sum(tidy_review$positive)-sum(tidy_review$negative)
  sentiment_review <- c(sentiment_review,sentiment)
  # print(i)
}

write.csv(sentiment_review,file = "../output/sentiment_review.csv")

#average_length_tip <- dim(unnest_tokens(tibble(txt=all_tip$text),word, txt))[1]/dim(all_tip)[1]
sentiment_tip <- c()
for (i in 1:dim(all_tip)[1]) {
  tidy_tip <- unnest_tokens(tibble(txt=all_tip$text[i]),word, txt)%>%
    anti_join(stop_words)%>% 
    inner_join(get_sentiments("bing"))%>%
    count(word,sentiment) %>%
    spread(sentiment,n,fill = 0)
  sentiment <- sum(tidy_tip$positive)-sum(tidy_tip$negative)
  sentiment_tip <- c(sentiment_tip,sentiment)
  # print(i)
}

write.csv(sentiment_tip,file = "../output/sentiment_tip.csv")

### sentiment analysis of reviews of pubs in Wisconsin
sentiment_review_WI <- c()
for (i in 1:dim(review_pubs_WI)[1]) {
  tidy_review <- unnest_tokens(tibble(txt=review_pubs_WI$text[i]),word, txt)%>%
    anti_join(stop_words) %>% 
    inner_join(get_sentiments("bing"))%>%
    count(word,sentiment)%>%
    spread(sentiment,n,fill = 0)
  sentiment <- sum(tidy_review$positive)-sum(tidy_review$negative)
  sentiment_review_WI <- c(sentiment_review_WI,sentiment)
  # print(i)
}
sentiment_review_WI_bus <- cbind(review_pubs_WI$business_id,sentiment_review_WI)
write.csv(sentiment_review_WI_bus,file = "../output/sentiment_review_WI.csv")

### sentiment analysis of tips of pubs in Wisconsin
sentiment_tip_WI <- c()
for (i in 1:dim(tip_pubs_WI)[1]) {
  tidy_tip <- unnest_tokens(tibble(txt=tip_pubs_WI$text[i]),word, txt)%>%
    anti_join(stop_words)%>% 
    inner_join(get_sentiments("bing"))%>%
    count(word,sentiment) %>%
    spread(sentiment,n,fill = 0)
  sentiment <- sum(tidy_tip$positive)-sum(tidy_tip$negative)
  sentiment_tip_WI <- c(sentiment_tip_WI,sentiment)
  print(i)
}

write.csv(sentiment_tip_WI,file = "../output/sentiment_tip_WI.csv")


########################### EDA ################################
#Below is the function for plots in all_pubs.
plotWordStar <- function(stars,DTM,wordList,mfrow = c(4,4)) {
  par(mfrow = mfrow)
  
  for (i in 1 :length(wordList)){
    starsY = rep(0,5)
    for(j in 2:10) {
      k=j/2
      dtm_vec = DTM[which(stars == k)]
      numbers = sum(grepl(wordList[i],dtm_vec))
      starsY[j]  = numbers / sum(stars == k)
    }
    
    names(starsY)<-c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)
    barplot(starsY,main=wordList[i],xlab="Stars",ylab="proportion")
  }  
   par(mfrow = mfrow)
   
   for (i in 1 :length(wordList)){
      starsY = rep(0,5)
      for(j in 2:10) {
         k=j/2
         dtm_vec = DTM[which(stars == k)]
         numbers = sum(grepl(wordList[i],dtm_vec))
         starsY[j]  = numbers / sum(stars == k)
      }

      names(starsY)<-c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0)
      barplot(starsY,main=wordList[i],xlab="Stars",ylab="proportion")
   }  
}

opentime <- function(time){
  len<-length(time)
  opentime<-rep(0,len)
  for (i in 1:len){
    time1_hour<- as.numeric(strsplit(strsplit(time[i],"-")[[1]][1],":")[[1]][1])
    time1_minute<-as.numeric(strsplit(strsplit(time[i],"-")[[1]][1],":")[[1]][2])/60
    time2_hour<-as.numeric(strsplit(strsplit(time[i],"-")[[1]][2],":")[[1]][1])
    time2_minute<-as.numeric(strsplit(strsplit(time[i],"-")[[1]][2],":")[[1]][2])/60
    if(time2_minute-time1_minute<0){minute<-time2_minute-time1_minute+1; time2_hour=time2_hour-1}else{minute<-time2_minute-time1_minute}
    if(time2_hour-time1_hour<0){hour<-time2_hour-time1_hour+24}else{hour<-time2_hour-time1_hour}
    opentime[i]<-hour+minute
  }
  return(opentime)
}

Test<-data.frame(No=1:8,H_0=rep(NA,8),method=rep(NA,8),p_value=rep(NA,8))
ave_star<-quantile(all_pubs$stars)[3]

##############################################################################################################
###Test the influence of takeout
plotWordStar(all_pubs$stars,all_pubs$attributes.RestaurantsTakeOut,wordList=c("True","False"),mfrow = c(1,2))
low_all<-all_pubs$attributes.RestaurantsTakeOut[all_pubs$stars<ave_star]
high_all<-all_pubs$attributes.RestaurantsTakeOut[all_pubs$stars>=ave_star]
high_takeout_yes<-sum(high_all=="True",na.rm = T)
high_takeout_no<-sum(high_all=="False",na.rm = T)
low_takeout_yes<-sum(low_all=="True",na.rm = T)
low_takeout_no<-sum(low_all=="False",na.rm = T)
x<-c(high_takeout_yes,high_takeout_no,low_takeout_yes,low_takeout_no)
dim(x)<- c(2,2)
chisq.test(x,correct = F)
Test$H_0[1]<-"High Ratings are not related with the existence of takeout"
Test$method[1]<-"chisq-test"
Test$p_value[1]<-chisq.test(x,correct = F)$p.value
#Refuse Ho, so Ratings are related with takout
##############################################################################################################

#Test the influence of Good for Groups
plotWordStar(all_pubs$stars,all_pubs$attributes.RestaurantsGoodForGroups,wordList=c("True","False"),mfrow = c(1,2))
low_all<-all_pubs$attributes.RestaurantsGoodForGroups[all_pubs$stars<ave_star]
high_all<-all_pubs$attributes.RestaurantsGoodForGroups[all_pubs$stars>=ave_star]
high_groups_yes<-sum(high_all=="True",na.rm = T)
high_groups_no<-sum(high_all=="False",na.rm = T)
low_groups_yes<-sum(low_all=="True",na.rm = T)
low_groups_no<-sum(low_all=="False",na.rm = T)
x<-c(high_groups_yes,high_groups_no,low_groups_yes,low_groups_no)
dim(x)<- c(2,2)
chisq.test(x,correct = F)
Test$H_0[2]<-"High Ratings are not related with the influence of GoodforGroups"
Test$method[2]<-"chisq-test"
Test$p_value[2]<-chisq.test(x,correct = F)$p.value
#Refuse Ho, so Ratings are related with Goodforgroups
##############################################################################################################
#Test the existence of TV

plotWordStar(all_pubs$stars,all_pubs$attributes.HasTV,wordList=c("True","False"),mfrow = c(1,2))
low_all<-all_pubs$attributes.HasTV[all_pubs$stars<ave_star]
high_all<-all_pubs$attributes.HasTV[all_pubs$stars>=ave_star]
high_tv_yes<-sum(high_all=="True",na.rm = T)
high_tv_no<-sum(high_all=="False",na.rm = T)
low_tv_yes<-sum(low_all=="True",na.rm = T)
low_tv_no<-sum(low_all=="False",na.rm = T)
x<-c(high_tv_yes,high_tv_no,low_tv_yes,low_tv_no)
dim(x)<- c(2,2)
chisq.test(x,correct = F)
Test$H_0[3]<-"High Ratings are not related with the existence of TV"
Test$method[3]<-"chisq-test"
Test$p_value[3]<-chisq.test(x,correct = F)$p.value
#Refuse Ho, so Ratings are related with tv

#############################################################################################################
word<-c("love","yummy","great","good","nice","wonderful", "amazing", "ordinary", "hate", "bad","worst","disappoint", "awful", "terrific", "decent", "average")
plotWordStar(all_review$stars,all_review$text,wordList=word,mfrow = c(4,4))
#here I use "love" to check whether it can influence the rate
key_word<-word[8]
all_stars_with_key<-all_review$stars[which(grepl(key_word,all_review$text))]
all_stars_without_key<-all_review$stars[-which(grepl(key_word,all_review$text))]
wilcox.test(all_stars_with_key,all_stars_without_key,alternative="less")
Test$H_0[4]<-"High Ratings are not related with the appearence of word 'love'"
Test$method[4]<-"wilcox-test"
Test$p_value[4]<-wilcox.test(all_stars_with_key,all_stars_without_key,alternative="less")$p.value
################ how different types of beer related to stars ######################
AlcoholDrinks <- c("beer","Ale","wine","Rum","rum","Brandy","Gin","gin","Whisky","whisky","Whiskey","whiskey","Texas whiskey","Vodka","Absinthe","Tequila","cocktails","Cocktails")
plotWordStar(all_review$stars,all_review$text,wordList=AlcoholDrinks,mfrow = c(1,2))

# Cocktails,Absinthe, Vodka, Whiskey, Brandy, Rum, wine are good for restaurant.
# beer, rum, gin, tequila are fairly normal for business.


######################################################
#Test the existence of wifi's influence on ratings.
#First we should transform the names in Wifi
all_pubs$attributes.WiFi<-gsub("u'free'","'free'",all_pubs$attributes.WiFi)
all_pubs$attributes.WiFi<-gsub("None","'no'",all_pubs$attributes.WiFi)
all_pubs$attributes.WiFi<-gsub("u'no'","'no'",all_pubs$attributes.WiFi)
all_pubs$attributes.WiFi<-gsub("u'paid'","'paid'",all_pubs$attributes.WiFi)

plotWordStar(all_pubs$stars,all_pubs$attributes.WiFi,wordList=c("u'no'","u'free'","'no'","'free'","u'paid'","'paid'","None" ),mfrow = c(2,4))
low_all<-all_pubs$attributes.WiFi[all_pubs$stars<ave_star]
high_all<-all_pubs$attributes.WiFi[all_pubs$stars>=ave_star]
wifi_word_paid<-c("u'paid'","'paid'")
wifi_word_free<-c("u'free'","'free'")
wifi_word_no<-c("u'no'","'no'","None")
high_wifi_paid<-sum(high_all%in% wifi_word_paid,na.rm = T)
low_wifi_paid<-sum(low_all%in% wifi_word_paid,na.rm = T)
high_wifi_free<-sum(high_all%in% wifi_word_free,na.rm = T)
low_wifi_free<-sum(low_all%in% wifi_word_free,na.rm = T)
high_all<-all_pubs$attributes.WiFi[all_pubs$stars>ave_star]
wifi_word_yes<-c("'free'","'paid'")
wifi_word_no<-c("'no'")
high_wifi_yes<-sum(high_all%in% wifi_word_yes,na.rm = T)
high_wifi_no<-sum(high_all%in% wifi_word_no,na.rm = T)
low_wifi_no<-sum(low_all%in% wifi_word_no,na.rm = T)
x<-c(high_wifi_paid,high_wifi_free,high_wifi_no,low_wifi_paid,low_wifi_free,low_wifi_no)
dim(x)<- c(3,2)
chisq.test(x,correct = F)
Test$H_0[5]<-"High Ratings are not related with the existence of Wifi"
Test$method[5]<-"chisq-test"
Test$p_value[5]<-chisq.test(x,correct = F)$p.value
#Can't refuse Ho, so Ratings are not related with Wifi

##############################################################################################################
#Test the restaurantdelivery

plotWordStar(all_pubs$stars,all_pubs$attributes.RestaurantsDelivery,wordList=c("True","False"),mfrow = c(1,2))
low_all<-all_pubs$attributes.RestaurantsDelivery[all_pubs$stars<ave_star]
high_all<-all_pubs$attributes.RestaurantsDelivery[all_pubs$stars>ave_star]
high_de_yes<-sum(high_all=="True",na.rm = T)
high_de_no<-sum(high_all=="False",na.rm = T)
low_de_yes<-sum(low_all=="True",na.rm = T)
low_de_no<-sum(low_all=="False",na.rm = T)
x<-c(high_de_yes,high_de_no,low_de_yes,low_de_no)
dim(x)<- c(2,2)
chisq.test(x,correct = F)
Test$H_0[6]<-"High Ratings are not related with the existence of delivery"
Test$method[6]<-"chisq-test"
Test$p_value[6]<-chisq.test(x,correct = F)$p.value
#Refuse Ho, so Ratings are related with delivery.

##############################################################################################################

#Test the GoodforDancing
plotWordStar(all_pubs$stars,all_pubs$attributes.GoodForDancing,wordList=c("True","False"),mfrow = c(1,2))
low_all<-all_pubs$attributes.GoodForDancing[all_pubs$stars<ave_star]
high_all<-all_pubs$attributes.GoodForDancing[all_pubs$stars>ave_star]
high_dance_yes<-sum(high_all=="True",na.rm = T)
high_dance_no<-sum(high_all=="False",na.rm = T)
low_dance_yes<-sum(low_all=="True",na.rm = T)
low_dance_no<-sum(low_all=="False",na.rm = T)
x<-c(high_dance_yes,high_dance_no,low_dance_yes,low_dance_no)
dim(x)<- c(2,2)
chisq.test(x,correct = F)
Test$H_0[7]<-"High Ratings are not related with the influence of Goodfordancing"
Test$method[7]<-"chisq-test"
Test$p_value[7]<-chisq.test(x,correct = F)$p.value
#Refuse Ho, so Ratings are related with dancing.

############################################################################################################
low_all<-all_pubs$hours.Friday[all_pubs$stars<ave_star&is.na(all_pubs$hours.Friday)==0]
high_all<-all_pubs$hours.Friday[all_pubs$stars>ave_star&is.na(all_pubs$hours.Friday)==0]
low_time<-opentime(low_all)
high_time<-opentime(high_all)
wilcox.test(low_time,high_time,alternative="less")
Test$H_0[8]<-"High Ratings are not related with the opentime on Friday"
Test$method[8]<-"chisq-test"
Test$p_value[8]<-chisq.test(x,correct = F)$p.value
#Can't refuse Ho, so Ratings are not related with opentime on Friday.


################################## multiple regression ####################################
attributes <- c()
for (i in colnames(all_pubs)) {
  if(sum(is.na(all_pubs[,i]))/dim(all_pubs)[1]<0.3 & i!="attributes.Alcohol"){
    attributes <- c(attributes,i)
  }
}
attributes <- attributes[-c(1:5,23:32)]

RegData <- all_pubs[all_pubs$state=="WI",c("business_id","stars",attributes)]
sentiment_bus <- tapply(sentiment_review_WI,review_pubs_WI$business_id,mean)
RegData <- cbind(RegData,sentiment_bus)

for (i in attributes) {
  as.factor(RegData[,i])
}

RegData$attributes.RestaurantsAttire<-gsub("u'","",RegData$attributes.RestaurantsAttire)
RegData$attributes.RestaurantsAttire<-gsub("'","",RegData$attributes.RestaurantsAttire)
RegData$attributes.NoiseLevel<-gsub("u'","",RegData$attributes.NoiseLevel)
RegData$attributes.NoiseLevel<-gsub("'","",RegData$attributes.NoiseLevel)

sum(na.omit(RegData$attributes.RestaurantsTakeOut)=="True")>sum(na.omit(RegData$attributes.RestaurantsTakeOut)=="False")
for (i in 1:466) {
  if(is.na(RegData$attributes.RestaurantsTakeOut[i])){
    RegData$attributes.RestaurantsTakeOut[i] <- "True"
  }
}
sum(na.omit(RegData$attributes.BusinessAcceptsCreditCards)=="True")>sum(na.omit(RegData$attributes.BusinessAcceptsCreditCards)=="False")
for (i in 1:466) {
  if(is.na(RegData$attributes.BusinessAcceptsCreditCards[i])){
    RegData$attributes.BusinessAcceptsCreditCards[i] <- "True"
  }
}
sum(na.omit(RegData$attributes.GoodForKids)=="True")>sum(na.omit(RegData$attributes.GoodForKids)=="False")
for (i in 1:466) {
  if(is.na(RegData$attributes.GoodForKids[i])){
    RegData$attributes.GoodForKids[i] <- "True"
  }
}
sum(na.omit(RegData$attributes.RestaurantsReservations)=="True")>sum(na.omit(RegData$attributes.RestaurantsReservations)=="False")
for (i in 1:466) {
  if(is.na(RegData$attributes.RestaurantsReservations[i])){
    RegData$attributes.RestaurantsReservations[i] <- "False"
  }
}
sum(na.omit(RegData$attributes.RestaurantsGoodForGroups)=="True")>sum(na.omit(RegData$attributes.RestaurantsGoodForGroups)=="False")
for (i in 1:466) {
  if(is.na(RegData$attributes.RestaurantsGoodForGroups[i])){
    RegData$attributes.RestaurantsGoodForGroups[i] <- "True"
  }
}
sum(na.omit(RegData$attributes.HasTV)=="True")>sum(na.omit(RegData$attributes.HasTV)=="False")
for (i in 1:466) {
  if(is.na(RegData$attributes.HasTV[i])){
    RegData$attributes.HasTV[i] <- "True"
  }
}
sum(na.omit(RegData$attributes.BikeParking)=="True")>sum(na.omit(RegData$attributes.BikeParking)=="False")
for (i in 1:466) {
  if(is.na(RegData$attributes.BikeParking[i])){
    RegData$attributes.BikeParking[i] <- "True"
  }
}
sum(na.omit(RegData$attributes.RestaurantsDelivery)=="True")>sum(na.omit(RegData$attributes.RestaurantsDelivery)=="False")
for (i in 1:466) {
  if(is.na(RegData$attributes.RestaurantsDelivery[i])){
    RegData$attributes.RestaurantsDelivery[i] <- "False"
  }
}
sum(na.omit(RegData$attributes.OutdoorSeating)=="True")>sum(na.omit(RegData$attributes.OutdoorSeating)=="False")
for (i in 1:466) {
  if(is.na(RegData$attributes.OutdoorSeating[i])){
    RegData$attributes.OutdoorSeating[i] <- "True"
  }
}
sum(na.omit(RegData$attributes.Caters)=="True")>sum(na.omit(RegData$attributes.Caters)=="False")
for (i in 1:466) {
  if(is.na(RegData$attributes.Caters[i])){
    RegData$attributes.Caters[i] <- "False"
  }
}
sum(na.omit(RegData$attributes.WiFi)=="'free'")
sum(na.omit(RegData$attributes.WiFi)=="'no'")
sum(na.omit(RegData$attributes.WiFi)=="'paid'")
for (i in 1:466) {
  if(is.na(RegData$attributes.WiFi[i])){
    RegData$attributes.WiFi[i] <- "'free'"
  }
}

sum(na.omit(RegData$attributes.NoiseLevel)=="very_loud")
sum(na.omit(RegData$attributes.NoiseLevel)=="loud")
sum(na.omit(RegData$attributes.NoiseLevel)=="average")
sum(na.omit(RegData$attributes.NoiseLevel)=="quiet")
for (i in 1:466) {
  if(is.na(RegData$attributes.NoiseLevel[i])){
    RegData$attributes.NoiseLevel[i] <- "average"
  }
}
sum(na.omit(RegData$attributes.RestaurantsAttire)=="formal")
sum(na.omit(RegData$attributes.RestaurantsAttire)=="dressy")
sum(na.omit(RegData$attributes.RestaurantsAttire)=="casual")
for (i in 1:466) {
  if(is.na(RegData$attributes.RestaurantsAttire[i])){
    RegData$attributes.RestaurantsAttire[i] <- "casual"
  }
}
sum(na.omit(RegData$attributes.RestaurantsPriceRange2)==1)
sum(na.omit(RegData$attributes.RestaurantsPriceRange2)==2)
sum(na.omit(RegData$attributes.RestaurantsPriceRange2)==3)
sum(na.omit(RegData$attributes.RestaurantsPriceRange2)==4)
for (i in 1:466) {
  if(is.na(RegData$attributes.RestaurantsPriceRange2[i])){
    RegData$attributes.RestaurantsPriceRange2[i] <- 2
  }
}

for (i in attributes) {
  as.factor(RegData[,i])
}

#### regression
RegData2 <- RegData[,c(2:9,11:17,20)]
model <- step(lm(stars~.,data = RegData2),direction = "both",k=2,trace = F)
summary(model)
summary(lm(stars~.,data = RegData2))

model <- lm(stars~attributes.RestaurantsAttire+attributes.RestaurantsTakeOut+attributes.BusinessAcceptsCreditCards+attributes.NoiseLevel+attributes.GoodForKids+attributes.RestaurantsReservations+attributes.RestaurantsGoodForGroups+attributes.RestaurantsPriceRange2+attributes.HasTV+attributes.BikeParking+attributes.RestaurantsDelivery+attributes.OutdoorSeating+attributes.Caters+attributes.WiFi+sentiment_bus,data=RegData2 )

model <- lm(stars~attributes.NoiseLevel+attributes.GoodForKids+attributes.RestaurantsReservations+attributes.HasTV+attributes.RestaurantsDelivery,data=RegData2 )
summary(model)







######################analysis the most frequent nouns in review and tips ####################################
NounCandidate <- inner_join(TopNoun,TopNoun_tip,by="word")
AdjCandidate <- inner_join(TopAdj,TopAdj_tip,by="word")
topics <- c("time","menu","beer","staff","wait","atmosphere","waitress","hour","day","wine","location","home","bartender","family","patio","seating","free","parking","mexican","game","quick","friday","reservation","fast","tap","cheap")
topics_index <- c()
for (i in topics) {
  topics_index <- c(topics_index,which(NounCandidate==i))
}
topic <- cbind(topics,topics_index)
colnames(topic) <- c("topics","frequency_rank")
write.csv(topic,file = "../output/topic_NLP.csv")


business_id <- unique(review_pubs_WI$business_id)
TimeSenti <- c()
MenuSenti <- c()
StaffSenti <- c()
ServerSenti <- c()
bartenderSenti <- c()
for (i in business_id) {
  TimeIndex <- grepl("time",review_pubs_WI$text[review_pubs_WI$business_id==i])
  TimeSentiBus <- mean(sentiment_review_WI[TimeIndex & review_pubs_WI$business_id==i])
  TimeSenti <- c(TimeSenti,TimeSentiBus)
  
  MenuIndex <- grepl("menu",review_pubs_WI$text[review_pubs_WI$business_id==i])
  MenuSentiBus <- mean(sentiment_review_WI[MenuIndex & review_pubs_WI$business_id==i])
  MenuSenti <- c(MenuSenti,MenuSentiBus)
  
  StaffIndex <- grepl("staff",review_pubs_WI$text[review_pubs_WI$business_id==i])
  StaffSentiBus <- mean(sentiment_review_WI[StaffIndex & review_pubs_WI$business_id==i])
  StaffSenti <- c(StaffSenti,StaffSentiBus)
  
  ServerIndex <- grepl("server",review_pubs_WI$text[review_pubs_WI$business_id==i])
  ServerSentiBus <- mean(sentiment_review_WI[ServerIndex & review_pubs_WI$business_id==i])
  ServerSenti <- c(ServerSenti,
ServerSentiBus)
  
  bartenderIndex <- grepl("bartender",review_pubs_WI$text[review_pubs_WI$business_id==i])
  bartenderSentiBus <- mean(sentiment_review_WI[bartenderIndex & review_pubs_WI$business_id==i])
  bartenderSenti <- c(bartenderSenti,bartenderSentiBus)
}

for (i in business_id) {
  print(i)
  TVIndex <- grepl("tv",review_pubs_WI$text[review_pubs_WI$business_id==i])
  TVSentiBus <- mean(sentiment_review_WI[TVIndex & review_pubs_WI$business_id==i])
  TVSenti <- c(TVSenti,TVSentiBus)
  
}

#lm(review_pubs_WI$stars.y~)

################ topic analysis
library(topicmodels)
data("AssociatedPress")
tidy(AssociatedPress)

review_words_WI <- tibble(txt=review_pubs_WI$text,review_id=review_pubs_WI$review_id) %>%
                    unnest_tokens(word, txt)%>% 
                    anti_join(stop_words)%>%
                  count(review_id,word,sort=TRUE)
                
review_words_WI_sentiment_dtm <- review_words_WI %>%cast_dtm(review_id,word,n)
                        
#%>%inner_join(get_sentiments("bing"), by = c(word = "word"))

ap_lda <- LDA(review_words_WI_sentiment_dtm, k = 3, control = list(seed = 1234))
ap_topics <- tidy(ap_lda, matrix = "beta") 
ap_topics #one-topic-per-term-per-row format

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms <- ap_top_terms


ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic))%>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() # if error , run dev.off(), then plot again







#############################################################################################################
word<-c("love","yummy","great","good","nice","wonderful", "amazing", "ordinary", "hate", "bad","worst","disappoint", "awful", "terrific", "decent", "average")
plotWordStar(all_review$stars,all_review$text,wordList=word,mfrow = c(4,4))
#here I use "love" to check whether it can influence the rate
key_word<-word[8]
all_stars_with_key<-all_review$stars[which(grepl(key_word,all_review$text))]
all_stars_without_key<-all_review$stars[-which(grepl(key_word,all_review$text))]
wilcox.test(all_stars_with_key,all_stars_without_key,alternative="less")

################ how different types of beer related to stars ######################
AlcoholDrinks <- c("beer","Ale","wine","Rum","rum","Brandy","Gin","gin","Whisky","whisky","Whiskey","whiskey","Texas whiskey","Vodka","Absinthe","Tequila","cocktails","Cocktails")
plotWordStar(all_review$stars,all_review$text,wordList=AlcoholDrinks,mfrow = c(1,2))

# Cocktails,Absinthe, Vodka, Whiskey, Brandy, Rum, wine are good for restaurant.
# beer, rum, gin, tequila are fairly normal for business.





############################# some other terms that may affect rating ########################################
terms <- c("atmosphere","service","kids","family","clean")
plotWordStar(all_review$stars,all_review$text,wordList=terms,mfrow = c(1,2))
# good atmosphere are good
# not for kids
# normal rating pubs are clean

## question: what kind of atmosphere?
#all_review1 <- all_review[1:1000,]
index <- str_detect(all_review[,'text'],"atmosphere")
atomsphere_words <- c("cozy","festive","amazing","great","free","inviting","chill","warm","welcoming","cool","great","nice","comfortable","casual")
plotWordStar(all_review$stars[index],all_review$text[index],wordList=atomsphere_words,mfrow = c(1,2))

# casual, welcoming, inviting, cozy atmosphere are good for rating
############################ does different city, state has different preference? #######################3
# preference of atmosphere
index1 <- all_review$business_id==all_pubs$business_id
index2 <- all_pubs$state == 'WI'
length(all_pubs$state)
sum(is.na(all_pubs$state))
plotWordStar(all_review$stars[all_review$business_id==all_pubs$business_id & all_pubs$state == 'WI'],all_review$text[all_review$business_id==all_pubs$business_id & all_pubs$state == 'WI'],wordList=atomsphere_words,mfrow = c(1,2))




##### words about alcohol drinks 
AlcoholDrinks <- c("beer","Ale","wine","Rum","rum","Brandy","Gin","gin","Whisky","whisky","Whiskey","whiskey","Texas whiskey","Vodka","Absinthe","Tequila","cocktails","Cocktails")
AlcoholBrands <- c("Smirnoff","Bacardi","Jack Daniel's","Crown Royal","Absolut","crown","bacardi","smirnoff")
Cocktails <- c("martini","Martini","Long Island","pink lady","champagne","Manhattan","coquito")

Alcohol <- matrix(0,nrow=33,ncol = 2)
colnames(Alcohol) <- c("noun","frequency")
Alcohol[,1] <- c(AlcoholDrinks,AlcoholBrands,Cocktails)
for (rownames(frequency_of_noun)%in%Alcohol[,1]) {
  
}
inner_join(Alcohol,freq,how="inner",on="noun")



##################### sentiment analysis ##################################
test_review <- all_review[1:1000,]
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
text <- test_review[,'text']

text %>%
  inner_join(get_sentiments("bing")) %>%
  filter(!is.na(sentiments)) %>%
  count(sentiments,sort=TRUE)

lexicon <- c("love","yummy","great","good","nice","wonderful", "amazing", "ordinary", "hate", "bad","worst","disappoint", "awful", "terrific", "decent", "average")


pubs_categories = all_pubs %>%
    select(name, business_id, review_count, stars, categories, state) %>% 
    filter(review_count>3) %>%
    group_by(business_id) %>%
    unnest_tokens(categories, categories, token = 'regex', pattern=", ") %>%
    mutate(value = 1) %>% 
    spread(categories, value, fill = 0)


####Exploring a new way to do the sentiment analysis
american_tokens = review_pubs %>%
  grepl("American", category)==TRUE %>%
  unnest_tokens(tibble(txt=review_pubs$text),word, text)
  
lex = sentiments %>%
  filter(lexicon =='bing')%>%
  select(word, bing_score=score)

pubs_sentiment = american_tokens %>%
  inner_join(lex, by='word') %>%
  group_by(review_id, stars)%>%
  summarize(sentiment = mean(bing_score))
  
counted_words = american_tokens %>%
  count(review_id, business_id, stars, word)






