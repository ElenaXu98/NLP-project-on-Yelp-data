
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
all_pubs<-subset(all_pubs, state=="WI")
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
#split the BusinessParking into 5 lines
all_pubs$attributes.BusinessParking.garage<-rep(NA,length(all_pubs$attributes.BusinessParking))
all_pubs$attributes.BusinessParking.street<-rep(NA,length(all_pubs$attributes.BusinessParking))
all_pubs$attributes.BusinessParking.validated<-rep(NA,length(all_pubs$attributes.BusinessParking))
all_pubs$attributes.BusinessParking.lot<-rep(NA,length(all_pubs$attributes.BusinessParking))
all_pubs$attributes.BusinessParking.valet<-rep(NA,length(all_pubs$attributes.BusinessParking))
for (i in 1:length(all_pubs$attributes.BusinessParking))
{
  if(is.na(all_pubs$attributes.BusinessParking[i])==0){
  if(all_pubs$attributes.BusinessParking[i]==" None"){
    all_pubs$attributes.BusinessParking.garage[i]<-" None"
    all_pubs$attributes.BusinessParking.street[i]<-" None"
    all_pubs$attributes.BusinessParking.validated[i]<-" None"
    all_pubs$attributes.BusinessParking.lot[i]<-" None"
    all_pubs$attributes.BusinessParking.valet[i]<-" None"
  }else{
  temp<-substr(all_pubs$attributes.BusinessParking[i],2,nchar(all_pubs$attributes.BusinessParking[i])-1)
  temp<-strsplit(temp,",")[[1]]
  all_pubs$attributes.BusinessParking.garage[i]<-strsplit(temp[1],":")[[1]][2]
  all_pubs$attributes.BusinessParking.street[i]<-strsplit(temp[2],":")[[1]][2]
  all_pubs$attributes.BusinessParking.validated[i]<-strsplit(temp[3],":")[[1]][2]
  all_pubs$attributes.BusinessParking.lot[i]<-strsplit(temp[4],":")[[1]][2]
  all_pubs$attributes.BusinessParking.valet[i]<-strsplit(temp[5],":")[[1]][2]
  }
}
}
all_pubs$attributes.BusinessParking<-NULL

all_pubs$attributes.GoodForMeal.dessert<-rep(NA,length(all_pubs$attributes.GoodForMeal))
all_pubs$attributes.GoodForMeal.latenight<-rep(NA,length(all_pubs$attributes.GoodForMeal))
all_pubs$attributes.GoodForMeal.lunch<-rep(NA,length(all_pubs$attributes.GoodForMeal))
all_pubs$attributes.GoodForMeal.dinner<-rep(NA,length(all_pubs$attributes.GoodForMeal))
all_pubs$attributes.GoodForMeal.brunch<-rep(NA,length(all_pubs$attributes.GoodForMeal))
all_pubs$attributes.GoodForMeal.breakfast<-rep(NA,length(all_pubs$attributes.GoodForMeal))
for (i in 1:length(all_pubs$attributes.GoodForMeal))
{
  if(is.na(all_pubs$attributes.GoodForMeal[i])==0){
    if(all_pubs$attributes.GoodForMeal[i]==" None"){
      all_pubs$attributes.GoodForMeal.dessert[i]<-" None"
      all_pubs$attributes.GoodForMeal.latenight[i]<-" None"
      all_pubs$attributes.GoodForMeal.lunch[i]<-" None"
      all_pubs$attributes.GoodForMeal.dinner[i]<-" None"
      all_pubs$attributes.GoodForMeal.brunch[i]<-" None"
      all_pubs$attributes.GoodForMeal.breakfast[i]<-" None"
    }else{
      temp<-substr(all_pubs$attributes.GoodForMeal[i],2,nchar(all_pubs$attributes.GoodForMeal[i])-1)
      temp<-strsplit(temp,",")[[1]]
      all_pubs$attributes.GoodForMeal.dessert[i]<-strsplit(temp[1],":")[[1]][2]
      all_pubs$attributes.GoodForMeal.latenight[i]<-strsplit(temp[2],":")[[1]][2]
      all_pubs$attributes.GoodForMeal.lunch[i]<-strsplit(temp[3],":")[[1]][2]
      all_pubs$attributes.GoodForMeal.dinner[i]<-strsplit(temp[4],":")[[1]][2]
      all_pubs$attributes.GoodForMeal.brunch[i]<-strsplit(temp[5],":")[[1]][2]
      all_pubs$attributes.GoodForMeal.breakfast[i]<-strsplit(temp[6],":")[[1]][2]
    }
  }
}
all_pubs$attributes.GoodForMeal<-NULL




all_pubs$attributes.Ambience.touristy<-rep(NA,length(all_pubs$attributes.Ambience))
all_pubs$attributes.Ambience.hipster<-rep(NA,length(all_pubs$attributes.Ambience))
all_pubs$attributes.Ambience.romantic<-rep(NA,length(all_pubs$attributes.Ambience))
all_pubs$attributes.Ambience.divey<-rep(NA,length(all_pubs$attributes.Ambience))
all_pubs$attributes.Ambience.intimate<-rep(NA,length(all_pubs$attributes.Ambience))
all_pubs$attributes.Ambience.trendy<-rep(NA,length(all_pubs$attributes.Ambience))
all_pubs$attributes.Ambience.upscale<-rep(NA,length(all_pubs$attributes.Ambience))
all_pubs$attributes.Ambience.classy<-rep(NA,length(all_pubs$attributes.Ambience))
all_pubs$attributes.Ambience.casual<-rep(NA,length(all_pubs$attributes.Ambience))
for (i in 1:length(all_pubs$attributes.Ambience))
{
  if(is.na(all_pubs$attributes.Ambience[i])==0){
    temp<-substr(all_pubs$attributes.Ambience[i],2,nchar(all_pubs$attributes.Ambience[i])-1)
    temp<-strsplit(temp,",")[[1]]
    
     if(length(temp)<9){
      all_pubs$attributes.Ambience.touristy[i]<-" None"
      all_pubs$attributes.Ambience.hipster[i]<-" None"
      all_pubs$attributes.Ambience.romantic[i]<-" None"
      all_pubs$attributes.Ambience.divey[i]<-" None"
      all_pubs$attributes.Ambience.intimate[i]<-" None"
      all_pubs$attributes.Ambience.trendy[i]<-" None"
      all_pubs$attributes.Ambience.upscale[i]<-" None"
      all_pubs$attributes.Ambience.classy[i]<-" None"
      all_pubs$attributes.Ambience.casual[i]<-" None"
      for (j in 1:length(temp))
      {
        temp2<-strsplit(temp[j],"'")[[1]][2]
        if(temp2=="touristy"){all_pubs$attributes.Ambience.touristy[i]<-strsplit(temp[j],":")[[1]][2]}
        if(temp2=="hipster"){all_pubs$attributes.Ambience.hipster[i]<-strsplit(temp[j],":")[[1]][2]}
        if(temp2=="romantic"){all_pubs$attributes.Ambience.romantic[i]<-strsplit(temp[j],":")[[1]][2]}
        if(temp2=="divey"){all_pubs$attributes.Ambience.divey[i]<-strsplit(temp[j],":")[[1]][2]}
        if(temp2=="intimate"){ all_pubs$attributes.Ambience.intimate[i]<-strsplit(temp[j],":")[[1]][2]}
        if(temp2=="trendy"){all_pubs$attributes.Ambience.trendy[i]<-strsplit(temp[j],":")[[1]][2]}
        if(temp2=="upscale"){all_pubs$attributes.Ambience.upscale[i]<-strsplit(temp[j],":")[[1]][2]}
        if(temp2=="classy"){all_pubs$attributes.Ambience.classy[i]<-strsplit(temp[j],":")[[1]][2]}
        if(temp2=="casual"){all_pubs$attributes.Ambience.casual[i]<-strsplit(temp[j],":")[[1]][2]}
      }
      
    }else{
      
      for (j in 1:9){
        temp2<-strsplit(temp[j],"'")[[1]][2]
      if(temp2=="touristy"){all_pubs$attributes.Ambience.touristy[i]<-strsplit(temp[j],":")[[1]][2]}
      if(temp2=="hipster"){all_pubs$attributes.Ambience.hipster[i]<-strsplit(temp[j],":")[[1]][2]}
      if(temp2=="romantic"){all_pubs$attributes.Ambience.romantic[i]<-strsplit(temp[j],":")[[1]][2]}
      if(temp2=="divey"){all_pubs$attributes.Ambience.divey[i]<-strsplit(temp[j],":")[[1]][2]}
      if(temp2=="intimate"){ all_pubs$attributes.Ambience.intimate[i]<-strsplit(temp[j],":")[[1]][2]}
      if(temp2=="trendy"){all_pubs$attributes.Ambience.trendy[i]<-strsplit(temp[j],":")[[1]][2]}
      if(temp2=="upscale"){all_pubs$attributes.Ambience.upscale[i]<-strsplit(temp[j],":")[[1]][2]}
      if(temp2=="classy"){all_pubs$attributes.Ambience.classy[i]<-strsplit(temp[j],":")[[1]][2]}
      if(temp2=="casual"){all_pubs$attributes.Ambience.casual[i]<-strsplit(temp[j],":")[[1]][2]}
      }
    }
  }
}
all_pubs$attributes.Ambience<-NULL

for (i in 50:69){
all_pubs[,i]<-gsub(" None","None",all_pubs[,i])
all_pubs[,i]<-gsub(" True","True",all_pubs[,i])
all_pubs[,i]<-gsub(" False","False",all_pubs[,i])
}

############## join review and pubs data frame and filter to get the reviews of all pubs in Wisconsin
review_pubs <- left_join(all_review,all_pubs,how="left",by="business_id") 
#sum(is.na(review_pubs$state))  # no NA's in state 
#review_pubs_WI <- review_pubs[review_pubs$state == "WI",]  
tip_pubs <- left_join(all_tip,all_pubs,how="left",by="business_id")
#tip_pubs_WI <- tip_pubs[tip_pubs$state == "WI",]  

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
sentiment_review <- as.numeric(sentiment_review)
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
    if(opentime[i]==0){opentime[i]=24}
  }
  return(opentime)
}

Test<-data.frame(No=1:6,H_0=rep(NA,6),method=rep(NA,6),p_value=rep(NA,6))
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
Test$H_0[2]<-"High Ratings are not related with the existence of TV"
Test$method[2]<-"chisq-test"
Test$p_value[2]<-chisq.test(x,correct = F)$p.value
#Refuse Ho, so Ratings are related with tv

#############################################################################################################
word<-c("love","yummy","great","good","nice","wonderful", "amazing", "ordinary", "hate", "bad","worst","disappoint", "awful", "terrific", "decent", "average")
plotWordStar(all_review$stars,all_review$text,wordList=word,mfrow = c(1,1))
#here I use "ordinary" to check whether it can influence the rate
key_word<-word[8]
all_stars_with_key<-all_review$stars[which(grepl(key_word,all_review$text))]
all_stars_without_key<-all_review$stars[-which(grepl(key_word,all_review$text))]
wilcox.test(all_stars_with_key,all_stars_without_key,alternative="less")
Test$H_0[3]<-"High Ratings are not related with the appearence of word 'love'"
Test$method[3]<-"wilcox-test"
Test$p_value[3]<-wilcox.test(all_stars_with_key,all_stars_without_key,alternative="less")$p.value
################ how different types of beer related to stars ######################
AlcoholDrinks <- c("beer","ale","wine","rum","brandy","gin","whisky","vodka","absinthe","tequila","cocktails")
plotWordStar(all_review$stars,all_review$text,wordList=AlcoholDrinks,mfrow = c(1,1))

# Cocktails,Absinthe, Vodka, Whiskey, Brandy, Rum, wine are good for restaurant.
# beer, rum, gin, tequila are fairly normal for business.


######################################################
#Test the existence of wifi's influence on ratings.
#First we should transform the names in Wifi
all_pubs$attributes.WiFi<-gsub("u'free'","'free'",all_pubs$attributes.WiFi)
all_pubs$attributes.WiFi<-gsub("None","'no'",all_pubs$attributes.WiFi)
all_pubs$attributes.WiFi<-gsub("u'no'","'no'",all_pubs$attributes.WiFi)
all_pubs$attributes.WiFi<-gsub("u'paid'","'paid'",all_pubs$attributes.WiFi)
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
Test$H_0[4]<-"High Ratings are not related with the existence of delivery"
Test$method[4]<-"chisq-test"
Test$p_value[4]<-chisq.test(x,correct = F)$p.value
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
Test$H_0[5]<-"High Ratings are not related with the influence of Goodfordancing"
Test$method[5]<-"chisq-test"
Test$p_value[5]<-chisq.test(x,correct = F)$p.value
#Refuse Ho, so Ratings are related with dancing.

############################################################################################################
low_all<-all_pubs$hours.Friday[all_pubs$stars<ave_star&is.na(all_pubs$hours.Friday)==0]
high_all<-all_pubs$hours.Friday[all_pubs$stars>ave_star&is.na(all_pubs$hours.Friday)==0]
low_time<-opentime(low_all)
high_time<-opentime(high_all)
wilcox.test(low_time,high_time,alternative="less")
Test$H_0[6]<-"High Ratings are not related with the opentime on Friday"
Test$method[6]<-"wilcox-test"
Test$p_value[6]<-wilcox.test(x,correct = F)$p.value
#Can't refuse Ho, so Ratings are not related with opentime on Friday.
write.csv(Test,"../output/test.csv")

par(mfrow=c(1,1))
hist(all_pubs$stars,main="pubs' stars",xlab="stars")
hist(all_review$stars,main="review' stars",xlab="stars")



#############################################################################################################
#Advice for the low stars pubs.
low_pub<-all_pubs[all_pubs$stars<ave_star,]
low_review<-subset(all_review, business_id %in% low_pub$business_id)
review_low_pubs <- left_join(low_review,low_pub,how="left",by="business_id")
low_Noun<-unnest_tokens(tibble(txt=low_review$text),word, txt)%>%anti_join(stop_words) %>%left_join(parts_of_speech) %>%filter(pos %in% c("Noun")) %>%pull(word)
frequency_of_low_noun <- table(low_Noun)
sort(frequency_of_low_noun,decreasing = TRUE)[1:100]
low_1<-sum(grepl("food",low_review$text))/length(low_review$text)
low_2<-sum(grepl("service",low_review$text))/length(low_review$text)
low_3<-sum(grepl("cheese",low_review$text))/length(low_review$text)
low_4<-sum(grepl("server",low_review$text))/length(low_review$text)
low_5<-sum(grepl("salad",low_review$text))/length(low_review$text)
low_6<-sum(grepl("staff",low_review$text))/length(low_review$text)
low_7<-sum(grepl("sauce",low_review$text))/length(low_review$text)
low_8<-sum(grepl("pizza",low_review$text))/length(low_review$text)
low_9<-sum(grepl("drink",low_review$text))/length(low_review$text)
low_10<-sum(grepl("beer",low_review$text))/length(low_review$text)
low_11<-sum(grepl("price",low_review$text))/length(low_review$text)
low_freq<-data.frame(topic=c("food","service","cheese","server","salad","staff","sauce","pizza","drink","beer","price"),frequence=c(low_1,low_2,low_3,low_4,low_5,low_6,low_7,low_8,low_9,low_10,low_11))
write.csv(low_freq,"../output/low_freq.csv")
###############################################################################################################
#Advice for the low stars pubs.
high_pub<-all_pubs[all_pubs$stars>=ave_star,]
high_tip<-subset(all_tip, business_id %in% high_pub$business_id)
tip_high_pubs <- left_join(high_tip,high_pub,how="left",by="business_id")
high_Noun<-unnest_tokens(tibble(txt=high_tip$text),word, txt)%>%anti_join(stop_words) %>%left_join(parts_of_speech) %>%filter(pos %in% c("Noun")) %>%pull(word)
frequency_of_high_noun <- table(high_Noun)
sort(frequency_of_high_noun,decreasing = TRUE)[1:100]
high_1<-sum(grepl("food",high_tip$text))/length(high_tip$text)
high_2<-sum(grepl("service",high_tip$text))/length(high_tip$text)
high_3<-sum(grepl("beer",high_tip$text))/length(high_tip$text)
high_4<-sum(grepl("menu",high_tip$text))/length(high_tip$text)
high_5<-sum(grepl("cheese",high_tip$text))/length(high_tip$text)
high_6<-sum(grepl("burger",high_tip$text))/length(high_tip$text)
high_7<-sum(grepl("atmosphere",high_tip$text))/length(high_tip$text)
high_8<-sum(grepl("fish",high_tip$text))/length(high_tip$text)
high_9<-sum(grepl("fries",high_tip$text))/length(high_tip$text)
high_10<-sum(grepl("brunch",high_tip$text))/length(high_tip$text)
high_freq<-data.frame(topic=c("food","service","beer","menu","cheese","burger","atmosphere","fish","fries","brunch"),frequence=c(high_1,high_2,high_3,high_4,high_5,high_6,high_7,high_8,high_9,high_10))
write.csv(high_freq,"../output/high_freq.csv")
#This is not obvious, so we try another way:use the 3 most useful tips made to the business in the shiny app.


################################## multiple regression ####################################
attributes <- c()
for (i in colnames(all_pubs)) {
  if(sum(is.na(all_pubs[,i]))/dim(all_pubs)[1]<0.3 & i!="attributes.Alcohol"){
    attributes <- c(attributes,i)
  }
}
attributes <- attributes[-c(1,2,4,20:26,27,28)]

RegData <- all_pubs[,c("business_id",attributes)]

sentiment <- data.frame(all_review$business_id,sentiment_review)
outcome <- sentiment %>% filter(sentiment_review>=0) %>% group_by(all_review.business_id)%>% summarise(positive=n())
colnames(outcome) <- c("business_id","positive_num")
RegData <- left_join(RegData,outcome,by="business_id")
RegData$positive_num[which(is.na(RegData$positive_num))] <- 0

Mon <- c()
a<- na.omit(all_pubs$hours.Monday)
for (i in 1:466) {
  if(is.na(all_pubs$hours.Monday[i])){
    Mon[i] <- mean(opentime(a))
  }else{
    Mon[i] <- opentime(all_pubs$hours.Monday[i])
  }
}
Tue <- c()
a<- na.omit(all_pubs$hours.Tuesday)
for (i in 1:466) {
  if(is.na(all_pubs$hours.Tuesday[i])){
    Tue[i] <- mean(opentime(a))
  }else{
    Tue[i] <- opentime(all_pubs$hours.Tuesday[i])
  }
}
Wed <- c()
a<- na.omit(all_pubs$hours.Wednesday)
for (i in 1:466) {
  if(is.na(all_pubs$hours.Wednesday[i])){
    Wed[i] <- mean(opentime(a))
  }else{
    Wed[i] <- opentime(all_pubs$hours.Wednesday[i])
  }
}
Thu <- c()
a<- na.omit(all_pubs$hours.Thursday)
for (i in 1:466) {
  if(is.na(all_pubs$hours.Thursday[i])){
    Thu[i] <- mean(opentime(a))
  }else{
    Thu[i] <- opentime(all_pubs$hours.Thursday[i])
  }
}
Fri <- c()
a<- na.omit(all_pubs$hours.Friday)
for (i in 1:466) {
  if(is.na(all_pubs$hours.Friday[i])){
    Fri[i] <- mean(opentime(a))
  }else{
    Fri[i] <- opentime(all_pubs$hours.Friday[i])
  }
}
Sat <- c()
a<- na.omit(all_pubs$hours.Saturday)
for (i in 1:466) {
  if(is.na(all_pubs$hours.Saturday[i])){
    Sat[i] <- mean(opentime(a))
  }else{
    Sat[i] <- opentime(all_pubs$hours.Saturday[i])
  }
}
Sun <- c()
a<- na.omit(all_pubs$hours.Sunday)
for (i in 1:466) {
  if(is.na(all_pubs$hours.Sunday[i])){
    Sun[i] <- mean(opentime(a))
  }else{
    Sun[i] <- opentime(all_pubs$hours.Sunday[i])
  }
}

RegData <- data.frame(RegData,Mon,Tue,Wed,Thu,Fri,Sat,Sun)  ###### open hour parts cleaning done

RegData$attributes.RestaurantsAttire<-gsub("u'","",RegData$attributes.RestaurantsAttire)
RegData$attributes.RestaurantsAttire<-gsub("'","",RegData$attributes.RestaurantsAttire)
RegData$attributes.NoiseLevel<-gsub("u'","",RegData$attributes.NoiseLevel)
RegData$attributes.NoiseLevel<-gsub("'","",RegData$attributes.NoiseLevel)
RegData$attributes.WiFi<-gsub("u'","",RegData$attributes.WiFi)
RegData$attributes.WiFi<-gsub("'","",RegData$attributes.WiFi)

for (i in attributes[-c(1,2,17)]) {
  as.factor(RegData[,i])
}

sum(na.omit(RegData$attributes.RestaurantsAttire)=="formal")
sum(na.omit(RegData$attributes.RestaurantsAttire)=="dressy")
sum(na.omit(RegData$attributes.RestaurantsAttire)=="casual")
for (i in 1:466) {
  if(is.na(RegData$attributes.RestaurantsAttire[i])){
    RegData$attributes.RestaurantsAttire[i] <- "casual"
  }
}

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
sum(na.omit(RegData$attributes.NoiseLevel)=="very_loud")
sum(na.omit(RegData$attributes.NoiseLevel)=="loud")
sum(na.omit(RegData$attributes.NoiseLevel)=="average")
sum(na.omit(RegData$attributes.NoiseLevel)=="quiet")
for (i in 1:466) {
  if(is.na(RegData$attributes.NoiseLevel[i])){
    RegData$attributes.NoiseLevel[i] <- "average"
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
sum(na.omit(RegData$attributes.RestaurantsPriceRange2)==1)
sum(na.omit(RegData$attributes.RestaurantsPriceRange2)==2)
sum(na.omit(RegData$attributes.RestaurantsPriceRange2)==3)
sum(na.omit(RegData$attributes.RestaurantsPriceRange2)==4)
for (i in 1:466) {
  if(is.na(RegData$attributes.RestaurantsPriceRange2[i])){
    RegData$attributes.RestaurantsPriceRange2[i] <- 2
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
sum(na.omit(RegData$attributes.WiFi)=="free")
sum(na.omit(RegData$attributes.WiFi)=="no")
sum(na.omit(RegData$attributes.WiFi)=="paid")
for (i in 1:466) {
  if(is.na(RegData$attributes.WiFi[i])){
    RegData$attributes.WiFi[i] <- "free"
  }
}


sum(is.na(RegData$attributes.BusinessParking.garage))
for (col in colnames(RegData)[19:38]) {
  if(sum(na.omit(RegData[,col])=="True")>sum(na.omit(RegData[,col])=="False")){
    for (i in 1:466) {
      if(is.na(RegData[i,col])){
        RegData[i,col] <- "True"
      }
    }
  }else{
    for (i in 1:466) {
      if(is.na(RegData[i,col])){
        RegData[i,col] <- "False"
      }
    }
  }
}

for (i in attributes[-c(1,2,17)]) {
  as.factor(RegData[,i])
}

RegData[,"Pos_Rev_rate"] <- RegData$positive_num/RegData$review_count
RegData <- RegData[,-c(1,39)]
#### regression
model <- step(lm(stars~.,data=RegData),direction = "both",k=2,trace = F)
summary(model)
### model diagnostics
plot(model)

RegData2 <- RegData[-c(113,30,13),]
model2 <- step(lm(stars~.,data=RegData2),direction = "both",k=2,trace = F)
summary(model2)
plot(model2)
id<-all_pubs$business_id
id<-id[-c(113,30,13)]
RegData2$id<-id
write.csv(RegData2,file = "../output/regdata.csv")
