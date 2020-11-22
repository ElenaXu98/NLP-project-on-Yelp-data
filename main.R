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
business<-jsonlite::stream_in(file("business_city.json"))
review<-jsonlite::stream_in(file("review_city.json"))
tip<-jsonlite::stream_in(file("tip_city.json"))
user<-jsonlite::stream_in(file("user_city.json"))
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
    all_pubs_pre<-data.frame(name=business$name[i],business_id=business$business_id[i],review_count=business$review_count[i],is.open=business$is_open[i],stars=business$stars[i],attributes=business$attributes[i,],hours=business$hours[i,],categories=business$categories[i])
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
#Then we will do the samething to the tip and user
all_tip<-subset(tip,business_id%in%all_pubs$business_id)

user_id_pre<-unique(all_review$user_id)
all_user<-subset(user,user_id%in%user_id_pre)
#In case that there are same comments, we decide to delete all the repeated data.
sq1 <- sqldf("select text, count(text ) count from all_review group by text having count > 1")
all_review<-subset(all_review,!(text%in%sq1$text))
all_user<-subset(all_user,user_id%in%all_review$user_id)
all_tip<-subset(all_tip,user_id%in%all_user$user_id)

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
      barplot(starsY,main=wordList[i],xlab="Stars",ylab="Word Freq")
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






ave_star<-quantile(all_pubs$stars)[3]
##############################################################################################################
#Test the influence of takeout
plotWordStar(all_pubs$stars,all_pubs$attributes.RestaurantsTakeOut,wordList=c("True","False"),mfrow = c(1,2))
low_all<-all_pubs$attributes.RestaurantsTakeOut[all_pubs$stars<ave_star]
high_all<-all_pubs$attributes.RestaurantsTakeOut[all_pubs$stars>ave_star]
high_takeout_yes<-sum(high_all=="True",na.rm = T)
high_takeout_no<-sum(high_all=="False",na.rm = T)
low_takeout_yes<-sum(low_all=="True",na.rm = T)
low_takeout_no<-sum(low_all=="False",na.rm = T)
x<-c(high_takeout_yes,high_takeout_no,low_takeout_yes,low_takeout_no)
dim(x)<- c(2,2)
chisq.test(x,correct = F)
#Refuse Ho, so Ratings are related with takout
##############################################################################################################
#Test the influence of GoodforGroups
plotWordStar(all_pubs$stars,all_pubs$attributes.RestaurantsGoodForGroups,wordList=c("True","False"),mfrow = c(1,2))
low_all<-all_pubs$attributes.RestaurantsGoodForGroups[all_pubs$stars<ave_star]
high_all<-all_pubs$attributes.RestaurantsGoodForGroups[all_pubs$stars>ave_star]
high_groups_yes<-sum(high_all=="True",na.rm = T)
high_groups_no<-sum(high_all=="False",na.rm = T)
low_groups_yes<-sum(low_all=="True",na.rm = T)
low_groups_no<-sum(low_all=="False",na.rm = T)
x<-c(high_groups_yes,high_groups_no,low_groups_yes,low_groups_no)
dim(x)<- c(2,2)
chisq.test(x,correct = F)
#Refuse Ho, so Ratings are related with Goodforgroups
##############################################################################################################
#Test the existence of TV
plotWordStar(all_pubs$stars,all_pubs$attributes.HasTV,wordList=c("True","False"),mfrow = c(1,2))
low_all<-all_pubs$attributes.HasTV[all_pubs$stars<ave_star]
high_all<-all_pubs$attributes.HasTV[all_pubs$stars>ave_star]
high_tv_yes<-sum(high_all=="True",na.rm = T)
high_tv_no<-sum(high_all=="False",na.rm = T)
low_tv_yes<-sum(low_all=="True",na.rm = T)
low_tv_no<-sum(low_all=="False",na.rm = T)
x<-c(high_tv_yes,high_tv_no,low_tv_yes,low_tv_no)
dim(x)<- c(2,2)
chisq.test(x,correct = F)
#Refuse Ho, so Ratings are related with tv
#############################################################################################################
word<-c("love","yummy","great","good","nice","wonderful", "amazing", "ordinary", "hate", "bad","worst","disappoint", "awful", "terrific", "decent", "average")
plotWordStar(all_review$stars,all_review$text,wordList=word,mfrow = c(4,4))
#here I use "love" to check whether it can influence the rate
key_word<-word[8]
all_stars_with_key<-all_review$stars[which(grepl(key_word,all_review$text))]
all_stars_without_key<-all_review$stars[-which(grepl(key_word,all_review$text))]
wilcox.test(all_stars_with_key,all_stars_without_key,alternative="less")
######################################################
#Test the existence of wifi's influence on ratings.
plotWordStar(all_pubs$stars,all_pubs$attributes.WiFi,wordList=c("u'no'","u'free'","'no'","'free'","u'paid'","'paid'","None" ),mfrow = c(2,4))
low_all<-all_pubs$attributes.WiFi[all_pubs$stars<ave_star]
high_all<-all_pubs$attributes.WiFi[all_pubs$stars>ave_star]
wifi_word_yes<-c("u'free'","'free'","u'paid'","'paid'")
wifi_word_no<-c("u'no'","'no'","None")
high_wifi_yes<-sum(high_all%in% wifi_word_yes,na.rm = T)
high_wifi_no<-sum(high_all%in% wifi_word_no,na.rm = T)
low_wifi_yes<-sum(low_all%in% wifi_word_yes,na.rm = T)
low_wifi_no<-sum(low_all%in% wifi_word_no,na.rm = T)
x<-c(high_wifi_yes,high_wifi_no,low_wifi_yes,low_wifi_no)
dim(x)<- c(2,2)
chisq.test(x,correct = F)
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
#Refuse Ho, so Ratings are related with dancing.
############################################################################################################
low_all<-all_pubs$hours.Friday[all_pubs$stars<ave_star&is.na(all_pubs$hours.Friday)==0]
high_all<-all_pubs$hours.Friday[all_pubs$stars>ave_star&is.na(all_pubs$hours.Friday)==0]
low_time<-opentime(low_all)
high_time<-opentime(high_all)
wilcox.test(low_time,high_time,alternative="less")
#Can't refuse Ho, so Ratings are not related with opentime on Friday.