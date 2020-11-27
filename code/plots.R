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

#Test the influence of Good for Groups
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

################ how different types of beer related to stars ######################
AlcoholDrinks <- c("beer","Ale","wine","Rum","rum","Brandy","Gin","gin","Whisky","whisky","Whiskey","whiskey","Texas whiskey","Vodka","Absinthe","Tequila","cocktails","Cocktails")
plotWordStar(all_review$stars,all_review$text,wordList=AlcoholDrinks,mfrow = c(1,2))

# Cocktails,Absinthe, Vodka, Whiskey, Brandy, Rum, wine are good for restaurant.
# beer, rum, gin, tequila are fairly normal for business.


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
index2 <- all_pubs$state == 'OH'
length(all_pubs$state)
sum(is.na(all_pubs$state))
plotWordStar(all_review$stars[all_review$business_id==all_pubs$business_id & all_pubs$state == 'OH'],all_review$text[all_review$business_id==all_pubs$business_id & all_pubs$state == 'OH'],wordList=atomsphere_words,mfrow = c(1,2))









