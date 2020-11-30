if (!require("shiny")) {
  install.packages("shiny")
  stopifnot(require("shiny"))
}
if (!require("rjson")) {
  install.packages("rjson")
  stopifnot(require("rjson"))
}
if (!require("jsonlite")) {
  install.packages("jsonlite")
  stopifnot(require("jsonlite"))
}
if (!require("fields")) {
  install.packages("fields")
  stopifnot(require("fields"))
}
business<-jsonlite::stream_in(file("business_city.json"))  
f<-function(x)
{
 
  alcohol<-business$attributes$Alcohol[business$business_id==x[1]]
  state<-business$state[business$business_id==x[1]]
   if(length(alcohol)!=0&length(state)!=0){
  n=0
  now_value=0
  future_value=0
  if(is.na(alcohol)==0 &is.na(state)==0){
    if(state=="WI"){
      if(alcohol=="'full_bar'"|alcohol=="u'full_bar'"){
        RegData<-read.csv("regdata.csv")
        RegData2<-RegData[,-dim(RegData)[2]]
        model_final <- step(lm(stars~.,data=RegData2),direction = "both",k=2,trace = F)
        VI<-rep(1,24)
        VI[2]<-business$review_count[business$business_id==x[1]]
        VI[3]<-sum(x[2]=="Not decided")
        VI[4]<-sum(x[2]=="Yes")
        VI[5]<-sum(x[3]=="Not decided")
        VI[6]<-sum(x[3]=="Yes")
        VI[7]<-sum(x[4]=="Yes")
        VI[8]<-sum(x[5]=="Yes")
        VI[9]<-sum(x[6]=="Not decided")
        VI[10]<-sum(x[6]=="Yes")
        VI[11]<-sum(x[7]=="No")
        VI[12]<-sum(x[7]=="Paid")
        VI[13]<-sum(x[8]=="Not decided")
        VI[14]<-sum(x[8]=="Yes")
        VI[15]<-sum(x[9]=="Yes")
        VI[16]<-sum(x[10]=="Not decided")
        VI[17]<-sum(x[10]=="Yes")
        VI[18]<-sum(x[11]=="Not decided")
        VI[19]<-sum(x[11]=="Yes")
        VI[20]<-x[12]
        VI[21]<-x[13]
        VI[22]<-1
        VI[23]<-sum(x[14]=="Not decided")
        VI[24]<-sum(x[14]=="Yes")
        now_value<-business$stars[business$business_id==x[1]]
        future_value<-sum(as.numeric(model_final$coefficients)*as.numeric(VI))
        if(future_value>5){future_value<-5}
        if(business$stars[business$business_id==x[1]]>=3.5){n="No"}else{n="Yes"}
      }else{n=0}
    }else{n=0}
  }else{n=0}
  
  x<-c(n,now_value,future_value)
  if(x[1]!=0){names(x)<-c("Less than average stars","Now stars","Stars after settings")}else{x<-"This is not a pub in WI or it doesn't show the characteristics of pub or its state"}
  }else{x<-"This isn't a vaild id for pubs, please input the id of the pub"}
  return(x)
}
g<-function(x){  
n=0  
alcohol<-business$attributes$Alcohol[business$business_id==x[1]]
state<-business$state[business$business_id==x[1]]
if(length(alcohol)!=0&length(state)!=0){
  if(is.na(alcohol)==0 &is.na(state)==0){
    if(state=="WI"){
      if(alcohol=="'full_bar'"|alcohol=="u'full_bar'"){
        if(business$stars[business$business_id==x[1]]>=3.5){n="Yes"}else{n="No"}
      }
    }
  }
}
  if(n=="No"){
x<-read.csv("low_freq.csv")
print(x)
print("You are below the average stars.But never mind, here are some advice to help you!")
print("(1)53% people gave low stars mentioned 'food', so pub should pay high attention to it, especially cheese, salad, sauce, pizza.")
print("(2)36% people mentioned 'service' in the low ratings, so pub should train staff well and make the server clean.")
print("(3)21% people mentioned 'drink' especially beer, so pub should provide fresh beer with high quality.")
print("(4)17% people mentioned 'price', pub should change the price according to the time to apply for customers.")
}
else if(n=="Yes"){
x<-read.csv("high_freq.csv")
print(x)
print("You are good, here are some tips to help you better.")
print("(1)14% people mentioned 'food' and 3% mentioned 'menu', so pub can give more tastes on these food, like cheese, burger, fish, fries, beer.")
print("(2)7% people mentioned 'service', so pub should keep encouraging staff and provide more kind service.")
print("(3)3% people mentioned 'atmosphere', so pub can do some survey to get more ideas about what customers love.")
print("(4)2% people mentioned 'brunch', pub can try to open in the afternoon or noon to provide brunch.")


  }

}




server<-function(input,output){
  
   output$value1 = renderPrint({
     g(input$n1)
  })
   output$value2 = renderPrint({
     f(c(input$n1,input$n2,input$n3,input$n4,input$n5,input$n6,input$n7,input$n8,input$n9,input$n10,input$n11,input$n12,input$n13,input$n14))
   })
   output$value3 = renderPrint({
     print("if p-value <0.05, we reject H_0")
     read.csv("test.csv")
   })
}
