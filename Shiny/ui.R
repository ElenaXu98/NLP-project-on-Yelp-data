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
ui<-fluidPage(
  
  titlePanel("Pubs Information"),
  sidebarPanel(
    splitLayout(textInput("n1",label=h5("Pub id"),width = 200),
                selectInput("n2",label=h5("AcceptsCreditCards"),choices = c("Yes","No","Not decided"),width = 120)),
    
    splitLayout(selectInput("n3",label=h5("Reservations"),choices = c("Yes","No","Not decided"),width = 120),
                selectInput("n4",label=h5("GoodForGroups"),choices = c("Yes","No","Not decided"),width = 120)),
    
    splitLayout(selectInput("n5",label=h5("Has TV"),choices = c("Yes","No","Not decided"),width = 120),
                selectInput("n6",label=h5("Has Delivery"),choices = c("Yes","No","Not decided"),width = 120)),
    
    splitLayout(selectInput("n7",label=h5("Has WIFI"),choices = c("Yes","No","Not decided","Paid"),width = 120),
                selectInput("n8",label=h5("Has parking lot"),choices = c("Yes","No","Not decided"),width = 120)),
    
    splitLayout(selectInput("n9",label=h5("Has parking valet"),choices = c("Yes","No","Not decided"),width = 120),
                selectInput("n10",label=h5("Meal in latenight"),choices = c("Yes","No","Not decided"),width = 120)),
    
    splitLayout(selectInput("n11",label=h5("divey Ambience"),choices = c("Yes","No","Not decided"),width = 120),
                numericInput("n12",label = h5("Open time on Monday"),value=12,min=0,max=24,width = 80)),
    
    splitLayout(numericInput("n13",label = h5("Open time on Saturday"),value=12,min=0,max=24,width = 80),
                selectInput("n14",label=h5("trendy Ambience"),choices = c("Yes","No","Not decided"),width = 120)),
  width = 3),
  mainPanel(
    hr(),
    verbatimTextOutput("value1"),
    verbatimTextOutput("value2"),
    verbatimTextOutput("value3"),
    width = 9
  )
)
