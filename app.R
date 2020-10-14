library(shiny)
library(plyr)
library(ggplot2)
library(RColorBrewer)
library(RMariaDB)
library(fmsb)
library(shinyWidgets)
library(cluster)
library(stringr)
library(maps)
library(reshape2)
library(caret)
library(e1071)
library(randomForest)
library(raster)
library(class)
library(dplyr)
library(DescTools)

# set memory limits
#options(java.parameters = "-Xmx64048m") # 64048 is 64 GB

# Connect to a MariaDB version of a MySQL database

#con <- dbConnect(RMariaDB::MariaDB(), host="datamine.rcac.purdue.edu", port=3306
 #                , dbname="kag_beer", user="kaggle_user", password="kaggle_pass")

# query
#beers <- dbGetQuery(con, "select * from beers")
#breweries <- dbGetQuery(con, "select * from breweries")
#beers<-read.csv("beers.csv")
#breweries<-read.csv("breweries.csv")

#beers <- distinct(beers)
#breweries <- distinct(breweries)
#beers<-dplyr::select(beers,-c("Unnamed..0","index","ounces","X"))
#breweries<-dplyr::select(breweries,-c("Unnamed..0","X"))
#beers<-left_join(beers,breweries,by=c("brewery_id"="index"))
#beers<-dplyr::select(beers,-c("brewery_id"))
#names(beers)<-c("ABV","IBU","id","Name","Style","Brewery","City","State")
#beers$ABV<-beers$ABV*100


#beers$Category<-"Ale"
#beers$Style<-ifelse(is.na(beers$Style),"Other",as.character(beers$Style))
#beers$Category<-ifelse(str_detect(beers$Style,regex("Lager|Pils|Pilsener|Pilsner",ignore_case = TRUE,dotall=TRUE)),"Lager",as.character(beers$Category))
#beers$Category<-ifelse(str_detect(beers$Style,regex(c("Other|Cider|Shandy|Fruit|Spiced|Saison"),ignore_case = TRUE,dotall=TRUE)),"Other",as.character(beers$Category))
#beers$Category<-ifelse(is.na(beers$Category),"Other",as.character(beers$Category))
#beers$Category<-ifelse(str_detect(beers$Style,regex(c("Berliner|Weissbier|Doppel|Dunkel|lsch|Altbier|Gose|Witbier|Oktoberfest|bier|Tripel|Hefeweizen|Radler|Belgian|Bier|Maibock|Dubbel|Quad"),ignore_case = TRUE,dotall=TRUE)),"Traditional Germanic",as.character(beers$Category))


#d<-beers[,c("id","IBU","ABV","Brewery","State","Style","Category")]

#dummies <- dummyVars(IBU ~ ., data = d)
#ex <- data.frame(predict(dummies, newdata = d))
#names(ex) <- gsub("\\.", "", names(ex))
#d <- cbind(d$IBU, ex)
#names(d)[1] <- "IBU"
#nzv <- nearZeroVar(d, saveMetrics = TRUE)
#d <- d[, c(TRUE,!nzv$zeroVar[3:ncol(d)])]
#preProcValues <- preProcess(d[,3:ncol(d)], method = c("range"))
#d <- predict(preProcValues, d)
#train=d[which(!is.na(d["IBU"])),]
#test=d[which(is.na(d["IBU"])),]
#ctrl <- trainControl(method="cv",number=5,classProbs = F, summaryFunction = defaultSummary, allowParallel=T)
#m1<- train(IBU ~ . - id, data = train, method = "lm", trControl = ctrl,na.action = na.pass)
#preds <-data.frame(predict(m1, newdata=test,na.action=na.pass))
#preds<-mutate(preds,"id"=as.numeric(rownames(preds)))
#beers[which(is.na(beers$IBU)),"IBU"]<-ifelse(as.integer(preds$predict.m1..newdata...test.)>0,as.integer(preds$predict.m1..newdata...test.),beers$IBU)

#d<-beers[,c("id","ABV","IBU","Brewery","State","Style","Category")]

#dummies <- dummyVars(ABV ~ ., data = d)
#ex <- data.frame(predict(dummies, newdata = d))
#names(ex) <- gsub("\\.", "", names(ex))
#d <- cbind(d$ABV, ex)
#names(d)[1] <- "ABV"
#nzv <- nearZeroVar(d, saveMetrics = TRUE)
#d <- d[, c(TRUE,!nzv$zeroVar[3:ncol(d)])]
#preProcValues <- preProcess(d[,3:ncol(d)], method = c("range"))
#d <- predict(preProcValues, d)
#train=d[which(!is.na(d["ABV"])),]
#test=d[which(is.na(d["ABV"])),]
#ctrl <- trainControl(method="cv",number=5,classProbs = F, summaryFunction = defaultSummary, allowParallel=T)
#m1<- train(ABV ~ . - id, data = train, method = "lm", trControl = ctrl,na.action = na.pass)
#preds <-data.frame(predict(m1, newdata=test,na.action=na.pass))
#preds<-mutate(preds,"id"=as.numeric(rownames(preds)))
#beers[which(is.na(beers$ABV)),"ABV"]<-ifelse(as.integer(preds$predict.m1..newdata...test.)>0,as.integer(preds$predict.m1..newdata...test.),beers$ABV)
#beers<-dplyr::select(beers,c(-"id"))
#write.table(beers,"beersComplete.csv",sep=",")
#write.table(breweries,"breweriesComplete.csv",sep=",")

beers<-read.csv("beersComplete.csv")
beers2<-beers
# disconnect from db
#dbDisconnect(con)
#################################################

# pull in the training data into a data.frame called tr
# it 'tr' in R

# disconnect from db
#dbDisconnect(con)

###################### UI

ui <- fluidPage(
  tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #DD4529;  color:white; }
  ")),
  #setBackgroundColor(color=c("white","#ffaa80"),gradient=c("linear"),direction="bottom"),
  title="Beer FindeR",

  fluidRow(titlePanel( div(column(width = 7, offset=3,tags$img(src = "Logo2.PNG", height="100%", width="100%",align="center"))))),
  
  headerPanel(''),
  
  ############ Sidebar
  sidebarPanel(
    width=3,
    setSliderColor(c("#DD4529","#DD4529","#DD4529"),c(1,2,3)),
    pickerInput('State', 'State', sort(as.character(unique(beers$State))),multiple=TRUE,options = list(`actions-box` = TRUE),selected=as.character(beers$State[1:length(beers$State)]))
    #selectInput('BeerName2','Choose your second favorite beer',sort(as.character(beers$Name)),multiple=FALSE,selected=100)
    ),
  ########### Main Tabs 
  mainPanel(
    tabsetPanel(type = "tabs",
        #tags$style(type='text/css',"color:black"),
        tabPanel("Start here: Current favorite beers",
        HTML("<h4 style='color:#DD4529'>","Choose your favorite beers below, then go to the next tab:","</h4>"),
        selectInput('BeerName1',label=NULL,choices=sort(as.character(sort(unique(beers2$Name)))),multiple=TRUE),
        tableOutput("PreferredBeerTable")
        ),
        
        tabPanel("Your next favorite beers",
                 HTML("<h4 style='color:#DD4529'>","Based on your choices, you should try these:","</h4>"),
                 dataTableOutput("preferred")),
          
        tabPanel("Your next trip",
                 HTML("<h4 style='color:#DD4529'>","This is where you'll find beers you like:","</h4>"),
                 plotOutput("plotMap")),
        
        tabPanel("I just want to get DRUNK!",
                 HTML("<h4 style='color:#DD4529'>","If you want the most ABV, try these:","</h4>"),
                 dataTableOutput("Drunk")
        ),
       
        tabPanel("Beer Groups",
                 HTML("<h4 style='color:#DD4529'>","Find your favorite group of beer:","</h4>"),
                 #fluidRow(column(6,textInput("Centers","Centers",value=3)),
                 #column(6,textInput("MaxIterations", "Max. Iterations",value=10))),
                 fluidRow(plotOutput("ClusterPlot")),
                 HTML("<h5>","Based on the beers you like, your favorite group is:","</h4>"),
                 textOutput("clusterFavoriteText"),
                 HTML("<h5>","Try other beers within that group:","</h4>"),
                 #fluidRow(plotOutput("ElbowPlot")),
                 fluidRow(dataTableOutput("ClusterTable"))),
        
      tabPanel("Graphic Summary",
      HTML("<h4 style='color:#DD4529'>","Explore IBU and ABV below:","</h4>"),
      plotOutput("plot1"),
      plotOutput("plot3"),
      fluidRow(plotOutput("plot2")),
      fluidRow(plotOutput("plot4"))
      ),
      
      tabPanel("Our Data",
               HTML("<h4 style='color:#DD4529'>","Explore all beers' data below:","</h4>"),
               fluidRow(column(6,sliderInput('IBUMin','IBU',value=c(1,140),min=0,max=140)),
               column(6,sliderInput('ABVMax',"Alcoholic Volume", value=c(1,15), min = 1, max = 15))),
               #sliderInput('IBUMin','IBU',value=c(1,140),min=0,max=140),
               #sliderInput('ABVMax',"Alcoholic Volume", value=c(1,15), min = 1, max = 15),
               
               fluidRow(column(6,pickerInput('Category', 'Beer Category', sort(as.character(unique(beers$Category))),multiple=TRUE,options = list(`actions-box` = TRUE),
                                             selected=as.character(beers$Category[1:length(beers$Category)]))),
                        column(6,pickerInput('Style', 'Beer Style', sort(as.character(unique(beers$Style))),multiple=TRUE,options = list(`actions-box` = TRUE),selected=as.character(beers$Style[1:length(beers$Style)])))),
               
               
               #pickerInput('Category', 'Beer Category', sort(as.character(unique(beers$Category))),multiple=TRUE,options = list(`actions-box` = TRUE),selected=as.character(beers$Category[1:length(beers$Category)])),
               #pickerInput('Style', 'Beer Style', sort(as.character(unique(beers$Style))),multiple=TRUE,options = list(`actions-box` = TRUE),selected=as.character(beers$Style[1:length(beers$Style)])),
               #sliderInput('IBUMax','Maximum IBU Per Serving',value=140,min=0,max=140),
               pickerInput('Brewery', 'Choose your brewer', sort(as.character(unique(beers$Brewery))),multiple=TRUE,options = list(`actions-box` = TRUE),selected=as.character(beers$Brewery[1:length(beers$Brewery)])),
               dataTableOutput("table1"),
               actionButton("ExportButton", "Export table data (csv)"),
               textInput("FileName","Choose a name for export file (optional)"),
               textOutput("text1")
      )
)))

################### Server
      
server <- function(input, output,session) {
  
  ################ Reactive data
  beersData <- reactive({
    beers<- beers[which(beers$Category %in% input$Category & beers$Style %in% input$Style & beers$IBU>=input$IBUMin[1] & beers$IBU<=input$IBUMin[2] & beers$ABV>=input$ABVMax[1] & beers$ABV<=input$ABVMax[2] & beers$Brewery %in% input$Brewery & beers$State %in% input$State),]
    })
  
  beersDataNoCat <- reactive({
    beers<- beers[which(beers$IBU>=input$IBUMin[1] & beers$IBU<=input$IBUMin[2] & beers$ABV<=input$ABVMax & beers$Brewery %in% input$Brewery & beers$State %in% input$State),]
  })
  
  beersDataNoStyle <- reactive({
    beers<- beers[which(beers$Category %in% input$Category & beers$IBU>=input$IBUMin[1] & beers$IBU<=input$IBUMin[2] & beers$ABV>=input$ABVMax[1] & beers$ABV<=input$ABVMax[2] & beers$Brewery %in% input$Brewery & beers$State %in% input$State),]
  })
  
  preferredBeers <- reactive({
    perfectbeer
  })
  
  beersDataNoBrewer <- reactive({
    beers<- beers[which(beers$IBU>=input$IBUMin[1] & beers$IBU<=input$IBUMin[2] & beers$ABV<=input$ABVMax  & beers$State %in% input$State),]
  })
  
  clusterData <-reactive({
    km<-kmeans(beersData()[,c("IBU","ABV")],centers=7,iter.max=15)
    bc<-mutate(beersData(),"Group"=km$cluster)
    bc
  })
  
  ################### Update inputs based on selections 
  
  observeEvent(input$Y,{
    if (input$Y=="Category") {
      updateSelectInput(session,"ModelType",choices=c("rf","rpart"))
    }
    
    else if (input$Y=="Style") {
      updateSelectInput(session,"ModelType",choices=c("rpart"))
    }
    
    else if (input$Y=="IBU"|input$Y=="ABV"){
      updateSelectInput(session,"ModelType",choices=c("lm","glm"))
    }})
  
  observeEvent(input$BeerName1,{
    beersinput <- beers[which(beers$Name %in% input$BeerName1),]
    perfectbeer <-cbind(mean(beersinput$ABV), mean(beersinput$IBU))
    distances=pointDistance(perfectbeer,beers[which(!(beers$Name %in% input$BeerName1)),c("ABV","IBU")], lonlat =FALSE)
    ranked=sort(distances, index.return =TRUE)
    preferred <-beers[ranked$ix[1:5],]
  })
  
  
  observeEvent(input$State,{
    if (!is.null(beersData())) {
      updatePickerInput(session,"Brewery",choices=sort(unique(as.character(beersDataNoBrewer()$Brewery))),selected=as.character(beersDataNoBrewer()$Brewery[1:length(beersDataNoBrewer()$Brewery)])) }
  })
  
  observeEvent(input$Brewery,{
    if (!is.null(beersData())) {
      updatePickerInput(session,"Category",choices=sort(unique(as.character(beersDataNoCat()$Category))),selected=as.character(beersDataNoCat()$Category[1:length(beersDataNoCat()$Category)])) }
  })
  
  observeEvent(input$Category,{
    if (!is.null(beersData())) {
      updatePickerInput(session,"Style",choices=sort(unique(as.character(beersDataNoStyle()$Style))),selected=as.character(beersDataNoStyle()$Style[1:length(beersDataNoStyle()$Style)])) 
      updateSelectInput(session,"BeerName1",choices=sort(unique(as.character(beersDataNoStyle()$Name)))) }
  })
  

  ################ Table Outputs 
  output$PreferredBeerTable <- renderTable ({
    beers[which(beers$Name %in% input$BeerName1),]
  })
  
  output$R2<- renderTable({
    if (input$ModelType == 'glm'){
      Out<-postResample(caretData()[9],caretData()[input$Y])
      dfOut<-data.frame("Metric"=c("RMSE","RSquared","MAE"),"Value"=Out)
      }})
  
  output$table1 <- renderDataTable({
    data.frame(beersData())
    })
  
  output$clusterFavoriteText <- renderText({
    d<-clusterData()[which(clusterData()$Name %in% input$BeerName1),"Group"]
    d<-data.frame(table(d))
    n<-as.matrix(d[order(-d$Freq),])
    m<-n[1,1]
    m
  })
   
  output$Drunk <-renderDataTable({
    head(arrange(beersData(),desc(ABV)),3)
  },options = list(lengthChange = FALSE))
  
  output$preferred <- renderDataTable({
    beersinput <- beers[which(beers$Name %in% input$BeerName1),]
    perfectbeer <-cbind(mean(beersinput$ABV), mean(beersinput$IBU))
    distances=pointDistance(perfectbeer,beersData()[,c("ABV","IBU")], lonlat =FALSE)
    ranked=sort(distances, index.return =TRUE)
    preferred <-beersData()[ranked$ix[1:5],]
    data.frame(preferred)
  },options = list(lengthChange = FALSE))
  
  output$tableRanked1 <- renderDataTable({
    head(arrange(beersData(),desc(IBU)),10)
  },options = list(lengthChange = FALSE))
  
  output$tableRanked2 <- renderDataTable({
    head(arrange(beersData(),desc(ABV)),10)
  },options = list(lengthChange = FALSE))
  
  output$ClusterTable<-renderDataTable(
    clusterData())
  
  output$CaretTable<- renderDataTable({
    data.frame(caretData())
  })
  
  ############## Plot Outputs

  output$plot1 <- renderPlot(
    ggplot(beersData(),aes(x=IBU,fill=..count..))+geom_bar()+labs(title="IBU Distribution")+xlab("IBU")+ylab("Count")+scale_fill_gradient(low="#FDED56",high="#7C1502")+theme(panel.background = element_blank())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  )
  output$plot2 <- renderPlot(
    ggplot(beersData(),aes(x=ABV,fill=..count..))+geom_bar()+labs(title="ABV Distribution")+xlab("ABV")+ylab("Count")+scale_fill_gradient(low="#FDED56",high="#7C1502")+theme(panel.background = element_blank())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  )
  output$plot3 <- renderPlot(
    ggplot(beersData(),aes(y=IBU,x=Category, fill=Category))+geom_boxplot()+labs(title="IBU by Style of Beer")+ylab("IBU")+scale_fill_brewer(palette="YlOrBr")+theme(panel.background = element_blank())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  )
  output$plot4 <- renderPlot(
    ggplot(beersData(),aes(y=ABV,x=Category, fill=Category))+geom_boxplot()+labs(title="ABV by Style of Beer")+ylab("ABV")+scale_fill_brewer(palette="YlOrBr")+theme(panel.background = element_blank())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  )
  output$plotRanked1 <- renderPlot(
    ggplot(head(arrange(beersData(),desc(IBU)),10),aes(x=reorder(Name,-IBU),y=IBU,fill=Style))+geom_col()+labs(title="Top 10 IBU")+xlab("Name")+ylab("IBU")+scale_fill_brewer(10,palette="YlOrBr")+theme(panel.background = element_blank())+theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), panel.grid.minor = element_blank())
  )
  output$plotRanked2 <- renderPlot(
    ggplot(head(arrange(beersData(),desc(ABV)),10),aes(x=reorder(Name,-ABV),y=ABV,fill=Style))+geom_col()+labs(title="Top 10 ABV")+xlab("Name")+ylab("ABV")+scale_fill_brewer(10,palette="YlOrBr")+theme(panel.background = element_blank())+theme(panel.grid.major = element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), panel.grid.minor = element_blank())
  )
  output$plotMap <- renderPlot ({
    MainStates <- map_data("state")
    MainStates$state<-state.abb[match(MainStates$region,tolower(state.name))]
    
    beersinput <- beers[which(beers$Name %in% input$BeerName1),]
    perfectbeer <-cbind(mean(beersinput$ABV), mean(beersinput$IBU))
    distances=pointDistance(perfectbeer,beersData()[,c("ABV","IBU")], lonlat =FALSE)
    ranked=sort(distances, index.return =TRUE)
    preferred <-beersData()[ranked$ix[1:150],]
    
    t<-data.frame(table(preferred$State))
    names(t)<-c("state","values")
    t$values<- (t$values-min(t$values))*100/max(t$values)
    t$state<-trimws(as.character(t$state))
    MergedStates <- inner_join(MainStates, t, by = "state")
    ggplot()+geom_polygon( data=MergedStates, aes(x=long, y=lat, group=group, fill = values), color="white", size = 0.2) + labs(fill="Score") + scale_fill_gradient(low="#FDED56",high="#7C1502") + theme(panel.background = element_blank())+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
    })

  output$ClusterPlot<- renderPlot(
    ggplot(clusterData(),aes(x=IBU,y=ABV,fill=Group,color=Group))+geom_point()+scale_fill_gradient(low="#FDED56",high="#7C1502")+scale_color_gradient(low="#FDED56",high="#7C1502")+ theme(panel.background = element_blank())+theme(panel.grid.major = element_blank(), legend.title=element_blank(),panel.grid.minor = element_blank(),axis.title.x = element_blank(),axis.title.y = element_blank())
  )
  
  output$ElbowPlot<- renderPlot({
    dist <- dist(beersData()[,c("IBU","ABV")], method="euclidean")
    sil4 <- silhouette(clusterData()[,"Clusters"], dist)
    plot(sil4, col=c("yellow","brown"), main="Silhouette plot",border=NA)
  })
  

  output$BeerPlot <- renderDataTable({
    m<-melt(beers[which(beers$Name==input$BeerName),c("Name","IBU","ABV")], id.vars="Name")
    ggplot(m,aes(x=variable,y=value,fill=variable))+geom_col()
  })

  ############# Buttons
  observeEvent(input$ExportButton, {
    if (is.null(input$FileName)){
      write.csv(tableSelectedData(),"ExportedTable.csv")
      }
    else {
      write.csv(tableSelectedData(),paste(input$FileName,".csv"))
      }
    })
  
}

shinyApp(ui = ui, server = server)