##ADSM
##Task 4 - Sentiment Analysis
library(tidyverse)

##DATASET PREPARATION

rm(list=ls())

#set working directory
setwd("C:\\Users\\smann\\OneDrive\\University\\Data Science\\ASDM\\Project\\Sentiment analysis")

#import dataset
df <- read.csv("tourist_accommodation_reviews.csv")

head(df)
tail(df)
str(df)
names(df)

colSums(is.na(df))
#no missing data


#Data exploration
df %>% count(Location)
n_distinct(df$Location)
n_distinct(df$ID)
n_distinct(df$Hotel.Restaurant.name)


#check for duplicated data
table(duplicated(df))
table(duplicated(df$ID))
df <- df %>% distinct()
table(duplicated(df))
#remove duplicate rows
df %>% distinct(Hotel.Restaurant.name, .keep_all=TRUE) %>% 
  select(Hotel.Restaurant.name, Location) %>% count(Location)

#want to work out number of rows per hotel - similar
prop_df <- df %>% count(Hotel.Restaurant.name) %>% as.data.frame()
prop_df <- merge(prop_df, df, by="Hotel.Restaurant.name")
prop_df <- prop_df %>% distinct(Hotel.Restaurant.name, .keep_all=TRUE) %>% select(Hotel.Restaurant.name, n, Location)
prop_df <- prop_df[order(-prop_df$n), ]

#convert all text to lowercase for consistency
df <- mutate_each(df, funs(tolower))
#location has whitespace at beginning
df$Location <- trimws(df$Location)


# PATONG VS CHOENG
#randomly sample 15 Patong venues
set.seed(123)  
patong_unique <- df %>% filter(Location=="patong") %>% distinct(Hotel.Restaurant.name) %>% 
  as.data.frame() %>% sample_n(15)
patong_sample <- merge(patong_unique, df, by="Hotel.Restaurant.name", all=FALSE) 
#check for duplicates
table(duplicated(patong_sample))
n_distinct(patong_sample$Hotel.Restaurant.name)
n_distinct(patong_sample$Location)

#randomly sample 15 Choeng Thale venues
set.seed(234)  
choeng_unique <- df %>% filter(Location=="choeng thale") %>% distinct(Hotel.Restaurant.name) %>% 
  as.data.frame() %>% sample_n(15)
choeng_sample <- merge(choeng_unique, df, by="Hotel.Restaurant.name", all=FALSE) 
#check for duplicates
table(duplicated(choeng_sample))
n_distinct(choeng_sample$Hotel.Restaurant.name)
n_distinct(choeng_sample$Location)


#combine to create sample 30 hotels dataset
sample_df <- rbind(patong_sample, choeng_sample)
n_distinct(sample_df$Hotel.Restaurant.name)
table(duplicated(sample_df))
#output dataset
write.csv(sample_df, "hotel-sample.csv",row.names = FALSE)



###TEXT ANALYSIS
library(tm)
library(wordcloud)
library(ggplot2)
library(tidyverse)

#set working directory
setwd("C:\\Users\\smann\\OneDrive\\University\\Data Science\\ASDM\\Project\\Sentiment analysis")


rm(list=ls())
#import sample dataset
df <- read.csv("hotel-sample.csv")


#subset by location
patong <- subset(df, Location=="patong")
choeng <- subset(df, Location=="choeng thale")

#combine reviews for hotels into one row
patong <- patong %>% group_by(Hotel.Restaurant.name) %>% 
  summarise(Review = str_c(Review, collapse=' ')) %>%
  ungroup 
head(patong$Review)

choeng <- choeng %>% group_by(Hotel.Restaurant.name) %>% 
  summarise(Review = str_c(Review, collapse=' ')) %>%
  ungroup 
head(choeng$Review)


#text cleaning
#PATONG
patong_reviews <- patong$Review
patong_reviews <- gsub("...more", " ", patong_reviews)
patong_reviews <- gsub("\n", " ", patong_reviews)
patong_reviews <- gsub("[[:digit:]]", " ", patong_reviews)
#patong_reviews <- gsub("...", " ", patong_reviews)
head(patong_reviews)

#CHOENG THALE
choeng_reviews <- choeng$Review
choeng_reviews <- gsub("...more", " ", choeng_reviews)
choeng_reviews <- gsub("\n", " ", choeng_reviews)
choeng_reviews <- gsub("[[:digit:]]", " ", choeng_reviews)
#choeng_reviews <- gsub("...", " ", choeng_reviews)
head(choeng_reviews)

#create corpus
patong_corpus <- Corpus(VectorSource(patong_reviews))
choeng_corpus <- Corpus(VectorSource(choeng_reviews))

#further cleaning
patong_corpus <- tm_map(patong_corpus,removeNumbers)
patong_corpus <- tm_map(patong_corpus,removePunctuation)
patong_corpus <- tm_map(patong_corpus,removeWords,stopwords("english"))
patong_corpus <- tm_map(patong_corpus,removeWords, c("patong", "phuket","thailand"))
patong_corpus <- tm_map(patong_corpus,stripWhitespace)
patong_corpus <- tm_map(patong_corpus,stemDocument) #--> cuts words up

choeng_corpus <- tm_map(choeng_corpus,removeNumbers)
choeng_corpus <- tm_map(choeng_corpus,removePunctuation)
choeng_corpus <- tm_map(choeng_corpus,removeWords,stopwords("english"))
choeng_corpus <- tm_map(choeng_corpus,removeWords, c("choeng", "phuket", "thailand"))
choeng_corpus <- tm_map(choeng_corpus,stripWhitespace)
choeng_corpus <- tm_map(choeng_corpus,stemDocument) #--> cuts words up

#create word matrix
dtm_patong <- TermDocumentMatrix(patong_corpus,
                          control = list(minWordLength=c(1,Inf)))
dtm_choeng <- TermDocumentMatrix(choeng_corpus,
                                 control = list(minWordLength=c(1,Inf)))

#find top most frequent words (chose over 50 frequency due to 
#large number of words)
#PATONG
termFreq_patong <- rowSums(as.matrix(dtm_patong))
wordFreq_patong <- termFreq_patong %>% 
  subset(termFreq_patong>=90) %>%
  sort(decreasing=TRUE) %>%
  as.data.frame() %>% rownames_to_column
#plot word frequency
ggplot(wordFreq_patong) +
  aes(x=reorder(rowname,(-.)), weight = .) +
  geom_bar(fill = "#112446") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Word") +
  ylab("Frequency")

#CHOENG THALE
termFreq_choeng <- rowSums(as.matrix(dtm_choeng))
wordFreq_choeng <- termFreq_choeng %>% 
  subset(termFreq_choeng>=90) %>%
  sort(decreasing=TRUE) %>%
  as.data.frame() %>% rownames_to_column
#plot word frequency
ggplot(wordFreq_choeng) +
  aes(x=reorder(rowname,(-.)), weight = .) +
  geom_bar(fill = "#112446") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Word") +
  ylab("Frequency")

#word clouds

#PATONG
wordcloud_patong <- termFreq_patong %>%
  as.data.frame() %>% rownames_to_column %>%
  subset(rowname!='food' & rowname!='good'& rowname!='restaur' & 
         rowname!='great' & rowname!='servic')
wordcloud(words = wordcloud_patong$rowname,
          freq=wordcloud_patong$.,max.words=50,
          min.freq = 5,
          scale = c(2,0.25),
          size = 0.05,
          random.order = F,
          colors = brewer.pal(6,"Dark2"))

#CHOENG THALE 
wordcloud_choeng <- termFreq_choeng %>%
  as.data.frame() %>% rownames_to_column %>%
  subset(rowname!='food' & rowname!='good'& rowname!='restaur' & 
           rowname!='great' & rowname!='servic')
wordcloud(words = wordcloud_choeng$rowname,
          freq=wordcloud_choeng$.,max.words=50,
          min.freq = 5,
          scale = c(2,0.1),
          size = 0.05,
          random.order = F,
          colors = brewer.pal(6,"Dark2"))


#words strongly associated with top 4 words for each
findAssocs(dtm_patong, terms = c("food","good","restaur", "great", "servic"), corlimit = 0.7)	

findAssocs(dtm_choeng, terms = c("food","good","restaur", "great", "servic"), corlimit = 0.7)




#top 5 words per hotel
library(corpus)
library(tidytext)

#PATONG
corpusdf_patong <- as_corpus_frame(patong_corpus)
corpusdf_patong <- cbind(patong,corpusdf_patong)
corpusdf_patong$Hotel.Restaurant.name <- as.factor(corpusdf_patong$Hotel.Restaurant.name)
corpusdf_patong$text <- as.character(corpusdf_patong$text)
head(corpusdf_patong)
str(corpusdf_patong)


#https://mlombera94.github.io/harry_potter_text_analysis/

corpus_patong <- corpusdf_patong %>% select(-Review) %>%
  unnest_tokens(words,text) %>% 
  subset(words!='food' & words!='e' &words!='u' &
           words!='restaur'&words!='servic'& words!='staff') %>% 
  group_by(Hotel.Restaurant.name) %>% 
  count(words, sort=TRUE) %>% 
  top_n(5) %>% ungroup()



ggplot(corpus_patong) + 
  aes(x= reorder(words, -n), n, fill = Hotel.Restaurant.name,) + # use data from each novel for plot and distinguish novel by color
  geom_bar(stat = "identity") +
  facet_wrap(~ Hotel.Restaurant.name, scales = "free_y") + # separates plots by novel, "free_y" shares scales across the y-axis
  labs(x = "", y = "Frequency") + # set x and y-axis labels
  coord_flip() + # Flip cartesian coordinates so horizontal becomes vertical vice versa.
  theme(legend.position="none")
  
names(corpus_patong)
str(corpus_patong)

#CHOENG THALE
corpusdf_choeng <- as_corpus_frame(choeng_corpus)
corpusdf_choeng <- cbind(choeng,corpusdf_choeng)
corpusdf_choeng$Hotel.Restaurant.name <- as.factor(corpusdf_choeng$Hotel.Restaurant.name)
corpusdf_choeng$text <- as.character(corpusdf_choeng$text)
head(corpusdf_choeng)
str(corpusdf_choeng)

corpus_choeng <- corpusdf_choeng %>% select(-Review) %>%
  unnest_tokens(words,text) %>% 
  subset(words!='food' & words!='e' &words!='u'&
           words!='restaur' & 
           words!='servic' & words!='staff') %>% 
  group_by(Hotel.Restaurant.name) %>% 
  count(words, sort=TRUE) %>% 
  top_n(5) %>% ungroup()


ggplot(corpus_choeng) + 
  aes(x= reorder(words, -n), n, fill = Hotel.Restaurant.name,) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ Hotel.Restaurant.name, scales = "free_y") + 
  labs(x = "", y = "Frequency") + 
  coord_flip() + 
  theme(legend.position="none")

names(corpus_patong)
str(corpus_patong)

		
##SENTIMENT ANALYSIS

library(syuzhet)
#whole patong area
emotions_patong <- get_nrc_sentiment(patong$Review)
emotionsDF_patong <- emotions_patong %>% colSums() %>% as.data.frame() %>% 
  rownames_to_column("emotions")
head(emotionsDF_patong)
emotionsDF_patong <- rename(emotionsDF_patong,freq = .)

#whole choeng area
emotions_choeng <- get_nrc_sentiment(choeng$Review)
emotionsDF_choeng <- emotions_choeng %>% colSums() %>% as.data.frame() %>% 
  rownames_to_column("emotions")
head(emotionsDF_choeng)
emotionsDF_choeng <- rename(emotionsDF_choeng,freq = .)

#plot emotion frequency
#PATONG
ggplot(emotionsDF_patong) +
  aes(x = reorder(emotions, -freq), weight = freq) +
  geom_bar(fill = "#112446") +
  labs(x = "Emotions", y = "Word Frequency") +
  theme_minimal()
#CHOENG THALE
ggplot(emotionsDF_choeng) +
  aes(x = reorder(emotions, -freq), weight = freq) +
  geom_bar(fill = "#112446") +
  labs(x = "Emotions", y = "Word Frequency") +
  theme_minimal()


#Create word clouds for negative and positive sentiment
#PATONG
c_patong <- corpusdf_patong %>% 
  select(-c(Review, Hotel.Restaurant.name)) %>%
  unnest_tokens(word,text) %>% 
  subset(word!='u' & word!='e') %>% 
  group_by(word) %>% 
  count(word, sort=TRUE)%>% 
  left_join(get_sentiments("nrc")) %>%
  subset(sentiment=="positive" | sentiment=="negative") %>% 
  ungroup()

neg_patong <- c_patong %>% subset(sentiment=="negative") %>%
  select(-sentiment)

wordcloud(words = neg_patong$word,
          freq=neg_patong$n,
          max.words=50,
          min.freq = 5,
          scale = c(2,0.25),
          size = 0.1,
          random.order = F,
          colors = brewer.pal(6, "OrRd")[3:6])

pos_patong <- c_patong %>% subset(sentiment=="positive") %>%
  select(-sentiment)

wordcloud(words = pos_patong$word,
          freq=pos_patong$n,
          max.words=50,
          min.freq = 5,
          scale = c(2,0.25),
          size = 0.1,
          random.order = F,
          colors = brewer.pal(6,"Greens")[4:7])

#CHOENG THALE
c_choeng <- corpusdf_choeng %>% 
  select(-c(Review, Hotel.Restaurant.name)) %>%
  unnest_tokens(word,text) %>% 
  subset(word!='u' & word!='e') %>% 
  group_by(word) %>% 
  count(word, sort=TRUE)%>% 
  left_join(get_sentiments("nrc")) %>%
  subset(sentiment=="positive" | sentiment=="negative") %>% 
  ungroup()

neg_choeng <- c_choeng %>% subset(sentiment=="negative") %>%
  select(-sentiment)
  
wordcloud(words = neg_choeng$word,
          freq=neg_choeng$n,
          max.words=50,
          min.freq = 5,
          scale=c(2,0.25),
          size = 0.1,
          random.order = F,
          colors = brewer.pal(6, "OrRd")[3:6])

pos_choeng <- c_choeng %>% subset(sentiment=="positive") %>%
  select(-sentiment)

wordcloud(words = pos_choeng$word,
          freq=pos_choeng$n,
          max.words=50,
          min.freq = 5,
          scale = c(2,0.25),
          size = 0.1,
          random.order = F,
          colors = brewer.pal(6,"Greens")[4:7])

#bing - both very positive - choeng > patong
bing_patong <- get_sentiment(patong$Review, method="bing")
bing_patong <- get_sentiment(patong$Review, method="nrc")
bing_patong <- get_sentiment(patong$Review, method="afinn")
bing_patong <- get_sentiment(patong$Review, method="syuzhet")
summary(bing_patong)

bing_choeng <- get_sentiment(choeng$Review, method="bing")
bing_choeng <- get_sentiment(choeng$Review, method="nrc")
bing_choeng <- get_sentiment(choeng$Review, method="afinn")
bing_choeng <- get_sentiment(choeng$Review, method="syuzhet")
summary(bing_choeng)


#comparing hotels
#PATONG
hotel_emot_df_patong <- as.data.frame(emotions_patong)
hotel_emot_df_patong <- cbind(patong, hotel_emot_df_patong)
hotel_emot_df_patong <- hotel_emot_df_patong %>% select(-Review) %>%
  pivot_longer(c(anger, anticipation, disgust, fear, joy, sadness,
                 surprise, trust, negative, positive))
#all emotions
ggplot(hotel_emot_df_patong) + 
  aes(x= reorder(name, -value), value, fill = Hotel.Restaurant.name,) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ Hotel.Restaurant.name, scales = "free_y") + 
  labs(x = "", y = "Frequency") + 
  coord_flip() + 
  theme(legend.position="none")

#CHOENG
hotel_emot_df_choeng <- as.data.frame(emotions_choeng)
hotel_emot_df_choeng <- cbind(choeng, hotel_emot_df_choeng)
hotel_emot_df_choeng <- hotel_emot_df_choeng %>% select(-Review) %>%
  pivot_longer(c(anger, anticipation, disgust, fear, joy, sadness,
                 surprise, trust, negative, positive))
#all emotions
ggplot(hotel_emot_df_choeng) + 
  aes(x= reorder(name, -value), value, fill = Hotel.Restaurant.name,) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ Hotel.Restaurant.name, scales = "free_y") + 
  labs(x = "", y = "Frequency") + 
  coord_flip() + 
  theme(legend.position="none")

#just positive and negative
#PATONG
hotel_posneg_df_patong <- hotel_emot_df_patong %>% 
  subset(name=="positive" | name=="negative")%>% as.data.frame()

ggplot(hotel_posneg_df_patong) + 
  aes(x= reorder(name, -value), value, fill = Hotel.Restaurant.name,) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ Hotel.Restaurant.name, scales = "free_y") + 
  labs(x = "", y = "Frequency") + 
  coord_flip() + 
  theme(legend.position="none")


#CHOENG
hotel_posneg_df_choeng <- hotel_emot_df_choeng %>% 
  subset(name=="positive" | name=="negative")%>% as.data.frame()
  
ggplot(hotel_posneg_df_choeng) + 
  aes(x= reorder(name, -value), value, fill = Hotel.Restaurant.name,) + 
  geom_bar(stat = "identity") +
  facet_wrap(~ Hotel.Restaurant.name, scales = "free_y") + 
  labs(x = "", y = "Frequency") + 
  coord_flip() + 
  theme(legend.position="none")





#####R SHINY DASHBOARD

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Comparing reviews of tourist venues 
                  in Patong and Choeng Thale", titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Location",
               tabName = "location_tab",
               icon = icon("mountain-city")
               ),
      menuItem("Venues",
               tabName = "venue_tab",
               icon = icon("hotel")
               ))
    ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName="location_tab",
        fluidRow(box(sliderInput(inputId = "no_words",label = "Number of Words",min = 1, max = 50,
                                 value = 5)),box(uiOutput("emotion_select"))),
        fluidRow(box(title="Patong","Frequency-based Wordcloud", plotOutput("patong_wordcloud","325px", "325px")),
                 box(title="Patong","Sentiment-based Wordcloud",plotOutput("patong_emotion","325px", "325px"))),
        
        fluidRow(box(title= "Choeng Thale","Frequency-based Wordcloud",plotOutput("choeng_wordcloud","325px", "325px")),
                 box(title= "Choeng Thale","Sentiment-based Wordcloud",plotOutput("choeng_emotion","325px", "325px"))),
      ),
      tabItem(tabName = "venue_tab",
              fluidRow(box(uiOutput("location_select")), box(uiOutput("hotel_select"))),
              fluidRow(box(title="Top 5 Frequent Terms",plotOutput("hotel_word")),
                       box(title="Sentiment Frequency",plotOutput("hotel_posneg"))),
              )
      )
    )
  )
    




server <- function(input,output) {
  output$emotion_select <- renderUI({
    selectInput(inputId = "v_emotion_select",
                label = "Sentiment",
                choices = c("positive", "negative", "anger", "fear", "anticipation", "trust", 
                            "surprise","sadness", "joy", "disgust"))
  })
  
  output$patong_wordcloud <- renderPlot({
    wordcloud(words = wordcloud_patong$rowname,
              freq=wordcloud_patong$.,max.words=input$no_words,
              scale = c(2,0.25),
              random.order = F,
              colors = brewer.pal(6,"Dark2"))
  })
  output$choeng_wordcloud <- renderPlot({
    wordcloud(words = wordcloud_choeng$rowname,
              freq=wordcloud_choeng$.,max.words=input$no_words,
              scale = c(2,0.25),
              random.order = F,
              colors = brewer.pal(6,"Dark2"))
  })
  output$patong_emotion <- renderPlot({
    sent_patong <- corpusdf_patong %>% 
      select(-c(Review, Hotel.Restaurant.name)) %>%
      unnest_tokens(word,text) %>% 
      subset(word!='u' & word!='e') %>% 
      group_by(word) %>% 
      count(word, sort=TRUE)%>% 
      left_join(get_sentiments("nrc")) %>% subset(sentiment==input$v_emotion_select)
    
    wordcloud(words = sent_patong$word,
              freq=sent_patong$n,
              max.words=input$no_words,
              scale = c(4,0.5),
              random.order = F,
              colors = brewer.pal(6, "Set1"))
  })
  output$choeng_emotion <- renderPlot({
    sent_choeng <- corpusdf_choeng %>% 
      select(-c(Review, Hotel.Restaurant.name)) %>%
      unnest_tokens(word,text) %>% 
      subset(word!='u' & word!='e') %>% 
      group_by(word) %>% 
      count(word, sort=TRUE)%>% 
      left_join(get_sentiments("nrc")) %>% subset(sentiment==input$v_emotion_select)
    
    wordcloud(words = sent_choeng$word,
              freq=sent_choeng$n,
              max.words=input$no_words,
              scale = c(4,0.5),
              random.order = F,
              colors = brewer.pal(6, "Set1"))
  })
  output$location_select <- renderUI({
    selectInput(inputId = "v_location_select",
                label = "Location",
                choices = df %>% select(Location) %>% distinct())
  })
  output$hotel_select <- renderUI({
    selectInput(inputId = "v_hotel_select",
                label = "Venue",
                choices = df %>% subset(Location==input$v_location_select) %>% 
                                          select(Hotel.Restaurant.name) %>% distinct())
  })
 

  output$hotel_word <- renderPlot({
    hw <- rbind(corpus_choeng, corpus_patong)
    
    ggplot(data = hw %>% filter(Hotel.Restaurant.name==input$v_hotel_select)) + 
      aes(x= reorder(words, -n), n, fill = words) + 
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette="Set3") +
      labs(x = "", y = "Frequency") + 
      coord_flip() + 
      theme(legend.position="none")

  })
  output$hotel_posneg <- renderPlot({
    he <- rbind(hotel_emot_df_choeng, hotel_emot_df_patong)
    
    ggplot(data = he %>% filter(Hotel.Restaurant.name==input$v_hotel_select)) + 
      aes(x= reorder(name, -value), value, fill = name) + 
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette="Set3") + 
      labs(x = "", y = "Frequency") + 
      coord_flip() + 
      theme(legend.position="none")
  })
}

shinyApp(ui= ui, server = server)




