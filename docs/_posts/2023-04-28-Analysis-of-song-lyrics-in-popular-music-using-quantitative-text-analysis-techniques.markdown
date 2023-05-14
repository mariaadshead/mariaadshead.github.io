---
layout: post
title: "Analysis of song lyrics in popular music using quantitative text analysis techniques"
date: 2023-04-28 12:00:00 -0000
font: "Permanent Marker"
categories: R API music quanteda
---
{% highlight ruby %}

{% endhighlight %}

This is a project that I did as my final assignment for the Quantitative Text Analysis graduate course. My primary source of data was the full history of 
Billboard Hot 100 published weekly, which I retrieved from Kaggle. I then downloaded the lyrics for the songs from the chart using the API of Genius, and gathered 
some additional information, such as genre and country where the track was released using the API of Discogs.com. Using the quanteda package, I transformed the data
into a corpus, treating the lyrics of each song as a separate document, and created a DFM and a tokens object for the respective functions that only accept data in this 
format. To analyse the lyrics, I implemented topic modelling and keyness analysis. Below I present the code for each step of my project.

_Note: racial slurs in plots were blurred out._

First, load the necessary packages.

{% highlight ruby %}
library(tidyverse)
library(geniusr)
library(stringr)
library(httr)
library(lubridate)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(stm)
library(ggplot2)
{% endhighlight %}

Gathering the lyrics using the Genius API (paths and access information for APIs are not shown for privacy reasons):

{% highlight ruby %}
#import data
tracks <- read_csv("/.../billboard-hot-100-tracks.csv",
                   show_col_types = FALSE)

#set access information for the API
Sys.setenv(GENIUS_API_TOKEN = "TOKEN")

#create empty lists to store information
lyrics_documents <- c()
date_list <- c()
names_list <- c()

for(i in 1:nrow(tracks)){
  #get information for a track
  song <- tracks[i,]
  song_date <- song$date
  
  print(i)
  try({
    #retrieve a tibble with lyrics for the song
    lyrics_tibble <- get_lyrics_search(song$artist, song$song)
    
    #concatenate all lines into one string
    doc <- str_c(lyrics_tibble$line, collapse = " ")
    
    #get the full name of the song to use as the name of a future document
    docname <- str_c(c(song$artist, song$song), collapse = " - ")
    names(doc) <- docname
    
    lyrics_documents[i] <- doc
    date_list[i] <- song_date 
    names_list[i] <- docname
  }
  )
}

#create a dataframe out of the information we retrieved
corpus_df <- data.frame(lyrics = lyrics_documents,
                        date = date_list,
                        names = names_list)

#drop NA values
corpus_df <- drop_na(corpus_df)

#convert date into a proper format
corpus_df$date <- as.Date(corpus_df$date, origin = lubridate::origin)
{% endhighlight %}

Downloading data from the Discogs.com database:

{% highlight ruby %}
#get login information for the API
discogs_key <- 'CONSUMER_KEY'
discogs_secret <- 'CONSUMER_SECRET'

#create empty columns for country and genre to fill them later
corpus_df$genre <- 0
corpus_df$country <- 0

for (i in 1:nrow(corpus_df)){
  try({
      song <- corpus_df$names[i]
      song <- str_remove_all(song, "-")
      song <- str_split(song, " ")
      song <- song[[1]]
      song <- song[song != ""]
      
      query <- str_c(song, collapse = "%20")
      
      #create a search query for the API
      search_url <- sprintf("https://api.discogs.com/database/search?q=%s&key=%s&secret=%s", query, discogs_key, discogs_secret)
      
      #submit the query
      search_r <- GET(search_url)
      
      search_c <- content(search_r, "parsed")
      
      #retrieve the country of origin
      s_country <- search_c$results[[1]]$country
      
      #retrieve the genre
      s_genre <- search_c$results[[1]]$genre[[1]]
      
      corpus_df$country[i] <- s_country
      corpus_df$genre[i] <- s_genre
      
      #make a pause to limit the number of requests per minute
      Sys.sleep(1)
  })
}

#store the newly created dataframe as a CSV
write.csv(corpus_df, file='.../df_genre_country.csv')
{% endhighlight %}

The original dataset contains charts published from 1959 until 2021, however due to time and API requests limits the dataset I used for this project only contains data from 1995-2021. Overall, it has 60400 observations. The chunk of code below removes rows with missing information and columns that are not going to be used and creates objects that we will use further (corpus, tokens object, and DFM).

{% highlight ruby %}
corpus_df <- read_csv('/.../df_genre_country.csv', 
                      show_col_types = FALSE)

#remove rows where country and genre were not retrieved
corpus_df <- corpus_df %>%
  filter(genre != 0) %>%
  select(-1) #remove the first columns with indices

#create a corpus object out of our dataset
billboard_corp <- corpus(corpus_df, text_field = "lyrics")

#set the names of songs as names of documents
docnames(billboard_corp) <- corpus_df$names

#create a tokens object
toks <- tokens(billboard_corp,
               remove_punct = TRUE,
               remove_numbers = TRUE,
               remove_url = TRUE,
               remove_symbols = TRUE,
               remove_separators = TRUE) %>%
  tokens_remove(stopwords("en")) 

#create a DFM object as well
b_dfm <- toks %>% 
  dfm(tolower = TRUE) 
{% endhighlight %}

<h3>Topic Modelling</h3>

Now we proceed to topic modelling. To be specific, I will be implementing structural topic models for K from 2 to 10,
where K is the number of topics. For each of those metrics of semantic coherence, exclusivity, and others will be calculated, based on
which I will select an optimal value of K and use the corresponding model for further analysis.

{% highlight ruby %}
#convert into an input for a structural topic model
stm_input <- convert(b_dfm, to = "stm")

#perform a search over K values
k_search_output <- searchK(stm_input$documents, stm_input$vocab,
                           K = c(2, 3, 4, 5, 6, 7, 8, 9, 10), 
                           data = stm_input$meta,
                           verbose = FALSE, heldout.seed = 42)

plot(k_search_output)
print(k_search_output)
{% endhighlight %}

<h5><strong>Results of K search</strong></h5>
<h5><em>Source: calculations performed in R</em></h5>
<img src="/assets/ksearch.png" width = "250" height = "170" alt="">


