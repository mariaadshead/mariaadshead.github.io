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
<img src="/assets/ksearch.png" width = "270" height = "170" alt="">

To choose an optimal value of K, a balance needs to be reached between semantic coherence and exclusivity: both need to be maximized, however as K increases, the two metrics change in opposite directions. As we may see, the highest semantic coherence is achieved for K = 2, while highest exclusivity - for K = 10.
As for the optimal K, I am inclined to choose between K = 4 and K = 5. Semantic coherence scores of - 50.4547 and -54.92121 are slightly worse than for K = 2, however they are significantly better than for higher values of K. Meanwhile, exclusivity scores of 8.253031 and 8.517537 are a noticeable improvement over the ones for lower values of K. To make the final choice, let us fit models for these values and see for which one the topics make more sense.

<h4><strong>Wordcloud for Topic 1</strong></h4>
<img src="/assets/topic1.png" width = "350" height = "250" alt="">

<h4><strong>Wordcloud for Topic 2</strong></h4>
<img src="/assets/topic2.png" width = "350" height = "250" alt="">

<h4><strong>Wordcloud for Topic 3</strong></h4>
<img src="/assets/topic3.png" width = "350" height = "250" alt="">

<h4><strong>Wordcloud for Topic 4</strong></h4>
<img src="/assets/topic4.png" width = "350" height = "250" alt="">

<h4><strong>Wordcloud for Topic 5</strong></h4>
<img src="/assets/topic5.png" width = "350" height = "250" alt="">

There is a lot of overlap in topics in both models, however it seems that the identified topics in the model with K = 5 make slightly more sense, so I decided to stick with it. I believe the topics could be characterized as follows:

1. slang, curses and slurs (most likely rap/hip-hop lyrics)
2. “romantic non-light-hearted”/emotional lyrics
3. dancing-related lyrics
4. “romantic light-hearted” lyrics
5. rather difficult to identify a common theme, however the presence of the words like “ride”, “town”, “car”, etc. indicates that this topic probably relates to country music lyrics or has something to do with travelling
   
It appears that topics 1, 3, and 4 feature more simplistic language than topics 2 and 5, however more complex analysis is needed to confirm this. Overall, it seems that some of the topics were formed based on typical lyrics in different music genres rather than actual topics in the common sense of the word.

Now we are going to look at the topic distribution across genres and countries.

{% highlight ruby %}
lyrics_topics <- findThoughts(topic_model_2,
                                 texts = corpus_df$lyrics,
                                 n = 1000)$index

#initialize the data frame
topics_df <- corpus_df[unlist(lyrics_topics[1]),]
topics_df$topic <- 1

#select the most likely songs for each topic and bind them into one dataframe
for (i in 2:length(lyrics_topics)){
  df <- corpus_df[unlist(lyrics_topics[i]),]
  df$topic <- i
  topics_df <- rbind(topics_df, df)
}

#bar chart showing the relationship between topic and genre
plot_genre <- ggplot(topics_df, 
       aes(x = topic, 
           fill = genre)) + 
  geom_bar(position = "stack") +
  ggtitle("Share of music genres in each topic")

#bar chart showing the relationship between topic and country
plot_country <- ggplot(topics_df, 
       aes(x = topic, 
           fill = country)) + 
  geom_bar(position = "stack") +
  ggtitle("Share of countries in each topic")
{% endhighlight %}

<h4><strong>Share of music genres in each topic</strong></h4>
<img src="/assets/topics_genres.png" width = "350" height = "250" alt="">

<h4><strong>Share of countries in each topic</strong></h4>
<img src="/assets/topics_countries.png" width = "350" height = "250" alt="">

Let us analyze the plots shown above.

As for the genre plot, we can clearly see that the most common genre for Topic 1 is hip-hop. I have briefly noted above that most likely the words encountered in the wordcloud for this topic relate to this genre of music, which turned out to be correct. Another important thing to note that in Topic 1 hip-hop occupies a noticeably larger share than in others. Hip-hop also occupied substantial shares in Topic 3 and 4 (dance-related lyrics and romantic light-hearted lyrics).
Meanwhile, Topic 2 is predominantly rock music, which has a small share in most topics. It also has a substantial, but a much smaller share in Topic 5 (ambiguous, possibly travel-related lyrics).

The other three topics do not have a highly dominant music genre as in the previous two. However, in Topic 3 the most common one is electronic, in Topic 4 it is funk/soul and hip-hop (electronic has a substantial share too), and in Topic 5 it is folk/world/country (with rock having a relatively big share too). Overall, the findings above make sense and confirm some of the guesses made above. To elaborate, we would anticipate that electronic music has dance-related lyrics (Topic 3), funk and hip-hop may have “light-hearted” romantic lyrics (Topic 4), and as was hypothesized, Topic 5 mostly relates to folk/country music.
Unfortunately, the plot depicting the country distribution for topics is not as insightful. In most of the topics the biggest share is occupied by the US, which is not surprising since the chart originates from this country. However, probably the interesting thing to note here is that the US occupies the largest shares in Topics 1 and 5, meaning that either hip-hop and country mostly originate from the US, or that people in the US prefer listening to music originating from their own country in these genres. Perhaps, the first is more likely to be the case for country, and how hip-hop it is either the second or a mix of both. Meanwhile, the US occupies the smallest share in Topic 3 which is mostly electronic music; perhaps, this is explained by the fact that a lot of popular electronic music originates from Europe.

Let us take this analysis further and study the genre distribution in each topic across the years.

<h4><strong>Topic occurrence in 1995-2005, grouped by genre</strong></h4>
<img src="/assets/years1.png" width = "350" height = "250" alt="">

<h4><strong>Topic occurrence in 2006-2021, grouped by genre</strong></h4>
<img src="/assets/years2.png" width = "350" height = "250" alt="">


