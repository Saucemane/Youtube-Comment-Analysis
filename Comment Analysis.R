##### Packages
require(tidyverse)
require(httr)
require(jsonlite)
require(tidytext)
require(textdata)
require(plotly)
require(topicmodels)
require(reshape2)
require(DT)


##### Get Data from Youtube API #####
## https://developers.google.com/youtube/v3/docs/commentThreads

# UPDATE to your api key file path
key <- read_file("C:/Keys/Youtube Data v3 Api Key.txt")

address <- paste0("https://www.googleapis.com/youtube/v3/"
                  ,"commentThreads?"
                  ,"key=", key
                  ,"&part=snippet,replies"
                  ,"&allThreadsRelatedToChannelId="
                  # UPDATE below to your Youtube channel ID
                  ,"UCgol8BzHCF5QysqhnEMM5wA"
                  ,"&maxresults=100"
                  ,"&textFormat=plainText"
)

responseLength <- 20
pageToken <- ""
dfResults <- data.frame()

rm(addressi)

while(responseLength >= 20){
  addressi <- address
  
  if(pageToken != ""){
    addressi <- paste0(address, "&pageToken=", pageToken)
  }
  
  res <- GET(addressi)
  resJSON <- fromJSON(rawToChar(res$content))
  
  df <- as.data.frame(resJSON)
  
  dfRes <- data.frame(textId = df$items.id
                      ,videoId = df$items.snippet$videoId
                      ,commenterId = df$items.snippet$topLevelComment$snippet$authorDisplayName
                      ,createdDate = df$items.snippet$topLevelComment$snippet$publishedAt
                      ,textDisplay = df$items.snippet$topLevelComment$snippet$textDisplay
  )
  
  if(pageToken == ""){
    dfResults <- dfRes
  }
  else{
    dfResults <- bind_rows(dfResults, dfRes)
  }
  
  pageToken <- resJSON$nextPageToken
  
  responseLength <- nrow(dfRes)
}


##### Process Text #####
dfUnnest <- dfResults %>%
  unnest_tokens(word, textDisplay)

# graph top words
dfWords <- dfUnnest %>%
  group_by(word) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(totalWordCount = sum(n)
         ,termFrequency = n/totalWordCount
         ,pctOfTotalWordCountDisplay = paste0(round(n/totalWordCount * 100, 1), '%')) %>%
  arrange(desc(n))

ggWords <- dfWords %>%
  filter(row_number() <= 100) %>%
  ggplot(aes(x = reorder(word, -n), y = n, group = pctOfTotalWordCountDisplay, fill = 'red')) +
  theme_bw() +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) +
  labs(x = 'Word', y = 'Count')

##### Analysis #####
## Summary Statistics
dfUnnest %>%
  summarize(
    across(everything(),
           n_distinct
    )
  )

## Plots
# plot to words
ggplotly(ggWords, tooltip = c('n', 'pctOfTotalWordCount'))


# tfidf
dfTfidf <- dfUnnest %>%
  group_by(textId, createdDate, commenterId, videoId, word) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  filter(!word %in% stop_words$word) %>%
  bind_tf_idf(word, textId, n)


# LDA
LDARes <- dfTfidf %>%
  cast_dtm(textId, word, n) %>%
  LDA(k = 10, control = list(seed = 1234))

dfTidy <- tidy(LDARes)

# graph LDA results
dfTidy %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

## by commenter
# top commenters :)
topCommenters <- dfTfidf %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(commenterId, word) %>%
  summarise(n = n()
            ,wordSentiment = sum(value)
  ) %>%
  group_by(commenterId) %>%
  summarise(n = sum(n)
            ,wordSentiment = sum(wordSentiment)
  ) %>%
  slice_max(wordSentiment, n = 10) %>%
  ungroup() %>%
  mutate(sentimentRank = row_number())

dfResults %>%
  select(commenterId, textDisplay) %>%
  filter(commenterId %in% topCommenters$commenterId) %>%
  left_join(topCommenters) %>%
  arrange(sentimentRank) %>%
  datatable(options = list(pageLength = 100))

# bottom commenters :(
bottomCommenters <- dfTfidf %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(commenterId, word) %>%
  summarise(n = n()
            ,wordSentiment = sum(value)
  ) %>%
  group_by(commenterId) %>%
  summarise(n = sum(n)
            ,wordSentiment = sum(wordSentiment)
  ) %>%
  slice_min(wordSentiment, n = 10) %>%
  ungroup() %>%
  mutate(sentimentRank = row_number())

dfResults %>%
  select(commenterId, textDisplay) %>%
  filter(commenterId %in% bottomCommenters$commenterId) %>%
  left_join(bottomCommenters) %>%
  arrange(sentimentRank) %>%
  datatable(options = list(pageLength = 100))


## by video
ggVideo <- dfTfidf %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(videoId, word) %>%
  summarise(n = n()
            ,wordSentiment = sum(value)
  ) %>%
  group_by(videoId) %>%
  mutate(videoSentiment = sum(wordSentiment)) %>%
  arrange(desc(videoSentiment), videoId) %>%
  ungroup() %>%
  mutate(videoSentimentRank = dense_rank(videoSentiment)
         ,maxVideoSentimentRank = max(videoSentimentRank)) %>%
  arrange(videoSentimentRank) %>%
  filter(videoSentimentRank >= maxVideoSentimentRank - 9) %>%
  ggplot(aes(x = reorder(word, -wordSentiment), y = wordSentiment)) +
  theme_bw() +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~videoId, scales = 'free')

# graph top 10 videos
ggplotly(ggVideo)


# by month
ggTime <- dfTfidf %>%
  mutate(createdDateTime = createdDate
         ,createdDate = as.Date(createdDate)
         ,createdYearMonth = floor_date(createdDate, "months")) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(createdYearMonth) %>%
  summarize(sentiment = sum(value)) %>%
  mutate(sentimentDirection = if_else(sentiment > 0, "green", "red")) %>%
  ggplot(aes(x = createdYearMonth, y = sentiment, fill = sentimentDirection)) +
  theme_bw() +
  geom_col() +
  scale_fill_manual(values = c("green3", "darkred")) +
  theme(legend.position = 'none')

ggplotly(ggTime)

