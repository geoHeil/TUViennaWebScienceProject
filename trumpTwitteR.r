############################# Nice plots (tewwts containing ##, quatation mark, number of tweets over time)

tweets <- read.csv("data/trumpTweets.csv")

ggplot(tweets, aes(factor(grepl("#", tweets$text)))) +
        geom_bar(fill = "midnightblue") + 
        theme(legend.position="none", axis.title.x = element_blank()) +
        ylab("Number of tweets") + 
        ggtitle("Tweets with Hashtags") +
        scale_x_discrete(labels=c("No hashtags", "Tweets with hashtags"))

ggplot(tweets, aes(factor(grepl('^"', tweets$text)))) +
        geom_bar(fill = "midnightblue") + 
        theme(legend.position="none", axis.title.x = element_blank()) +
        ylab("Number of tweets") + 
        ggtitle("Tweets with quatation mark") +
        scale_x_discrete(labels=c("No quatation mark", "Tweets with quatation mark"))

tweets$created <- ymd_hms(tweets$created)

ggplot(data = tweets, aes(x = created)) +
        geom_histogram(aes(fill = ..count..)) +
        theme(legend.position = "none") +
        xlab("Time") + ylab("Number of tweets") + 
        scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

ggplot(data = tweets, aes(x = wday(created))) +
        geom_histogram(breaks = seq(0.5, 8, by =1), aes(fill = ..count..)) +
        theme(legend.position = "none") +
        xlab("Day of the Week") + ylab("Number of tweets") + 
        scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

############################### 
 # https://www.r-bloggers.com/twitter-sentiment-analysis-with-r/
 # http://www.rdatamining.com/docs/twitter-analysis-with-r


############################# Devices from which is tweeted 

tweets3 <- read.csv("tweets3.csv")
tweetsSmall <- tweets3 %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, "source", "Twitter for (.*?)<") %>%
  filter(source %in% c("iPhone", "Android"))

tweetsSmall %>%
  count(source, hour = hour(with_tz(created, "EST"))) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")


tweet_picture_counts <- tweetsSmall %>%
  filter(!str_detect(text, '^"')) %>%
  count(source,
        picture = ifelse(str_detect(text, "t.co"),
                         "Picture/link", "No picture/link"))
ggplot(tweet_picture_counts, aes(source, n, fill = picture)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "")

tweetsSmall %>%
  count(source,
        quoted = ifelse(str_detect(text, '^"'), "Quoted", "Not quoted")) %>%
  ggplot(aes(source, n, fill = quoted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "") +
  ggtitle('Whether tweets start with a quotation mark (")')


tweet_words %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip()

library(tidytext)
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- tweetsSmall %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

tweet_words

android_iphone_ratios <- tweet_words %>%
  count(word, source) %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_each(funs((. + 1) / sum(. + 1)), -word) %>%
  mutate(logratio = log2(Android / iPhone)) %>%
  arrange(desc(logratio))


########################
wordCorpus <- Corpus(VectorSource(tweet_words))
tdm <- TermDocumentMatrix(wordCorpus,
                          control = list(wordLengths = c(1, Inf)))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 20)
df <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df, aes(x=term, y=freq)) +
  geom_bar(stat="identity") +
  xlab("Terms") +
  ylab("Count") +
  coord_flip() 

m <- as.matrix(tdm)
word.freq <- sort(rowSums(m), decreasing = T)
pal <- brewer.pal(9, "BuGn")[-(1:4)]

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)

######################### Wordcloud

nohandles <- str_replace_all(tweets$text, "@\\w+", "") 
wordCorpus <- Corpus(VectorSource(nohandles))
wordCorpus <- tm_map(wordCorpus,content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),mc.cores=1)

wordCorpus <- tm_map(wordCorpus, removePunctuation,lazy=TRUE)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower),lazy=TRUE)
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"),lazy=TRUE)
wordCorpus <- tm_map(wordCorpus, removeNumbers,lazy=TRUE)
wordCorpus <- tm_map(wordCorpus, stripWhitespace,lazy=TRUE)
wordCorpus <- tm_map(wordCorpus, removeWords, c("trump","realdonaldtrump"))


pal <- brewer.pal(9,"Reds")
pal <- pal[-(1:4)]
set.seed(123)

# pal <- brewer.pal(9, "BuGn")[-(1:4)]

wordcloud(words = wordCorpus, scale=c(6,0.1), max.words=250, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)

tdm <- TermDocumentMatrix(wordCorpus, control = list(wordLengths = c(1, Inf)))
(freq.terms <- findFreqTerms(tdm, lowfreq = 20))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 20)
df <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip() 

############################ Visualizating Twitter history with streamgraph

	tweets_df <- read.csv("data/trumpTweets.csv")%>%
   select(created, text) %>%
   mutate(text = tolower(text))

# Pick hashtags with regexp
hashtags_list <- regmatches(tweets_df$text, gregexpr("#[[:trump:]]+", tweets_df$text))
hashtags_list <- regmatches(tweets_df$text, gregexpr("#trump", tweets_df$text))


hashtag_list <- aes(factor(grepl("#trump", tweets$text)))


# Create a new data_frame with (timestamp, hashtag) -pairs
hashtags_df <- data_frame()
for (i in which(sapply(hashtags_list, length) > 0)) {
  hashtags_df <- bind_rows(hashtags_df, data_frame(timestamp = tweets_df$created[i],
                                                   hashtag = hashtags_list[[i]]))
}

# Process data for plotting
hashtags_df <- hashtags_df %>%
  # Pick top 20 hashtags
  filter(hashtag %in% names(sort(table(hashtag), decreasing=TRUE))[1:20]) %>%
  # Group by year-month (daily is too messy)
  # Need to add '-01' to make it a valid date for streamgraph
  mutate(yearmonth = paste0(format(as.Date(timestamp), format="%Y-%m"), "-01")) %>%
  group_by(yearmonth, hashtag) %>%
  summarise(value = n())

# Create streamgraph
sg <- streamgraph(data = hashtags_df, key = "hashtag", value = "value", date = "yearmonth",
                 offset = "silhouette", interpolate = "cardinal",
                 width = "700", height = "400") %>%
  sg_legend(TRUE, "hashtag: ") %>%
  sg_axis_x(tick_interval = 1, tick_units = "year", tick_format = "%Y")



