##################### wordcloud  

library(tm)
library(stringr)
library(wordcloud)
library(readxl)

tweets <- read_excel("data/tweets.xlsx")

nohandles <- str_replace_all(tweets$tweets, "@\\w+", "") # just plain english words
wordCorpus <- Corpus(VectorSource(nohandles))
wordCorpus <- tm_map(wordCorpus,content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),mc.cores=1)

wordCorpus <- tm_map(wordCorpus, removePunctuation,lazy=TRUE)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower),lazy=TRUE)
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"),lazy=TRUE)
wordCorpus <- tm_map(wordCorpus, removeNumbers,lazy=TRUE)
wordCorpus <- tm_map(wordCorpus, stripWhitespace,lazy=TRUE)
wordCorpus <- tm_map(wordCorpus, removeWords, c("the", "https","httpst","like","one"))

# wordcloud(wordCorpus,max.words = 100, random.order = FALSE)

pal <- brewer.pal(9,"Reds")
pal <- pal[-(1:4)]
set.seed(123)

wordcloud(words = wordCorpus, scale=c(6,0.1), max.words=100, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=pal)
