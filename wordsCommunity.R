# words for communities
wants <- c("SocialMediaLab", "magrittr", "igraph", "ggplot2", "wordcloud", "tm", "stringr", "lubridate", "scales", "dplyr", "purrr", "tidyr","twitteR","tm","wordcloud","readr","htmlwidgets")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)

load('tweets.Rdata') # will overwrite tweets variable
g_twitter_actor <- tweets %>% Create("Actor", writeToFile=FALSE)

# Number and sizes of connected components
c <- igraph::components(g_twitter_actor, mode = 'weak')
c$no
summary(c$csize)
sort(c$csize, decreasing = T)[1:10]

makeWordcloud <- function(t){
  nohandles <- str_replace_all(t$text, "@\\w+", "") 
  wordCorpus <- Corpus(VectorSource(nohandles))
  wordCorpus <- tm_map(wordCorpus,
                       content_transformer(function(x) iconv(x,
                                                             to='UTF-8',
                                                             sub='byte')),
                       mc.cores=1)
  wordCorpus <- tm_map(wordCorpus,
                       removePunctuation,
                       lazy=TRUE)
  wordCorpus <- tm_map(wordCorpus,
                       content_transformer(tolower),
                       lazy=TRUE)
  wordCorpus <- tm_map(wordCorpus,
                       removeWords,
                       stopwords("english"),
                       lazy=TRUE)
  wordCorpus <- tm_map(wordCorpus,
                       removeNumbers,
                       lazy=TRUE)
  wordCorpus <- tm_map(wordCorpus,
                       stripWhitespace,
                       lazy=TRUE)
  wordCorpus <- tm_map(wordCorpus,
                       removeWords,
                       c("trump","realdonaldtrump","the",
                         "like","http","https","httpst"))
  pal <- brewer.pal(9,"Reds")
  pal <- pal[-(1:4)]
  set.seed(123)
  wordcloud(words = wordCorpus,
            scale=c(6,0.1),
            max.words=250,
            random.order=FALSE, 
            rot.per=0.35,
            use.r.layout=FALSE,
            colors=pal)
}
makeWordcloud(tweets)
# this creates a sub cluster
# TODO iterate over the top 3 clusters (or coose any other)
# and just execute the wordcount code
subCluster <- induced.subgraph(g_twitter_actor, V(g_twitter_actor)[which(c$membership == which.max(c$csize))])

# TODO how to access tweets / text object wihtin cluster
# open question here: http://stackoverflow.com/questions/41661096/subgraph-text-analysis-in-r-igraph

for(index in 0:2) {
  print(index)
  # TODO use subcluster here
}
