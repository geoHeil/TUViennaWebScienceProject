# words for communities
wants <- c("SocialMediaLab", "magrittr", "igraph", "ggplot2", "wordcloud", "tm", "stringr", "lubridate", "scales", "dplyr", "purrr", "tidyr","twitteR","tm","wordcloud","readr","htmlwidgets","streamgraph")
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

# this creates a sub cluster
# TODO iterate over the top 3 clusters (or coose any other)
# and just execute the wordcount code
subCluster <- induced.subgraph(g_twitter_actor, V(g_twitter_actor)[which(c$membership == which.max(c$csize))])
