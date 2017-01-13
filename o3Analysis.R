wants <- c("SocialMediaLab", "magrittr", "igraph", "ggplot2", "wordcloud", "tm", "stringr", "lubridate", "scales", "dplyr", "purrr", "tidyr","twitteR","tm","wordcloud","readr","htmlwidgets","streamgraph")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)

# TODO https://github.com/vosonlab/SocialMediaLab/issues/24
# WARNING this doesnt work, need to have the data loaded into memory!!!!! directly after import
#tweets will be populated then
load('tweets.Rdata') # will overwrite tweets variable
#tweets <- importData("singleBigTweets.csv","twitter")

g_twitter_actor <- tweets %>% Create("Actor", writeToFile=TRUE)
# In this actor network, edges represent interactions between Twitter
#users. An interaction is deﬁned as a ‘mention’ or ‘reply’ or ‘retweet’ from user i to user j, given ‘tweet’ m. In
#a nutshell, a Twitter actor network shows us who is interacting with who in relation to a particular hashtag
#or search term

g_twitter_bimodal <- tweets %>% Create("Bimodal", writeToFile=TRUE)
# post cannot follow users
#In this bimodal network there are two types of nodes: users and posts. A user can either comment or like a
#post, therefore there are two types of edges (i.e. ties between nodes): comments and likes. If a user i has
#both liked and commented on a post j, then there are two edges e1 and e2 directed from i to j.The bimodal network is therefore:
#directed (users can like or comment on posts, but posts can’t like or comment back)
#weighted (users can comment multiple times on a post)
#bipartite (users can like or comment on posts, but posts can’t like or comment back)
#multiple edges or parallel edges (we have one edge for each interaction from user i to post j)

#g_twitter_sem <- tweets %>% Create("Semantic", writeToFile=TRUE) # TODO check why this is no longer working !!!!!!!!!!!!
# semantic network. In this network nodes represent unique concepts (in this case
# unique terms/words extracted from a set of 150 tweets), and edges represent the co-occurrence of terms for allobservations in the data set. For example, for this Twitter semantic network, nodes represent either hashtags
# (e.g. “#auspol”) or single terms (“politics”). If there are 150 tweets in the data set (i.e. 150 observations),
#and the term auspol and the term politics  appear together in every tweet, then this would be represented
#by an edge with weight equal to 150.

#g_twitter_semantic_reduced <- tweets %>%
  #Create("Semantic",termFreq=100,removeTermsOrHashtags=c("#trump"), writeToFile=TRUE) # this is a lot bigger # TODO check why this is no longer working !!!!!!!!!!!!
# TODO compare network representations!!

####################################################
# g1 <- read.graph("actorNetwork.graphml", format = "graphml")
# g1 <- read.graph("bimodalNetwork.graphml", format = "graphml")
# g1 <- simplify(g1)
# choose some layout
coolPlot <- function(graphName){
  glay = layout.fruchterman.reingold(graphName)
  ver_labs = igraph::get.vertex.attribute(graphName, "name", index=V(graphName))
  
  # plot
  par(bg="gray15", mar=c(1,1,1,1))
  plot(graphName, layout=igraph::layout.fruchterman.reingold,
       vertex.color="gray25",
       vertex.size=10,
       vertex.label=ver_labs,
       vertex.label.family="sans",
       vertex.shape="none",
       vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
       vertex.label.cex=0.85,
       edge.arrow.size=0.8,
       edge.arrow.width=0.5,
       edge.width=3,
       edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))
  # add title
  title("some network",
        cex.main=1, col.main="gray95")
}
plot(simplify(g_twitter_actor, remove.multiple = F, remove.loops = T),
     layout=igraph::layout.fruchterman.reingold)
coolPlot(g_twitter_actor)
#plot(g_twitter_actor)
coolPlot(g_twitter_bimodal)
#coolPlot(g_twitter_sem)
#coolPlot(g_twitter_semantic_reduced)
#################
pageRankPerGraph <- function(graphName){
  pageRank_graph <- sort(page.rank(graphName)$vector,decreasing=TRUE)
  print(head(pageRank_graph,n=3))
  print(tail(pageRank_graph,n=3))
}

pageRankPerGraph(g_twitter_actor)
pageRankPerGraph(g_twitter_bimodal)
pageRankPerGraph(g_twitter_sem)
pageRankPerGraph(g_twitter_semantic_reduced)

imc <- infomap.community(g_twitter_actor, nb.trials = 100)
# create a vector of users with their assigned community number
communityMembership_auspol <- membership(imc)
# summarise the distribution of users to communities
commDistribution <- summary(as.factor(communityMembership_auspol))
# which community has the max number of users
tail(sort(commDistribution),n=1)
# create a list of communities that includes the users assigned to each community
communities_auspol <- communities(imc)
# look at the members of the most populated community
communities_auspol[names(tail(sort(commDistribution),n=1))]
# same as doing it manually
communities_auspol[5]


#############################################################################
calcStats <- function(graphName){
  # "summary" prints the number of vertices,
  #edges and whether the graph is directed: 
  print("Summary")
  summary(graphName)  
  
  # Number of nodes and edges 
  print(paste("Number of vertices:", vcount(graphName)))
  print(paste("Number of edges:", ecount(graphName)))
  print(paste("Graph is directed", is_directed(graphName)))
  print(paste("Edge density:", edge_density(graphName)))
  print(paste("Average distance:",
              mean_distance(graphName, directed=T)))
  print(paste("Diameter:", diameter(graphName, directed=T)))
  print(paste("Reciprocity: ",
              reciprocity(graphName, ignore.loops =TRUE,
                          mode = c("default", "ratio"))))
  
  # Degree distribution (in-degree,out-degree)
  deg <- igraph::degree(graphName)
  print(paste("Average degree:", mean(deg) ))
  deg_in <- igraph::degree(graphName, mode = "in")
  deg_out <- igraph::degree(graphName, mode = "out")
  print(paste("Average in-degree:",mean(deg_in) ))
  print(paste("Average out-degree:", mean(deg_out)))
  
  print("In-degree centrality (from highest to lowest)")
  print(sort(deg_in, decreasing = T)[1:10])
  print("Out-degree centrality (from highest to lowest)")
  print(sort(deg_out, decreasing = T)[1:10])
  
  # Eigenvector centrality
  eig <- eigen_centrality(graphName, directed=T)$vector
  print("Eigenvector centrality (from highest to lowest)")
  print(sort(eig, decreasing = T)[1:10] )
  
  #  Closeness centrality
  clos_in <- igraph::closeness(graphName, mode = "in") 
  print("In-closeness centrality (from highest to lowest)")
  print(sort(clos_in, decreasing = T)[1:10])
  clos_out <- igraph::closeness(graphName, mode = "out") 
  print("Out-closeness centrality (from highest to lowest)")
  print(sort(clos_out, decreasing = T)[1:10])
  
  # Betweeness centrality
  betw <- igraph::betweenness(graphName, directed=T) 
  print("Betweeness centrality (from highest to lowest)")
  print(sort(betw, decreasing = T)[1:10])

  hist (sort(betw, decreasing = T)[1:10])
  
  # Hubs and Authorities
  hs <- hub_score(graphName)$vector
  print("Hub scores(from highest to lowest)")
  print(sort(hs, decreasing = T)[1:10])
  
  as <- authority_score(graphName)$vector
  print("Authoritie scores(from highest to lowest)")
  print(sort(as, decreasing = T)[1:10])
  
  # PageRank
  pr<-page_rank(graphName, algo =
                  c("prpack", "arpack", "power"),
                vids = V(graphName),
                directed = TRUE,
                damping = 0.85,
                personalized = NULL,
                weights = NULL,
                options = NULL)$vector
  print("Page rank (from highest to lowest)")
  print(sort(pr, decreasing = T)[1:10])
  
  # Number and sizes of connected components
  print(paste("Number of connected components:",
              igraph::components(graphName)$no))
  print("Sizes of connected components:")
  print(summary(igraph::components(graphName)$csize))
  # unfortunately graph will not show up in function
  # qplot(igraph::components(graphName)$csize,
  # geom="histogram", binwidth = 100,
  # main="Sizes of connected components")
  
  # Transitivity measures the probability
  # that the adjacent vertices of a vertex are connected.
  # This is sometimes also called the clustering coefficient. 
  print("Clustering coefficient (type=local)")
  clcoef <- transitivity(graphName, type = "local")
  print(tail(table(clcoef)))
  print(paste("Clustering coefficient, (type=global):",
              transitivity(graphName, type = "global"))) 
  print(paste("Clustering coefficient, (type=average):",
              transitivity(graphName, type = "average"))) 
}

calcStats(g_twitter_actor)
calcStats(g_twitter_bimodal)

##################### wordcloud  

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

m <- as.matrix(tdm)
word.freq <- sort(rowSums(m), decreasing = T)


findAssocs(tdm, "realtrump", 0.2)
#findAssocs(tdm, "data", 0.2)
dtm <- as.DocumentTermMatrix(tdm)
lda <- LDA(dtm, k = 8) # find 8 topics
term <- terms(lda, 7) # first 7 terms of every topic (term <- apply(term, MARGIN = 2, paste, collapse = ", "))
#############################################################################
library(networkD3)
# http://curleylab.psych.columbia.edu/netviz/netviz2.html#/9
#http://curleylab.psych.columbia.edu/netviz/netviz2.html

# Create fake data
src <- c("A", "A", "A", "A",
         "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")
networkData <- data.frame(src, target)

# Plot
#simpleNetwork(networkData)
simpleNetwork(tweets)


# some visualization link http://kateto.net/network-visualization
# to collect more data http://www.bnosac.be/index.php/blog/57-new-rstudio-add-in-to-schedule-r-scripts

# TODO visualize tweets over time http://ouzor.github.io/blog/2015/08/31/twitter-streamgraph.html
####### visualization over time  does NOT work

library("readr")
library("dplyr")
library("lubridate")
library("streamgraph")
library("htmlwidgets")

# Read  tweets
  tweets_df <- read_csv("tweets3.csv") %>%
  select(created, text) %>%
  mutate(text = tolower(text))

# Pick hashtags with regexp
hashtags_list <- regmatches(tweets_df$text, gregexpr("#[[:trump:]]+", tweets_df$text))

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




