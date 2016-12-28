wants <- c("SocialMediaLab", "magrittr", "igraph", "ggplot2", "ggthemr", "wordcloud", "tm", "stringr")
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
# TODO switch to instagram! dynamic network is supported


####################################################
# choose some layout
coolPlot <- function(graphName){
  glay = layout.fruchterman.reingold(graphName)
  ver_labs = igraph::get.vertex.attribute(graphName, "name", index=V(graphName))
  
  # plot
  par(bg="gray15", mar=c(1,1,1,1))
  plot(graphName, layout=glay,
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
  print("summary")
  summary(graphName)  
  
  # Number of nodes and edges 
  print("number of vertices")
  vcount(graphName)
  print("number of edges")
  ecount(graphName)
  
  print(paste("graph is directed", is_directed(graphName)))
  
  print(paste("edge density", edge_density(graphName)))
  
  # TODO add all the stuff to this function
  # TODO minim is to use print statements, if
  # you want to get fancy returning an object which allows a comparison chart
  # or plots is cool
  
}

calcStats(g_twitter_actor)
calcStats(g_twitter_bimodal)

#  Average distance, diameter
mean_distance(g1, directed=T)
diameter(g1, directed=T)

# Degree distribution (in-degree,out-degree)
deg <- igraph::degree(g1)
summary(deg)
mean(deg)

deg_in <- igraph::degree(g1, mode = "in")
deg_out <- igraph::degree(g1, mode = "out")

mean(deg_in)
mean(deg_out)

# Centrality indices (degree, eigenvector, closeness, betweeness)
# Degree centrylity
sort(deg_in, decreasing = T)[1:10]
sort(deg_out, decreasing = T)[1:10]

# Eigenvector centrality ######################  not applicable for directed graph, warning that all are 000
#eig <- eigen_centrality(g1, directed=T)$vector
#sort(eig, decreasing = T)[1:10] 

# Closeness centrality
clos_in <- igraph::closeness(g1, mode = "in") 
sort(clos_in, decreasing = T)[1:10]
clos_out <- igraph::closeness(g1, mode = "out") 
sort(clos_out, decreasing = T)[1:10]

######################## Results!?!
# Betweeness centrality
betw <- igraph::betweenness(g1, directed=T) 
sort(betw, decreasing = T)[1:10]

# Hubs and Authorities
hs <- hub_score(g1)$vector
sort(hs, decreasing = T)[1:10]

as <- authority_score(g1)$vector
sort(as, decreasing = T)[1:10]

# PageRank
pr<-page_rank(g1, algo = c("prpack", "arpack", "power"), vids = V(g1),
              directed = TRUE, damping = 0.85, personalized = NULL, weights = NULL,
              options = NULL)$vector
sort(pr, decreasing = T)[1:10]
# Clustering coefficiant

# Reciprocity
# The measure of reciprocity defines the proporsion of mutual connections, in a directed graph. 
# It is most commonly defined as the probability that the opposite counterpart of a directed edge
# is also included in the graph.
# Value:  A numeric scalar between zero and one. 
reciprocity(g1, ignore.loops = TRUE, mode = c("default", "ratio"))
# value for tweets: 0  ?!?
# Shortest paths

# Density
edge_density(g1)

# Number and sizes of connected components
igraph::components(g1)$no
igraph::components(g1)$csize
############################################################################
# Communities

# Clique ?!?
igraph::clique_num(g1) #warning

# K-core ?!?
kcores <- coreness(g1)
head(kcores)
table(kcores)

##################### wordcloud  
#tweetsCloud <- tweets
# TODO fix this that all words are removed
#nohandles <- str_replace_all(tweetsCloud$tweets, "@\\w+", "") # just plain english words
wordCorpus <- Corpus(VectorSource(tweets))
wordCorpus <- tm_map(wordCorpus, removePunctuation,lazy=TRUE)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower),lazy=TRUE)

#removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
#wordCorpus <- tm_map(wordCorpus, content_transformer(removeURL))

wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"),lazy=TRUE)
wordCorpus <- tm_map(wordCorpus, removeNumbers,lazy=TRUE)
wordCorpus <- tm_map(wordCorpus, stripWhitespace,lazy=TRUE)
#wordCorpus <- tm_map(wordCorpus, removeWords, c("the", "https","httpst","like","one"))# TODO fix

tdm <- TermDocumentMatrix(wordCorpus, control = list(wordLengths = c(1, Inf)))
(freq.terms <- findFreqTerms(tdm, lowfreq = 20))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 20)
df <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip() 

m <- as.matrix(tdm)
word.freq <- sort(rowSums(m), decreasing = T)
pal <- brewer.pal(9, "BuGn")[-(1:4)]

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)

findAssocs(tdm, "realtrump", 0.2)
#findAssocs(tdm, "data", 0.2)
dtm <- as.DocumentTermMatrix(tdm)
lda <- LDA(dtm, k = 8) # find 8 topics
term <- terms(lda, 7) # first 7 terms of every topic (term <- apply(term, MARGIN = 2, paste, collapse = ", "))
#############################################################################
library(networkD3)
# http://curleylab.psych.columbia.edu/netviz/netviz2.html#/9
# TODO make a cool interactive visualization
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

# TODO visualize tweets over time http://ouzor.github.io/blog/2015/08/31/twitter-streamgraph.html
# some visualization link http://kateto.net/network-visualization
# to collect more data http://www.bnosac.be/index.php/blog/57-new-rstudio-add-in-to-schedule-r-scripts