# Network analysis

# setup the libraries
wants <- c("GGally", "igraph", "ggplot2", "GGally", "dplyr", "readxl", "network", "sna", "intergraph", "ggthemr", "rgexf")
has   <- wants %in% rownames(installed.packages())
if(any(!has)) install.packages(wants[!has])
lapply(wants, library, character.only=T)

# ui theming
ggthemr("fresh")

# TODO set path for project relative / start a real Rstudio project
# load the data
tweets <- read_excel("data/tweets.xlsx")

# Convert edgelist into an igraph object; in this case the network is undirected 
g1 <- graph.data.frame(tweets, directed = TRUE)
# In the edgelist, each edge occured twice, thus we have to simply the graph
g1 <- simplify(g1)
plot(g1)


# doc: https://briatte.github.io/ggnet/ nicer plots
ggnet2(g1, mode = "circle")
ggnet2(g1, mode = "kamadakawai")
############################################################################

# "summary" prints the number of vertices, edges and whether the graph is directed:
summary(g1)

summary(igraph::degree(g1))

############################################################################
############################################################################
############################################################################
# Network Statistics

# Number of nodes and edges 
	vcount(g1)
	ecount(g1)
	is_directed(g1)

# Edge density
	edge_density(g1)

#  Average distance, diameter
	mean_distance(g1, directed=T)
	diameter(g1, directed=T)

# Degree distribution (in-degree,out-degree)
	deg <- degree(g1)
	mean(deg)

	deg_in <- degree(g1, mode = "in")
	deg_out <- degree(g1, mode = "out")

	mean(deg_in)
	mean(deg_out)

# Centrality indices (degree, eigenvector, closeness, betweeness)
	# Degree centrylity
	sort(deg_in, decreasing = T)[1:10]
	sort(deg_out, decreasing = T)[1:10]
	
	# Eigenvector centrality ######################  warning message
	#eig <- eigen_centrality(g1, directed=T)$vector
	#sort(eig, decreasing = T)[1:10] 
	
	# Closeness centrality
	clos_in <- closeness(g1, mode = "in") 
	sort(clos_in, decreasing = T)[1:10]
	clos_out <- closeness(g1, mode = "out") 
	sort(clos_out, decreasing = T)[1:10]

	######################## Results!?!
	# Betweeness centrality
	betw <- betweenness(g1, directed=T) 
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
	components(g1)$no
	components(g1)$csize



############################################################################
############################################################################
############################################################################
# Communities

# Clique ?!?
	clique_num(g1) # result only: 2

# K-core ?!?
	kcores <- coreness(g1)
	head(kcores)
	table(kcores)





############################################################################
############################################################################
############################################################################
# Visualization

# Different layouts
	# Simple Visualization
	

# Map metrics and measures calculated onto visual properties such as size, color and opacity



write.graph(g1, "graph.graphml", format="graphml")



