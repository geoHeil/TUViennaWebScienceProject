# graph analysis

# setup the libraries
wants <- c("GGally", "igraph", "ggplot2", "GGally", "dplyr", "readxl", "network", "sna", "intergraph", "ggthemr")
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

#number of vertices

#number of edges and their weights
table(E(comics)$weight)

#degree distribution (resp. in-degree and out-degree in the directed case), 

#centrality indices,
# Degree centrality (from highest to lowest), i.e., the vertex with ID 56 has the highest degree in the network
sort(deg, decreasing = T)
# Display the 10 highest degrees in the network together with the IDs of these vertices
sort(deg, decreasing = T)[1:10]

# Eigenvector centrality
eig <- eigen_centrality(g1, directed=F)$vector
sort(eig, decreasing = T)[1:10]

# Closeness centrality
clos <- closeness(g1) 
sort(clos, decreasing = T)[1:10]

# Betweeness centrality
betw <- betweenness(g1, directed=F) 
sort(betw, decreasing = T)[1:10]

#pr <- page.rank(heroes) 
#sort(pr$vector, decreasing = T)[1:10]



#clustering coefficient, 
#clcoef <- transitivity(heroes, type = "local")
#tail(table(clcoef))
# Both global clustering coefficients that we discussed in the lecutre can also be computed with the same command.
# Global clustering coefficient as average of the local ones
#transitivity(heroes, type = "average") 
# the same as mean(clcoef, na.rm = T) but in the latter case one has to make sure that does nodes, for which no
# clustering coefficient is defined are removed (with the option "na.rm")
# Global clustering coefficient as ratio of triangles and connected triples
#transitivity(heroes, type = "global") 


#reciprocity, 

#shortest paths, 

#network diameter, 
mean_distance(g1, directed=F)
diameter(g1, directed=F)

#density, number and size of connected components.
edge_density(g1)

# Hubs and Authorities
hs <- hub_score(g1)$vector
sort(hs, decreasing = T)[1:10]

as <- authority_score(g1)$vector
sort(as, decreasing = T)[1:10]



# The number and sizes of connected components are retrieved with the help of "components" 
components(g1)$no
components(g1)$csize
clique_num(heroes)

#kcores <- coreness(heroes)
#head(kcores)
# With "table" we get a better overview.
#table(kcores)
