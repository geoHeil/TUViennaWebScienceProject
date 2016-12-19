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
tweets <- read_excel("~/Dropbox/9.Semester/webScience/ue/project/data/tweets.xlsx")

# Convert edgelist into an igraph object; in this case the network is undirected 
g1 <- graph.data.frame(tweets, directed = FALSE)
# In the edgelist, each edge occured twice, thus we have to simply the graph
g1 <- simplify(g1)
plot(g1)


# doc: https://briatte.github.io/ggnet/ nicer plots
ggnet2(g1, mode = "circle")
ggnet2(g1, mode = "kamadakawai")
