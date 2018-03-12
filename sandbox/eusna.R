#eusna user-topic analysis for blog post
#author: Katerina bohle Carbonell
#date: 28/09/2017

df <- read.csv("eu_sna.csv", header=T, sep=",")
df <- df[,1:2]
names(df)[1]<- "user"

M <- as.matrix(table( df ))
library('Matrix')
A <- spMatrix(nrow=length(unique(df$user)),
              ncol=length(unique(df$topic)),
              i = as.numeric(factor(df$user)),
              j = as.numeric(factor(df$topic)),
              x = rep(1, length(as.numeric(df$user))) )
row.names(A) <- levels(factor(df$user))
colnames(A) <- levels(factor(df$topic))
A
Arow <- tcrossprod(A)

library(ggraph)
library(igraph)

graph <- graph_from_data_frame(df)
V(graph)$name
V(graph)$type <- "topic"

ggraph(graph) + 
  geom_edge_link() + 
  geom_node_point(aes(colour = type)) 
ggsave("eu_sna_2017_user_topic.png")
