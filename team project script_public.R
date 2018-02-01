# packages
library(dplyr)
library(statnet)
library(ggraph) # installed on othe rmac
library(igraph)

#help source: 
# http://jfaganuk.github.io/2014/12/18/An-introduction-to-network-analysis-in-R/

# inspiration

# functions ---------------------------------------------------------------

ego.effective.size <- function(g, ego, ...) {
  egonet <- induced.subgraph(g, neighbors(g, ego, ...))
  n <- vcount(egonet)
  t <- ecount(egonet)
  return(n - (2 * t) / n)
}

effective.size <- function(g, ego=NULL, ...) {
  if(!is.null(ego)) {
    return(ego.effective.size(g, ego, ...))
  }
  return (sapply(V(g), function(x) {ego.effective.size(g,x, ...)}))
}

shortest.path.vertex <- function(g){
  shortest.path.m <- shortest.paths(g, v=V(g), to=V(g))
  shortest.path.v <- apply(shortest.path.m, 1, mean)
  return(shortest.path.v)
}

msloc_snametric <- function(gs) {
  # Centrality
  V(gs)$degree      <- degree(gs, mode = "total", normalized=T)
  V(gs)$indegree    <- degree(gs, mode = "in", normalized=T)
  V(gs)$outdegree   <- degree(gs, mode = "out", normalized=T)
  V(gs)$betweenness <- betweenness(gs, normalized=T, weights=NA)
  # error w/ evcent. results don't make sense. needs symmetrized data
  # V(gs)$evcent      <- eigen_centrality(gs, directed=T, scale=T, weights=NA)
  V(gs)$closeness_in<- closeness(gs, normalized=T, mode="in")
  V(gs)$flowbet     <- sna::flowbet(as.matrix(get.adjacency(gs)))
  E(gs)$betweenness <- edge.betweenness(gs)
  # Local position
  V(gs)$effsize     <- effective.size(gs, mode = "all")
  V(gs)$constraint  <- constraint(gs)
  # Clustering
  com <- edge.betweenness.community(gs, directed=T)
  V(gs)$memb        <- com$membership
  # Whole network
  gs <- set.graph.attribute(gs, "density", graph.density(gs))
  gs <- set.graph.attribute(gs, "avgpathlength", average.path.length(gs))
  gs <- set.graph.attribute(gs, "modularity", modularity(com))
  gs <- set.graph.attribute(gs, "betcentralization", centralization.betweenness(gs)$centralization)
  gs <- set.graph.attribute(gs, "degcentralization", centralization.degree(gs, mode = "total")$centralization)
  gs <- set.graph.attribute(gs, "size", vcount(gs))
  gs <- set.graph.attribute(gs, "edgecount", ecount(gs))
  vertexsdata <- get.data.frame(gs, what = "vertices")
  edgedata <- get.data.frame(gs, what ="edges")
  #gstats <- #do.call('rbind', lapply(gs, function(y) {
  ga <- list.graph.attributes(gs)
  gstats <- sapply(ga, function(x) {
    get.graph.attribute(gs, x)
  })
  allresults <- list(gs, vertexsdata, edgedata, gstats)
  return(allresults)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Load Data ---------------------------------------------------------------

# data can not be shared

# subset by relationship type ---------------------------------------------


info.el <- subset(ed, select = c(Source, Target, InformationInteger))
comm.el <- subset(ed, select = c(Source, Target, CommunicationInteger))


# select relationship to work with ----------------------------------------

relationship <- "Communication" # or "comm"

graph.el <- comm.el # MODIFY THIS LINE DEPENDING ON RELATIONSHIP

###########MODIFY ALSO LINE 117 FOR SAVING THE ROBJECT

# creating networks using igraph ------------------------------------------


library(igraph)
names(graph.el)[3]<-"weight"
graph.g <- graph.data.frame(graph.el, directed=T, vertices = nodes[,c(1,3:10)])
graph.g

# dichotomize data
E(graph.g)$bin <- E(graph.g)$weight
graph.g <- graph.g %>% set_edge_attr("bin", which(E(graph.g)$weight > 3), 1)
graph.g <- graph.g %>% set_edge_attr("bin", which (E(graph.g)$weight < 4), 0)



# subset networks by branch -----------------------------------------------

# delete weak ties
graph.str <- graph.g %>% delete_edges(which(E(graph.g)$weight < 4))

graph.exec <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Executive Area") )
graph.strat <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Strategic Systems Branch") )
graph.op <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Operational Comms") )
graph.infra <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Infrastructure Support") )
graph.engin <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Engineering") )

# save graph objects for plotting -----------------------------------------
save.image("comm_graphs.RData") #change line according to name of network shown in line 36

# analysing network -----------------------------------------------------

graph.g <- info.g # modify this line
graph.exec <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Executive Area") )
graph.strat <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Strategic Systems Branch") )
graph.op <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Operational Comms") )
graph.infra <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Infrastructure Support") )
graph.engin <- induced_subgraph(graph.g, which(V(graph.g)$Branch.Area == "Engineering") )

# vertex descriptions
vert_desc <- rbind("all"= rs_graph.g[[2]],"exec" = rs_graph.exec[[2]], "strat"= rs_graph.strat[[2]],
                   "op"= rs_graph.op[[2]], "infra"= rs_graph.infra[[2]], "engin" = rs_graph.engin[[2]])
View(vert_desc)

# edge descriptives
edge_desc <- rbind("all"= rs_graph.g[[3]],"exec" = rs_graph.exec[[3]], "strat"= rs_graph.strat[[3]],
                   "op"= rs_graph.op[[3]], "infra"= rs_graph.infra[[3]], "engin" = rs_graph.engin[[3]])
# graph descriptives
graph_desc <- rbind("all"= rs_graph.g[[4]],"exec" = rs_graph.exec[[4]], "strat"= rs_graph.strat[[4]],
                    "op"= rs_graph.op[[4]], "infra"= rs_graph.infra[[4]], "engin" = rs_graph.engin[[4]])
# save output -------------------------------------------------------------
write.table(vert_desc, "vertices_descriptives.csv", append = T)
write.table(edge_desc, "edges_descriptives.csv", append = T)
write.table(graph_desc, "graph_descriptives.csv", append = T)

# create networks using statnet

# density tables ----------------------------------------------------------

library(rDNA)
# info.el[which(info.el$weight < 4),3] <- 0
# info.el[which(info.el$weight > 3),3] <- 1
# info.ed <- info.el[which(info.el$weight == 1),]

# turn into a matrix
library(igraph)  
library(reshape2)
library(ggplot2)

density_tables <-function(graphobject, col_index_for_groups, relationship, group){
  graph <- get.adjacency(graphobject , names=T, attr="bin")
  groups <- matrix(nodes[,col_index_for_groups]) 
  row.names(groups) <- nodes[,1]
  
  #calculate binary density table
  density_table <- dna.density(graph, partitions = groups, weighted=F)
  
  #prep for drawing
  dens_melt <- melt(density_table)
  
  #draw and return
  p<- ggplot(dens_melt, aes(Var1, Var2)) + 
    geom_tile(aes(fill=value), color = "white") + 
    #geom_text(aes(y=1:5, x=1:5, label=dep_dens_melt$value))
    scale_fill_gradient2(low="#e5f5f9", mid = "#99d8c9", high="#2ca25f", 
                         space = "Lab", midpoint = mean(dep_dens_melt$value)-0.05, guide="colorbar")+
    labs(title=paste("Proportion of ", relationship,"within and between Groups (",group,")"),
         x ="Group of Sender", y = "Group of Receiver") +
    theme(axis.text.x = element_text(angle=45, hjust=+1)
    )
  return(p)
  
}

density_tables_numbers <-function(graphobject, col_index_for_groups, relationship, group){
  graph <- get.adjacency(graphobject , names=T, attr="bin")
  groups <- matrix(nodes[,col_index_for_groups]) 
  row.names(groups) <- nodes[,1]
  
  #calculate binary density table
  density_table <- dna.density(graph, partitions = groups, weighted=F)
  
  return(density_table)
}

colnames(nodes)
density_tables(graph.g, 4, relationship, group="Age")
group="Age"
ggsave(paste("density_table_bin_", relationship,"_", group,".png"))

density_tables(graph.g, 5, relationship, group="Job")
group="Job"
ggsave(paste("density_table_bin_", relationship,"_", group,".png"))

density_tables(graph.g, 6, relationship, group="Tenure")
group="Tenure"
ggsave(paste("density_table_bin_", relationship,"_", group,".png"))

density_tables(graph.g, 7, relationship, group="Position")
group="Department"
ggsave(paste("density_table_bin_", relationship,"_", group,".png"))


density_tables(graph.g, 8, relationship, group="Embedded")
group="Embedded"
ggsave(paste("density_table_bin_", relationship,"_", group,".png"))

density_tables(graph.g, 9, relationship, group="Gender")
group="Gender"
ggsave(paste("density_table_bin_", relationship,"_", group,".png"))

density_tables(graph.g, 10, relationship, group="Position")
group="Position"
ggsave(paste("density_table_bin_", relationship,"_", group,".png"))



#original script for calculating density table. keep for reference
graphstr.m <- get.adjacency(graph.str , names=T, attr="bin")
graph.m <- get.adjacency(graph.g , names=T, attr="weight")
groups <- matrix(nodes[,7]) # groups = departments
row.names(groups) <- nodes[,1]

dep_density_table <- dna.density(graph.g, partitions = group, weighted=F)
dep_density_table_value <- dna.density(graph.m, partitions = groups, weighted=T)
dep_dens_melt <- melt(dep_density_table)

dep_dens_melt <- melt(dep_density_table)

ggplot(dep_dens_melt, aes(Var1, Var2)) + 
  geom_tile(aes(fill=value), color = "white") + 
  #geom_text(aes(y=1:5, x=1:5, label=dep_dens_melt$value))
  scale_fill_gradient2(low="#e5f5f9", mid = "#99d8c9", high="#2ca25f", 
                       space = "Lab", midpoint = mean(dep_dens_melt$value)-0.05, guide="colorbar")+
  labs(title=paste("Proportion of ", relationship,"within and between Departments"),
       x ="Department of Sender", y = "Department of Receiver") +
  theme(axis.text.x = element_text(angle=45, hjust=+1)
        )
ggsave(paste("density_table_bin_", relationship,".png"))


# draw graph of conultants ------------------------------------------------

graph <-graph_from_adjacency_matrix(as.matrix(con), mode="directed", weighted=T)

E(graph)$strong <- as.matrix(con) > 4
E(graph)$knowing <- as.matrix(con_kw)
E(graph)$valuing <-as.matrix(con_val)
  
V(graph)$tenure <- conatt$tenure
V(graph)$Extraversion <- conatt$Extraversion
V(graph)$Agreeableness <- conatt$Agreeableness
V(graph)$selfefficacy <- conatt$self.efficacy

layout <- create_layout(graph, layout='kk')

ggraph(layout) + 
  geom_edge_link(aes(color=weight, alpha = ..index..), width = 2) + 
  scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  scale_edge_color_continuous(low="white", high = "dodgerblue4") + 
  geom_node_label(aes(label=name)) +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  labs(title = "Information Exchange Network at a Consulting Company", 
       subtitle = "Ranging from 1 (never) to 5 (very frequent) information exchange")
ggsave("consulting_ie_simple_complete.png")

ggraph(layout) + 
  geom_edge_link(aes(filter = weight > 4, alpha = ..index..), width = 0.8) + 
  scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_continuous(low="white", high = "dodgerblue4") + 
  geom_node_label(aes(label=name)) +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  labs(title = "Information Exchange Network at a Consulting Company", 
       subtitle = "Ranging from 1 (never) to 5 (very frequent) information exchange")
ggsave("consulting_ie_simple_strong.png")


ggraph(layout) + 
  geom_edge_link(aes(alpha = ..index..)) + 
  scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_continuous(low="white", high = "blue") + 
  geom_node_label(aes(label=name)) +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  facet_edges(~weight, drop = TRUE) + 
  labs(title = "Information Exchange Network at a Consulting Company", 
       subtitle = "Ranging from 1 (never) to 5 (very frequent) information exchange")
ggsave("consulting_ie_simple_facet.png")

ggraph(layout) + 
  geom_edge_link2(aes(filter = weight > 4), width = 1.23) + 
  #scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_gradientn(colors = c("white", "darkblue"), values=c(0,0,0,0,1,1)) + 
  geom_node_label(aes(size = tenure, label=name), color="dodgerblue4") +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  labs(title = "Information Exchange Network at a Consulting Company", 
       subtitle = "Only strong ties are shown") 
ggsave("consulting_ie_with_tenure.png")

ggraph(layout) + 
  geom_edge_link2(aes(filter = weight > 4), width = 1.23) + 
  #scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_gradientn(colors = c("white", "darkblue"), values=c(0,0,0,0,1,1)) + 
  geom_node_label(aes(size = Extraversion, label=name), color="dodgerblue4") +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  labs(title = "Information Exchange Network at a Consulting Company", 
       subtitle = "Only strong ties are shown") 
ggsave("consulting_ie_with_extra.png")

ggraph(layout) + 
  geom_edge_link2(aes(filter = weight > 4), width = 1.23) + 
  #scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_gradientn(colors = c("white", "darkblue"), values=c(0,0,0,0,1,1)) + 
  geom_node_label(aes(size = Agreeableness, label=name), color="dodgerblue4") +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  labs(title = "Information Exchange Network at a Consulting Company", 
       subtitle = "Only strong ties are shown") 
ggsave("consulting_ie_with_agree.png")

ggraph(layout) + 
  geom_edge_link2(aes(filter = weight > 4), width = 1.23) + 
  #scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_gradientn(colors = c("white", "darkblue"), values=c(0,0,0,0,1,1)) + 
  geom_node_label(aes(size = selfefficacy, label=name), color="dodgerblue4") +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  labs(title = "Information Exchange Network at a Consulting Company", 
       subtitle = "Only strong ties are shown") 
ggsave("consulting_ie_with_se.png")

# dichotomize data
graph <- graph %>% set_edge_attr("weight", which (E(graph)$weight < 5), 0)
graph <- graph %>% delete_edges(which(E(graph)$weight == 0))
graph <- graph %>% set_edge_attr("weight", which(E(graph)$weight > 4), 1)
consulting_metrics <- msloc_snametric(graph)
consulting_metrics[[2]]
write.csv(consulting_metrics[[2]], "consulting_metrics_vertex.csv")
write.csv(consulting_metrics[[3]], "consulting_metrics_graph.csv")

graph <- graph %>% set_edge_attr("knowing", which (E(graph)$knowing < 5), 0)
graph <- graph %>% delete_edges(which(E(graph)$knowing == 0))
graph <- graph %>% set_edge_attr("knowing", which(E(graph)$knowing > 4), 1)

graph <- graph %>% set_edge_attr("valuing", which (E(graph)$valuing < 5), 0)
graph <- graph %>% delete_edges(which(E(graph)$valuing == 0))
graph <- graph %>% set_edge_attr("valuing", which(E(graph)$valuing > 4), 1)


# multiplex graph ---------------------------------------------------------
graph <-graph_from_adjacency_matrix(as.matrix(con), mode="directed", weighted=T)

graph <- graph %>% set_edge_attr("weight", which (E(graph)$weight < 5), 0)
graph <- graph %>% set_edge_attr("weight", which (as.character(E(graph)$weight) == "NA"), 0)
E(graph)$knowing <- as.matrix(con_kw)
E(graph)$valuing <-as.matrix(con_val)
E(graph)$strong <- as.numeric(as.matrix(con) > 3)

# library(rgexf)
# g1.gexf <- igraph.to.gexf(graph)
# 
# # You have to create a file connection.
# f <- file("consulting.gexf")
# writeLines(g1.gexf$graph, con = f)
# close(f)

# ggraph(graph) + 
#   geom_edge_link2(aes(filter = weight > 4)) +
#   geom_edge_link2(aes(filter = knowing > 4))
#   geom_node_label(aes(label=name), color="dodgerblue4") +
  # theme_graph(foreground = 'steelblue', fg_text_colour = 'white') +
  # labs(title = "Information Exchange Network at a Consulting Company",
  #      subtitle = "Only strong ties are shown")

ggraph(graph) + 
  #geom_edge_arc(aes(filter = strong == 1, color="black")) + 
  geom_edge_arc(aes(filter = knowing > 5, color="Aware of Expertise"),
                arrow = arrow(length = unit(4, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm')) + 
  geom_edge_arc(aes(filter = valuing > 5, color="Value of Expertise"),
                arrow = arrow(length = unit(4, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm')) + 
  geom_edge_arc(aes(filter = weight > 5, color="Information Exchange"),
                arrow = arrow(length = unit(4, 'mm')), 
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm')) + 
  scale_edge_color_brewer("Type of Edge", type= "qual", palette=2) +
  geom_node_label(aes(label=name), color="dodgerblue4") +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') +
  labs(title = "Being Awarena and Valuing Expertise")
ggsave("multiplex_network_example_with_arrow.png")

ggraph(graph, layout = 'linear') + 
  #geom_edge_arc(aes(filter = weight == 5 , color = "red"), show.legend=F, curvature = 0) + 
  geom_edge_arc(aes(filter = knowing > 5, color="blue"), show.legend=F, curvature = 0.3) + 
  geom_edge_arc(aes(filter = valuing > 5, color = "green"), show.legend=F, curvature = 0.6) + 
  geom_node_label(aes(label=name), color="dodgerblue4") +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') +
  labs(title = "Information Exchange Network at a Consulting Company",
        caption = c("Red = strong information exchage , 
                    blue = awareness of expertise, 
                    green = valuing the expertise"))

ggraph(graph) + 
  geom_edge_link(aes(filter = weight > 5, color = "red"), show.legend=F) + 
  geom_edge_link(aes(filter = knowing > 5, color="blue"), show.legend=F) + 
  geom_edge_link(aes(filter = valuing > 5, color = "green"), show.legend=F) + 
  geom_node_label(aes(label=name), color="dodgerblue4") +
  facet_edges(~knowing + valuing)
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') +
  labs(title = "Information Exchange Network at a Consulting Company",
       caption = c("Red = strong information exchage , 
                   blue = awareness of expertise, 
                   green = valuing the expertise"))



#knowing graph
kwlaytout <- create_layout(graph, '')
g.kw <-graph_from_adjacency_matrix(as.matrix(con_kw), mode="directed", weighted=T)

p.kw <- ggraph(g.kw) + 
  geom_edge_link2(aes(filter = weight > 5)) + 
  #scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_gradientn(colors = c("white", "darkblue"), values=c(0,0,0,0,1,1)) + 
  geom_node_label(aes(label=name), color="dodgerblue4") +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  labs(title = "Expertise awareness
       Network at a Consulting Company", 
       subtitle = "Only strong ties are shown") 
ggsave("consulting_kw.png")


g.val <-graph_from_adjacency_matrix(as.matrix(con_val), mode="directed", weighted=T)

ggraph(g.val) + 
  geom_edge_link2(aes(filter = weight > 5)) + 
  #scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_gradientn(colors = c("white", "darkblue"), values=c(0,0,0,0,1,1)) + 
  geom_node_label(aes(label=name), color="dodgerblue4") +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  labs(title = "Valuing Network at a Consulting Company", 
       subtitle = "Only strong ties are shown") 
ggsave("consulting_val.png")


# pkwlayout <- create_layout(g.kw, 'kk')
# ggraph(g.val) + 
#   geom_edge_link2(aes(filter = weight > 5, color="darkblue")), arrow
#   geom_edge_link2(data = pkwlayout) + 

detach(igraph)
library(network)

con.bin <- as.matrix(con)
con.bin[con.bin < 5] <- 0
con.bin[con.bin > 4] <- 1

con.kw.b<-as.matrix(con_kw)
con.kw.b[con.kw.b < 5] <- 0
con.kw.b[con.kw.b > 4] <- 1

kw.tail <- as.vector(con.kw.b)
kw.head <- as.vector(t(con.kw.b))

m <- network::network(con.bin, multiple=T)

#m <- network::add.edges(m, tail = kw.tail, head = kw.head, edge.check=F), names.eval="type", vals.eval= rep("awareness", length(kw.tail))  )

multi<- add.edges(multi,tail=c(1,1,1,1),head=c(2,2,3,4),
                  names.eval='type',vals.eval=c("A","B","A","B"))

# lw dataset --------------------------------------------------------------

lw <- read.csv("~/Documents/msloc/LW_informal_RS_NITS.csv", header=T, sep=",")

graph <-graph_from_adjacency_matrix(as.matrix(lw[,-1]), mode="directed", weighted=T)

E(graph)$strong <- as.matrix(con) > 3
V(graph)$name
V(graph)$shortname <- LETTERS[1:length(V(graph)$name)]
V(graph)$school <- c("Business", "Business", "Medicine", "Medicine", "Library",
                     "Psychology", "Library", "Law", "Medicine", "Business", "Law", "Business",
                     "Administration", "Governance", "Law", "Law", "Medicine", "Humanities", "Business", "Business")
V(graph)$role <- c("manager (new)", "manager (old)", "initiator", "member", "support",
                   "pilot leader", "pilot leader", "member", "pilot leader", "pilot leader",
                   "member", "member", "support", "pilot leader", "member", 
                   "initiator", "initiator", "pilot leader", "member", "project leader")

layoutkk <- create_layout(graph, layout='kk')
layoutdh <- create_layout(graph, layout='dh')
layoutgem <- create_layout(graph, layout='gem')
layoutmds <- create_layout(graph, layout='mds')
layoutstar <- create_layout(graph, layout='star')

ggraph(layout) + 
  geom_edge_link(aes(color=weight, alpha = ..index..), width = 2) + 
  scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  scale_edge_color_continuous(low="white", high = "dodgerblue4") + 
  geom_node_label(aes(label=shortname)) +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  labs(title = "Information Exchange Network in a Project Team", 
       subtitle = "Ranging from 1 (never) to 5 (very frequent) information exchange")
ggsave("lw_ie_simple_complete.png")

ggraph(layout) + 
  geom_edge_link2(aes(alpha = ..index.., filter = weight %in% c(1,2,3,4,5))) + 
  scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_continuous(low="white", high = "blue") + 
  geom_node_label(aes(label=shortname)) +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  facet_edges(~weight, drop = TRUE, ncol =3) + 
  labs(title = "Information Exchange Network in a Project Team", 
       subtitle = "Ranging from 1 (never) to 5 (very frequent) information exchange")
ggsave("lw_ie_simple_facet.png")

p1<- ggraph(layout) + 
  geom_edge_link2(aes(alpha = ..index.., filter = weight < 3)) + 
  #scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_continuous(low="white", high = "blue") + 
  geom_node_label(aes(label=shortname)) +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  #facet_edges(~weight, drop = TRUE, ncol =3) + 
  labs(title = "Information Exchange Network in a Project Team", 
       subtitle = "No to Rare Exchange")
p1
ggsave("lw_ie_simple_rare.png")

p2<- ggraph(layout) + 
  geom_edge_link2(aes(alpha = ..index.., filter = weight > 2.5 & weight < 4)) + 
  #scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_continuous(low="white", high = "blue") + 
  geom_node_label(aes(label=shortname)) +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  #facet_edges(~weight, drop = TRUE, ncol =3) + 
  labs(title = "Information Exchange Network in a Project Team", 
        subtitle  = "Some Exchange")
p2
ggsave("lw_ie_simple_mid.png")

p3<- ggraph(layout) + 
  geom_edge_link2(aes(alpha = ..index.., filter = weight > 3.5)) + 
  #scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_continuous(low="white", high = "blue") + 
  geom_node_label(aes(label=shortname)) +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  #facet_edges(~weight, drop = TRUE, ncol =3) + 
  labs(title = "Information Exchange Network in a Project Team", 
       subtitle = "Frequent to Very Frequent Exchange")
p3
ggsave("lw_ie_simple_high.png")

multiplot(p1, p2, p3, cols=1)
#ggsave("lw_ie_simple_facet.png")


ggraph(layout) + 
  geom_edge_link2(aes(filter = weight > 3.5, color=weight), width = 1.23) + 
  #scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_gradientn(colors = c("white", "darkblue"), values=c(0,0,0,0,1,1)) + 
  geom_node_label(aes(fill = school, label=shortname), color="dodgerblue4") +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  labs(title = "Information Exchange Network in a Project Team", 
       subtitle = "Only strong ties are shown") 
ggsave("lw_ie_with_school.png")

ggraph(layout) + 
  geom_edge_link2(aes(filter = weight > 3.5, color=weight), width = 1.23) + 
  #scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_gradientn(colors = c("white", "darkblue"), values=c(0,0,0,0,1,1)) + 
  geom_node_label(aes(fill = role, label=shortname), color="dodgerblue4") +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  labs(title = "Information Exchange Network in a Project Team", 
       subtitle = "Only strong ties are shown") 
ggsave("lw_ie_with_role.png")


# dichotomize data
graph <- graph %>% set_edge_attr("weight", which (E(graph)$weight < 5), 0)
graph <- graph %>% delete_edges(which(E(graph)$weight < 5))
graph <- graph %>% set_edge_attr("weight", value=1)
metrics <- msloc_snametric(graph)

write.csv(metrics[[2]], "lw_metrics_vertex.csv")
write.csv(metrics[[4]], "lw_metrics_graph.csv")


ggraph(layoutstar) + 
  geom_edge_link(aes(color=weight, alpha = ..index..), width = 2) + 
  #scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_continuous(low="white", high = "dodgerblue4") + 
  geom_node_label(aes(label=shortname)) +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  labs(title = "Information Exchange Network in a Project Team (Star outline)", 
       subtitle = "Ranging from 1 (never) to 5 (very frequent) information exchange")
ggsave("star_layout.png")

ggraph(layoutkk) + 
  geom_edge_link(arrow = arrow(length = unit(4, 'mm')),  end_cap = circle(3, 'mm')) + 
  #scale_edge_alpha('Edge direction', guide = 'edge_direction') + 
  #scale_edge_color_continuous(low="white", high = "dodgerblue4") + 
  geom_node_label(aes(label=shortname)) +
  theme_graph(foreground = 'steelblue', fg_text_colour = 'white') + 
  labs(title = "Information Exchange Network in a Project Team (KK outline)", 
       subtitle = "Ranging from 1 (never) to 5 (very frequent) information exchange")
ggsave("graph_w_arrow.png")
        
