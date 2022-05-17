library(tidyverse)
library(dplyr)

prueba<-SP1_4_

casa<-select(prueba, (4), (5), (8))
fuera<-select(prueba, (5), (4), (8))
colnames(casa)<-c("team1", "team2", "puntos")
casa$puntos[casa$puntos == "H"] <- 1
casa$puntos[casa$puntos == "D"] <- 0
casa$puntos[casa$puntos == "A"] <- 0
colnames(fuera)<-c("team1", "team2", "puntos")
fuera$puntos[fuera$puntos == "H"] <- 0
fuera$puntos[fuera$puntos == "D"] <- 0
fuera$puntos[fuera$puntos == "A"] <- 1

casa<-filter(casa, puntos==1)
fuera<-filter(fuera, puntos==1)

a<-rbind(casa, fuera)


sources <- a %>%
  distinct(team1) %>%
  rename(label = team1)

destinations <- a %>%
  distinct(team2) %>%
  rename(label = team2)

nodes <- full_join(sources, destinations, by = "label")
nodes <- nodes %>% rowid_to_column("id")
a$puntos<-as.double(a$puntos)


per_route <- a %>%  
  group_by(team1, team2) %>%
  summarise(goles = sum(puntos)) %>% 
  ungroup()

per_route<-filter(per_route, goles==2)
edges <- per_route %>% 
  left_join(nodes, by = c("team1" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("team2" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, goles)

library(network)

routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)

plot(routes_network, vertex.cex = 0.8)

degree(routes_igraph)

library(igraph)

routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

plot(routes_igraph, edge.arrow.size = 0.5)

plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.5)

install_github('thomasp85/tidygraph')
devtools::install_github("thomasp85/ggraph", dependencies=TRUE) 
devtools::install_github("thomasp85/tidygraph", dependencies=TRUE) 
library(tidygraph)
library(ggraph)

library(visNetwork)
library(networkD3)

visNetwork(nodes, edges)


visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle")



routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)
routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(goles))

ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = goles), alpha = 0.8) + 
  scale_edge_width(range = c(0.1, 1)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Puntos") +
  theme_graph()



ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = goles), alpha = 0.1) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
  theme_graph()

ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = goles), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Letters") +
  theme_graph()

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_with_fr") 



