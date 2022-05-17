library(dplyr)
library(jsonlite)

eventEng <- fromJSON("/Users/lahlo/Documents/ProjetFoot4A/Soccer 2017-2018/events/events_England.json")

eventEng1 <- eventEng %>%
  group_by(matchId)

eventEng1 <- eventEng1[1:1768,]

eventEng1 <- eventEng1[1:901,]

for (i in 1:nrow(eventEng1)) {
  #print(i)
  #print(eventEng1$tags[i])
  #print(eventEng1$tags[i][[1]]$id)
  #print(eventEng1$tags[i][[1]])
  if (nrow(eventEng1$tags[i][[1]])==0) {
    eventEng1$tags[i] <- 0
  }
  else {
    eventEng1$tags[i] <- eventEng1$tags[i][[1]]$id[nrow(eventEng1$tags[i][[1]])]
  }
}

for (i in 1:nrow(eventEng1)) {
  #print(i)
  #print(eventEng1$tags[i])
  #print(eventEng1$tags[i][[1]]$id)
  #print(eventEng1$tags[i][[1]])
  if (nrow(eventEng1$tags[i][[1]])==0) {
    eventEng1$tags[i] <- 0
  }
  else {
    eventEng1$tags[i] <- eventEng1$tags[i][[1]]$id[nrow(eventEng1$tags[i][[1]])]
  }
}


tab <-  data.frame(eventId = 1,subEventName=1,tags=1,playerId=1,positions=1,matchId=1,eventName=1,teamId=1,matchPeriod=1,
                   eventSec=1,subEventId=1,id=1)

for(i in 1:nrow(eventEng1)){
  if (eventEng1$eventName[i] == "Pass"){
    tab <- rbind(tab,eventEng1[i,],eventEng1[i+1,])
    tab <- distinct(tab)
  } 
}


tab <- tab[-1,]

tab <- select(tab, (3),(4),(7),(8),(10))

tab <- tab[tab$teamId==1609,]

tab <- tab[1:nrow(tab)-1,]

for (i in 1:nrow(tab)) {
  tab$test[i] <- tab$tags[i][[1]]
}

tab <- select(tab, (1),(2),(3),(5),(6))

dest <-  data.frame(tags=1,playerId=1,eventName=1,eventSec=1,test=1)

for (i in 1:nrow(tab)) {
  if (tab$eventName[i]=="Pass" & tab$test[i]==1801) {
    dest <- rbind(dest,tab[i+1,])
  }
}

srcs <-  data.frame(tags=1,playerId=1,eventName=1,eventSec=1,test=1)

for (i in 1:nrow(tab)) {
  if (tab$eventName[i]=="Pass" & tab$test[i]==1801) {
    srcs <- rbind(srcs,tab[i,])
  }
}

dest <- dest[-1,]
srcs <- srcs[-1,]

srcs <- data.frame(source=srcs$playerId, label=1)
dest <- data.frame(destination=dest$playerId,label=1)

edges <- cbind(source=srcs$source,destination=dest$destination)

edges <- data.frame(source=edges[,1],destination=edges[,2],nmbre=1)

edges <- edges %>%
  group_by(source,destination) %>%
  summarise(nmbre = sum(nmbre))

nodes1 <- edges %>%
  distinct(source) %>%
  rename(label = source)

nodes2 <- edges[,2]
nodes2 <- unique(nodes2)
colnames(nodes2) <- c("label")

nodes <- full_join(nodes1, nodes2, by = "label")


players <- fromJSON("/Users/lahlo/Documents/ProjetFoot4A/Soccer 2017-2018/players.json")
playersEng <- players[,c("wyId","shortName","currentTeamId")]
playersEng <- data.frame(currentTeamId = playersEng$currentTeamId,
                         playerId = playersEng$wyId,
                         shortName = playersEng$shortName,
                         values = 1)
playersEng <- playersEng %>%
  group_by(currentTeamId,shortName,playerId) %>%
  summarise(gr_sum = sum(values))

playersEng <- playersEng[,2:3]


playersEngARS <- arrange(playersEng,playerId)

nodes <- nodes %>% 
  left_join(playersEngARS, by = c("label" = "playerId")) 

edges <- edges %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = shortName)

edges <- edges %>% 
  left_join(nodes, by = c("destination" = "label")) %>% 
  rename(to = shortName)

nodes <- select(nodes, (2),(1))


nodes111 <- nodes[,2]
edges111 <- edges[,1:3]
library(igraph)

routes_igraph_pass <- graph_from_data_frame(d = edges111, vertices = nodes111, directed = TRUE)

plot(routes_igraph_pass, edge.arrow.size = 0.5)

plot(routes_igraph_pass, vertex.label=nodes$shortName, vertex.size=20,layout = layout_with_graphopt, edge.arrow.size = 0.5)



