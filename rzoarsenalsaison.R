library(dplyr)
library(jsonlite)
library(igraph)

teamslist <- c(2444,2449,2482,2447,2446,2975,2445,2462,2454,2457,2443,2481,2455,2460,2453,2451,2450,2463)
nbpoints <- c(84,63,55,55,55,53,51,49,47,43,42,41,39,36,36,33,31,22)
eventEng <- fromJSON("/Users/lahlo/Documents/ProjetFoot4A/Soccer 2017-2018/events/events_Germany.json")

for (u in 1:length(teamslist)) {
  

  

  #eventEngmodif <- arrange(eventEng, eventEng$teamId, eventEng$matchId)

  eventEngmodif <- eventEng[eventEng$teamId==teamslist[u],]

  for (i in 1:nrow(eventEngmodif)) {
    if (nrow(eventEngmodif$tags[i][[1]])==0) {
      eventEngmodif$tags[i] <- 0
    }
    else {
      eventEngmodif$tags[i] <- eventEngmodif$tags[i][[1]]$id[nrow(eventEngmodif$tags[i][[1]])]
    }
    print(i)
  }

  for (i in 1:nrow(eventEngmodif)) {
    eventEngmodif$test[i] <- eventEngmodif$tags[i][[1]]
  }

  eventEngmodif <- eventEngmodif[,c("test","playerId","eventName")]
  #eventEngmodif <- select(eventEngmodif,(4),(2),(3))

  tab <- data.frame(test=1,playerId=1,eventName=1)



  for(i in 1:nrow(eventEngmodif)){
    if (eventEngmodif$eventName[i] == "Pass"){
      if (eventEngmodif$test[i]==tab$test[nrow(tab)] & eventEngmodif$playerId[i]==tab$playerId[nrow(tab)] & eventEngmodif$eventName==tab$eventName[nrow(tab)]) {
        tab <- rbind(tab,eventEngmodif[i+1,])
      }
      else {
        tab <- rbind(tab,eventEngmodif[i,],eventEngmodif[i+1,])
      }
    } 
    print(i)
  }


  tab <- tab[-1,]

  dest <-  data.frame(test=1,playerId=1,eventName=1)

  for (i in 1:nrow(tab)) {
    if (!(is.na(tab$eventName[i]))) {      
      if (tab$eventName[i]=="Pass" & tab$test[i]==1801) {
        dest <- rbind(dest,tab[i+1,])
      }
    }
  }

  srcs <-  data.frame(test=1,playerId=1,eventName=1)

  for (i in 1:nrow(tab)) {
    if (!(is.na(tab$eventName[i]))) {
      if (tab$eventName[i]=="Pass" & tab$test[i]==1801) {
        srcs <- rbind(srcs,tab[i,])
      }
    }  
  }

  dest <- dest[-1,]
  srcs <- srcs[-1,]

  edges <- cbind(source=srcs$playerId,destination=dest$playerId)
  edges <- as.data.frame(edges)

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
  playersEng <- arrange(playersEng,playersEng$currentTeamId,playersEng$playerId)

  playersEng <- playersEng[,1:3]

  playersEng <- arrange(playersEng,playerId)


  nodes <- nodes %>% 
    left_join(playersEng, by = c("label" = "playerId")) 



  routes_igraph_pass <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

  #plot(routes_igraph_pass, vertex.label=nodes$shortName, vertex.size=15,layout = layout_with_graphopt, edge.arrow.size = 0.5)


  #tabdensity <- graph.density(routes_igraph_pass)
  #moyenne <- mean(degree(routes_igraph_pass))
  #tabtransitivity <- transitivity(routes_igraph_pass)
  #tabdiameter <- diameter(routes_igraph_pass)
  #nbpoints <- nbpoints[i]
  #medianedegree <- median(degree(routes_igraph_pass))
  #ecartypedegree <- sd(degree(routes_igraph_pass))
  #medianebetw <- median(betweenness(routes_igraph_pass))
  #ecartypebetw <- sd(betweenness(routes_igraph_pass))
  #betweeness <- betweenness(routes_igraph_pass)
  
  tabtest <- data.frame(tabdensity=graph.density(routes_igraph_pass),
                        moyenne=mean(degree(routes_igraph_pass)),
                        tabtransitivity=transitivity(routes_igraph_pass),
                        tabdiameter=diameter(routes_igraph_pass),
                        nbpoints = nbpoints[u],
                        medianedegree = median(degree(routes_igraph_pass)),
                        ecartypedegree = sd(degree(routes_igraph_pass)),
                        medianebetw = median(betweenness(routes_igraph_pass)),
                        ecartypebetw = sd(betweenness(routes_igraph_pass)),
                        betweeness = mean(betweenness(routes_igraph_pass)))


  tabfinal <- rbind(tabfinal,tabtest)
  print("OKKKKKK")
  print(u)
}



