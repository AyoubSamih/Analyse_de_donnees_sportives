eventFrPlayer <- eventFr[,c("playerId","eventName")]
eventFrPlayer <- data.frame(eventName = eventFrPlayer$eventName,
                             playerId = eventFrPlayer$playerId,
                             values = 1)
eventFrPlayer <- eventFrPlayer %>%
  group_by(eventName, playerId) %>%
  summarise(gr_sum = sum(values))

passPlayerFr <- eventFrPlayer[2257:2770,]
playersFr <- players[,c("wyId","shortName","currentTeamId")]
playersFr <- data.frame(currentTeamId = playersFr$currentTeamId,
                         playerId = playersFr$wyId,
                         shortName = playersFr$shortName,
                         values = 1)
playersFr <- playersFr %>%
  group_by(currentTeamId,shortName,playerId) %>%
  summarise(gr_sum = sum(values))
playersFr <- rbind(playersFr[2247:3804,],playersFr[887:910,])
playersFr <- playersFr %>%
  group_by(playerId,currentTeamId,shortName) %>%
  summarise(gr_sum = sum(gr_sum))
playersFr <- playersFr[,c("playerId","currentTeamId","shortName")]
passFrplayers <- full_join(passPlayerFr,playersFr)
passFrplayers$gr_mean <- passFrplayers$gr_sum/38

ggplot(passFrplayers,
       aes(passFrplayers$shortName,passFrplayers$gr_mean))+
  geom_point()+
  geom_text_repel(aes(label = passFrplayers$shortName),
                  box.padding   = 0.5, 
                  point.padding = 1,
                  segment.color = 'grey50') 
