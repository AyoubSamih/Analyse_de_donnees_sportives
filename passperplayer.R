library(ggplot2)
library(ggrepel)
eventEngPlayer <- eventEng[,c("playerId","eventName")]
eventEngPlayer <- data.frame(eventName = eventEngPlayer$eventName,
                             playerId = eventEngPlayer$playerId,
                             values = 1)
eventEngPlayer <- eventEngPlayer %>%
  group_by(eventName, playerId) %>%
  summarise(gr_sum = sum(values))

passPlayerEng <- eventEngPlayer[2257:2770,]
playersEng <- players[,c("wyId","shortName","currentTeamId")]
playersEng <- data.frame(currentTeamId = playersEng$currentTeamId,
                         playerId = playersEng$wyId,
                         shortName = playersEng$shortName,
                         values = 1)
playersEng <- playersEng %>%
  group_by(currentTeamId,shortName,playerId) %>%
  summarise(gr_sum = sum(values))
playersEng <- rbind(playersEng[278:853,],playersEng[13:29,])
playersEng <- playersEng %>%
  group_by(playerId,currentTeamId,shortName) %>%
  summarise(gr_sum = sum(gr_sum))
playersEng <- playersEng[,c("playerId","currentTeamId","shortName")]
passEngplayers <- full_join(passPlayerEng,playersEng)
passEngplayers$gr_mean <- passEngplayers$gr_sum/38

ggplot(passEngplayers,
       aes(passEngplayers$shortName,passEngplayers$gr_mean))+
  geom_point()+
  geom_label_repel(aes(label = passEngplayers$shortName),
                   box.padding   = 0.5, 
                   point.padding = 1,
                   segment.color = 'grey50') 
