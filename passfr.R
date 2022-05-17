library(dplyr)



tablepass <- data.frame(eventName = eventFrmodif$eventName,
                        teamId = eventFrmodif$teamId,
                        values = 1)


tablepass <- tablepass %>%
  group_by(eventName,teamId) %>%
  summarise(gr_sum = sum(values))



tablepass$gr_mean <- tablepass$gr_sum/38

tablepassmean <- tablepass[141:160,]
tablepassmean <- tablepassmean
```


teamsmodif <- teams[,c("name","wyId","area")]
datateams <- data.frame(teamsmodif$name,teamsmodif$wyId,teamsmodif$area$name,values=1)
colnames(datateams) <- c("name","teamId","area","values")


datateams <- datateams %>%
  group_by(teamId,area,name) %>%
  summarise(sum(values))

datateamsFr <- rbind(datateams[82:100,],datateams[142,])
datateamsFr <- datateamsFr[,c("teamId","name")]

test <- full_join(tablepassmean,datateamsFr)

ggplot(test,
       aes(test$name,test$gr_mean))+
  geom_point()+
  geom_label_repel(aes(label = test$name),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic()
