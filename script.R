data <- read.csv('./dpc-covid19-ita-andamento-nazionale.csv')
newdata <- data[160:251, c('data', 'nuovi_positivi')]
newdata$time <- newdata$data %>% str_replace("2020-","")
newdata$time <- newdata$time %>% str_replace("T17:00:00","")
newdata$nuovi_positivi_2021 <- data[525:616, 'nuovi_positivi']
library(ggplot2)
library(stringr)
melted<-reshape2::melt(newdata[,c(2,3,4)], id="time")
ggplot(melted, aes(x=time, y=value, group=variable, color=variable))+
  geom_line()+ scale_color_discrete(name="Nuovi Positivi", labels=c('2020','2021'))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = c(0.1,0.85))+
  xlab('Data') + ylab('Numero di Persone')+
  scale_x_discrete(breaks=newdata$time[c(1,15,31,46,62,76,92)])+
  scale_y_continuous(breaks=c(0,2500,5000,7500,10000,12500,15000,
                            17500,20000,22500,25000,27500,30000,
                            302500))
for (x in 2:616){
data[x,'tamponi_effettuati'] <- data[x,'tamponi']-data[x-1,'tamponi']
}
data$pos.on.tamp <- round(data$nuovi_positivi/data$tamponi_effettuati*100,3)
pos.on.tamp <- data[160:251, c('data','pos.on.tamp')]
pos.on.tamp$time <- pos.on.tamp$data %>% str_replace("2020-","")
pos.on.tamp$time <- pos.on.tamp$time %>% str_replace("T17:00:00","")
pos.on.tamp$pos.on.tamp.2021 <- data[525:616, 'pos.on.tamp']
ptmelted<-reshape2::melt(pos.on.tamp[,c(2,3,4)], id="time")
ggplot(ptmelted, aes(x=time, y=value, group=variable, color=variable))+
  geom_line()+scale_color_discrete(name="% Positivi/Tamponi", labels=c('2020','2021'))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = c(0.1,0.85))+
  xlab('Data') + ylab('% Positivi/Tamponi')+
  scale_x_discrete(breaks=newdata$time[c(1,15,31,46,62,76,92)])
ggsave('./perc_posontamp.png', device = 'png', units = 'cm', height = 13, width = 23)
