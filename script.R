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
  xlab('Data') + ylab('Numero di Persone')
                    