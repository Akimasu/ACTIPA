
# Lade Daten
touren <- read_excel("APP-Test Auswertung_3Touren2.xlsx")
#person <- rep(c(1,2,3,4), each = 27)
#tournummer <- rep(rep(c(1,2,3), each = 9),4)
#touren <- cbind(touren, person, tournummer)
#View(touren)  
inter <- read_excel("APP-Test Auswertung_Interaktionen.xlsx")
#View(inter)

library(readxl)         # for reading in Excel data
library(dplyr)          # for data manipulation
library(tidyr)          # for data shaping
library(ggplot2)        # for generating the visualizations


### 1. Auswertungen der Touren
#tdf <- touren %>%
#  group_by(touren$Frage, touren$tournummer) %>%
#  summarise(skala = sum(c(0,8), na.rm = TRUE)))# %>%
 # ungroup() %>%
  #mutate(tour = factor(touren, levels = touren$Frage))


# plot the dot chart
#touren <- as.data.frame.matrix(touren) 
#touren <- arrange(touren, desc(touren$Frage))
VP1 <- (as.numeric(touren$AOVP1)+as.numeric(touren$MARVP1)+as.numeric(touren$MMSVP1))/3
VP1einzel <- cbind(as.numeric(touren$AOVP1),as.numeric(touren$MARVP1),as.numeric(touren$MMSVP1))
VP2 <- (as.numeric(touren$AOVP2)+as.numeric(touren$MARVP2)+as.numeric(touren$MMSVP2))/3
VP2einzel <- cbind(as.numeric(touren$AOVP2),as.numeric(touren$MARVP2),as.numeric(touren$MMSVP2))
VP3 <- (as.numeric(touren$AOVP3)+as.numeric(touren$MARVP3)+as.numeric(touren$MMSVP3))/3
VP3einzel <- cbind(as.numeric(touren$AOVP3),as.numeric(touren$MARVP3),as.numeric(touren$MMSVP3))
VP4 <- (as.numeric(touren$AOVP4)+as.numeric(touren$MARVP4)+as.numeric(touren$MMSVP4))/3
VP4einzel <- cbind(as.numeric(touren$AOVP4),as.numeric(touren$MARVP4),as.numeric(touren$MMSVP4))


AO <- (as.numeric(touren$AOVP1)+as.numeric(touren$AOVP2)+as.numeric(touren$AOVP3)+as.numeric(touren$AOVP4))/4
MAR <- (as.numeric(touren$MARVP1)+as.numeric(touren$MARVP2)+as.numeric(touren$MARVP3)+as.numeric(touren$MARVP4))/4
MMS <- (as.numeric(touren$MMSVP1)+as.numeric(touren$MMSVP2)+as.numeric(touren$MMSVP3)+as.numeric(touren$MMSVP4))/4
  
Skalenbewertung <- touren$Mittelwert

# VPs
Fragestellung <- touren$Frage

ggplot(touren) +
  geom_point(aes(x = Skalenbewertung, y = Fragestellung), col="darkgrey")+ 
  xlab("Skalenbewertung: 1 - stimme gar nicht zu bis 7 - stimme sehr zu") + 
  xlim(c(1,7.5))+
  scale_x_continuous(breaks = seq(1, 7, by = 1))+
  geom_line(aes(x=Skalenbewertung,y=Fragestellung), group=0, col="darkgrey") +
  geom_point(aes(x = VP1, y = touren$Frage), col="#00AFBB") +
  geom_line(aes(x=VP1,y=touren$Frage),group=2, col="#00AFBB") +
  geom_point(aes(x = VP1einzel[,1], y = touren$Frage),alpha=0.3, col="#00AFBB") +
  geom_point(aes(x = VP1einzel[,2], y = touren$Frage),alpha=0.3, col="#00AFBB") +
  geom_point(aes(x = VP1einzel[,3], y = touren$Frage),alpha=0.3, col="#00AFBB") +
  geom_point(aes(x = VP2, y = touren$Frage), col="#293352") +
  geom_line(aes(x=VP2,y=touren$Frage),group=2, col="#293352") +
  geom_point(aes(x = VP2einzel[,1], y = touren$Frage),alpha=0.25, col="#293352") +
  geom_point(aes(x = VP2einzel[,2], y = touren$Frage),alpha=0.25, col="#293352") +
  geom_point(aes(x = VP2einzel[,3], y = touren$Frage),alpha=0.25, col="#293352") +
  geom_point(aes(x = VP3, y = touren$Frage), col="#FC4E07") +
  geom_line(aes(x=VP3,y=touren$Frage),group=2, col="#FC4E07")+ 
  geom_point(aes(x = VP3einzel[,1], y = touren$Frage),alpha=0.25, col="#FC4E07") +
  geom_point(aes(x = VP3einzel[,2], y = touren$Frage),alpha=0.25, col="#FC4E07") +
  geom_point(aes(x = VP3einzel[,3], y = touren$Frage),alpha=0.25, col="#FC4E07") +
  geom_point(aes(x = VP4, y = touren$Frage), col="green") +
  geom_line(aes(x=VP4,y=touren$Frage),group=2, col="green")+ 
  geom_point(aes(x = VP4einzel[,1], y = touren$Frage),alpha=0.25, col="green") +
  geom_point(aes(x = VP4einzel[,2], y = touren$Frage),alpha=0.25, col="green") +
  geom_point(aes(x = VP4einzel[,3], y = touren$Frage),alpha=0.25, col="green") +
  scale_size(guide="none")

  
# Touren
Fragestellung <- touren$Frage

ggplot(touren) +
  geom_point(aes(x = Skalenbewertung, y = Fragestellung, size = 1.3), col="darkgrey")+ 
  xlab("Skalenbewertung: 1 - stimme gar nicht zu bis 7 - stimme sehr zu") + 
  xlim(c(1,7.5))+
  scale_x_continuous(breaks = seq(1, 7, by = 1))+
  geom_line(aes(x=Skalenbewertung,y=Fragestellung, size = 1.2), group=0, col="darkgrey") +
  geom_point(aes(x = AO, y = touren$Frage), col="#00AFBB") +
   geom_line(aes(x=AO,y=touren$Frage),group=1, col="#00AFBB") +
  geom_point(aes(x = MAR, y = touren$Frage), col="#293352") +
   geom_line(aes(x=MAR,y=touren$Frage),group=2, col="#293352") +
  geom_point(aes(x = MMS, y = touren$Frage), col="#FC4E07") +
  geom_line(aes(x=MMS,y=touren$Frage),group=2, col="#FC4E07")+ 
  scale_size(guide="none")
  

