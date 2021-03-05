################################################################################
head(Accra_seas)

Acc_seas <- subset(
  dta,
  format.Date(
    dta$date,
    "%m %d"
    ) =="06 09"
  )

Acc_seas$Cat <- ifelse(
  Acc_seas[,2] == 0, 
  -1, 
  1
)
  

Acc_seas$occu <- ifelse(
  Acc_seas[,2] == 0, 
  0, 
  1
)

cumsum(Acc_seas[,4]) -> Acc_seas$Cumsum


Acc_seas$Activity <- as.factor(ifelse(
  Acc_seas[,4] == 0,
  "inactive",
  "active"
))

ggplot(Acc_seas, aes(date,Cumsum, col=Activity)) +
  geom_point()


ggplot(Acc_seas, aes(date, Cat, 
                             ymin = 0,ymax = Cat)) + 
  geom_linerange(data = Acc_seas,
                 aes(colour = ifelse(Cat >0, "Positive", "Negative")),
                 stat = "identity", position = "identity",size=2) + 
  theme(legend.title = element_text(size = 2, colour = F)) + 
  scale_x_date(limits = as.Date(c("1960-06-09","2018-06-09")),
               minor_breaks = as.Date(c("1960-06-09", "1980-06-09",
                                        "2005-06-09", "2010-06-09","2018-06-09"))) +
  geom_hline(yintercept=0) + 
  labs(title="Rainy Events on June 9th (1960-2018)\n Accra",
       x="Year", y="Anomaly") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(
  png, filename="Rainy Events on June 9th.png",
  width=1450, height=750
)
dev.off()


ggplot(
  Acc_seas, aes(date,Prcp) 
) + geom_line(col="darkblue", lwd=2) + 
  geom_smooth(method = "loess", col="red", lwd=2)





#########################
Acc_seas_anom <- data.frame(
  Date=Acc_seas[,1],
  Anom=(Acc_seas[,2] - mean(Acc_seas[,2])) / sd(Acc_seas[,2])
)

ggplot(Acc_seas_anom, aes(Date, Anom, 
                     ymin = 0,ymax = Anom)) + 
  geom_linerange(data = Acc_seas_anom,
                 aes(colour = ifelse(Anom >0, "Positive", "Negative")),
                 stat = "identity", position = "identity",size=2) + 
  theme(legend.title = element_text(size = 2, colour = F)) + 
  scale_x_date(limits = as.Date(c("1960-06-09","2018-06-09")),
               minor_breaks = as.Date(c("1960-06-09", "1980-06-09",
                                        "2005-06-09", "2010-06-09","2018-06-09"))) +
  geom_hline(yintercept=0) + 
  labs(title="Rainfall on June 9th (1960-2018)\n Accra",
       x="Year", y="Anomaly") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))


dev.copy(
  png, filename="Rainfall on June 9th.png",
  width=1450, height=750
)
dev.off()












