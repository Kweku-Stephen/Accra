#####################################################################################################
require(tidyverse)
require(xts)


Accra_proj <- read.csv(dir(pattern = "SR"), 
                       header = T, 
                       sep = ",", 
                       na.strings = -99
                       
                       )
Accra_proj[,1] <- as.Date(Accra_proj[,1], format = "%m/%d/%Y")
Accra_proj$Rain <- as.numeric(Accra_proj$Rain)


Accra_proj <- Accra_proj[,c(1:3, 5)]

#Full Projected
Accra_proj_full <- Accra_proj

#replacing time from 1960 to 2018 with gauge Accra data
Accra_proj <- rbind(
  rb,
  Accra_proj[Accra_proj$Date >= "2019-01-01" & Accra_proj$Date <= "2100-12-31", ]
)


##################################################################################################

#Rain
Accra_proj_rr <- xts(Accra_proj$Rain, Accra_proj$Date) %>% 
  apply.monthly(sum, na.rm = T)

Accra_proj_rr <- data.frame(
  Date = index(Accra_proj_rr), 
  Rain = coredata(Accra_proj_rr)
  )

#Three Divisions
Ar_1 <- Accra_proj_rr[
  Accra_proj_rr[,1] >= "1960-01-01" 
  & 
    Accra_proj_rr[,1] <= "1989-12-31", ] %>% 
  rbind(
    data.frame(Date = rep(NA,11),
               Rain = rep(NA,11))
  ) 
Ar_1 <- data.frame(
  Date = seq(as.Date("1990-01-01"), by = "month", length.out = 12),
  Rain  = c(
    sum(subset(Ar_1, format.Date(Ar_1$Date, "%m") == "01")[,2], na.rm = T)/length(1960:1989),
    sum(subset(Ar_1, format.Date(Ar_1$Date, "%m") == "02")[,2], na.rm = T)/length(1960:1989),
    sum(subset(Ar_1, format.Date(Ar_1$Date, "%m") == "03")[,2], na.rm = T)/length(1960:1989),
    sum(subset(Ar_1, format.Date(Ar_1$Date, "%m") == "04")[,2], na.rm = T)/length(1960:1989),
    sum(subset(Ar_1, format.Date(Ar_1$Date, "%m") == "05")[,2], na.rm = T)/length(1960:1989),
    sum(subset(Ar_1, format.Date(Ar_1$Date, "%m") == "06")[,2], na.rm = T)/length(1960:1989),
    sum(subset(Ar_1, format.Date(Ar_1$Date, "%m") == "07")[,2], na.rm = T)/length(1960:1989),
    sum(subset(Ar_1, format.Date(Ar_1$Date, "%m") == "08")[,2], na.rm = T)/length(1960:1989),
    sum(subset(Ar_1, format.Date(Ar_1$Date, "%m") == "09")[,2], na.rm = T)/length(1960:1989),
    sum(subset(Ar_1, format.Date(Ar_1$Date, "%m") == "10")[,2], na.rm = T)/length(1960:1989),
    sum(subset(Ar_1, format.Date(Ar_1$Date, "%m") == "11")[,2], na.rm = T)/length(1960:1989),
    sum(subset(Ar_1, format.Date(Ar_1$Date, "%m") == "12")[,2], na.rm = T)/length(1960:1989)
  )
)



Ar_2 <- Accra_proj_rr[
  Accra_proj_rr[,1] >= "1990-01-01"
  & 
    Accra_proj_rr[,1] <= "2019-12-31", ] %>% 
  rbind(
    data.frame(Date = rep(NA,11),
               Rain = rep(NA,11))
  )

Ar_2 <- data.frame(
  Date = seq(as.Date("1990-01-01"), by = "month", length.out = 12),
  Rain  = c(
    sum(subset(Ar_2, format.Date(Ar_2$Date, "%m") == "01")[,2], na.rm = T)/length(1990:2019),
    sum(subset(Ar_2, format.Date(Ar_2$Date, "%m") == "02")[,2], na.rm = T)/length(1990:2019),
    sum(subset(Ar_2, format.Date(Ar_2$Date, "%m") == "03")[,2], na.rm = T)/length(1990:2019),
    sum(subset(Ar_2, format.Date(Ar_2$Date, "%m") == "04")[,2], na.rm = T)/length(1990:2019),
    sum(subset(Ar_2, format.Date(Ar_2$Date, "%m") == "05")[,2], na.rm = T)/length(1990:2019),
    sum(subset(Ar_2, format.Date(Ar_2$Date, "%m") == "06")[,2], na.rm = T)/length(1990:2019),
    sum(subset(Ar_2, format.Date(Ar_2$Date, "%m") == "07")[,2], na.rm = T)/length(1990:2019),
    sum(subset(Ar_2, format.Date(Ar_2$Date, "%m") == "08")[,2], na.rm = T)/length(1990:2019),
    sum(subset(Ar_2, format.Date(Ar_2$Date, "%m") == "09")[,2], na.rm = T)/length(1990:2019),
    sum(subset(Ar_2, format.Date(Ar_2$Date, "%m") == "10")[,2], na.rm = T)/length(1990:2019),
    sum(subset(Ar_2, format.Date(Ar_2$Date, "%m") == "11")[,2], na.rm = T)/length(1990:2019),
    sum(subset(Ar_2, format.Date(Ar_2$Date, "%m") == "12")[,2], na.rm = T)/length(1990:2019)
  )
)


Ar_3 <- Accra_proj_rr[
  Accra_proj_rr[,1] >= "2020-01-01"
  & 
    Accra_proj_rr[,1] <= "2060-12-31", ]

Ar_3 <- data.frame(
  Date = seq(as.Date("1990-01-01"), by = "month", length.out = 12),
  Rain  = c(
    sum(subset(Ar_3, format.Date(Ar_3$Date, "%m") == "01")[,2], na.rm = T)/length(2020:2060),
    sum(subset(Ar_3, format.Date(Ar_3$Date, "%m") == "02")[,2], na.rm = T)/length(2020:2060),
    sum(subset(Ar_3, format.Date(Ar_3$Date, "%m") == "03")[,2], na.rm = T)/length(2020:2060),
    sum(subset(Ar_3, format.Date(Ar_3$Date, "%m") == "04")[,2], na.rm = T)/length(2020:2060),
    sum(subset(Ar_3, format.Date(Ar_3$Date, "%m") == "05")[,2], na.rm = T)/length(2020:2060),
    sum(subset(Ar_3, format.Date(Ar_3$Date, "%m") == "06")[,2], na.rm = T)/length(2020:2060),
    sum(subset(Ar_3, format.Date(Ar_3$Date, "%m") == "07")[,2], na.rm = T)/length(2020:2060),
    sum(subset(Ar_3, format.Date(Ar_3$Date, "%m") == "08")[,2], na.rm = T)/length(2020:2060),
    sum(subset(Ar_3, format.Date(Ar_3$Date, "%m") == "09")[,2], na.rm = T)/length(2020:2060),
    sum(subset(Ar_3, format.Date(Ar_3$Date, "%m") == "10")[,2], na.rm = T)/length(2020:2060),
    sum(subset(Ar_3, format.Date(Ar_3$Date, "%m") == "11")[,2], na.rm = T)/length(2020:2060),
    sum(subset(Ar_3, format.Date(Ar_3$Date, "%m") == "12")[,2], na.rm = T)/length(2020:2060)
  )
)

ggplot(data = Ar_1, aes(x = Ar_1[,1])) + 
  geom_line(aes(y = Ar_1[,2], col = "1960-1989"), lwd = 1.2) +
  geom_line(aes(y = Ar_2[,2], col = "1990-2019"), lwd = 1.2) +
  geom_line(aes(y = Ar_3[,2], col = "2020-2060"), lwd = 1.2) +
  scale_colour_manual("Legend",
                      values = c("1960-1989" = "red", "1990-2019" = "darkblue",
                                 "2020-2060" = "darkgreen")) +
  ggtitle(label = "") + xlab(label = "Month") + ylab(label = "Rainfall(mm)") +
  theme(axis.text = element_text(size = 19),
        axis.title = element_text(size = 21),
        legend.text = element_text(size = 16),
        legend.title = element_blank()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

dev.copy(png, filename = "Monthly Projected Rainfall.png", height = 750, width = 1450)
dev.off()


#Rainy Days Annual
Acc_rndys <- Accra_proj[Accra_proj$Rain > 0.85, c(1,4)] %>% 
  drop_na()

Acc_rndys  <- group_by(Acc_rndys, format(Acc_rndys$Date, "%Y")) %>% 
  summarise(length = length(Rain)) %>% 
  sapply(as.numeric) %>% 
  data.frame() %>% 
  rename(Date = format.Acc_rndys.Date....Y..)

ggplot(data = Acc_rndys, aes(x = Date, y = length)) + 
  geom_line(col = "darkblue", lwd = 1.2) +
  scale_x_continuous(breaks = seq(1960,2100, by = 20)) +
  labs(title = "", x = "Year", y = "Number of Days") +
  theme(axis.title = element_text(size = 21),
        axis.text = element_text(size = 19))
dev.copy(png, filename = "AnnuAl Rainy Days_Proj.png", height = 750, width = 1450)
dev.off()


  
#Rainy Days Monthly
Acc_rndys_mn <- Accra_proj[Accra_proj$Rain > 0.85, c(1,4)] %>% 
  drop_na()

Acc_rndys_mn  <- group_by(Acc_rndys_mn, format(Acc_rndys_mn$Date, "%m")) %>% 
  summarise(length = length(Rain) / length(1960:2100)) %>% 
  sapply(as.numeric) %>% 
  data.frame() %>% 
  rename(Date = format.Acc_rndys_mn.Date....m..) %>% 
  data.frame()

ggplot(data = Acc_rndys_mn, aes(x = Date, y = length)) + 
  geom_line(col = "darkblue", lwd = 1.2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "", x = "Month", y = "Number of Days") +
  theme(axis.title = element_text(size = 21),
        axis.text = element_text(size = 19))

dev.copy(png, filename = "Monthly Rainy Days_Proj.png", height = 750, width = 1450)
dev.off()



#Anomaly on the annual
Accra_proj_ann <- xts(
  Accra_proj$Rain,Accra_proj$Date
) %>% 
  apply.yearly(sum, na.rm = T)

Acc_proj_rr <- data.frame(
  Date = index(Accra_proj_ann),
  Rain = coredata(Accra_proj_ann)
)
#Anomaly
Acc_proj_rr_60 <- Acc_proj_rr[Acc_proj_rr$Date >= "1960-12-31" & Acc_proj_rr$Date <= "2100-12-31", ]
Acc_anom <- data.frame (Date = seq(as.Date("1960-12-31"), as.Date("2100-12-31"), by = "year") ,
                        Rain = (Acc_proj_rr_60$Rain - mean(Acc_proj_rr_60$Rain)) / sd(Acc_proj_rr_60$Rain)
)


##ggplot
ggplot(Acc_anom, aes(Date, Rain, 
                             ymin = 0,ymax = Rain)) + 
  geom_linerange(data = Acc_anom,
                 aes(colour = ifelse(Rain > 0, "Positive", "Negative")),
                 stat = "identity", position = "identity", size=2) + 
  theme(legend.title = element_text(size = 2, colour = FALSE)) + 
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  geom_vline(xintercept = as.Date("2030-12-31"), lwd = 1) + 
  geom_hline(yintercept = 0) +
  labs(title="Rainfall Total Anomaly (1960-2100)\n Accra",
       x="Year", y="Anomaly") +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 19),
        axis.title = element_text(size = 21),
        legend.text = element_text(size = 16))

dev.copy(png, filename = "AnnuAl Rainfall Anaomaly_Proj.png", height = 750, width = 1450)
dev.off()




####################################################################################################

#Tmax
Accra_proj_mx <- xts(Accra_proj$Tmax, Accra_proj$Date) %>% 
  apply.monthly(mean, na.rm = T)

Accra_proj_mx <- data.frame(
  Date = index(Accra_proj_mx), 
  Tmax = coredata(Accra_proj_mx)
)

#Three Divisions
Tx_1 <- Accra_proj_mx[
  Accra_proj_mx[,1] >= "1960-01-01" 
  & 
    Accra_proj_mx[,1] <= "1989-12-31", ] %>% 
  rbind(
    data.frame(Date = rep(NA,11),
               Tmax = rep(NA,11))
  )

Tx_1 <- data.frame(
  Date = seq(as.Date("1990-01-01"), by = "month", length.out = 12),
  Rain  = c(
    mean(subset(Tx_1, format.Date(Tx_1$Date, "%m") == "01")[,2], na.rm = T),
    mean(subset(Tx_1, format.Date(Tx_1$Date, "%m") == "02")[,2], na.rm = T),
    mean(subset(Tx_1, format.Date(Tx_1$Date, "%m") == "03")[,2], na.rm = T),
    mean(subset(Tx_1, format.Date(Tx_1$Date, "%m") == "04")[,2], na.rm = T),
    mean(subset(Tx_1, format.Date(Tx_1$Date, "%m") == "05")[,2], na.rm = T),
    mean(subset(Tx_1, format.Date(Tx_1$Date, "%m") == "06")[,2], na.rm = T),
    mean(subset(Tx_1, format.Date(Tx_1$Date, "%m") == "07")[,2], na.rm = T),
    mean(subset(Tx_1, format.Date(Tx_1$Date, "%m") == "08")[,2], na.rm = T),
    mean(subset(Tx_1, format.Date(Tx_1$Date, "%m") == "09")[,2], na.rm = T),
    mean(subset(Tx_1, format.Date(Tx_1$Date, "%m") == "10")[,2], na.rm = T),
    mean(subset(Tx_1, format.Date(Tx_1$Date, "%m") == "11")[,2], na.rm = T),
    mean(subset(Tx_1, format.Date(Tx_1$Date, "%m") == "12")[,2], na.rm = T)
  )
)


Tx_2 <- Accra_proj_mx[
  Accra_proj_mx[,1] >= "1990-01-01"
  & 
    Accra_proj_mx[,1] <= "2019-12-31", ] %>% 
  rbind(
    data.frame(Date = rep(NA,11),
               Tmax = rep(NA,11))
  )


Tx_2 <- data.frame(
  Date = seq(as.Date("1990-01-01"), by = "month", length.out = 12),
  Rain  = c(
    mean(subset(Tx_2, format.Date(Tx_2$Date, "%m") == "01")[,2], na.rm = T),
    mean(subset(Tx_2, format.Date(Tx_2$Date, "%m") == "02")[,2], na.rm = T),
    mean(subset(Tx_2, format.Date(Tx_2$Date, "%m") == "03")[,2], na.rm = T),
    mean(subset(Tx_2, format.Date(Tx_2$Date, "%m") == "04")[,2], na.rm = T),
    mean(subset(Tx_2, format.Date(Tx_2$Date, "%m") == "05")[,2], na.rm = T),
    mean(subset(Tx_2, format.Date(Tx_2$Date, "%m") == "06")[,2], na.rm = T),
    mean(subset(Tx_2, format.Date(Tx_2$Date, "%m") == "07")[,2], na.rm = T),
    mean(subset(Tx_2, format.Date(Tx_2$Date, "%m") == "08")[,2], na.rm = T),
    mean(subset(Tx_2, format.Date(Tx_2$Date, "%m") == "09")[,2], na.rm = T),
    mean(subset(Tx_2, format.Date(Tx_2$Date, "%m") == "10")[,2], na.rm = T),
    mean(subset(Tx_2, format.Date(Tx_2$Date, "%m") == "11")[,2], na.rm = T),
    mean(subset(Tx_2, format.Date(Tx_2$Date, "%m") == "12")[,2], na.rm = T)
  )
)


Tx_3 <- Accra_proj_mx[
  Accra_proj_mx[,1] >= "2020-01-01"
  & 
    Accra_proj_mx[,1] <= "2060-12-31", ]


Tx_3 <- data.frame(
  Date = seq(as.Date("1990-01-01"), by = "month", length.out = 12),
  Rain  = c(
    mean(subset(Tx_3, format.Date(Tx_3$Date, "%m") == "01")[,2], na.rm = T),
    mean(subset(Tx_3, format.Date(Tx_3$Date, "%m") == "02")[,2], na.rm = T),
    mean(subset(Tx_3, format.Date(Tx_3$Date, "%m") == "03")[,2], na.rm = T),
    mean(subset(Tx_3, format.Date(Tx_3$Date, "%m") == "04")[,2], na.rm = T),
    mean(subset(Tx_3, format.Date(Tx_3$Date, "%m") == "05")[,2], na.rm = T),
    mean(subset(Tx_3, format.Date(Tx_3$Date, "%m") == "06")[,2], na.rm = T),
    mean(subset(Tx_3, format.Date(Tx_3$Date, "%m") == "07")[,2], na.rm = T),
    mean(subset(Tx_3, format.Date(Tx_3$Date, "%m") == "08")[,2], na.rm = T),
    mean(subset(Tx_3, format.Date(Tx_3$Date, "%m") == "09")[,2], na.rm = T),
    mean(subset(Tx_3, format.Date(Tx_3$Date, "%m") == "10")[,2], na.rm = T),
    mean(subset(Tx_3, format.Date(Tx_3$Date, "%m") == "11")[,2], na.rm = T),
    mean(subset(Tx_3, format.Date(Tx_3$Date, "%m") == "12")[,2], na.rm = T)
  )
)


ggplot(data = Tx_3, aes(x = Tx_3[,1])) + 
  geom_line(aes(y = Tx_1[,2], col = "1960-1989"), lwd = 1.2) +
  geom_line(aes(y = Tx_2[,2], col = "1990-2019"), lwd = 1.2) +
  geom_line(aes(y = Tx_3[,2], col = "2020-2060"), lwd = 1.2) +
  scale_colour_manual("Legend",
                      values = c("1960-1989" = "red", "1990-2019" = "darkblue",
                                 "2020-2060" = "darkgreen")) +
  ggtitle(label = "") + xlab(label = "Month") + 
  ylab(label = expression("Temperature ("~degree*c*")")) +
  theme(axis.text = element_text(size = 19),
        axis.title = element_text(size = 21),
        legend.text = element_text(size = 16),
        legend.title = element_blank()) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b")

dev.copy(png, filename = "Mean Monthly Max Temp_Proj.png", height = 750, width = 1450)
dev.off()



#Anomaly on the annual
Accra_proj_ann_mx <- xts(
  Accra_proj$Tmax,Accra_proj$Date
) %>% 
  apply.yearly(mean, na.rm = T)

Acc_proj_tmax <- data.frame(
  Date = index(Accra_proj_ann_mx),
  Tmax = coredata(Accra_proj_ann_mx)
)
#Anomaly
Acc_anom_mx <- data.frame (Date = Acc_proj_tmax$Date,
                        Tmax = (Acc_proj_tmax$Tmax - mean(Acc_proj_tmax$Tmax)) / 
                          sd(Acc_proj_tmax$Tmax)
)


##ggplot
ggplot(Acc_anom_mx, aes(as.Date(Date), Tmax, 
                     ymin = 0,ymax = Tmax)) + 
  geom_linerange(data = Acc_anom_mx,
                 aes(colour = ifelse(Tmax > 0, "Positive", "Negative")),
                 stat = "identity", position = "identity", size=2) + 
  theme(legend.title = element_text(size = 2, colour = FALSE)) + 
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  geom_vline(xintercept = as.Date("2030-12-31"), lwd = 1) + 
  geom_hline(yintercept = 0) +
  labs(title="Mean Maximum Temperature Anomaly (1960-2100)\n Accra",
       x="Year", y="Anomaly") +
  theme(plot.title = element_text(size = 23, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 19),
        axis.title = element_text(size = 21))

dev.copy(png, filename = "Mean Annual Max Tmp Anomaly_proj.png", height = 750, width = 1450)
dev.off()



###################################################################################################

  
  
#Tmin
Accra_proj_mn <- xts(Accra_proj$Tmin, Accra_proj$Date) %>% 
  apply.monthly(mean, na.rm = T)

Accra_proj_mn <- data.frame(
  Date = index(Accra_proj_mn), 
  Tmin = coredata(Accra_proj_mn)
)

#Three Divisions
Tn_1 <- Accra_proj_mn[
  Accra_proj_mn[,1] >= "1960-01-01" 
  & 
    Accra_proj_mn[,1] <= "1989-12-31", ] %>% 
  rbind(
    data.frame(Date = rep(NA,11),
               Tmin = rep(NA,11))
  )
Tn_1 <- data.frame(
  Date = seq(as.Date("1990-01-01"), by = "month", length.out = 12),
  Rain  = c(
    mean(subset(Tn_1, format.Date(Tn_1$Date, "%m") == "01")[,2], na.rm = T),
    mean(subset(Tn_1, format.Date(Tn_1$Date, "%m") == "02")[,2], na.rm = T),
    mean(subset(Tn_1, format.Date(Tn_1$Date, "%m") == "03")[,2], na.rm = T),
    mean(subset(Tn_1, format.Date(Tn_1$Date, "%m") == "04")[,2], na.rm = T),
    mean(subset(Tn_1, format.Date(Tn_1$Date, "%m") == "05")[,2], na.rm = T),
    mean(subset(Tn_1, format.Date(Tn_1$Date, "%m") == "06")[,2], na.rm = T),
    mean(subset(Tn_1, format.Date(Tn_1$Date, "%m") == "07")[,2], na.rm = T),
    mean(subset(Tn_1, format.Date(Tn_1$Date, "%m") == "08")[,2], na.rm = T),
    mean(subset(Tn_1, format.Date(Tn_1$Date, "%m") == "09")[,2], na.rm = T),
    mean(subset(Tn_1, format.Date(Tn_1$Date, "%m") == "10")[,2], na.rm = T),
    mean(subset(Tn_1, format.Date(Tn_1$Date, "%m") == "11")[,2], na.rm = T),
    mean(subset(Tn_1, format.Date(Tn_1$Date, "%m") == "12")[,2], na.rm = T)
  )
)



Tn_2 <- Accra_proj_mn[
  Accra_proj_mn[,1] >= "1990-01-01"
  & 
    Accra_proj_mn[,1] <= "2019-12-31", ] %>% 
  rbind(
    data.frame(Date = rep(NA,11),
               Tmin = rep(NA,11))
  )
Tn_2 <- data.frame(
  Date = seq(as.Date("1990-01-01"), by = "month", length.out = 12),
  Rain  = c(
    mean(subset(Tn_2, format.Date(Tn_2$Date, "%m") == "01")[,2], na.rm = T),
    mean(subset(Tn_2, format.Date(Tn_2$Date, "%m") == "02")[,2], na.rm = T),
    mean(subset(Tn_2, format.Date(Tn_2$Date, "%m") == "03")[,2], na.rm = T),
    mean(subset(Tn_2, format.Date(Tn_2$Date, "%m") == "04")[,2], na.rm = T),
    mean(subset(Tn_2, format.Date(Tn_2$Date, "%m") == "05")[,2], na.rm = T),
    mean(subset(Tn_2, format.Date(Tn_2$Date, "%m") == "06")[,2], na.rm = T),
    mean(subset(Tn_2, format.Date(Tn_2$Date, "%m") == "07")[,2], na.rm = T),
    mean(subset(Tn_2, format.Date(Tn_2$Date, "%m") == "08")[,2], na.rm = T),
    mean(subset(Tn_2, format.Date(Tn_2$Date, "%m") == "09")[,2], na.rm = T),
    mean(subset(Tn_2, format.Date(Tn_2$Date, "%m") == "10")[,2], na.rm = T),
    mean(subset(Tn_2, format.Date(Tn_2$Date, "%m") == "11")[,2], na.rm = T),
    mean(subset(Tn_2, format.Date(Tn_2$Date, "%m") == "12")[,2], na.rm = T)
  )
)


Tn_3 <- Accra_proj_mn[
  Accra_proj_mn[,1] >= "2020-01-01"
  & 
    Accra_proj_mn[,1] <= "2060-12-31", ]

Tn_3 <- data.frame(
  Date = seq(as.Date("1990-01-01"), by = "month", length.out = 12),
  Rain  = c(
    mean(subset(Tn_3, format.Date(Tn_3$Date, "%m") == "01")[,2], na.rm = T),
    mean(subset(Tn_3, format.Date(Tn_3$Date, "%m") == "02")[,2], na.rm = T),
    mean(subset(Tn_3, format.Date(Tn_3$Date, "%m") == "03")[,2], na.rm = T),
    mean(subset(Tn_3, format.Date(Tn_3$Date, "%m") == "04")[,2], na.rm = T),
    mean(subset(Tn_3, format.Date(Tn_3$Date, "%m") == "05")[,2], na.rm = T),
    mean(subset(Tn_3, format.Date(Tn_3$Date, "%m") == "06")[,2], na.rm = T),
    mean(subset(Tn_3, format.Date(Tn_3$Date, "%m") == "07")[,2], na.rm = T),
    mean(subset(Tn_3, format.Date(Tn_3$Date, "%m") == "08")[,2], na.rm = T),
    mean(subset(Tn_3, format.Date(Tn_3$Date, "%m") == "09")[,2], na.rm = T),
    mean(subset(Tn_3, format.Date(Tn_3$Date, "%m") == "10")[,2], na.rm = T),
    mean(subset(Tn_3, format.Date(Tn_3$Date, "%m") == "11")[,2], na.rm = T),
    mean(subset(Tn_3, format.Date(Tn_3$Date, "%m") == "12")[,2], na.rm = T)
  )
)

ggplot(data = Tn_1, aes(x = Tn_1[,1])) + 
  geom_line(aes(y = Tn_1[,2], col = "1960-1989"), lwd = 1.2) +
  geom_line(aes(y = Tn_2[,2], col = "1990-2019"), lwd = 1.2) +
  geom_line(aes(y = Tn_3[,2], col = "2020-2060"), lwd = 1.2) +
  scale_colour_manual("Legend",
                      values = c("1960-1989" = "red", "1990-2019" = "darkblue",
                                 "2020-2060" = "darkgreen")) +
  ggtitle(label = "") + xlab(label = "Month") + 
  ylab(label = expression("Temperature ("~degree*c*")")) +
  theme(axis.text = element_text(size = 21),
        axis.title = element_text(size = 19),
        legend.text = element_text(size = 16),
        legend.title = element_blank()) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month")

dev.copy(png, filename = "Mean Monthly Min Tmp_proj.png", height = 750, width = 1450)
dev.off()


  
#Anomaly  
Accra_proj_ann_mn <- xts(
  Accra_proj$Tmin,Accra_proj$Date
) %>% 
  apply.yearly(mean, na.rm = T)

Acc_proj_tmin <- data.frame(
  Date = index(Accra_proj_ann_mn),
  Tmin = coredata(Accra_proj_ann_mn)
)
#Anomaly
Acc_anom_mn <- data.frame (Date = Acc_proj_tmin$Date,
                           Tmin = (Acc_proj_tmin$Tmin - mean(Acc_proj_tmin$Tmin)) / 
                             sd(Acc_proj_tmin$Tmin)
)


##ggplot
ggplot(Acc_anom_mn, aes(as.Date(Date), Tmin, 
                        ymin = 0,ymax = Tmin)) + 
  geom_linerange(data = Acc_anom_mn,
                 aes(colour = ifelse( Tmin> 0, "Positive", "Negative")),
                 stat = "identity", position = "identity", size=2) + 
  theme(legend.title = element_text(size = 2, colour = FALSE)) + 
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  geom_vline(xintercept = as.Date("2030-12-31"), lwd = 1) + 
  geom_hline(yintercept = 0) +
  labs(title="Mean Minimum Temperature Anomaly (1960-2100)\n Accra",
       x="Year", y="Anomaly") +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17))

dev.copy(png, filename = "Mean Annual Min Tmp Anomaly_proj.png", height = 750, width = 1450)
dev.off()



#################################################################################################
#Full projected Data
head(Accra_proj_full)

#Prcp
Accra_proj_full_rain <- xts(
  Accra_proj_full$Rain,Accra_proj_full$Date
) %>% 
  apply.yearly(sum, na.rm = T)

Acc_proj_full_rr <- data.frame(
  Date = index(Accra_proj_full_rain),
  Rain = coredata(Accra_proj_full_rain)
  )
  
  
ggplot(Acc_proj_full_rr , aes(as.Date(Date),Rain)) + 
  geom_line(col = "darkblue", lwd = 1.2) + 
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  labs(title="Rainfall Total (1951-2100)\n Accra",
       x="Year", y="Rainfall (mm)") +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17))
  
dev.copy(png, filename = "Default_Rainfall_projection.png", height = 750, width = 1450)
dev.off()
  

#Tmax
Accra_proj_full_Tmax <- xts(
  Accra_proj_full$Tmax,Accra_proj_full$Date
) %>% 
  apply.yearly(mean, na.rm = T)

Acc_proj_full_mx <- data.frame(
  Date = index(Accra_proj_full_Tmax),
  Tmax = coredata(Accra_proj_full_Tmax)
)


ggplot(Acc_proj_full_mx , aes(as.Date(Date),Tmax)) + 
  geom_line(col = "firebrick", lwd = 1.2) + 
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  labs(title="Mean Maximum Temperature (1951-2100)\n Accra",
       x="Year", y=expression("Temperature("~degree*C*")")) +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17))

dev.copy(png, filename = "Default_Max Temperature_projection.png", height = 750, width = 1450)
dev.off()



#Tmax
Accra_proj_full_Tmin <- xts(
  Accra_proj_full$Tmin,Accra_proj_full$Date
) %>% 
  apply.yearly(mean, na.rm = T)

Acc_proj_full_mn <- data.frame(
  Date = index(Accra_proj_full_Tmin),
  Tmin = coredata(Accra_proj_full_Tmin)
)


ggplot(Acc_proj_full_mn , aes(as.Date(Date),Tmin)) + 
  geom_line(col = "firebrick", lwd = 1.2) + 
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  labs(title="Mean Minimum Temperature (1951-2100)\n Accra",
       x="Year", y=expression("Temperature("~degree*C*")")) +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17))

dev.copy(png, filename = "Default_Min Temperature_projection.png", height = 750, width = 1450)
dev.off()







