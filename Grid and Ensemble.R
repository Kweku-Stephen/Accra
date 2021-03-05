###############################################################################################
#faceting Ensemble Prcp, Tmin and Tmax

require(tidyverse)

#Prcp
Prcp_models_60 <- 
data.frame(
  Date = rep(Prcp_models$Date, times = 4), 
  Prcp = unlist(Prcp_models[,1:4]), 
  Model = factor(
    rep(
      c("mpi","noaa", "cnrm","miroc5"), 
      each = nrow(Prcp_models)
      )
    )
  ) %>% 
  rbind(
    Prcp_ensemble
  ) -> un


ggplot(data = un, aes(Date, Prcp)) + 
  geom_line(col = "darkblue", lwd = 0.6) + 
  facet_wrap(.~Model) + 
  geom_smooth(method = "lm", col = "black", se = FALSE) +
  theme(axis.title = element_text(size = 21),
        axis.text = element_text(size = 17.4),
        strip.text.x = element_text(size = 19.5, angle = 360)) +
  labs(y = expression("Rainfall(mm)"), x = "Year")
dev.copy(png, filename = "Prcp_grid_3_parts.png",width = 1450, height = 750)
dev.off()




#Tmax
data.frame(
  Date = rep(Tmax_models$Date, times = 3), 
  Tmax = unlist(Tmax_models[,1:3]), 
  Model = factor(
    rep(
      c("mpi","noaa","miroc5"), 
      each = nrow(Tmax_models)
    )
  )
) %>% 
  rbind(
    Tmax_ensemble
  ) -> un_mx


ggplot(data = un_mx, aes(Date, Tmax)) + 
  geom_line(col = "firebrick") + 
  facet_wrap(.~Model) + 
  geom_smooth(method = "lm", col = "black", se = FALSE) + 
  theme(axis.title = element_text(size = 21),
        axis.text = element_text(size = 17.4),
        strip.text.x = element_text(size = 19.5, angle = 360)) +
  labs(y = expression("Temperature("~degree*c*")"), x = "Year")
dev.copy(png, filename = "Tmax_grid_3parts.png",width = 1450, height = 750)
dev.off()


  
#Tmin
data.frame(
  Date = rep(Tmin_models$Date, times = 4), 
  Tmin = unlist(Tmin_models[,1:4]), 
  Model = factor(
    rep(
      c("mpi","noaa","cnrm","miroc5"), 
      each = nrow(Tmin_models)
    )
  )
) %>% 
  rbind(
    Tmin_ensemble
  ) -> un_mn


ggplot(data = un_mn, aes(Date, Tmin)) + 
  geom_line(col = "firebrick") + 
  facet_wrap(.~Model) + 
  geom_smooth(method = "lm", col = "black", se = FALSE) + 
  theme(axis.title = element_text(size = 21),
        axis.text = element_text(size = 17.4),
        plot.title = element_blank(),
        strip.text.x = element_text(size = 19.5, angle = 360)) +
  labs(y = expression("Temperature("~degree*c*")"), x = "Year")
dev.copy(png, filename = "Tmin_grid_3parts.png",width = 1450, height = 750)
dev.off()



#Rainy Events >= 20mm
head(Accra_proj)
`Acc_proj>=20` <- Accra_proj[Accra_proj$Rain >= 20, c(1,ncol(Accra_proj))]

group_by(
  `Acc_proj>=20`, 
  Date = format(`Acc_proj>=20`[,1], "%Y")) %>% 
  summarise(
    `Rainy Days` = length(
      Rain
      )
    ) %>% 
  sapply(as.numeric) %>% 
  data.frame -> new

  ggplot(data = new,aes(x = Date, y = Rainy.Days)) + 
  geom_line(col = "darkblue", lwd = 1.1) + 
  geom_smooth(method = "lm", col = "black", se = FALSE) +
  theme(axis.title = element_text(size = 21),
        axis.text = element_text(size = 17.4)) + 
  labs(x = "Year", y = "Number of Days") +
  geom_vline(xintercept = c(2018, 2050), lty = 2, size = 1.3, col = "red") +
  annotate("text", x = c(1980, 2027, 2084), y = c(20, 20, 20), 
    label = c(round(mean(new[new$Date  >= 1951 & new$Date < 2018, ][,2], na.rm = T),1),
              round(mean(new[new$Date  >= 2019 & new$Date < 2050, ][,2], na.rm = T),1),
              round(mean(new[new$Date  >= 2051 & new$Date < 2100, ][,2], na.rm = T),1)
    ),
    cex = 10
  )
dev.copy(png, filename = "Prcp_proj20mm.png", width = 1450, height = 750)
dev.off()


#Rainy Days
head(Accra_proj)
`Acc_proj>=0.85` <- Accra_proj[Accra_proj$Rain >= 0.85, c(1,ncol(Accra_proj))]

group_by(
  `Acc_proj>=0.85`, 
  Date = format(`Acc_proj>=0.85`[,1], "%Y")) %>% 
  summarise(
    `Rainy Days` = length(
      Rain
    )
  ) %>% 
  sapply(as.numeric) %>% 
  data.frame -> new_1

  ggplot(data = new_1,aes(x = Date, y = Rainy.Days)) + 
  geom_line(col = "darkblue", lwd = 1.1) + 
  geom_smooth(method = "lm", col = "black", se = FALSE) +
  theme(axis.title = element_text(size = 21),
        axis.text = element_text(size = 17.4)) + 
  labs(x = "Year", y = "Number of Days") +
  geom_vline(xintercept = c(2018, 2050), lty = 2, size = 1.3, col = "red") +
  annotate("text", x = c(1980, 2027, 2084), y = c(20, 20, 20), 
    label = c(round(mean(new_1[new_1$Date  >= 1951 & new_1$Date < 2018, ][,2], na.rm = T),1),
              round(mean(new_1[new_1$Date  >= 2019 & new_1$Date < 2050, ][,2], na.rm = T),1),
              round(mean(new_1[new_1$Date  >= 2051 & new_1$Date < 2100, ][,2], na.rm = T),1)
           ),cex = 10
  )  
dev.copy(png, filename = "Prcp_proj0.85mm.png", width = 1450, height = 750)
dev.off()



#aggregate(
#  format(`Acc_proj>=20`[,1],"%Y") ~ Rain,
#  data = `Acc_proj>=20`,
#  FUN = sum,
#  na.rm = T
#)



#Averages for three periods of the entire 1950-2100
head(Prcp_ensemble)

#vertical.lines <- c(as.Date("2018-12-31"), as.Date("2050-12-31"))
  
ggplot(
  data = Prcp_ensemble,aes(x = Date, y = Prcp)
) + 
  geom_line(col = "darkblue", lwd = 1.3) + 
  geom_smooth(method = "lm", col = "black", se = FALSE) + 
  geom_vline(xintercept = c(
    as.Date("2018-12-31"), as.Date("2050-12-31")
    ), lty = 2, size = 1.3, col = "red") + 
  annotate("text", x = c(
    as.Date("1980-12-31"), as.Date("2027-12-31"), as.Date("2084-12-31")
    ), y = c(1200, 1350, 1300), 
    label = c(round(mean(Prcp_ensemble[Prcp_ensemble$Date >= "1951-12-31" & Prcp_ensemble$Date < "2018-12-31", ]$Prcp, na.rm = T),2),
              round(mean(Prcp_ensemble[Prcp_ensemble$Date >= "2019-12-31" & Prcp_ensemble$Date < "2050-12-31", ]$Prcp, na.rm = T),2),
              round(mean(Prcp_ensemble[Prcp_ensemble$Date >= "2051-12-31" & Prcp_ensemble$Date < "2100-12-31", ]$Prcp, na.rm = T),2)
              ),
    cex = 6
        ) + 
  theme(axis.title = element_text(size = 21),
        axis.text = element_text(size = 19)) +
  labs(y = expression("Rainfall(mm)"), X = "Year")
dev.copy(png, filename = "Prcp_Ensemble_3parts.png",width = 1450, height = 750)
dev.off()



#Tmax
ggplot(
  data = Tmax_ensemble, aes(x = Date, y = Tmax)
) + 
  geom_line(col = "firebrick", lwd = 1.3) +
  geom_smooth(method = "lm", col = "black", se = FALSE) + 
  geom_vline(xintercept = c(
    as.Date("2018-12-31"), as.Date("2050-12-31")
  ), lty = 2, size = 1.3, col = "black") + 
  annotate("text", x = c(
    as.Date("1980-12-31"), as.Date("2027-12-31"), as.Date("2084-12-31")
  ), y = c(32.5, 33, 31.5), 
  label = c(round(mean(Tmax_ensemble[Tmax_ensemble$Date >= "1951-12-31" & Tmax_ensemble$Date < "2018-12-31", ][,2], na.rm = T),0),
            round(mean(Tmax_ensemble[Tmax_ensemble$Date >= "2019-12-31" & Tmax_ensemble$Date < "2050-12-31", ][,2], na.rm = T),0),
            round(mean(Tmax_ensemble[Tmax_ensemble$Date >= "2051-12-31" & Tmax_ensemble$Date < "2100-12-31", ][,2], na.rm = T),1)
  ),
  cex = 10
  ) + 
  theme(axis.title = element_text(size = 21),
        axis.text = element_text(size = 19)) +
  labs(y = expression("Temperature("~degree*c*")"), x = "Year")
dev.copy(png, filename = "Tmax_Ensemble_3parts.png",width = 1450, height = 750)
dev.off()



#Tmin
ggplot(
  data = Tmin_ensemble, aes(x = Date, y = Tmin)
) + 
  geom_line(col = "firebrick", lwd = 1.3) +
  geom_smooth(method = "lm", col = "black", se = FALSE) +
  geom_vline(xintercept = c(
    as.Date("2018-12-31"), as.Date("2050-12-31")
  ), lty = 2, size = 1.3, col = "black") + 
  annotate("text", x = c(
    as.Date("1980-12-31"), as.Date("2027-12-31"), as.Date("2084-12-31")
  ), y = c(25.25, 25.25, 24.5), 
  label = c(round(mean(Tmin_ensemble[Tmin_ensemble$Date >= "1951-12-31" & Tmin_ensemble$Date < "2018-12-31", ][,2], na.rm = T),1),
            round(mean(Tmin_ensemble[Tmin_ensemble$Date >= "2019-12-31" & Tmin_ensemble$Date < "2050-12-31", ][,2], na.rm = T),1),
            round(mean(Tmin_ensemble[Tmin_ensemble$Date >= "2051-12-31" & Tmin_ensemble$Date < "2100-12-31", ][,2], na.rm = T),1)
  ),
  cex = 10
  ) + 
  theme(axis.title = element_text(size = 21),
        axis.text = element_text(size = 19)) + 
  labs(y = expression("Temperature("~degree*c*")"), x = "Year")
dev.copy(png, filename = "Tmin_Ensemble_3parts.png",width = 1450, height = 750)
dev.off()




