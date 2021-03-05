
#Annuals
grid.arrange(
             ggplot(Prcp.df.ann, aes(x=date,y=Prcp)) +
               geom_line(col="darkblue", lwd=2) +
               labs(title = "(A)",subtitle = "Rainfall Total (1960-2018) - Accra",
                    x="",y="Rainfall (mm)") +
               geom_smooth(method=lm, col="red",se = FALSE) +
               scale_x_date(date_breaks = "4 years",date_labels = "%Y") +
               annotate("text", x=Prcp.df.ann[Prcp.df.ann[,1] == "1978-12-31", ]$date,
                        y=1200, label="P-value = 0.40995", cex=10) +
               theme(plot.subtitle = element_text(hjust = 0.5, size = 26, face = "bold"),
                     axis.title = element_text(size = 24, face = "bold"),
                     axis.text.y = element_text(size = 22, face = "bold"),
                     axis.title.x = element_blank(),
                     plot.title = element_text(face = "bold", size = 17),
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank()),
             
             ggplot(Tmax.ann.df, aes(x=date, y=Tmax)) + geom_line(col="firebrick", lwd=2) + 
               geom_smooth(method = lm, se = FALSE) +  
               labs(title = "(B)",subtitle="Maximum Temperature (1960-2018) - Accra",
                    x="Year", y=expression("Temperature("*~degree*c*")")) + 
               scale_x_date(date_breaks= "4 years", date_labels = "%Y") + 
               annotate("text", x=Tmax.ann.df[Tmax.ann.df[,1] == "1982-12-31", ]$date, 
                        y=32, label="P-value=< 2.22e-16", cex=10) +
               theme(plot.subtitle = element_text(size = 26, hjust = 0.5, face = "bold"),
                     axis.text.y = element_text(size = 22, face = "bold"),
                     axis.title.x = element_blank(),
                     axis.title.y.left = element_text(size = 24),
                     plot.title = element_text(face = "bold", size = 17),
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank()),
             
             ggplot(Tmin.ann.df, aes(x=date, y=Tmin)) + 
               geom_line(col="firebrick", lwd=2) + 
               geom_smooth(method = lm, col="blue", se = FALSE) +  
               labs(title = "(C)",subtitle="Minimum Temperature (1960-2018) - Accra",
                    x="Year", y=expression("Temperature("*~degree*c*")")) + 
               scale_x_date(date_breaks= "6 years", date_labels = "%Y") + 
               annotate("text", x = Tmin.ann.df[Tmin.ann.df$date == "1996-12-31", ]$date,
                        y = 23, label="P-value = 2.22e-16", cex = 10) +
               theme(plot.subtitle = element_text(size = 26, hjust = 0.5, face = "bold"),
                     axis.text = element_text(size = 23, face = "bold"),
                     axis.title = element_text(size = 24, face = "bold"),
                     plot.title = element_text(face = "bold", size = 17))
)
dev.copy(png, filename="Ann.png", 
         width=1600, height=1000)
dev.off()


#Anomalies
grid.arrange(
  ggplot(Prcp.df.ann.anom, aes(date.ann.1, Prcp.df.ann.anom.v, 
                               ymin = 0,ymax = Prcp.df.ann.anom.v)) + 
    geom_linerange(data = Prcp.df.ann.anom,
                   aes(colour = ifelse(Prcp.df.ann.anom.v > 0, "Positive", "Negative")),
                   stat = "identity", position = "identity", size=2) + 
    theme(legend.title = element_text(size = 2, colour = FALSE)) + 
    scale_x_date(limits = as.Date(c("1960-12-31","2018-12-31")),
                 minor_breaks = as.Date(c("1970-12-31", "1980-12-31",
                                          "2005-12-31", "2010-12-31","2018-12-31"))) +
    geom_hline(yintercept=0, size = 1) + 
    labs(title = "(A)",subtitle="Rainfall Total Anomaly (1960-2018) - Accra",
         x="Year", y="Anomaly") +
    theme(plot.subtitle = element_text(size = 26, hjust = 0.5, face = "bold"),
          plot.title = element_text(size = 17, face = "bold"),
          axis.text.x  = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y.left = element_text(size = 24, face = "bold"),
          axis.text = element_text(size = 23, face = "bold"),
          legend.position = "none",
          axis.ticks.x = element_blank()),
  
  ggplot(Tmax.ann.df.anom, aes(Tmax.ann.df$date, Tmax.ann.df.anom.v, ymin = 0,
                               ymax = Tmax.ann.df.anom.v)) + 
    geom_linerange(data = Tmax.ann.df.anom, aes(colour = ifelse(Tmax.ann.df.anom.v >0, 
              "Positive", "Negative")),stat = "identity", position = "identity",size=2) + 
    geom_hline(yintercept=0, size = 1) +
    theme(legend.title = element_text(size = 2, colour = FALSE)) + 
    (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +geom_hline(yintercept=0) + 
    labs(title = "(B)",subtitle="Maximum Temperature Anomaly (1960-2018) - Accra", 
         x="Year", y="Anomaly") +
    theme(plot.subtitle = element_text(size = 26, hjust = 0.5, face = "bold"),
          plot.title = element_text(size = 17, face = "bold"),
          axis.text.x  = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y.left = element_text(size = 24, face = "bold"),
          axis.text = element_text(size = 23, face = "bold"),
          legend.position = "none",
          axis.ticks.x = element_blank()),
  
  ggplot(Tmin.ann.df.anom, aes(Tmin.ann.df.1$date, Tmin.ann.df.anom.v,
                               ymin = 0,ymax = Tmin.ann.df.anom.v)) + 
    geom_linerange(data = Tmin.ann.df.anom, aes(colour = ifelse(Tmin.ann.df.anom.v >0,
              "Positive", "Negative")),stat = "identity",position = "identity",size=2) + 
    (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +
    geom_hline(yintercept=0, size = 1) + 
    labs(title = "(C)",subtitle=" Minimum Temperature Anomaly (1960-2018) - Accra",
         x="Year", y="Anomaly") +
    theme(plot.subtitle = element_text(size = 26, hjust = 0.5, face = "bold"),
          plot.title = element_text(size = 17, face = "bold"),
          axis.title.x = element_text(size = 24,face = "bold"),
          axis.title.y.left = element_text(size = 24, face = "bold"),
          axis.text = element_text(size = 23, face = "bold"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 27,face = "bold"))
  
)
dev.copy(png, filename="Annoms.png", 
         width=1900, height=900)
dev.off()
         

#ggplot(
#  data = anoms, aes(x = Date, y = Anomaly, ymin = 0, ymax = Anomaly)) + 
#  geom_linerange(data = anoms, 
#                aes(colour = ifelse(Anomaly > 0, "Positive","Negative")),
#                 stat = "identity", position = "identity", size = 2) +
#  facet_grid(Status~.) + 
#  geom_hline(yintercept=0, size = 1) + 
#  labs(title=" (A)                                                                                     (B)                                                                                    (C)",
#       x="Year", y="Anomaly") +
#  scale_x_date(date_labels = "%Y",
#               limits = as.Date(c("1960-12-31","2020-12-31")),
#               minor_breaks = as.Date(c("1990-12-31")) ) +
#  theme(axis.text = element_text(size = 22, face = "bold"),
#        axis.title = element_text(size = 22, face = c("bold")),
#        strip.text.x = element_text(size = 23, angle = 360, face = "bold"),
#        legend.title = element_blank(),
#        legend.text = element_text(size = 20, face = c("italic")),
#        plot.title = element_text(hjust = 0, face = "bold", size = 24),
#        legend.position = "bottom")
#dev.copy(png, filename="Annoms.png", 
#         width=1900, height=900)
#dev.off()



#Monthlies
grid.arrange(
  ggplot(mean.mn.Prcpfh,aes(x=mean.mn.Prcpfh[,1])) + 
    geom_line(aes(y=mean.mn.Prcpfh[,2], col="1960-1989"), lwd= 2) +
    geom_line(aes(y=mean.mn.Prcpsh[,2],col="1990-2018"), lwd=2) +
    scale_colour_manual("Legend",
                        values = c("1960-1989"="darkblue", "1990-2018"="red")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + xlab("Month") +
    ylab("Rainfall(mm)") + labs(title = "(A)",subtitle = "Mean Monthly Rainfall (1960-2018) - Accra")+
    theme(plot.subtitle = element_text(size = 26, hjust = 0.5, face = "bold"),
          plot.title = element_text(face = "bold", size = 17),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 23, face = "bold"),
          axis.text.y = element_text(size = 22, face = "bold"),
          legend.position = "none",
          axis.ticks.x = element_blank()),
  
  ggplot(mean.mn.Tmaxfh,aes(x=mean.mn.Tmaxfh[,1])) + 
    geom_line(aes(y=mean.mn.Tmaxfh[,2],col="1960-1989"),lwd= 2) +
    geom_line(aes(y=mean.mn.Tmaxsh[,2],col="1990-2018"), lwd=2) + 
    scale_colour_manual("Time Period",
                        values=c("1960-1989"="darkblue","1990-2018"="red")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + xlab("Month") +
    ylab(yyy) + labs(title = "(B)",subtitle = "Mean Monthly Maximum Temperature (1960-2018) -  Accra") +
    theme(plot.subtitle = element_text(size = 26, hjust = 0.5, face = "bold"),
          plot.title = element_text(face = "bold", size = 17),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y.left = element_text(size = 23, face = "bold"),
          axis.text.y = element_text(size = 22, face = "bold"),
          legend.position = "none",
          axis.ticks.x = element_blank()),
  
  ggplot(mean.mn.Tminfh,aes(x=mean.mn.Tminfh[,1])) + 
    geom_line(aes(y=mean.mn.Tminfh[,2],col="1960-1989"), lwd= 2) +
    geom_line(aes(y=mean.mn.Tminsh[,2],col="1990-2018"), lwd=2) + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    scale_colour_manual("Legend:",
                        values = c("1960-1989"="darkblue","1990-2018"="red")) +
    xlab("Month") +
    ylab(yyy) + labs(title = "(C)",subtitle = "Mean Monthly Minimum Temperature (1960-2018) - Accra") +
    theme(plot.subtitle = element_text(size = 26, hjust = 0.5, face = "bold"),
          plot.title = element_text(face = "bold", size = 17),
          axis.title = element_text(size = 23,face = "bold"),
          axis.text = element_text(size = 22, face = "bold"),
          legend.position = "bottom",
          legend.text = element_text(size = 23.7, face = "bold"),
          legend.title = element_text(size = 25, face = "bold"))
)
dev.copy(png, filename="Months.png", 
         width=1200, height=950)
dev.off()


#Rainy Days
ggplot(Ann.rndys, aes(x=date.ann, y=Ann.rndys.v)) + 
  geom_line(col="darkblue", lwd=2) + 
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") + 
  labs(title="Number of Days when Rainfall >= 0.85mm (1960-2018)\n Accra", 
       x="Year",y="Day") + 
  geom_smooth(method = lm, col="red", se = FALSE) + 
  annotate("text", x=Ann.rndys[Ann.rndys$date.ann == "1977-01-01", ]$date.ann, 
           y=90, label="P-value=0.97388", cex=10) +
  theme(plot.title = element_text(size = 26, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 24, face = "bold"))

dev.copy(png, filename="Annual Rainy Days of Accra (1960-2018).png", 
         width=1500, height=900)
dev.off()

























