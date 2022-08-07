
# Importing New Data (2019 - 2021) into R ####
Accra_newtmin <- readxl::read_excel(
  "C:/Users/pc/Downloads/Accra_data_2019_2021.xlsx",
  sheet = 2
) |> 
  as.data.frame()


Accra_newtmin[ ,1] <- format(
  Accra_newtmin[ ,1], 
  format = "%d-%b"
)

# Reshaping into long format ####
require(magrittr, quietly = TRUE)
Accra_newtmin <- Accra_newtmin |> tidyr::gather(
  key = "Year",
  value = "Tmin",
  2:4
) |> 
  tidyr::unite(
    "Date",
    1:2,
    sep = "-"
  ) |> 
  with(
    data.frame(
      Date = as.Date(Date, format = "%d-%b-%Y"),
      Tmin = Tmin
    )
  )



# Importing Old Accra DAta (1960 - 2018) ####
Accra_Tmin <- read.table(
  "Acc_Tmin_6016_StackedData.txt",
  sep = "\t", 
  header = T,
  na.strings = -99.9
)

Accra_Tmin %<>% with(
  data.frame(
    Date = as.Date(paste(Year, Month, Day, sep = "-")),
    Tmin = Tmin
  )  
)


# Row-binding both old and new into a single dataframe
Accra_Min_Temperature <- rbind(
  Accra_Tmin,
  Accra_newtmin
)



# Mean Annual Minimum Temperature ####
Annual_Min_Temp <- split(
  Accra_Min_Temperature,
  format(Accra_Min_Temperature[ ,1], format = "%Y")
) |> 
  lapply(
    \(data = "") mean(data[ ,2], na.rm = T)
  )  %>% 
  do.call(
    rbind,
    .
  ) |> as.data.frame() %>% 
  data.frame(
    Year = as.numeric(rownames(.)),
    .
  )

ggplot(data = Annual_Min_Temp, aes(x = Year, y = V1)) + 
  geom_line(col = "firebrick", lwd = 1.5) +
  labs(title = "Mean Annual Minimum Temperature of Accra \n 1960-2021", x = "Year", 
       y = expression("Temperature(" *~degree*c*")")) +
  geom_smooth(method = "lm", col = "black") +
  annotate("text", x = 1970, y =  25, label = paste("p-value = ", unclass(Kendall::MannKendall(Annual_Min_Temp[ ,1]))[["sl"]]), size = 8) +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 20))


# Mean Monthly Minimum Temperature ####
assign(
  "Accra_Mean_Monthly_Tmin",
  aggregate(
    Tmin ~ format(Accra_Min_Temperature[ ,"Date"], format = "%B"),
    data = Accra_Min_Temperature,
    FUN = mean,
    na.rm = T
  ) 
)

#Renaming columns of "Accra Monthly Minimum Temperature"
colnames(Accra_Mean_Monthly_Tmin) <- c("Month", "Tmin")

#Rearranging the output above to match the arrangement of the calender months
Accra_Mean_Monthly_Tmin %<>% 
  .[order(match(.[ ,1], month.name)), ]

#Plotting mean monthly minimum Temperature
ggplot(data = Accra_Mean_Monthly_Tmin, aes(x = seq(as.Date("2001-01-01"), by = "month", length.out = 12), y = Tmin)) + 
  geom_line(col = "firebrick", size = 2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Mean Monthly Minimum Temperature of Accra \n 1960 - 2021 ",
       x = "Month",
       y = expression("Temperature("*~degree*C*")")) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))




# Annual Minimum Temperature Anomaly ####
assign(
  "Annual Minimum Temperture Anomaly",
  data.frame(
    Year = Annual_Min_Temp[ ,"Year"],
    Anomaly = (Annual_Min_Temp[ ,"V1"] - mean(Annual_Min_Temp[ ,"V1"], na.rm = T)) / 
      sd(Annual_Min_Temp[ ,"V1"])
  )
)

#Plotting Anomalies
ggplot(data = `Annual Minimum Temperture Anomaly`, aes(x = Year, y = Anomaly, ymin = 0, ymax = Anomaly)) +
  geom_linerange(data = `Annual Minimum Temperture Anomaly`, aes(
    colour = ifelse(Anomaly > 0, "Positive", "Negative")),
    stat = "identity", position = "identity", size = 4) +
  geom_hline(yintercept = 0) +
  labs(title = "Mean Minimum Temperature Anomaly", x = "Year", y = expression("Temperature("*~degree*C*")")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
        axis.title = element_text(size = 28, face = "bold"),
        axis.text = element_text(size = 26),
        legend.title = element_blank(),
        legend.text = element_text(size = 23))

dev.copy(
  png, filename = "Plots_Data/Mean Annual Minimum Temperature Anomaly.png",
  width = 1450,
  height = 850
)
dev.off()



# Seasonal Cycle of Minimum Temperature of Accra ####
assign(
  "Minimum Temperature Seasonal Cycle",
  aggregate(
    Tmin ~ format(Accra_Min_Temperature[ ,"Date"], format = "%m-%d"), 
    data = Accra_Min_Temperature, 
    FUN = mean,
    na.rm = T
  ) %>%
    transform(
      `format(Accra_Min_Temperature[, "Date"], format = "%m-%d")` = as.Date(paste(2000, .[ ,1], sep = "-"))
    )
)

#Renaming the columns of Rainfall Seasonal cycle
colnames(`Minimum Temperature Seasonal Cycle`) <- c("Date", "Tmin")

#Smoother
smoother <- ksmooth(
  1:366,
  `Minimum Temperature Seasonal Cycle`[ ,"Tmin"],
  bandwidth = 10
) |> 
  as.data.frame()

#Plotting Seasonal Cycle of Rainfall
ggplot(data = `Minimum Temperature Seasonal Cycle`, aes(x = Date, y = Tmin)) +
  geom_line(col = "darkblue", size = 2) +
  geom_line(data = smoother, aes(x = `Minimum Temperature Seasonal Cycle`[,1], y = y), col = "red", size = 1.5) +
  scale_x_date(date_breaks = "40 days", date_labels = "%d %b") +
  labs(title = "Minimum Temperature Seasonal Cycle of Accra \n 1960 - 2021", 
       x = "Day",
       y = expression("Temperature("*~degree*C*")")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25),
        legend.title = element_blank(),
        legend.text = element_text(size = 13))

dev.copy(
  png, filename = "Plots_Data/Minimum Temperature Seasonal of Accra.png",
  height = 850, 
  width = 1450
)  
dev.off()




# Dividing into two halves
# Dividing Accra into Two halves ####
# 1960 - 1989
Accra_1960_1990_tn <- Accra_Min_Temperature[Accra_Min_Temperature[ ,"Date"] <= "1990-12-31", ]

# 1990 - 2021
Accra_1991_2021_tn <- Accra_Min_Temperature[Accra_Min_Temperature[ ,"Date"] >= "1991-01-01", ]



# First half Mean Monthly Max Tmp
# Mean Annual Maximum Temperature of Accra (1969-1990) 
Annual_Min_Temp_60_90 <- split(
  dplyr::filter(Accra_Min_Temperature, Date <= "1990-12-31"),
  format(dplyr::filter(Accra_Min_Temperature, Date <= "1990-12-31")[ ,1], format = "%Y")
) |> 
  lapply(
    \(data = "") mean(data[ ,2], na.rm = T)
  )  %>% 
  do.call(
    rbind,
    .
  ) |> as.data.frame() %>% 
  data.frame(
    Year = as.numeric(rownames(.)),
    .
  )

# Mean Monthly Maximum Temperature of Accra (1960 - 1989) ####
`Mean Monthly Min Tmp Accra 60-90` <- split(
  Accra_1960_1990_tn,
  format(Accra_1960_1990_tn[ ,"Date"], "%B")
) %>% 
  lapply(
    FUN = \(data = "") mean(data[ ,"Tmin"], na.rm = T)
  ) %>% 
  do.call(
    rbind,
    .
  ) |> 
  as.data.frame()

#Reordering the above
`Mean Monthly Min Tmp Accra 60-90` <- data.frame(
  Month = rownames(`Mean Monthly Min Tmp Accra 60-90`),
  Prcp = `Mean Monthly Min Tmp Accra 60-90`[ ,1] 
) %>% 
  .[order(match(.[ ,1], month.name)), ]



# SEcond half mean monthly max tmp
# Mean Annual Maximum Temperature of Accra (1991-2021) 
Annual_Min_Temp_91_21 <- split(
  dplyr::filter(Accra_Min_Temperature, Date > "1990-12-31"),
  format(dplyr::filter(Accra_Min_Temperature, Date > "1990-12-31")[ ,1], format = "%Y")
) |> 
  lapply(
    \(data = "") mean(data[ ,2], na.rm = T)
  )  %>% 
  do.call(
    rbind,
    .
  ) |> as.data.frame() %>% 
  data.frame(
    Year = as.numeric(rownames(.)),
    .
  )


# Mean Monthly Maximum Temperature of Accra (1991 - 2021) ####
`Mean Monthly Min Tmp Accra 91-21` <- split(
  Accra_1991_2021_tn,
  format(Accra_1991_2021_tn[ ,"Date"], "%B")
) %>% 
  lapply(
    FUN = \(data = "") mean(data[ ,"Tmin"], na.rm = T)
  ) %>% 
  do.call(
    rbind,
    .
  ) |> 
  as.data.frame()

#Reordering the above
`Mean Monthly Min Tmp Accra 91-21` <- data.frame(
  Month = rownames(`Mean Monthly Min Tmp Accra 91-21`),
  Prcp = `Mean Monthly Min Tmp Accra 91-21`[ ,1] 
) %>% 
  .[order(match(.[ ,1], month.name)), ]
