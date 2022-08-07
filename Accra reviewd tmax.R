
# Importing New Data (2019 - 2021) into R ####
Accra_newtmax <- readxl::read_excel(
  "C:/Users/pc/Downloads/Accra_data_2019_2021.xlsx",
  sheet = 1
) |> 
  as.data.frame()
  

Accra_newtmax[ ,1] <- format(
  Accra_newtmax[ ,1], 
  format = "%d-%b"
)

# Reshaping into long format ####
require(magrittr, quietly = TRUE)
Accra_newtmax <- Accra_newtmax |> tidyr::gather(
  key = "Year",
  value = "Tmax",
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
      Tmax = Tmax
    )
  )



# Importing Old Accra DAta (1960 - 2018) ####
Accra_Tmax <- read.table(
  "Acc_Tmax_6016_StackedData.txt",
  sep = "\t", 
  header = T,
  na.strings = -99.9
)

Accra_Tmax %<>% with(
data.frame(
  Date = as.Date(paste(Year, Month, Day, sep = "-")),
  Tmax = Tmax
  )  
)


# Row-binding both old and new into a single dataframe
Accra_Max_Temperature <- rbind(
  Accra_Tmax,
  Accra_newtmax
)



# Mean Annual Maximum Temperature ####
Annual_Max_Temp <- split(
  Accra_Max_Temperature,
  format(Accra_Max_Temperature[ ,1], format = "%Y")
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

ggplot(data = Annual_Max_Temp, aes(x = Year, y = V1)) + 
  geom_line(col = "firebrick", lwd = 1.5) +
  labs(title = "Mean Annual Maximum Temperature of Accra \n 1960-2021", x = "Year", 
       y = expression("Temperature(" *~degree*c*")")) +
  geom_smooth(method = "lm", col = "black") +
  annotate("text", x = 1980, y =  32, label = paste("p-value = ", unclass(Kendall::MannKendall(Annual_Max_Temp[ ,1]))[["sl"]]), size = 8) +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 20))


# Mean Monthly Maximum Temperature ####
assign(
  "Accra_Mean_Monthly_Tmax",
  aggregate(
    Tmax ~ format(Accra_Max_Temperature[ ,"Date"], format = "%B"),
    data = Accra_Max_Temperature,
    FUN = mean,
    na.rm = T
  ) 
)

#Renaming columns of "Accra Monthly Rainfall"
colnames(Accra_Mean_Monthly_Tmax) <- c("Month", "Tmax")

#Rearranging the output above to match the arrangement of the calender months
Accra_Mean_Monthly_Tmax %<>% 
  .[order(match(.[ ,1], month.name)), ]

#Plotting mean monthly Rainfall
ggplot(data = Accra_Mean_Monthly_Tmax, aes(x = seq(as.Date("2001-01-01"), by = "month", length.out = 12), y = Tmax)) + 
  geom_line(col = "firebrick", size = 2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Mean Monthly Maximum Temperature of Accra \n 1960 - 2021 ",
       x = "Month",
       y = expression("Temperature("*~degree*C*")")) + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))



  
# Annual Maximum Temperature Anomaly ####
assign(
  "Annual Maximum Temperture Anomaly",
  data.frame(
    Year = Annual_Max_Temp[ ,"Year"],
    Anomaly = (Annual_Max_Temp[ ,"V1"] - mean(Annual_Max_Temp[ ,"V1"], na.rm = T)) / 
      sd(Annual_Max_Temp[ ,"V1"])
  )
)

#Plotting Anomalies
ggplot(data = `Annual Maximum Temperture Anomaly`, aes(x = Year, y = Anomaly, ymin = 0, ymax = Anomaly)) +
  geom_linerange(data = `Annual Maximum Temperture Anomaly`, aes(
    colour = ifelse(Anomaly > 0, "Positive", "Negative")),
    stat = "identity", position = "identity", size = 4) +
  geom_hline(yintercept = 0) +
  labs(title = "Mean Maximum Temperature Anomaly", x = "Year", y = expression("Temperature("*~degree*C*")")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
        axis.title = element_text(size = 28, face = "bold"),
        axis.text = element_text(size = 26),
        legend.title = element_blank(),
        legend.text = element_text(size = 23))

dev.copy(
  png, filename = "Plots_Data/Mean Annual Maximum Temperature Anomaly.png",
  width = 1450,
  height = 850
)
dev.off()



# Seasonal Cycle of Rainfall in Accra ####
assign(
  "Maximum Temperature Seasonal Cycle",
  aggregate(
    Tmax ~ format(Accra_Max_Temperature[ ,"Date"], format = "%m-%d"), 
    data = Accra_Max_Temperature, 
    FUN = mean,
    na.rm = T
  ) %>%
    transform(
      `format(Accra_Max_Temperature[, "Date"], format = "%m-%d")` = as.Date(paste(2000, .[ ,1], sep = "-"))
    )
)

#Renaming the columns of Rainfall Seasonal cycle
colnames(`Maximum Temperature Seasonal Cycle`) <- c("Date", "Tmax")

#Smoother
smoother_max <- ksmooth(
  1:366,
  `Maximum Temperature Seasonal Cycle`[ ,"Tmax"],
  bandwidth = 10
) |> 
  as.data.frame()

#Plotting Seasonal Cycle of Rainfall
ggplot(data = `Maximum Temperature Seasonal Cycle`, aes(x = Date, y = Tmax)) +
  geom_line(col = "darkblue", size = 2) +
  geom_line(data = smoother_max, aes(x = `Maximum Temperature Seasonal Cycle`[,1], y = y), col = "red", size = 1.5) +
  scale_x_date(date_breaks = "40 days", date_labels = "%d %b") +
  labs(title = "Maximum Temperature Seasonal Cycle of Accra \n 1960 - 2021",
       x = "Day",
       y = expression("Temperature("*~degree*C*")")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25),
        legend.title = element_blank(),
        legend.text = element_text(size = 13))
dev.copy(
  png, filename = "Plots_Data/Maximum Temperature Seasonal of Accra.png",
  height = 850, 
  width = 1450
)  
dev.off()

  
  


# Dividing into two halves
# Dividing Accra into Two halves ####
# 1960 - 1989
Accra_1960_1990_tx <- Accra_Max_Temperature[Accra_Max_Temperature[ ,"Date"] <= "1990-12-31", ]

# 1990 - 2021
Accra_1991_2021_tx <- Accra_Max_Temperature[Accra_Max_Temperature[ ,"Date"] >= "1991-01-01", ]



# First half Mean Monthly Max Tmp
# Mean Annual Maximum Temperature of Accra (1969-1990) 
Annual_Max_Temp_60_90 <- split(
  dplyr::filter(Accra_Max_Temperature, Date <= "1990-12-31"),
  format(dplyr::filter(Accra_Max_Temperature, Date <= "1990-12-31")[ ,1], format = "%Y")
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
`Mean Monthly Max Tmp Accra 60-90` <- split(
  Accra_1960_1990_tx,
  format(Accra_1960_1990_tx[ ,"Date"], "%B")
) %>% 
  lapply(
    FUN = \(data = "") mean(data[ ,"Tmax"], na.rm = T)
  ) %>% 
  do.call(
    rbind,
    .
  ) |> 
  as.data.frame()

#Reordering the above
`Mean Monthly Max Tmp Accra 60-90` <- data.frame(
  Month = rownames(`Mean Monthly Max Tmp Accra 60-90`),
  Prcp = `Mean Monthly Max Tmp Accra 60-90`[ ,1] 
) %>% 
  .[order(match(.[ ,1], month.name)), ]



# SEcond half mean monthly max tmp
# Mean Annual Maximum Temperature of Accra (1991-2021) 
Annual_Max_Temp_91_21 <- split(
  dplyr::filter(Accra_Max_Temperature, Date > "1990-12-31"),
  format(dplyr::filter(Accra_Max_Temperature, Date > "1990-12-31")[ ,1], format = "%Y")
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
`Mean Monthly Max Tmp Accra 91-21` <- split(
  Accra_1991_2021_tx,
  format(Accra_1991_2021_tx[ ,"Date"], "%B")
) %>% 
  lapply(
    FUN = \(data = "") mean(data[ ,"Tmax"], na.rm = T)
  ) %>% 
  do.call(
    rbind,
    .
  ) |> 
  as.data.frame()

#Reordering the above
`Mean Monthly Max Tmp Accra 91-21` <- data.frame(
  Month = rownames(`Mean Monthly Max Tmp Accra 91-21`),
  Prcp = `Mean Monthly Max Tmp Accra 91-21`[ ,1] 
) %>% 
  .[order(match(.[ ,1], month.name)), ]







data.frame(
  Index = "Days with rain event > 0.85mm",
  Period = "60-90 & 91-21",
  `Percentage change` = (mean(`Annual Rainy Days 1991 - 2021`[,2], na.rm = T) - mean(`Annual Rainy Days 1960 - 1990`[,2], na.rm = T))*100/
    mean(`Annual Rainy Days 1991 - 2021`[,2], na.rm = T)
) |> 
  rbind(
    data.frame(
      Index = "Annual Rainy Days with events >= 20mm",
      Period = "60-90 & 91-21",
      `Percentage change` = (mean(`Annual Rainy Days >= 20mm 1991 - 2021`[,2], na.rm = T) - mean(`Annual Rainy Days 1960 - 1990 >= 20`[,2], na.rm = T))*100/
        mean(`Annual Rainy Days >= 20mm 1991 - 2021`[,2], na.rm = T)
    )
  ) |> 
  rbind(
    data.frame(
      Index = "Monthly rainy days with events > 0.85mm",
      Period = "60-90 & 91-21",
      `Percentage change` = (mean(`Monthly Rainy Days of Accra 91-21`[,2], na.rm = T) - mean(`Monthly Rainy Days of Accra 60-90`[,2], na.rm = T))*100/
        mean(`Monthly Rainy Days of Accra 91-21`[,2], na.rm = T)
    )
  ) |> 
  rbind(
    data.frame(
      Index = "Monthly rainy days with events >= 20mm",
      Period = "60-90 & 91-21",
      `Percentage change` = (mean(`Number of Days with Rainfall >= 20mm 91-21`[,2], na.rm = T) - mean(`Monthly Rainy Days of Accra 60-90 >= 20mm`[,2], na.rm = T))*100/
        mean(`Number of Days with Rainfall >= 20mm 91-21`[,2], na.rm = T)
    )
  ) |> 
  write.csv(
    file = "Rainfall Indices Statistics.csv",
    row.names = FALSE
  )
