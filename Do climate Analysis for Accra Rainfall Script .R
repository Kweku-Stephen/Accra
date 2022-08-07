# ###################################################################################################### ####
# Requiring the following packages ####
sapply(
  c("dplyr", "magrittr", "patchwork", "ggplot2", "tidyr"),
  require,
  character.only = TRUE
)


# Data Import and cleaning #### 
#Data Import into a Data frame
Accra_update <- readxl::read_excel(
  "C:/Users/pc/Downloads/Accra Rainfall Kofi Asare.xlsx"
) |> 
  #tidyr::drop_na() |> 
  as.data.frame()

# #formating column 1
# Accra_update[ ,1] <- strsplit(
#   Accra_update[ ,1],
#   " "
# )

#Formatting:Alternative 2
require(magrittr, quietly = TRUE)
Accra_update %<>% tidyr::separate(
  ...1,
  c("Day", "Months"),
  sep = " "
) 

Accra_update[ ,"Day"] <- gsub(
  "[A-Za-z]",
  "",
  Accra_update[ ,"Day"],
  ignore.case = TRUE
)

#For Date Formatting
attach(Accra_update)
Accra_update[ ,"Months"] <- rep(
  month.abb,
  time = c(
    length(Months[Months %in% "JAN"]),
    length(Months[Months %in% "FEB"]),
    length(Months[Months %in% "MAR"]),
    length(Months[Months %in% "APRIL"]),
    length(Months[Months %in% "MAY"]),
    length(Months[Months %in% "JUN"]),
    length(Months[Months %in% "JULY"]),
    length(Months[Months %in% "AUG"]),
    length(Months[Months %in% "SEPT"]),
    length(Months[Months %in% "OCT"]),
    length(Months[Months %in% "NOV"]),
    length(Months[Months %in% "DEC"])
  )
)

detach(Accra_update)

# Data Reshaping
Accra_update %<>% tidyr::gather(
  key = "Year",
  value = "Prcp",
  3:ncol(Accra_update)
) %>% 
  tidyr::unite(
    "Date",
    Day, Months, Year,
    sep = "-"
  )

#Converting the variable "Prcp" to numeric
Accra_update[ ,"Prcp"] <- as.numeric(Accra_update[ ,"Prcp"])

#Converting Date of class character to Date Class
Accra_update[ ,"Date"] <- as.Date(
  Accra_update[ ,"Date"], 
  format = "%d-%b-%Y"
)

#Appending Data from 1960 to 2018 to Accra_update's 2019-2021
Accra_old <- read.csv(
  "C:/Users/pc/Downloads/Accra data.csv",
  header = TRUE,
  sep = ","
)

#Converting Accra_old Date-like object to Date Class
Accra_old[ ,"Date"] <- as.Date(
  Accra_old[ ,"Date"],
  format = "%m/%d/%Y"
)

#Joining the Datasets by row
Accra <- rbind(
  Accra_old[ ,c("Date", "Prcp")],
  Accra_update[Accra_update[ ,"Date"] >= "2019-01-01", ]
)


#Annual Rainfall Total of Accra
# split(
#   Accra,
#   format(Accra[ ,"Date"], "%Y")
# ) %>% 
#   lapply(
#     \(data = "") sum(data[ ,"Prcp"], na.rm = T)
#   ) %>% 
#   do.call(
#     rbind,
#     .
#   ) %>% 
#   assign(
#     .,
#     "Accra_Total_Rainfall"
#   )



# Annual Rainfall total of Accra ####
assign(
  "Accra Rainfall Total",
  aggregate(
    Prcp ~ format(Accra[ ,"Date"], format = "%Y"),
    Accra,
    FUN = sum, 
    na.rm = T
  ) |> 
    transform(
      `format(Accra[, "Date"], format = "%Y")` = as.numeric(`format(Accra[, "Date"], format = "%Y")`)
    )
)

#Renaming the columns of Accra
colnames(`Accra Rainfall Total`) <- c("Year", "Prcp")

# Directory for storing plots and datasets ####
if (!dir.exists("Plots_Data")) dir.create("Plots_Data")


#Plotting
require(ggplot2)
ggplot(data = `Accra Rainfall Total`, aes(x = Year, y = Prcp)) +
  geom_line(col = "darkblue", size = 2) +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Rainfall Total of Accra \n 1960 - 2021", 
       x = "Year", 
       y = "Rainfall (mm)") +
  annotate("text", x = 2011, y = 1350, label = paste(
    'p-value = ', round((Kendall::MannKendall(`Accra Rainfall Total`[ ,2]) |> unlist())["sl"]), 2),
    size = 10) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))

dev.copy(png, filename = "Plots_Data/Annual Rainfall Total of Accra.png",
         width = 1450,
         height = 850)
dev.off()



# Mean Monthly Rainfall ####
assign(
  "Accra Monthly Rainfall",
  aggregate(
    Prcp ~ format(Accra[ ,"Date"], format = "%B") + format(Accra[ ,"Date"], format = "%Y"),
    data = Accra,
    FUN = sum,
    na.rm = T
  ) %>% 
    aggregate(
      Prcp ~ `format(Accra[, "Date"], format = "%B")`,
      data = .,
      FUN = mean,
      na.rm = T
    )
)

#Renaming columns of "Accra Monthly Rainfall"
colnames(`Accra Monthly Rainfall`) <- c("Month", "Prcp")

#Rearranging the output above to match the arrangement of the calender months
`Accra Monthly Rainfall` %<>% 
  .[order(match(.[ ,1], month.name)), ]

#Plotting mean monthly Rainfall
ggplot(data = `Accra Monthly Rainfall`, aes(x = seq(as.Date("2000-01-31"), by = "month", length.out = 12), y = Prcp)) + 
  geom_line(col = "darkblue", size = 2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Mean Monthly Rainfall of Accra \n 1960 - 2021 ",
       x = "Month",
       y = "Rainfall (mm)") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))

dev.copy(png, filename = "Plots_Data/Mean monthly Rainfall of Accra.png",
         width = 1450,
         height = 850)
dev.off()



# Annual Rainfall Total Anomaly ####
assign(
  "Annual Rainfall Total Anomaly",
  data.frame(
    Year = `Accra Rainfall Total`[ ,"Year"],
    Anomaly = (`Accra Rainfall Total`[ ,"Prcp"] - mean(`Accra Rainfall Total`[ ,"Prcp"], na.rm = T)) / 
      sd(`Accra Rainfall Total`[ ,"Prcp"])
  )
)

#Plotting Anomalies
ggplot(data = `Annual Rainfall Total Anomaly`, aes(x = Year, y = Anomaly, ymin = 0, ymax = Anomaly)) +
  geom_linerange(data = `Annual Rainfall Total Anomaly`, aes(
    colour = ifelse(Anomaly > 0, "Positive", "Negative")),
    stat = "identity", position = "identity", size = 4) +
  geom_hline(yintercept = 0) +
  labs(title = "Annual Rainfall Anomaly", x = "Year", y = "Rainfall (mm)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
        axis.title = element_text(size = 28, face = "bold"),
        axis.text = element_text(size = 26),
        legend.title = element_blank(),
        legend.text = element_text(size = 23))

dev.copy(
  png, filename = "Plots_Data/Annual Rainfall Total Anomaly.png",
  width = 1450,
  height = 850
)
dev.off()



# Seasonal Cycle of Rainfall in Accra ####
assign(
  "Rainfall Seasonal Cycle",
  aggregate(
    Prcp ~ format(Accra[ ,"Date"], format = "%m-%d"), 
    data = Accra, 
    FUN = mean,
    na.rm = T
  ) %>%
    transform(
      `format(Accra[, "Date"], format = "%m-%d")` = as.Date(paste(2000, .[ ,1], sep = "-"))
    )
)

#Renaming the columns of Rainfall Seasonal cycle
colnames(`Rainfall Seasonal Cycle`) <- c("Date", "Prcp")

#Smoother
smoother <- ksmooth(
  1:366,
  `Rainfall Seasonal Cycle`[ ,"Prcp"],
  bandwidth = 15
) |> 
  as.data.frame()

#Plotting Seasonal Cycle of Rainfall
ggplot(data = `Rainfall Seasonal Cycle`, aes(x = Date, y = Prcp)) +
  geom_line(col = "darkblue", size = 2) +
  geom_line(data = smoother, aes(x = `Rainfall Seasonal Cycle`[,1], y = y), col = "red", size = 1.5) +
  scale_x_date(date_breaks = "40 days", date_labels = "%d %b") +
  labs(title = "Rainfall Seasonal Cycle of Accra \n 1960 - 2021", 
       x = "Day",
       y = "Rainfall (mm)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25),
        legend.title = element_blank(),
        legend.text = element_text(size = 13))
dev.copy(
  png, filename = "Plots_Data/Rain fall Seasonal of Accra.png",
  height = 850, 
  width = 1450
)  
dev.off()
  

  
# Annual Rainy Days in Accra ####  
`Annual Rainy Days` <- Accra[!is.na(Accra[ ,"Prcp"]) & !Accra[ ,"Prcp"] < 0.85, ] %>%
  aggregate(
    Prcp ~ format(.[ ,"Date"], format = "%Y"),
    data = .,
    FUN = length
  )

#Renaming the columns of Annual Rainy Days
colnames(`Annual Rainy Days`) <- c("Year", "Prcp")

#Plotting Annual Rainy Days
ggplot(data = `Annual Rainy Days`, aes(x = as.numeric(Year), y = Prcp)) +
  geom_line(col = "darkblue", size = 2) + 
  labs(title = "Annual Rainy Days of Accra \n 1960 - 2021",
       x = "Year", y = "Rainfall (mm)") + 
  geom_smooth(method = "lm", col = "red") + 
  annotate("text", x = 2011, y = 90, label = paste(
    "p-value = ", round((unlist(Kendall::MannKendall(`Annual Rainy Days`[ ,2])))["sl"], 2)),
    size = 10) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))

dev.copy(
  png, filename = "Plots_Data/Annual Rainy Days of Accra.png",
  height = 850, 
  width = 1450
)  
dev.off()



#Number of Days of Rainfall >= 20mm in Accra ####
`Annual Rainy Days >= 20` <- Accra[!is.na(Accra[ ,"Prcp"]) & !Accra[ ,"Prcp"] < 20, ] %>%
  aggregate(
    Prcp ~ format(.[ ,"Date"], format = "%Y"),
    data = .,
    FUN = length
  )

#Renaming the columns of Number of Days of Raifall >= 20mm
colnames(`Annual Rainy Days >= 20`) <- c("Year", "Prcp")

#Plotting Annual Rainy Days
ggplot(data = `Annual Rainy Days >= 20`, aes(x = as.numeric(Year), y = Prcp)) +
  geom_line(col = "darkblue", size = 2) + 
  annotate("text", x = 2012, y = 22, label = paste(
    "p-value = ", round((unlist(Kendall::MannKendall(`Annual Rainy Days >= 20`[ ,2])))["sl"], 2)),
    size = 10) +
  labs(title = "Number of Days with Rainfall >= 20mm in Accra \n 1960 - 2021",
       x = "Year", y = "Rainfall (mm)") + 
  geom_smooth(method = "lm", col = "red") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))

dev.copy(
  png, filename = "Plots_Data/Number of Days with Rainfall grt 20 mm in Accra.png",
  height = 850, 
  width = 1450
)  
dev.off()



# Monthly Rainy Days of Accra ####
`Monthly Rainy Days of Accra` <- Accra[!is.na(Accra[ ,"Prcp"]) & !Accra[ ,"Prcp"] < 0.85, ] %>%
  aggregate(
    Prcp ~ format(.[ ,"Date"], format = "%B"),
    data = .,
    FUN = \(x) length(x)/length(`Annual Rainfall Total Anomaly`[ ,"Year"]) 
  ) %>% 
  .[order(match(.[ ,1], month.name)), ]

#Renaming columns of Monthly Rainy Days
colnames(`Monthly Rainy Days of Accra`) <- c("Months", "Prcp")

#Plotting mean monthly Rainfall
ggplot(data = `Monthly Rainy Days of Accra`, aes(x = seq(as.Date("2000-01-31"), by = "month", length.out = 12), y = Prcp)) + 
  geom_line(col = "darkblue", size = 2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Mean Monthly Rainy Days of Accra \n 1960 - 2021 ",
       x = "Month",
       y = "Rainfall (mm)") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))

dev.copy(png, filename = "Monthly Rainy Days of Accra.png",
         width = 1450,
         height = 850)
dev.off()

  

# Number of Days with Rainfall >= 20mm for each Month in Accra ####
`Number of Days with Rainfall >= 20mm` <- Accra[!is.na(Accra[ ,"Prcp"]) & !Accra[ ,"Prcp"] < 20, ] %>%
  aggregate(
    Prcp ~ format(.[ ,"Date"], format = "%B"),
    data = .,
    FUN = \(x) length(x)/length(`Accra Rainfall Total`[ ,"Year"]) 
  ) %>% 
  .[order(match(.[ ,1], month.name)), ]

#Renaming columns of Monthly Rainy Days
colnames(`Number of Days with Rainfall >= 20mm`) <- c("Months", "Prcp")

#Plotting mean monthly Rainfall
ggplot(data = `Number of Days with Rainfall >= 20mm`, aes(x = seq(as.Date("2000-01-31"), by = "month", length.out = 12), y = Prcp)) + 
  geom_line(col = "darkblue", size = 2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Number of Days with Rainfall >= 20mm in Accra \n 1960 - 2021 ",
       x = "Month",
       y = "Rainfall (mm)") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))

dev.copy(png, filename = "Number of Days with Rainfall grt 20mm for each month in Accra.png",
         width = 1450,
         height = 850)
dev.off()



# Number of Days of rain event for each day of the calendar Year ####
`Number of Days with recorded events` <- Accra %>% 
  aggregate(
    Prcp ~ format(.[ ,"Date"], format = "%m-%d"),
    data = .,
    FUN = \(x) length(which(x >= 0.85)) 
  )

#Renaming the columns of the 
colnames(`Number of Days with recorded events`) <- c("Days", "Number of Events")

#smoother
smoother_2 <- ksmooth(
  1:366, 
  `Number of Days with recorded events`[ ,"Number of Events"],
  bandwidth = 15
) |> 
  as.data.frame()

#Plotting events on daily bssis
ggplot(data = `Number of Days with recorded events`, aes(x = seq(as.Date("2000-01-01"), by = "day", length.out = 366), y = `Number of Events`)) + 
  geom_line(col = "darkblue", size = 2) + 
  geom_line(data = smoother_2, aes(x = seq(as.Date("2000-01-01"), by = "day", length.out = 366), y = y), col = "red", size = 1.5) +
  scale_x_date(date_breaks = "40 days", date_labels = "%d %b") +
  labs(title = "Number of Days with Rainfall for Each Day in Accra \n 1960 - 2021 ",
       x = "Day",
       y = "Number of Days") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))

dev.copy(png, filename = "Number of evens per day in Accra.png",
         width = 1450,
         height = 850)
dev.off()







# Dividing Accra into Two halves ####
# 1960 - 1989
Accra_1960_1990 <- Accra[Accra[ ,"Date"] <= "1990-12-31", ]

# 1990 - 2021
Accra_1991_2021 <- Accra[Accra[ ,"Date"] >= "1991-01-01", ]






# Analysis on First half of Accra ####
#Annual Rainfall Total of Accra (1960 - 1989) ####
`Annual Rainfall Total Accra 60-90` <- split(
  Accra_1960_1990,
  format(Accra_1960_1990[ ,"Date"], "%Y")
) %>% 
  lapply(
    FUN = \(data = "") sum(data[ ,"Prcp"], na.rm = T)
  ) %>% 
  do.call(
    rbind,
    .
  ) %>% data.frame(
    Year = as.numeric(rownames(.)),
    Prcp = .
  )


# Plotting Annual rainfall Total of Accra (1960 - 1989)
ggplot(data = `Annual Rainfall Total Accra 60-90`, aes(x = Year, y = Prcp)) + 
  geom_line(col = "darkblue", size = 2) + 
  geom_smooth(method = "lm", col = "red") +
  annotate("text", x = 1985, y = 1200, label = paste(
    "p-value = ", round((unlist(kendall::MannKendall(`Annual Rainfall Total Accra 60-90`[ ,2])))["sl"], 2)),
    size = 8) +
  labs(title = "Annual Rainfall Total of Accra \n 1960 - 1990",
       x = "Year",
       y = "Rainfall (mm)") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))

dev.copy(png, filename = "Accra Rainfall Total of Accra (1960-1990).png",
         width = 1450,
         height = 850)
dev.off()



# Mean Monthly Rainfall of Accra (1960 - 1989) ####
`Mean Monthly Rainfall Accra 60-90` <- split(
  `Accra_1960_1990`,
  format(Accra_1960_1990[ ,"Date"], "%B")
) %>% 
  lapply(
    FUN = \(data = "") sum(data[ ,"Prcp"], na.rm = T)/length(1960:1990)
  ) %>% 
  do.call(
    rbind,
    .
  ) |> 
  as.data.frame()

#Reordering the above
`Mean Monthly Rainfall Accra 60-90` <- data.frame(
  Month = rownames(`Mean Monthly Rainfall Accra 60-90`),
  Prcp = `Mean Monthly Rainfall Accra 60-90`[ ,1] 
) %>% 
  .[order(match(.[ ,1], month.name)), ]

# Plotting Annual rainfall Total of Accra (1960 - 1989)
ggplot(data = `Mean Monthly Rainfall Accra 60-90`, aes(
  x = seq(as.Date("2000-01-01"), by = "month", length.out = 12),
  y = Prcp)) + 
  geom_line(col = "darkblue", size = 2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Mean Monthly Rainfall of Accra \n 1960 - 1990",
       x = "Month",
       y = "Rainfall (mm)") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))

dev.copy(png, filename = "Mean Monthly Rainfall Accra 1960-1990.png",
         width = 1450,
         height = 850)
dev.off()


# Annual Rainfall Total Anomaly 1960 - 1990 ####
assign(
  "Annual Rainfall Total Anomaly 1960 - 1990",
  data.frame(
    Year = `Annual Rainfall Total Accra 60-90`[ ,"Year"],
    Anomaly = (`Annual Rainfall Total Accra 60-90`[ ,"Prcp"] - mean(`Annual Rainfall Total Accra 60-90`[ ,"Prcp"], na.rm = T)) / 
      sd(`Annual Rainfall Total Accra 60-90`[ ,"Prcp"])
  )
)

#Plotting Anomalies
ggplot(data = `Annual Rainfall Total Anomaly 1960 - 1990`, aes(x = Year, y = Anomaly, ymin = 0, ymax = Anomaly)) +
  geom_linerange(data = `Annual Rainfall Total Anomaly 1960 - 1990`, aes(
    colour = ifelse(Anomaly > 0, "Positive", "Negative")),
    stat = "identity", position = "identity", size = 4) +
  geom_hline(yintercept = 0) +
  labs(title = "Annual Rainfall Anomaly of Accra \n 1960-1990", x = "Year", y = "Rainfall (mm)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
        axis.title = element_text(size = 28, face = "bold"),
        axis.text = element_text(size = 26),
        legend.title = element_blank(),
        legend.text = element_text(size = 23))

dev.copy(
  png, filename = "Plots_Data/Annual Rainfall Total Anomaly of Accra (1960 - 1990).png",
  width = 1450,
  height = 850
)
dev.off()



# Seasonal Cycle of Rainfall in Accra (1940 - 1990) ####
assign(
  "Rainfall Seasonal Cycle 1960-1990",
  aggregate(
    Prcp ~ format(Accra_1960_1990[ ,"Date"], format = "%m-%d"), 
    data = Accra_1960_1990, 
    FUN = mean,
    na.rm = T
  ) %>%
    transform(
      `format(Accra_1960_1990[, "Date"], format = "%m-%d")` = as.Date(paste(2000, .[ ,1], sep = "-"))
    )
)

#Renaming the columns of Rainfall Seasonal cycle
colnames(`Rainfall Seasonal Cycle 1960-1990`) <- c("Date", "Prcp")

#Smoother
smoother <- ksmooth(
  1:366,
  `Rainfall Seasonal Cycle 1960-1990`[ ,"Prcp"],
  bandwidth = 15
) |> 
  as.data.frame()

#Plotting Seasonal Cycle of Rainfall
ggplot(data = `Rainfall Seasonal Cycle 1960-1990`, aes(x = Date, y = Prcp)) +
  geom_line(col = "darkblue", size = 2) +
  geom_line(data = smoother, aes(x = `Rainfall Seasonal Cycle 1960-1990`[,1], y = y), col = "red", size = 1.5) +
  scale_x_date(date_breaks = "40 days", date_labels = "%d %b") +
  labs(title = "Seasonal Cycle of Rainfall in Accra \n 1960 - 1990", 
       x = "Day",
       y = "Rainfall (mm)") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))
dev.copy(
  png, filename = "Rain fall Seasonal of Accra 1960-1990.png",
  height = 850, 
  width = 1450
)  
dev.off()



# Annual Rainy Days of Accra 1960 - 1990 ####
`Annual Rainy Days 1960 - 1990` <- Accra_1960_1990[!is.na(Accra_1960_1990[ ,"Prcp"]) & !Accra_1960_1990[ ,"Prcp"] < 0.85, ] %>%
  aggregate(
    Prcp ~ format(.[ ,"Date"], format = "%Y"),
    data = .,
    FUN = length
  )

#Renaming the columns of Annual Rainy Days
colnames(`Annual Rainy Days 1960 - 1990`) <- c("Year", "Prcp")

#Plotting Annual Rainy Days
ggplot(data = `Annual Rainy Days 1960 - 1990`, aes(x = as.numeric(Year), y = Prcp)) +
  geom_line(col = "darkblue", size = 2) + 
  labs(title = "Annual Rainy Days of Accra \n 1960 - 1990",
       x = "Year", y = "Rainfall (mm)") + 
  geom_smooth(method = "lm", col = "red") + 
  annotate("text", x = 1985, y = 120, label = paste(
    "p-value = ", round((unlist(Kendall::MannKendall(`Annual Rainy Days 1960 - 1990`[ ,2])))["sl"], 2)),
    size = 10) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))

dev.copy(
  png, filename = "Annual Rainy Days of Accra 1960 -1989.png",
  height = 850, 
  width = 1450
)  
dev.off()


# Annual Rainy Days of Accra 1960 - 1989 >= 20mm ####
`Annual Rainy Days 1960 - 1990 >= 20` <- Accra_1960_1990[!is.na(Accra_1960_1990[ ,"Prcp"]) & !Accra_1960_1990[ ,"Prcp"] < 20, ] %>%
  aggregate(
    Prcp ~ format(.[ ,"Date"], format = "%Y"),
    data = .,
    FUN = length
  )

#Renaming the columns of Annual Rainy Days
colnames(`Annual Rainy Days 1960 - 1990 >= 20`) <- c("Year", "Prcp")

#Plotting Annual Rainy Days
ggplot(data = `Annual Rainy Days 1960 - 1990 >= 20`, aes(x = as.numeric(Year), y = Prcp)) +
  geom_line(col = "darkblue", size = 2) + 
  labs(title = "Annual Rainy Days of Accra >= 20mm \n 1960 - 1990",
       x = "Year", y = "Rainfall (mm)") + 
  geom_smooth(method = "lm", col = "red") + 
  annotate("text", x = 1985, y = 20, label = paste(
    "p-value = ", round((unlist(Kendall::MannKendall(`Annual Rainy Days 1960 - 1990 >= 20`[ ,2])))["sl"], 2)),
    size = 10) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))

dev.copy(
  png, filename = "Annual Rainy Days of Accra grt 20mm 1960 -1990.png",
  height = 850, 
  width = 1450
)  
dev.off()

#Monthly Rainy Days of Accra (1960 - 1990)
`Monthly Rainy Days of Accra 60-90` <- Accra_1960_1990[!is.na(Accra_1960_1990[ ,"Prcp"]) & !Accra_1960_1990[ ,"Prcp"] < 0.85, ] %>%
  aggregate(
    Prcp ~ format(.[ ,"Date"], format = "%B"),
    data = .,
    FUN = \(x) length(x)/length(`Annual Rainfall Total Anomaly`[ ,"Year"]) 
  ) %>% 
  .[order(match(.[ ,1], month.name)), ]

#Renaming columns of Monthly Rainy Days
colnames(`Monthly Rainy Days of Accra 60-90`) <- c("Months", "Prcp")


#Monthly Rainy Days of Accra with events >= 20mm (1960 - 1990)
`Monthly Rainy Days of Accra 60-90 >= 20mm` <- Accra_1960_1990[!is.na(Accra_1960_1990[ ,"Prcp"]) & !Accra_1960_1990[ ,"Prcp"] < 20, ] %>%
  aggregate(
    Prcp ~ format(.[ ,"Date"], format = "%B"),
    data = .,
    FUN = \(x) length(x)/length(`Annual Rainfall Total Anomaly`[ ,"Year"]) 
  ) %>% 
  .[order(match(.[ ,1], month.name)), ]

#Renaming columns of Monthly Rainy Days
colnames(`Monthly Rainy Days of Accra 60-90 >= 20mm`) <- c("Months", "Prcp")






#Second half of Accra Rainfall Data
# Annual Rainfall Total for Accra (1991 - 2021)
split(
  Accra_1991_2021,
  format(Accra_1991_2021[ ,"Date"], format = "%Y")
) |> 
  lapply(
    \(data = "") sum(data[ ,2], na.rm = T)
  ) %>% do.call(
    rbind,
    .
  ) -> `Annual Rainfall Total Accra 1990 - 2021`


# Mean Monthly Rainfall of Accra (1991 - 2021) ####
`Mean Monthly Rainfall Accra 91-21` <- split(
  `Accra_1991_2021`,
  format(Accra_1991_2021[ ,"Date"], "%B")
) %>% 
  lapply(
    FUN = \(data = "") sum(data[ ,"Prcp"], na.rm = T) / length(1991:2021)
  ) %>% 
  do.call(
    rbind,
    .
  ) |> 
  as.data.frame()

#Reordering the above
`Mean Monthly Rainfall Accra 91-21` <- data.frame(
  Month = rownames(`Mean Monthly Rainfall Accra 91-21`),
  Prcp = `Mean Monthly Rainfall Accra 91-21`[ ,1] 
) %>% 
  .[order(match(.[ ,1], month.name)), ]

# Plotting Annual rainfall Total of Accra (1960 - 1989)
ggplot(data = `Mean Monthly Rainfall Accra 91-21`, aes(
  x = seq(as.Date("2000-01-01"), by = "month", length.out = 12),
  y = Prcp)) + 
  geom_line(col = "darkblue", size = 2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Mean Monthly Rainfall of Accra \n 1991 - 2021",
       x = "Month",
       y = "Rainfall (mm)") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))

dev.copy(png, filename = "Mean Monthly Rainfall Accra (1991-2021).png",
         width = 1450,
         height = 850)
dev.off()



# Annual Rainy Days of Accra (1991-2021)
`Annual Rainy Days 1991 - 2021` <- Accra_1991_2021[!is.na(Accra_1991_2021[ ,"Prcp"]) & !Accra_1991_2021[ ,"Prcp"] < 0.85, ] %>%
  aggregate(
    Prcp ~ format(.[ ,"Date"], format = "%Y"),
    data = .,
    FUN = length
  )
# Rename columns of Annual Rainy Days of Accra 1991-2021


# Annual Rainy Days of Accra with events >= 20mm (1991-2021)
`Annual Rainy Days >= 20mm 1991 - 2021` <- Accra_1991_2021[!is.na(Accra_1991_2021[ ,"Prcp"]) & !Accra_1991_2021[ ,"Prcp"] < 20, ] %>%
  aggregate(
    Prcp ~ format(.[ ,"Date"], format = "%Y"),
    data = .,
    FUN = length
  )



# Monthly Rainy Days of Accra (1991 - 2021)
`Monthly Rainy Days of Accra 91-21` <- Accra_1991_2021[!is.na(Accra_1991_2021[ ,"Prcp"]) & !Accra_1991_2021[ ,"Prcp"] < 0.85, ] %>%
  aggregate(
    Prcp ~ format(.[ ,"Date"], format = "%B"),
    data = .,
    FUN = \(x) length(x)/length(`Annual Rainfall Total Anomaly`[ ,"Year"]) 
  ) %>% 
  .[order(match(.[ ,1], month.name)), ]

#Renaming columns of Monthly Rainy Days
colnames(`Monthly Rainy Days of Accra 91-21`) <- c("Months", "Prcp")

#Plotting mean monthly Rainfall
ggplot(data = `Monthly Rainy Days of Accra 91-21`, aes(x = seq(as.Date("2000-01-31"), by = "month", length.out = 12), y = Prcp)) + 
  geom_line(col = "darkblue", size = 2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Mean Monthly Rainy Days of Accra \n 1991 - 2021 ",
       x = "Month",
       y = "Rainfall (mm)") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))

dev.copy(png, filename = "Monthly Rainy Days of Accra 1991-2021.png",
         width = 1450,
         height = 850)
dev.off()


# Monthly Rainy Days in Accra >= 20mm ####
`Number of Days with Rainfall >= 20mm 91-21` <- Accra_1991_2021[!is.na(Accra_1991_2021[ ,"Prcp"]) & !Accra_1991_2021[ ,"Prcp"] < 20, ] %>%
  aggregate(
    Prcp ~ format(.[ ,"Date"], format = "%B"),
    data = .,
    FUN = \(x) length(x)/length(`Accra Rainfall Total`[ ,"Year"]) 
  ) %>% 
  .[order(match(.[ ,1], month.name)), ]

#Renaming columns of Monthly Rainy Days
colnames(`Number of Days with Rainfall >= 20mm 91-21`) <- c("Months", "Prcp")

#Plotting mean monthly Rainfall
ggplot(data = `Number of Days with Rainfall >= 20mm 91-21`, aes(x = seq(as.Date("2000-01-31"), by = "month", length.out = 12), y = Prcp)) + 
  geom_line(col = "darkblue", size = 2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title = "Number of Days with Rainfall >= 20mm in Accra \n 1991 - 2021 ",
       x = "Month",
       y = "Rainfall (mm)") + 
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 28),
        axis.title = element_text(size = 26, face = "bold"),
        axis.text = element_text(size = 25))

dev.copy(png, filename = "Number of Days with Rainfall grt 20mm for each month in Accra 91-21.png",
         width = 1450,
         height = 850)
dev.off()







# Differences when comparing Indices between both time periods ####
data.frame(
  Index = "Annual Rainfall Total",
  Period = "60-90 & 91-21",
  `Mean Change` = mean(`Annual Rainfall Total Accra 1990 - 2021`[, 1]) - mean(`Annual Rainfall Total Accra 60-90`[ ,"Prcp"], na.rm = T)
) |> 
  rbind(
    data.frame(
      Index = "Mean Annual Max Tmp",
      Period = "60-90 & 91-21",
      `Mean Change` = mean(Annual_Max_Temp_91_21[ ,2], na.rm = T) - mean(Annual_Max_Temp_60_90[ ,2], na.rm = T)
    )
  ) |> 
  rbind(
    data.frame(
      Index = "Mean Annual Min Temp",
      Period = "60-90 & 91-21",
      `Mean Change` = mean(Annual_Min_Temp_91_21[ ,2], na.rm = T) - mean(Annual_Min_Temp_60_90[ ,2], na.rm = T)
    )
  ) |> 
  write.csv(
    file = "Annual Rain and Temp change.csv",
    row.names = FALSE
  )


# Difference between Annual Rainy Days of Accra
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
      `Percentage change` = ((mean(`Annual Rainy Days >= 20mm 1991 - 2021`[,2], na.rm = T) - mean(`Annual Rainy Days 1960 - 1990 >= 20`[,2], na.rm = T))*100)/
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


# 









