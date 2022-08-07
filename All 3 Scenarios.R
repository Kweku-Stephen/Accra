require(magrittr)
require(ggplot2)

#Importing scenarios RCP 26 AND 85 (RCP45 already exists in my blobal environment)
model_26_85 <- dir(pattern = "RCP") %>% 
  rio::import_list() %>% 
  lapply(
    function(data = "") data.frame(
      Date = seq(as.Date("1980-01-01"), 
                 as.Date("2100-12-31"),
                 by = "day"),
      data[-1]
    )
  )
  


model_26_45_85 <-
  #Creating a list with observed and RCP 26,45,85
  (list(
    Observed = rb,
    AccraRCP26 = model_26_85$AccraRCP26, 
    AccraRCP45 = subset(Accra_proj_full, Date >= "1980-01-01" & Date <= "2100-12-31"),
    AccraRCP85 = model_26_85$AccraRCP85
  ) %>% 
  lapply(
    #looping the selection function below on the list above
    function(data = "") {
      dplyr::select(
        data, 
        grep("Date|[Tt][Mm][Aa][Xx]|[Tt][Mm][Ii][Nn]|[Rr][Aa][Ii][Nn]|Pluie",
             names(data), 
             value = TRUE
        )
      ) -> data ; colnames(data) <- c("Date","Tmax","Tmin","Prcp")
      return(data)
    }
  )) %>% 
  lapply(
    #Looping the filtering function below on the list above
    function(data = ""){
      #control statement
      if(data[,1][1] != "1960-01-01")
        subset(data, Date >= "2022-01-01" & Date <= "2100-12-31") else  # changed from 2019 to 2022-01-01
          data
    }
  ) 
  

#Aggregating function conditioned by variable type
group_agg <- function(data = ""){
  dplyr::group_by(data, 
                  Date = as.numeric(
                    format(
                      data[ ,"Date"], 
                      format = "%Y"
                  )
                )
              ) %>% 
    dplyr::summarise(
      Tmax = mean(Tmax, na.rm = T), 
      Tmin = mean(Tmin, na.rm = T), 
      Prcp = sum(Prcp, na.rm = T)
    )
}   



#Evaluating group_agg via a looping construct on the list "model_26_45_85"
model_26_45_85 <- lapply(
  model_26_45_85,
  group_agg
)

#Extracting datasets from the list "model_26_45_85"
Observed <- model_26_45_85[["Observed"]]
AccRCP26 <- model_26_45_85[["AccraRCP26"]]
AccRCP45 <- model_26_45_85[["AccraRCP45"]]
AccRCP85 <- model_26_45_85[["AccraRCP85"]]


#Creating grided plots with ggplot2
gridExtra::grid.arrange(
  ggplot(data = Observed,aes(x = Date, y = Prcp)) +
    geom_line(col = "darkblue", lwd = 1) +
    geom_line(data = AccRCP26, aes(x = Date, y = Prcp), lwd = 1, col = "red") +
    geom_line(data = AccRCP45, aes(x = Date, y = Prcp), lwd = 1, col = "darkgreen") +
    geom_line(data = AccRCP85, aes(x = Date, y = Prcp), lwd = 1, col = "brown") +
    geom_smooth(data = Observed,
                aes(x = Date, y = Prcp, fill = "OBSERVED"), lwd = 0, method = "loess") +
    geom_smooth(data = AccRCP26, 
                aes(x = Date, y = Prcp, fill = "RCP26"), lwd = 0, method = "loess") +
    geom_smooth(data = AccRCP45,
                aes(x = Date,y = Prcp, fill = "RCP45"), lwd = 0, method = "loess") +
    geom_smooth(data = AccRCP85,
                aes(x = Date, y = Prcp, fill = "RCP85"), lwd = 0, method = "loess") +
    geom_vline(xintercept = c(2021.9, 2050.9), lwd = 2, col = "black", lty = 2) +
    scale_fill_manual("",values = c("OBSERVED" = "darkblue", 
                                    "RCP26" = "red", 
                                    "RCP45" = "darkgreen",
                                    "RCP85" = "brown"))  +
    #scale_colour_manual(values = "darkblue", name = "OBSERVED") +
    scale_x_continuous(breaks = seq(1960,2100 ,by = 35), limits = c(1960, 2100)) +
    labs(subtitle = "Accra Rainfall", y = "Rainfall(mm)", x = "Year") +
    theme(plot.subtitle = element_text(size = 23, hjust = 0, face = "bold"),
          axis.text = element_text(size = 19),
          axis.title = element_text(size = 21),
          legend.text = element_text(size = 17)), 
  
  
  ggplot(data = Observed,aes(x = Date, y = Tmax)) +
    geom_line(col = "darkblue", lwd = 1) +
    geom_line(data = AccRCP26, aes(x = Date, y = Tmax), lwd = 1, col = "red") +
    geom_line(data = AccRCP45, aes(x = Date, y = Tmax), lwd = 1, col = "darkgreen") +
    geom_line(data = AccRCP85, aes(x = Date, y = Tmax), lwd = 1, col = "brown") +
    geom_smooth(data = Observed,
                aes(x = Date, y = Tmax, fill = "OBSERVED"), lwd = 0, method = "loess") +
    geom_smooth(data = AccRCP26, 
                aes(x = Date, y = Tmax, fill = "RCP26"), lwd = 0, method = "loess") +
    geom_smooth(data = AccRCP45,
                aes(x = Date,y = Tmax, fill = "RCP45"), lwd = 0, method = "loess") +
    geom_smooth(data = AccRCP85,
                aes(x = Date, y = Tmax, fill = "RCP85"), lwd = 0, method = "loess") +
    geom_vline(xintercept = c(2018.9, 2050.9), lwd = 2, col = "black", lty = 2) +
    scale_fill_manual("",values = c("OBSERVED" = "darkblue", 
                                    "RCP26" = "red", 
                                    "RCP45" = "darkgreen",
                                    "RCP85" = "brown"))  +
    #scale_colour_manual(values = "darkblue", name = "OBSERVED") +
    scale_x_continuous(breaks = seq(1960,2100 ,by = 35), limits = c(1960, 2100)) +
    labs(subtitle = "Accra Max Tmp", y = expression("Temperature("~degree*C*")"), 
         x = "Year") +
    theme(plot.subtitle = element_text(size = 23, hjust = 0, face = "bold"),
          axis.text = element_text(size = 19),
          axis.title = element_text(size = 21),
          legend.text = element_text(size = 17)),
  
  
  ggplot(data = Observed,aes(x = Date, y = Tmin)) +
    geom_line(col = "darkblue", lwd = 1) +
    geom_line(data = AccRCP26, aes(x = Date, y = Tmin), lwd = 1, col = "red") +
    geom_line(data = AccRCP45, aes(x = Date, y = Tmin), lwd = 1, col = "darkgreen") +
    geom_line(data = AccRCP85, aes(x = Date, y = Tmin), lwd = 1, col = "brown") +
    geom_smooth(data = Observed,
                aes(x = Date, y = Tmin, fill = "OBSERVED"), lwd = 0, method = "loess") +
    geom_smooth(data = AccRCP26, 
                aes(x = Date, y = Tmin, fill = "RCP26"), lwd = 0, method = "loess") +
    geom_smooth(data = AccRCP45,
                aes(x = Date,y = Tmin, fill = "RCP45"), lwd = 0, method = "loess") +
    geom_smooth(data = AccRCP85,
                aes(x = Date, y = Tmin, fill = "RCP85"), lwd = 0, method = "loess") +
    geom_vline(xintercept = c(2018.9, 2050.9), lwd = 2, col = "black", lty = 2) +
    scale_fill_manual("",values = c("OBSERVED" = "darkblue", 
                                    "RCP26" = "red", 
                                    "RCP45" = "darkgreen",
                                    "RCP85" = "brown"))  +
    #scale_colour_manual(values = "darkblue", name = "OBSERVED") +
    scale_x_continuous(breaks = seq(1960, 2100 ,by = 35), limits = c(1960, 2100)) +
    labs(subtitle = "Accra Min Tmp", y = expression("Temperature("~degree*C*")"), 
         x = "Year") +
    theme(plot.subtitle = element_text(size = 23, hjust = 0, face = "bold"),
          axis.text = element_text(size = 19),
          axis.title = element_text(size = 21),
          legend.text = element_text(size = 17)),
  
  ncol = 2, nrow = 2
)
dev.copy(
  png, 
  filename = "Observed and Projected data of Accra.png", 
  width = 1500, 
  height = 800
)

dev.off()


#Mean Stats for Prcp, Tmax and Tmin for Accra(Observed and Projected)
data.frame(
  Scenario = "Observed",
  Date = "1960 - 2021",
  Prcp = mean(unlist(Observed[, "Prcp"])),
  Tmax = mean(unlist(Observed[, "Tmax"])),
  Tmin = mean(unlist(Observed[, "Tmin"]))
) %>% 
  rbind(
    data.frame(
      Scenario = "RCP26",
      Date = "2021 - 2050",
      Prcp = mean(unlist(subset(AccRCP26, Date >= 2021 & Date <= 2050)[, "Prcp"])),
      Tmax = mean(unlist(subset(AccRCP26, Date >= 2021 & Date <= 2050)[, "Tmax"])),
      Tmin = mean(unlist(subset(AccRCP26, Date >= 2021 & Date <= 2050)[, "Tmin"]))
    )
  ) %>% 
  rbind(
    data.frame(
      Scenario = "RCP45",
      Date = "2021 - 2050",
      Prcp = mean(unlist(subset(AccRCP45, Date >= 2021 & Date <= 2050)[, "Prcp"])),
      Tmax = mean(unlist(subset(AccRCP45, Date >= 2021 & Date <= 2050)[, "Tmax"])),
      Tmin = mean(unlist(subset(AccRCP45, Date >= 2021 & Date <= 2050)[, "Tmin"]))
    )
  ) %>% 
  rbind(
    data.frame(
        Scenario = "RCP85",
        Date = "2021 - 2050",
        Prcp = mean(unlist(subset(AccRCP85, Date >= 2021 & Date <= 2050)[, "Prcp"])),
        Tmax = mean(unlist(subset(AccRCP85, Date >= 2021 & Date <= 2050)[, "Tmax"])),
        Tmin = mean(unlist(subset(AccRCP85, Date >= 2021 & Date <= 2050)[, "Tmin"]))
    )
  ) %>% 
  rbind(
    data.frame(
      Scenario = "RCP26",
      Date = "2051 - 2100",
      Prcp = mean(unlist(subset(AccRCP26, Date >= 2051 & Date <= 2100)[, "Prcp"])),
      Tmax = mean(unlist(subset(AccRCP26, Date >= 2051 & Date <= 2100)[, "Tmax"])),
      Tmin = mean(unlist(subset(AccRCP26, Date >= 2051 & Date <= 2100)[, "Tmin"]))
    )
  ) %>% 
  rbind(
    data.frame(
      Scenario = "RCP45",
      Date = "2051 - 2100",
      Prcp = mean(unlist(subset(AccRCP45, Date >= 2051 & Date <= 2100)[, "Prcp"])),
      Tmax = mean(unlist(subset(AccRCP45, Date >= 2051 & Date <= 2100)[, "Tmax"])),
      Tmin = mean(unlist(subset(AccRCP45, Date >= 2051 & Date <= 2100)[, "Tmin"]))
    )
  ) %>% 
  rbind(
    data.frame(
      Scenario = "RCP85",
      Date = "2051 - 2100",
      Prcp = mean(unlist(subset(AccRCP85, Date >= 2051 & Date <= 2100)[, "Prcp"])),
      Tmax = mean(unlist(subset(AccRCP85, Date >= 2051 & Date <= 2100)[, "Tmax"])),
      Tmin = mean(unlist(subset(AccRCP85, Date >= 2051 & Date <= 2100)[, "Tmin"]))
      
    )
  ) %>% 
  write.csv(
    file = "Mean Stats for observed and Projected Accra.csv",
    row.names = FALSE
  )


#Mean Stats for Observed Accra Data
data.frame(
  Data = "Observed",
  Variable = "Prcp",
  `First half` = unlist(Observed[1:31, ][, "Prcp"]) %>% mean,
  `Second Half` = unlist(Observed[32:nrow(Observed), ][, "Prcp"]) %>% mean
) %>% 
  rbind(
    data.frame(
      Data = "Observed",
      Variable = "Tmax",
      `First half` = unlist(Observed[1:31, ][, "Tmax"]) %>% mean,
      `Second Half` = unlist(Observed[32:nrow(Observed), ][, "Tmax"]) %>% mean
    )
  ) %>% 
  rbind(
    data.frame(
      Data = "Observed",
      Variable = "Tmin",
      `First half` = unlist(Observed[1:31, ][, "Tmin"]) %>% mean,
      `Second Half` = unlist(Observed[32:nrow(Observed), ][, "Tmin"]) %>% mean
    )
  ) %>% 
  write.csv(
    file = "Stats for Observed Data for Accra.csv", row.names = FALSE)













