###########################################################################################
require(tidyverse)
require(xts)

models <- read.csv(
  dir(pattern = "[Aa]ssembly"),
  header = T,
  sep = ",",
  na.strings = -99
) %>% 
  sapply(
    as.numeric
    ) %>% 
  data.frame()

View(
  models
)


#Accra
Acc_model <- models[ ,grepl("[Aa]ccra", names(models))]

#Prcp
Acc_md_prcp <- Acc_model[ ,grepl("rr", names(Acc_model))] %>% 
  mutate(
    Date = seq(
      as.Date("1951-01-01"),
      as.Date("2100-12-31"),
      by = 'day'
    )
  )
  

#Tmax
Acc_md_Tmax <- Acc_model[ ,grepl("tmax", names(Acc_model))] %>% 
  mutate(
    Date = seq(
      as.Date("1951-01-01"),
      as.Date("2100-12-31"),
      by = 'day'
    )
  )

#Tmin
Acc_md_Tmin <- Acc_model[ ,grepl("tmin", names(Acc_model))] %>% 
  mutate(
    Date = seq(
      as.Date("1951-01-01"),
      as.Date("2100-12-31"),
      by = 'day'
    )
  ) 



require(xts)

agg <- function(vec = x, op = y){
  xts(
    vec, 
    seq(
      as.Date("1951-01-01"), 
      as.Date("2100-12-31"), 
      by = "day")
    ) %>% 
    apply.yearly(
      op,
      na.rm = T
    ) -> dt
  
  data.frame(
    Date = index(dt),
    coredata(dt) 
  ) ->  dt2
    return(dt2[,2])
}


#Aggregating
#Prcp
sapply(Acc_md_prcp[,1:4],agg,sum) %>% 
  data.frame() %>% 
  mutate(Date = seq(as.Date("1951-12-31"), as.Date("2100-12-31"), by = "year"))-> Prcp_models


#Tmax
sapply(Acc_md_Tmax,agg,mean) %>% 
  data.frame() %>% 
  mutate(Date = seq(as.Date("1951-12-31"), as.Date("2100-12-31"), by = "year"))-> Tmax_models


#Tmin
sapply(Acc_md_Tmin,agg,mean) %>% 
  data.frame() %>% 
  mutate(Date = seq(as.Date("1951-12-31"), as.Date("2100-12-31"), by = "year"))-> Tmin_models






#Ensemle
Ensemble <- dir(pattern = "projected.csv") %>% 
  lapply(read.csv, na.strings = -99) 

#Function convert elements/dataframes within Ensemble to aggregates on annual basis 
agg_1 <- function(data = x, op = y){
  
  xts(
    data[,3] %>% as.numeric(), 
    seq(
      as.Date("1951-01-01"), 
      as.Date("2100-12-31"), 
      by = "day")
  ) %>% 
    apply.yearly(
      op,
      na.rm = T
    ) -> dt
  
  data.frame(
    Date = index(dt),
    coredata(dt) 
  ) %>% 
    return()
}

#Prcp
Prcp_ensemble <- agg_1(
  data = Ensemble[[3]], 
  op = sum
  )

#Tmax
Tmax_ensemble <- agg_1(
  data = Ensemble[[2]], 
  op = mean
  )

#Tmin
Tmin_ensemble <- agg_1(
  data = Ensemble[[1]], 
  op = mean
  )

#Zeroes in Prcp to be converted to NAs
rp <- function(x){x[x == 0] <- NA;return(x)}
Prcp[,1:4] <- apply(Prcp[,1:4], 2, rp)


#Ploting
#Prcp
ggplot(data = Prcp_models, aes(x=Date))+
  geom_smooth(
    method = "loess",aes(y = Prcp[,1],  col = "mpi"), lwd = 1.3, se = FALSE) +
  geom_smooth(method = "loess",aes(y = Prcp_models[,2], col = "noaa"), lwd = 1.3, se = FALSE) +
  geom_smooth(method = "loess",aes(y = Prcp_models[,3], col = "cnrm"), lwd = 1.3, se = FALSE) +
  geom_smooth(method = "loess",aes(y = Prcp_models[,4], col = "miroc5"), lwd = 1.3, se = FALSE) + 
  geom_smooth(method = "loess",aes(y = Prcp_ensemble[,2], col = "ensemble"), lwd = 1.3, se = FALSE) + 
  scale_colour_manual("Legend",
                      values = c("mpi" = "red","noaa" = "darkblue","cnrm" = "darkgreen",
                                 "miroc5" = "chocolate","ensemble" = "black")) +
  labs(title = "", x = "Year", y = "rainfall(mm)") + 
  theme(axis.title = element_text(size = 15.5),
        axis.text = element_text(size = 13.5),
        legend.title = element_text(size = 15.5),
        legend.text = element_text(size = 13.5))

dev.copy(png, filename = "Rainfall_allmodels.png", height = 750, width = 1450)
dev.off()


#Tmax
ggplot(data = Tmax_models, aes(x=Date))+
  geom_smooth(method = "loess",aes(y = Tmax_models[,1], col = "mpi"), lwd = 1.3, se = FALSE) +
  geom_smooth(method = "loess",aes(y = Tmax_models[,2], col = "noaa"), lwd = 1.3, se = FALSE) +
  geom_smooth(method = "loess",aes(y = Tmax_models[,3], col = "miroc5"), lwd = 1.3, se = FALSE) + 
  geom_smooth(method = "loess",aes(y = Tmax_ensemble[,2], col = "ensemble"), lwd = 1.3, se = FALSE) + 
  scale_colour_manual("Legend",
                      values = c("mpi" = "red","noaa" = "darkblue",
                                 "miroc5" = "chocolate","ensemble" = "black")) +
  labs(title = "", x = "Year", y = expression("Temperature("~degree*C*")")) + 
  theme(axis.title = element_text(size = 15.5),
        axis.text = element_text(size = 13.5),
        legend.title = element_text(size = 15.5),
        legend.text = element_text(size = 13.5))

dev.copy(png, filename = "Tmax_allmodels.png.png", height = 750, width = 1450)
dev.off()



#Tmin
ggplot(data = Tmin_models, aes(x=Date))+
  geom_smooth(method = "loess",aes(y = Tmin_models[,1], col = "mpi"), lwd = 1.3, se = FALSE) +
  geom_smooth(method = "loess",aes(y = Tmin_models[,2], col = "noaa"), lwd = 1.3, se = FALSE) +
  geom_smooth(method = "loess",aes(y = Tmin_models[,3], col = "cnrm"), lwd = 1.3, se = FALSE) +
  geom_smooth(method = "loess",aes(y = Tmin_models[,4], col = "miroc5"), lwd = 1.3, se = FALSE) + 
  geom_smooth(method = "loess",aes(y = Tmin_ensemble[,2], col = "ensemble"), lwd = 1.3, se = FALSE) + 
  scale_colour_manual("Legend",
                      values = c("mpi" = "red","noaa" = "darkblue","cnrm" = "darkgreen",
                                 "miroc5" = "chocolate","ensemble" = "black")) +
  labs(title = "", x = "Year", y = expression("Temperature("~degree*C*")")) + 
  theme(axis.title = element_text(size = 15.5),
        axis.text = element_text(size = 13.5),
        legend.title = element_text(size = 15.5),
        legend.text = element_text(size = 13.5))

dev.copy(png, filename = "Tmin_allmodels.png.png", height = 750, width = 1450)
dev.off()








