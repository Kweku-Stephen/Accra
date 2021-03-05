#Summary Statistics
require(tidyverse)


head(Prcp.df.ann)
Summary_Table <- data.frame(
  Station = "Accra",
  Variable = "Annual Rainfall Total",
  Max = Prcp.df.ann[Prcp.df.ann[,2] == max(Prcp.df.ann[,2]), ],
  Min = Prcp.df.ann[Prcp.df.ann[,2] == min(Prcp.df.ann[,2]), ],
  Mean = mean(Prcp.df.ann[,2]),
  Stand.Dev = sd(Prcp.df.ann[,2]),
  Range = range(Prcp.df.ann[,2])[2] - range(Prcp.df.ann[,2])[1]
  )
  
  Summary_Table[2,1:ncol(Summary_Table)] <-  data.frame(
    Station = "Accra",
    Variable = "Monthly Rainy Days",
    Max = monthly.rdys[monthly.rdys[,2] == max(monthly.rdys[,2]), ],
    Min = monthly.rdys[monthly.rdys[,2] == min(monthly.rdys[,2]), ],
    Mean = mean(monthly.rdys[,2]),
    Stand.Dev = sd(monthly.rdys[,2]),
    Range = range(monthly.rdys[,2])[2] - range(monthly.rdys[,2])[1]
  ) 
  
  Summary_Table[3,1:ncol(Summary_Table)] <-  data.frame(
    Station = "Accra",
    Variable = "Seas.Cycle of Ann.Rainfall",
    Max = Seasonal_cycle_Acc[Seasonal_cycle_Acc[,2] == max(Seasonal_cycle_Acc[,2]), ],
    Min = Seasonal_cycle_Acc[Seasonal_cycle_Acc[,2] == min(Seasonal_cycle_Acc[,2]), ],
    Mean = mean(Seasonal_cycle_Acc[,2]),
    Stand.Dev = sd(Seasonal_cycle_Acc[,2]),
    Range = range(Seasonal_cycle_Acc[,2])[2] - range(Seasonal_cycle_Acc[,2])[1]
  )
  
  
###########################################################################################
#Seasonal Cycle of Annual Rainy Days
head(Accra_seas)
  
  





Summary_Table <- data.frame(
  Station = "Accra",
  Variable = "Annual Rainfall Total",
  Max = Prcp.df.ann %>% subset(Prcp.df.ann[,2] == max(Prcp.df.ann[,2])) %>% 
    select(date) %>% 
    format("%Y") %>% 
    data.frame(subset(Prcp.df.ann,Prcp.df.ann[,2]==max(Prcp.df.ann[,2]), Prcp)),
  Min = Prcp.df.ann %>% subset(Prcp.df.ann[,2] == min(Prcp.df.ann[,2])) %>% 
    select(date) %>% 
    format("%Y") %>% 
    data.frame(subset(Prcp.df.ann,Prcp.df.ann[,2]==min(Prcp.df.ann[,2]), Prcp)),
  Mean = mean(Prcp.df.ann[,2]),
  Stand.Dev = sd(Prcp.df.ann[,2]),
  Range = range(Prcp.df.ann[,2])[2] - range(Prcp.df.ann[,2])[1]
) 
  
Summary_Table[2,1:ncol(Summary_Table)] <- data.frame(
  Station = "Accra",
  Variable = "Monthly Rainy Days",
  Max = monthly.rdys %>% subset(monthly.rdys[,2] == max(monthly.rdys[,2])) %>% 
    select(date.mn) %>% 
    format("%B") %>% 
    data.frame(subset(monthly.rdys,monthly.rdys[,2]==max(monthly.rdys[,2]),2)),
  Min =  monthly.rdys %>% subset(monthly.rdys[,2] == min(monthly.rdys[,2])) %>% 
    select(date.mn) %>% 
    format("%B") %>% 
    data.frame(subset(monthly.rdys,monthly.rdys[,2]==min(monthly.rdys[,2]),2)),
  Mean = mean(monthly.rdys[,2]),
  Stand.Dev = sd(monthly.rdys[,2]),
  Range = range(monthly.rdys[,2])[2] - range(monthly.rdys[,2])[1]
) 
  
Summary_Table[3:15,1:ncol(Summary_Table)] <-  data.frame(
  Station = "Accra",
  Variable = "Seas.Cycle of Ann.Rainy Days",
  Max = Accra_seas %>% subset(Accra_seas[,2] == max(Accra_seas[,2])) %>% 
    select(1) %>% 
    format("%B-%d") %>% 
    data.frame(subset(Accra_seas,Accra_seas[,2]==max(Accra_seas[,2]),2)),
  Min = Accra_seas %>% subset(Accra_seas[,2] == min(Accra_seas[,2])) %>% 
    select(1) %>% 
    format("%B-%d") %>% 
    data.frame(subset(Accra_seas,Accra_seas[,2]==min(Accra_seas[,2]),2)),
  Mean = mean(Accra_seas[,2]),
  Stand.Dev = sd(Accra_seas[,2]),
  Range = range(Accra_seas[,2])[2] - range(Accra_seas[,2])[1]
)

Summary_Table[nrow(Summary_Table)+1,1:ncol(Summary_Table)] <- data.frame(
  Station = "Accra",
  Variable = "Daily Max Temperature",
  Max = Tmax_1 %>% subset(Tmax_1[,2] == max(Tmax_1[,2])) %>% 
    select(1) %>% 
    format("%Y-%B-%d") %>% 
    data.frame(subset(Tmax_1,Tmax_1[,2]==max(Tmax_1[,2]),2)),
  Min = Tmax_1 %>% subset(Tmax_1[,2] == min(Tmax_1[,2])) %>% 
    select(1) %>% 
    format("%Y-%B-%d") %>% 
    data.frame(subset(Tmax_1,Tmax_1[,2]==min(Tmax_1[,2]),2)),
  Mean = mean(Tmax_1[,2]),
  Stand.Dev = sd(Tmax_1[,2]),
  Range = range(Tmax_1[,2])[2] - range(Tmax_1[,2])[1]
)


Summary_Table[17:18, ] <- data.frame(
  Station = "Accra",
  Variable = "Daily Min Temperature",
  Max = Tmin_1 %>% subset(Tmin_1[,2] == max(Tmin_1[,2])) %>% 
    select(1) %>% 
    format("%Y-%B-%d") %>% 
    data.frame(subset(Tmin_1,Tmin_1[,2]==max(Tmin_1[,2]),2)),
  Min = Tmin_1 %>% subset(Tmin_1[,2] == min(Tmin_1[,2])) %>% 
    select(1) %>% 
    format("%Y-%B-%d") %>% 
    data.frame(subset(Tmin_1,Tmin_1[,2]==min(Tmin_1[,2]),2)),
  Mean = mean(Tmin_1[,2]),
  Stand.Dev = sd(Tmin_1[,2]),
  Range = range(Tmin_1[,2])[2] - range(Tmin_1[,2])[1]
)


names(Summary_Table)[6] <- "Min"








Monthly_ST <- data.frame(
  Station = "Accra",
  Variable = "Monthly Rainy Days",
  Max = monthly.rdys[monthly.rdys[,2] == max(monthly.rdys[,2]), ],
  Min = monthly.rdys[monthly.rdys[,2] == min(monthly.rdys[,2]), ],
  Mean = mean(monthly.rdys[,2]),
  Stand.Dev = sd(monthly.rdys[,2]),
  Range = range(monthly.rdys[,2])[2] - range(monthly.rdys[,2])[1]
) 

Seasonal_Cycle_ST <-  data.frame(
  Station = "Accra",
  Variable = "Seas.Cycle of Ann.Rainfall",
  Max = Seasonal_cycle_Acc_1[Seasonal_cycle_Acc_1[,2] == max(Seasonal_cycle_Acc_1[,2]), ],
  Min = Seasonal_cycle_Acc_1[Seasonal_cycle_Acc_1[,2] == min(Seasonal_cycle_Acc_1[,2]), ],
  Mean = mean(Seasonal_cycle_Acc_1[,2]),
  Stand.Dev = sd(Seasonal_cycle_Acc_1[,2]),
  Range = range(Seasonal_cycle_Acc_1[,2])[2] - range(Seasonal_cycle_Acc_1[,2])[1]
)





Seasonal_cycle_Acc_1 <- Seasonal_cycle_Acc
Seasonal_cycle_Acc_1[,1] <- as.Date(Seasonal_cycle_Acc[,1],origin="2017-01-01")










