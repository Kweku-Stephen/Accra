#Tmin
require(magrittr)

data.table::data.table(
  Date = Mn.mn.tmn[,1],
  Mean_LNG = Mn.mn.tmn[,2],
  Mean_FH = mean.mn.Tminfh[,2],
  Mean_SH = mean.mn.Tminsh[,2],
  Change =  (mean.mn.Tminsh[,2]) -  (mean.mn.Tminfh[,2]),
  Per_Change = ((mean.mn.Tminsh[,2] -  mean.mn.Tminfh[,2])*100)/
    mean.mn.Tminfh[,2]
) %>% 
  write.csv(file = "Table_Min_Tmp_Accra.csv")


#Tmax
data.table::data.table(
  Date = Mn.mn.tmx[,1],
  Mean_LNG = Mn.mn.tmx[,2],
  Mean_FH = mean.mn.Tmaxfh[,2],
  Mean_SH = mean.mn.Tmaxsh[,2],
  Change =  (mean.mn.Tmaxsh[,2]) - (mean.mn.Tmaxfh[,2]),
  Per_Change = ((mean.mn.Tmaxsh[,2] -  mean.mn.Tmaxfh[,2])*100)/
    mean.mn.Tmaxfh[,2]
) %>% 
  write.csv(file = "Table_Max_Tmp_Accra.csv")

#Prcp
data.table::data.table(
  Date =mean.mn.Prcp[,1],
  Mean_LNG = mean.mn.Prcp[,2],
  Mean_FH = mean.mn.Prcpfh[,2],
  Mean_SH = mean.mn.Prcpsh[,2],
  Change =  (mean.mn.Prcpsh[,2]) -  (mean.mn.Prcpfh[,2]),
  Per_Change = ((mean.mn.Prcpsh[,2] -  mean.mn.Prcpfh[,2])*100)/
    mean.mn.Prcpfh[,2]
) %>% 
  write.csv(file = "Table_Prcp_Accra.csv")
