head(Accra)
Accra.1 <- tidyr::drop_na(Accra)

Monthly.Accra <- xts::xts(Accra.1[,c(2:4)],Accra.1[,1])
head(Monthly.Accra)

Monthly.Accra.Prcp <- apply.monthly(Monthly.Accra[,1],sum,na.rm=T)
Monthly.Accra.Temp <- apply.monthly(Monthly.Accra[,c(2,3)], mean, na.rm=T)
head(Monthly.Accra.Prcp)
head(Monthly.Accra.Temp)
Monthly.Accra.Prcp <- data.frame(Date=index(Monthly.Accra.Prcp),coredata(Monthly.Accra.Prcp))
Monthly.Accra.Temp <- data.frame(Date=index(Monthly.Accra.Temp),coredata(Monthly.Accra.Temp))
head(Monthly.Accra.Temp)
Monthly.Accra.Prcp[,3:4] <- Monthly.Accra.Temp[,2:3]

head(Monthly.Accra.Prcp)


Monthly.Accra.Prcp_1 <- Monthly.Accra.Prcp[
  Monthly.Accra.Prcp[,1] >= "1960-01-30" & Monthly.Accra.Prcp[,1] <= "1989-12-31", 
  ] 


Monthly.Accra.Prcp_2 <- Monthly.Accra.Prcp[!(
  Monthly.Accra.Prcp[,1] >= "1960-01-30" & Monthly.Accra.Prcp[,1] <= "1989-12-31"), 
  ] 



Accra.daily.first.half <- Accra.1[Accra.1[,1] >= "1960-01-01" & Accra.1[,1] <= "1989-12-31", ]
Accra.daily.second.half <- Accra.1[!(Accra.1[,1] >= "1960-01-01" & Accra.1[,1] <= "1989-12-31"), ]


Yearly.Accra <- xts(Accra.1[,c(2:4)], Accra.1[,1])
head(Yearly.Accra)

Yearly.Accra.Prcp <- apply.yearly(Yearly.Accra[,1],sum,na.rm=T)
Yearly.Accra.Prcp <-data.frame(date=index(Yearly.Accra.Prcp),coredata(Yearly.Accra.Prcp))
Yearly.Accra.Prcp[Yearly.Accra.Prcp[,1] == "2011-12-04", 2] <- NA
Yearly.Accra.Temp <- apply.yearly(Yearly.Accra[,2:3], mean,na.rm=T)
Yearly.Accra.Temp <- data.frame(date=index(Yearly.Accra.Temp),coredata(Yearly.Accra.Temp))

Yearly.Accra.Prcp[,3:4] <- Yearly.Accra.Temp[,2:3]                               
Yearly.Accra.data <- Yearly.Accra.Prcp
