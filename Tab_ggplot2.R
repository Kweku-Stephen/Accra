
ggplot(data = Prcp, aes(x=Date))+
  geom_line(aes(y = Prcp[,1],  col = "mpi"), lwd = 1.3) +
  geom_line(aes(y = Prcp[,2], col = "noaa"), lwd = 1.3) +
  geom_line(aes(y = Prcp[,3], col = "cnrm"), lwd = 1.3) +
  geom_line(aes(y = Prcp[,4], col = "miroc5"), lwd = 1.3) + 
  geom_line(aes(y = Prcp_ensemble[,2], col = "ensemble"), lwd = 1.3) + 
  scale_colour_manual("Legend",
                      values = c("mpi" = "red","noaa" = "darkblue","cnrm" = "darkgreen",
                                 "miroc5" = "chocolate","ensemble" = "black")) +
  labs(title = "", x = "Year", y = "rainfall(mm)") + 
  theme(axis.title = element_text(size = 13.5),
        axis.text = element_text(size = 11.5))





data.frame(
  Variable = "Prcp",
  change_1 = mean(Ar_2[,2]) - mean(Ar_1[,2]),
  change_2 = mean(Ar_3[,2]) - mean(Ar_2[,2])
) %>% 
  rbind(
    data.frame(
      Variable = "Tmax",
      change_1 = mean(Tx_2[,2]) - mean(Tx_1[,2]),
      change_2 = mean(Tx_3[,2]) - mean(Tx_2[,2])
    )
  ) %>% 
  rbind(
    data.frame(
      Variable = "Tmin",
      change_1 = mean(Tn_2[,2]) - mean(Tn_1[,2]),
      change_2 = mean(Tn_3[,2]) - mean(Tn_2[,2])
    )
  ) %>% 
  write.csv(
    file = "Diff.csv",
    row.names = FALSE
  )
