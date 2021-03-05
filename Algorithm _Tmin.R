

dta2 <- read.table(
  "Acc_Tmin_6016_StackedData.txt",
  sep = "\t", 
  header = T,
  na.strings = -99.9
)

head(dta2)

#date1 <- 

###############################################################################################

Tmin <- data.frame(
  date1, 
  dta2$Tmin
)

names(Tmin) <- c("date","Tmin")
head(Tmin)

#................................................................................................................................#

#Annual Minimum Temperature
head(Tmin)
#Tmin <- Tmin %>% drop_na()
Tmin.xts <- xts(
  Tmin$Tmin,
  Tmin$date
)

Tmin.ann.xts <- apply.yearly(
  Tmin.xts, 
  mean,
  na.rm=T
)

head(Tmin.ann.xts)

#xonversion to dataframe
Tmin.ann.df <- data.frame(
  date=index(Tmin.ann.xts),
  coredata(Tmin.ann.xts
  )
)

names(Tmin.ann.df)[2] <- "Tmin"

#date conversion
Tmin.ann.df$date <- as.Date(
  Tmin.ann.df$date, 
  format="%Y-%m-%d"
)
head(Tmin.ann.df)

write.csv(Tmin.ann.df, 
          file="Annual Minimum Temperature of Accra (1960-2018).csv")

head(Tmin.ann.df)
MK <- MannKendall(Tmin.ann.df[,2])
summary(MK)


ggplot(Tmin.ann.df, aes(x=date, y=Tmin)) + 
  geom_line(col="firebrick", lwd=2) + geom_smooth(method = lm, col="blue") +  
  labs(title="Minimum Temperature (1960-2018)\n Accra",
       x="Year", y=expression("Temperature("*~degree*c*")")) + 
  scale_x_date(date_breaks= "6 years", date_labels = "%Y") + 
  annotate("text", x = Tmin.ann.df[Tmin.ann.df$date == "1996-12-31", ]$date,
           y = 23, label="P-value = 2.22e-16", cex = 7) +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20))

dev.copy(png, filename="Annual Minimum Temperature of Accra (1960-2018).png", 
         width=1000, height=750)
dev.off()

sumn <- summary(Tmin.ann.df)
write.csv(sumn,
          file = "summary stats for minimum Tmp of Accra.csv")
#or
sumn <- Tmin.ann.df[Tmin.ann.df[,2] == max(Tmin.ann.df[,2]), ]
sumn[2,c(1,2)] <- Tmin.ann.df[Tmin.ann.df[,2] == min(Tmin.ann.df[,2]), ]

dimnames(sumn) <- list(
  c("Max:","Min:"), 
  c("Year","Tmax")
)  

write.csv(sumn,
          file = "summary stats for ann Min Tmp (1960-2018).csv")



#Mean Monthly Temperature
head(Tmin.xts)
Tmin.mn.xts <- apply.monthly(
  Tmin.xts, 
  mean,na.rm=T
)

#conversion to (d.f)
Tmin.mn.df <- data.frame(
  date=index(Tmin.mn.xts),
  coredata(Tmin.mn.xts
  )
)

names(Tmin.mn.df)[2] <- "Tmin"
head(Tmin.mn.df)

Tmin.mn.df$date <- as.Date(
  Tmin.mn.df$date, 
  format="%Y-%m-%d"
)

#Monthly Subsetting
J.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="01")$Tmin, na.rm=T)
F.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="02")$Tmin, na.rm=T)
M.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="03")$Tmin, na.rm=T)
A.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="04")$Tmin, na.rm=T)
Ma.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="05")$Tmin, na.rm=T)
Ju.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="06")$Tmin, na.rm=T)
Jl.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="07")$Tmin, na.rm=T)
Au.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="08")$Tmin, na.rm=T)
S.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="09")$Tmin, na.rm=T)
O.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="10")$Tmin, na.rm=T)
N.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="11")$Tmin, na.rm=T)
D.n <- mean(subset(Tmin.mn.df, format.Date(Tmin.mn.df$date, "%m")=="12")$Tmin, na.rm=T)

Mn.m.tn.v <- c(J.n, F.n,M.n,A.n,Ma.n,Ju.n,
               Jl.n,Au.n,S.n,O.n,N.n,D.n)

date.mn
Mn.mn.tmn <- data.frame(
  date.mn, 
  Mn.m.tn.v
)

write.csv(Mn.mn.tmn, 
          file="Mean Monthly Min Tmp of Accra (1960-2018).csv")

ggplot(Mn.mn.tmn, aes(x=date.mn, y=Mn.m.tn.v)) + 
  geom_line(col="firebrick", lwd=2) +
  labs(title="Mean Monthly Minimum Temperature (1960-2018)\n Accra", 
       x="Month", y=expression("Temperature("*~degree*c*")")) + 
  scale_x_date(date_breaks= "1 month", date_labels = "%b") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Monthly Minimum Temperature of Accra (1960-2018).png", 
         width=1000, height=750)
dev.off()

sumn1 <- summary(Mn.mn.tmn)
write.csv(sumn1,
          file = "summary stats for mean monthly tmin of Accra.csv")
#or
sumn1 <- Mn.mn.tmn[Mn.mn.tmn[,2] == max(Mn.mn.tmn[,2]), ]
sumn1[2,c(1,2)] <- Mn.mn.tmn[Mn.mn.tmn[,2] == min(Mn.mn.tmn[,2]), ]

sumn1[,1] <- data.frame(month.name[c(03,08)])
dimnames(sumn1) <- list(
  c("Max:","Min:"), 
  c("Month","Tmax")
)  

write.csv(sumn1,
          file = "summary stats for Monthly Min Tmp (1960-2018).csv")




# seasonal Cycle of Min Tmp of Tamale
head(Tmin)
data <- Tmin
names(data) <- c("dates","Rain")
head(data)

mean_dailytmn_Acc<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,Jan_11, Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,Jan_18,Jan_19,Jan_20,
                      Jan_21,Jan_22,Jan_23,Jan_24,Jan_25,Jan_26,Jan_27,Jan_28,Jan_29,Jan_30,Jan_31,Feb_32,Feb_33,Feb_34,Feb_35,Feb_36,Feb_37,Feb_38,Feb_39,Feb_40,
                      Feb_41,Feb_42,Feb_43, Feb_44,Feb_45,Feb_46,Feb_47,Feb_48,Feb_49,Feb_50,Feb_51,Feb_52,Feb_53,Feb_54,Feb_55,Feb_56,Feb_57,Feb_58,Feb_59,Feb_59_1,Mar_60, 
                      Mar_61,Mar_62,Mar_63,Mar_64,Mar_65,Mar_66,Mar_67,Mar_68,Mar_69,Mar_70,Mar_71,Mar_72,Mar_73,Mar_74,Mar_75,Mar_76,Mar_77,Mar_78,Mar_79,Mar_80,Mar_81,Mar_82,
                      Mar_83,Mar_84,Mar_85,Mar_86,Mar_87,Mar_88,Mar_89,Mar_90,Apr_91,Apr_92,
                      Apr_93,Apr_94,Apr_95,Apr_96,Apr_97,Apr_98,Apr_99,Apr_100,Apr_101,Apr_102,
                      Apr_103,Apr_104,Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,
                      Apr_112,Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,
                      May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,
                      May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,
                      May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,
                      May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,
                      Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,
                      Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,
                      Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,
                      Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,
                      Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Acctmn <-data.frame(
  c(1:366),
  mean_dailytmn_Acc
)

write.csv(Seasonal_cycle_Acctmn, 
          file = "data_for_seasonal_cycle_of_Min tmp of Accra (1960-2018).csv")

oo <- ksmooth(
  c(1:366), 
  mean_dailytmn_Acc, 
  bandwidth = 15
)

oo.1 <- data.frame(oo) # converting to a dataframe

ggplot(Seasonal_cycle_Acctmn, aes(x=c(1:366), y=mean_dailytmn_Acc)) + 
  geom_line(col="firebrick", lwd=1) + 
  labs(title="Seasonal Cycle of Minimum Tmperature (1960-2018)\n Accra",
       x="Day", y=yyy) + 
  geom_line(data = oo.1, aes(x=x, y=y), col="blue", lwd=1) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Min. Temperature of Accra (1960-2018).png",
         width=1000, height=750)
dev.off()

sumn2 <- summary(Seasonal_cycle_Acctmn)
write.csv(sumn2,
          file = "Summary stats for seasonal cycle of min tmp of Accra.csv")



#Indexing of Min tmp of Tamale (1960-2016)
head(Tmin.ann.df)

lng.mn.Tmin <- mean(Tmin.ann.df$Tmin, na.rm = T)
sd.Tmin <- sd(Tmin.ann.df$Tmin, na.rm = T)

Tmin.ann.df.anom.v <- (Tmin.ann.df$Tmin - lng.mn.Tmin)/
  sd.Tmin

Tmin.ann.df.anom <- data.frame(
  Tmin.ann.df$date,
  Tmin.ann.df.anom.v
)

write.csv(Tmin.ann.df.anom, 
          file="Index of Annual Min Tmp of Accra (1960-2018).csv")

ggplot(Tmin.ann.df.anom, aes(Tmin.ann.df.1$date, Tmin.ann.df.anom.v,
                             ymin = 0,ymax = Tmin.ann.df.anom.v)) + 
  geom_linerange(data = Tmin.ann.df.anom, aes(colour = ifelse(Tmin.ann.df.anom.v >0,
      "Positive", "Negative")),stat = "identity",position = "identity",size=2) + 
  theme(legend.title = element_text(size = 2, colour = FALSE)) + 
  (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +
  geom_hline(yintercept=0) + 
  labs(title=" Minimum Temperature Anomaly (1960-2018)\n Accra",
       x="Year", y="Anomaly") +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17))

dev.copy(png, filename="Index of AnnuAl Minimum Tmp of Accra (1960 - 2018).png",
         height=750, width=1000)
dev.off()

sumn2 <- summary(Tmin.ann.df.anom)
write.csv(sumn2,
          file = "summary stats for Anomaly of Min Tmp of Accra.csv")
#or
sumn2 <- Tmin.ann.df.anom[Tmin.ann.df.anom[,2] == max(Tmin.ann.df.anom[,2]), ]
sumn2[2,c(1,2)] <- Tmin.ann.df.anom[Tmin.ann.df.anom[,2] == min(Tmin.ann.df.anom[,2]), ]

dimnames(sumn2) <- list(
  c("Max:","Min:"), 
  c("Year","Anomaly")
)  

write.csv(sumn2,
          file = "summary stats for Ann Min Tmp  Anomaly(1960-2018).csv")





#Dividing Into two halves
#Dividing Tmax into two halves
#Dividing daily Tmin
head(Tmin)
Tmin.fh <- Tmin[(Tmin$date >= "1960-01-01" & Tmin$date <= "1989-12-31"), ]
Tmin.sh <- Tmin[(Tmin$date >= "1990-01-01" & Tmin$date <= "2018-12-31"), ]
#Dividing annual Tmin
Tmin_fh <- Tmin.ann.df[(Tmin.ann.df$date >= "1960-12-31" & 
                          Tmin.ann.df$date <= "1989-12-31"), ]

Tmin_sh <- Tmin.ann.df[(Tmin.ann.df$date >= "1990-12-31" & 
                          Tmin.ann.df$date <= "2018-12-31"), ]


#Annual Tmax first half (1960-1988)
head(Tmin_fh)

write.csv(Tmin_fh,
          file="Annual Min.Tmp of Accra (1960-1989).csv")

MK <- MannKendall(Tmin_fh$Tmin)
summary(MK)

ggplot(Tmin_fh, aes(x=Tmin_fh$date, y=Tmin_fh$Tmin)) +
  geom_line(col="firebrick", lwd=2) + 
  geom_smooth(method = lm, col="blue") + 
  labs(title='Minimum Temperature (1960-1989)\n Accra',
       x="Year", y=yyy) + 
  annotate("text", x = Tmin_fh[Tmin_fh$date == "1967-12-31", ]$date, 
           y=22.0, label = "P-value=0.00013459", cex = 7) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Annual Minimum Tmp of Accra (1960 - 1989).png",
         width=1000, height=750)
dev.off()

sumn3 <- summary(Tmin_fh)
write.csv(sumn3,
          file = "summary stats for Min Tmp of Accra (1960-1989).csv")

#or
sumn3 <- Tmin_fh[Tmin_fh[,2] == max(Tmin_fh[,2]), ]
sumn3[2,c(1,2)] <- Tmin_fh[Tmin_fh[,2] == min(Tmin_fh[,2]), ]

dimnames(sumn3) <- list(
  c("Max:","Min:"), 
  c("Year","Tmax")
)  

write.csv(sumn3,
          file = "summary stats for Ann Min Tmp  (1960-1989).csv")



#Mean Monthly Tmin
head(Tmin.mn.df)
#Dividing monthly
Tmin.mn.dffh <- Tmin.mn.df[(Tmin.mn.df$date >= "1960-01-31" & 
                              Tmin.mn.df$date <= "1989-12-31"), ]

Tmin.mn.dfsh <- Tmin.mn.df[(Tmin.mn.df$date >= "1990-01-31" & 
                              Tmin.mn.df$date <= "2018-12-31"), ]


#First Half
head(Tmin.mn.dffh)
Jann.fh <- mean(subset(Tmin.mn.dffh, format.Date(Tmin.mn.dffh$date, "%m")=="01")$Tmin, na.rm=T)
Febn.fh <- mean(subset(Tmin.mn.dffh, format.Date(Tmin.mn.dffh$date, "%m")=="02")$Tmin, na.rm=T)
Marchn.fh <- mean(subset(Tmin.mn.dffh, format.Date(Tmin.mn.dffh$date, "%m")=="03")$Tmin, na.rm=T)
Apriln.fh <- mean(subset(Tmin.mn.dffh, format.Date(Tmin.mn.dffh$date, "%m")=="04")$Tmin, na.rm=T)
Mayn.fh <- mean(subset(Tmin.mn.dffh, format.Date(Tmin.mn.dffh$date, "%m")=="05")$Tmin, na.rm=T)
Junen.fh <- mean(subset(Tmin.mn.dffh, format.Date(Tmin.mn.dffh$date, "%m")=="06")$Tmin, na.rm=T)
Julyn.fh <- mean(subset(Tmin.mn.dffh, format.Date(Tmin.mn.dffh$date, "%m")=="07")$Tmin, na.rm=T)
Augustn.fh <- mean(subset(Tmin.mn.dffh, format.Date(Tmin.mn.dffh$date, "%m")=="08")$Tmin, na.rm=T)
Septembern.fh <- mean(subset(Tmin.mn.dffh, format.Date(Tmin.mn.dffh$date, "%m")=="09")$Tmin, na.rm=T)
Octobern.fh <- mean(subset(Tmin.mn.dffh, format.Date(Tmin.mn.dffh$date, "%m")=="10")$Tmin, na.rm=T)
Novembern.fh <- mean(subset(Tmin.mn.dffh, format.Date(Tmin.mn.dffh$date, "%m")=="11")$Tmin, na.rm=T)
Decembern.fh <- mean(subset(Tmin.mn.dffh, format.Date(Tmin.mn.dffh$date, "%m")=="12")$Tmin, na.rm=T)


#Averaging all months
#creating a vector
mean.mn.Tminfh.v <- c(Jann.fh,Febn.fh,Marchn.fh,Apriln.fh,Mayn.fh,Junen.fh,
                      Julyn.fh,Augustn.fh,Septembern.fh,Octobern.fh,Novembern.fh,Decembern.fh)

date.mn 

mean.mn.Tminfh <- data.frame(
  date.mn,
  mean.mn.Tminfh.v
)

write.csv(mean.mn.Tminfh,
          file="Mean Monthly Minimum Tmp of Accra (1960-1989).csv")

ggplot(mean.mn.Tminfh, aes(x=date.mn, y=mean.mn.Tminfh.v)) + 
  geom_line(col="firebrick", lwd=2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(title="Mean Monthly Minimum Temperature (1960-1989)\n Accra",
       x="Month",y=yyy) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Monthly Min Tmp of Accra (1960-1989).png",
         width=1000, height=750)
dev.off()

sumn4 <- summary(mean.mn.Tminfh)
write.csv(sumn4,
          file = "summary stats for monthly tmin of Accra (1960-1989).csv")
#or
sumn4 <- mean.mn.Tminfh[mean.mn.Tminfh[,2] == max(mean.mn.Tminfh[,2]), ]
sumn4[2,c(1,2)] <- mean.mn.Tminfh[mean.mn.Tminfh[,2] == min(mean.mn.Tminfh[,2]), ]

sumn4[,1] <- data.frame(month.name[c(04,08)])
dimnames(sumn4) <- list(
  c("Max:","Min:"), 
  c("Month","Tmax")
)  

write.csv(sumn4,
          file = "summary stats for mean month Min Tmp (1960-1989).csv")




#Annual Tmin second half (1989 - 2017)
head(Tmin_sh)
write.csv(Tmin_sh, 
          file="Annual Tmin of Accra (1990-2018).csv")

MK <- MannKendall(Tmin_sh[,2])
summary(MK)

ggplot(Tmin_sh, aes(x=Tmin_sh$date, y=Tmin_sh$Tmin)) +
  geom_line(col="firebrick", lwd=2) + 
  geom_smooth(method = lm, col="blue") + 
  labs(title='Minimm Temperature (1990-2018)\n Accra',
       x="Years", y=yyy) + 
  annotate("text", x=Tmin_sh[Tmin_sh$date == "2000-12-31", ]$date ,
           y=24.75, label="P-value=2.3842e-07", cex=7) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Annual Minimum Tmp. of Accra (1990-2018).csv", 
         width=1000, height=750)
dev.off()

sumn5 <- summary(Tmin_sh)
write.csv(sumn5,
          file = "summary stats for min Tmp of Accra (1990-2018).csv")

#or
sumn55 <- Tmin_sh[Tmin_sh[,2] == max(Tmin_sh[,2]), ]
sumn55[2,c(1,2)] <- Tmin_sh[Tmin_sh[,2] == min(Tmin_sh[,2]), ]

dimnames(sumn55) <- list(
  c("Max:","Min:"), 
  c("Year","Tmax")
)  

write.csv(sumn55,
          file = "summary stats for Ann Min Tmp  Anomaly(1990-2018).csv")


Tmin_sh_1 <- Tmin_sh
Tmin_sh_1[30,c(1,2)] <- list(NA,NA)

ggplot(Tmin_fh, aes(x=1:30)) +
  geom_line(aes(y=Tmin_fh[,2],col="1960-1989"), lwd=2) + 
  geom_line(aes(y=Tmin_sh_1[,2],col="1990-2018"), lwd=2) +
  scale_colour_manual("Time Period",
                      values = c("1960-1989"="darkblue","1990-2018"="red")) +
  labs(title='Minimum Temperature (1990-2018)\n Accra',
       x="Years", y=yyy) + 
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))
dev.copy(png, filename="Minimum Temperature of Accra (1960-89-2018.png",
         height=750, width =1000)
dev.off()




#second Half
head(Tmin.mn.dfsh)

#Averaging all months
Jann.sh <- mean(subset(Tmin.mn.dfsh, format.Date(Tmin.mn.dfsh$date, "%m")=="01")$Tmin, na.rm=T)
Febn.sh <- mean(subset(Tmin.mn.dfsh, format.Date(Tmin.mn.dfsh$date, "%m")=="02")$Tmin, na.rm=T)
Marchn.sh <- mean(subset(Tmin.mn.dfsh, format.Date(Tmin.mn.dfsh$date, "%m")=="03")$Tmin, na.rm=T)
Apriln.sh <- mean(subset(Tmin.mn.dfsh, format.Date(Tmin.mn.dfsh$date, "%m")=="04")$Tmin, na.rm=T)
Mayn.sh <- mean(subset(Tmin.mn.dfsh, format.Date(Tmin.mn.dfsh$date, "%m")=="05")$Tmin, na.rm=T)
Junen.sh <- mean(subset(Tmin.mn.dfsh, format.Date(Tmin.mn.dfsh$date, "%m")=="06")$Tmin, na.rm=T)
Julyn.sh <- mean(subset(Tmin.mn.dfsh, format.Date(Tmin.mn.dfsh$date, "%m")=="07")$Tmin, na.rm=T)
Augustn.sh <- mean(subset(Tmin.mn.dfsh, format.Date(Tmin.mn.dfsh$date, "%m")=="08")$Tmin, na.rm=T)
Septembern.sh <- mean(subset(Tmin.mn.dfsh, format.Date(Tmin.mn.dfsh$date, "%m")=="09")$Tmin, na.rm=T)
Octobern.sh <- mean(subset(Tmin.mn.dfsh, format.Date(Tmin.mn.dfsh$date, "%m")=="10")$Tmin, na.rm=T)
Novembern.sh <- mean(subset(Tmin.mn.dfsh, format.Date(Tmin.mn.dfsh$date, "%m")=="11")$Tmin, na.rm=T)
Decembern.sh <- mean(subset(Tmin.mn.dfsh, format.Date(Tmin.mn.dfsh$date, "%m")=="12")$Tmin, na.rm=T)


#creating a vector
mean.mn.Tminsh.v <- c(Jann.sh,Febn.sh,Marchn.sh,Apriln.sh,Mayn.sh,Junen.sh,Julyn.sh,
                      Augustn.sh,Septembern.sh,Octobern.sh,Novembern.sh,Decembern.sh)

date.mn 
mean.mn.Tminsh <- data.frame(
  date.mn, 
  mean.mn.Tminsh.v
)

write.csv(mean.mn.Tminsh, 
          file="Mean Monthly Minimum Temperature of Accra (1990-2018).csv")

ggplot(mean.mn.Tminsh, aes(x=date.mn, y=mean.mn.Tminsh.v)) + 
  geom_line(col="firebrick", lwd=2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  labs(title="Mean Monthly Minimum Temperature (1990-2018)\n Accra", 
       x="Month",y=yyy, ylim=c(18,26)) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Mean Monthly Minimum Tmp. of Accra (1990-2018).csv",
         width=1000, height=750)
dev.off()

sumn6 <- summary(mean.mn.Tminsh)
write.csv(sumn6,
          file = "summary stats for monthly min Tmp of Accra (1990-2018).csv")
#or
sumn66 <- mean.mn.Tminsh[mean.mn.Tminsh[,2] == max(mean.mn.Tminsh[,2]), ]
sumn66[2,c(1,2)] <- mean.mn.Tminsh[mean.mn.Tminsh[,2] == min(mean.mn.Tminsh[,2]), ]

sumn66[,1] <- data.frame(month.name[c(03,08)])
dimnames(sumn66) <- list(
  c("Max:","Min:"), 
  c("Month","Tmax")
)  

write.csv(sumn66,
          file = "summary stats for Ann Min Tmp (1990-2018).csv")




#plot(mean.mn.Tminfh, type="l",ylim=c(15,25) ,lwd=3, 
#     col="darkblue", main="Mean Monthly Min Tmp of Damongo- 1960-2017", 
#     xlab="Months", ylab=yyy)

#lines(mean.mn.Tminsh, col="red", lwd=3, type="l")

#legend("topright", legend = c("1960-1988", "1989-2017"), 
#       col=c("darkblue","red"), 
#       bty = "n", lty = 1,lwd=3)


ggplot(mean.mn.Tminfh,aes(x=mean.mn.Tminfh[,1])) + 
  geom_line(aes(y=mean.mn.Tminfh[,2],col="1960-1989"), lwd= 2) +
  geom_line(aes(y=mean.mn.Tminsh[,2],col="1990-2018"), lwd=2) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
  scale_colour_manual("Time Period",
                      values = c("1960-1989"="darkblue","1990-2018"="red")) +
  xlab("Month") +
  ylab(yyy) + ggtitle("Mean Monthly Minimum Temperature (1960-2018)\n Accra") +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 17))

dev.copy(png, filename = "Mean Monthly Min Tmp of Accra (1960-89-2018).png",
         width = 1000, height = 750)
dev.off()




#Max Tmp Change between two halves
Tmpn_change <- (mean(Tmin_sh[,2], na.rm = T) - mean(Tmin_fh[,2], na.rm = T))*100/
  mean(Tmin_fh[,2], na.rm = T)

#change <- expression("0.8815479(" ~ degree * c * ")")


#Monthly difference in Min Tmp b/n two halves
head(mean.mn.Tminfh)
head(mean.mn.Tminsh)

#First Half
storage2 <- numeric(12)
o <- c(mean.mn.Tminfh[,2])

for (i in 1:12) {
  storage2[i] <- o[i]
}

#Second Half
storage3 <- numeric(12)
p <- c(mean.mn.Tminsh[,2])

for (i in 1:12) {
  storage3[i] <- p[i]
}

change_in_Min_Temp <- data.frame(
  Month=month.name,
  Change=storage3-storage2)

#Percentage Change in Monthly Tmin
storage5 <- numeric(12)
k <- c(change_in_Min_Temp[,2])

for (i in 1:12) {
  storage5[i] <- ((k*100)/mean.mn.Tminfh[,2])[i]
}

Percen_change_tmin <- storage5





#seasonal Cycle 
#(first half)
head(Tmin.fh)
data <- Tmin.fh
names(data) <- c('dates','Rain')
head(data)

mean_dailytmn_Accfh<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
                        Jan_11, Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,Jan_18,Jan_19,
                        Jan_20,Jan_21,Jan_22,Jan_23,Jan_24,Jan_25,Jan_26,Jan_27,Jan_28,
                        Jan_29,Jan_30,Jan_31,Feb_32,Feb_33,Feb_34,Feb_35,Feb_36,Feb_37,
                        Feb_38,Feb_39,Feb_40,Feb_41,Feb_42,Feb_43, Feb_44,Feb_45,Feb_46,Feb_47,
                        Feb_48,Feb_49,Feb_50,Feb_51,Feb_52,Feb_53,Feb_54,Feb_55,Feb_56,Feb_57,
                        Feb_58,Feb_59,Feb_59_1,Mar_60, Mar_61,Mar_62,Mar_63,Mar_64,Mar_65,
                        Mar_66,Mar_67,Mar_68,Mar_69,Mar_70,Mar_71,Mar_72,Mar_73,Mar_74,Mar_75,
                        Mar_76,Mar_77,Mar_78,Mar_79,Mar_80,Mar_81,Mar_82,Mar_83,Mar_84,Mar_85,
                        Mar_86,Mar_87,Mar_88,Mar_89,Mar_90,Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,
                        Apr_96,Apr_97,Apr_98,Apr_99,Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,
                        Apr_105,Apr_106,Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,
                        Apr_113,Apr_114,Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,
                        May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,
                        May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,
                        May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Acctmnfh <-data.frame(
  c(1:366),
  mean_dailytmn_Accfh
)

write.csv(Seasonal_cycle_Acctmnfh, 
          file = "data_for_seasonal_cycle_of_Min tmp of Accra (1960-1989).csv")

gg <- ksmooth(
  c(1:366), 
  mean_dailytmn_Accfh, 
  bandwidth = 15
)

gg.1 <- data.frame(gg) # converting to a dataframe

ggplot(Seasonal_cycle_Acctmnfh, aes(x=c(1:366), y=mean_dailytmn_Accfh)) + 
  geom_line(col="firebrick", lwd=1) + 
  labs(title="Seasonal Cycle of Minimum Temperature (1960-1989)\n Accra",
       x="Day", y=yyy) + 
  geom_line(data = gg.1, aes(x=x, y=y), col="blue", lwd=1) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Minimum Tmp. of Accra (1960-1989).csv", 
         width=1000, height=750)
dev.off()

sumn8 <- summary(Seasonal_cycle_Acctmnfh)
write.csv(sumn8,
          file = "summary stats for seasonal cycle of min tmp of Accra (1960-1989).csv")




#Second Half
head(Tmin.sh)
data <- Tmin.sh
names(data) <- c("dates","Rain")
head(data)

mean_dailytmn_Accsh<- c(Jan_1,Jan_2, Jan_3, Jan_4, Jan_5,Jan_6,Jan_7,Jan_8,Jan_9,Jan_10,
                        Jan_11, Jan_12,Jan_13,Jan_14,Jan_15,Jan_16,Jan_17,Jan_18,Jan_19,
                        Jan_20,Jan_21,Jan_22,Jan_23,Jan_24,Jan_25,Jan_26,Jan_27,Jan_28,Jan_29,
                        Jan_30,Jan_31,Feb_32,Feb_33,Feb_34,Feb_35,Feb_36,Feb_37,Feb_38,Feb_39,
                        Feb_40,Feb_41,Feb_42,Feb_43, Feb_44,Feb_45,Feb_46,Feb_47,Feb_48,Feb_49,
                        Feb_50,Feb_51,Feb_52,Feb_53,Feb_54,Feb_55,Feb_56,Feb_57,Feb_58,Feb_59,
                        Feb_59_1,Mar_60, Mar_61,Mar_62,Mar_63,Mar_64,Mar_65,Mar_66,Mar_67,
                        Mar_68,Mar_69,Mar_70,Mar_71,Mar_72,Mar_73,Mar_74,Mar_75,Mar_76,Mar_77,
                        Mar_78,Mar_79,Mar_80,Mar_81,Mar_82,Mar_83,Mar_84,Mar_85,Mar_86,Mar_87,
                        Mar_88,Mar_89,Mar_90,Apr_91,Apr_92,Apr_93,Apr_94,Apr_95,Apr_96,Apr_97,
                        Apr_98,Apr_99,Apr_100,Apr_101,Apr_102,Apr_103,Apr_104,Apr_105,Apr_106,
                        Apr_107,Apr_108,Apr_109,Apr_110,Apr_111,Apr_112,Apr_113,Apr_114,
                        Apr_115,Apr_116,Apr_117,Apr_118,Apr_119,Apr_120,May_121,May_122,May_123,May_124,May_125,May_126,May_127,May_128,May_129,May_130,May_131,May_132,May_133,May_134,May_135,May_136,May_137,May_138,May_139,May_140,May_141,May_142,May_143,May_144,May_145,May_146,May_147,May_148,May_149,May_150,May_151,Jun_152,Jun_153,Jun_154,Jun_155,Jun_156,Jun_157,Jun_158,Jun_159,Jun_160,Jun_161,Jun_162,Jun_163,Jun_164,Jun_165,Jun_166,Jun_167,Jun_168,Jun_169,Jun_170,Jun_171,Jun_172,Jun_173,Jun_174,Jun_175,Jun_176,Jun_177,Jun_178,Jun_179,Jun_180,Jun_181,Jul_182,Jul_183,Jul_184,Jul_185,Jul_186,Jul_187,Jul_188,Jul_189,Jul_190,Jul_191,Jul_192,Jul_193,Jul_194,Jul_195,Jul_196,Jul_197,Jul_198,Jul_199,Jul_200,Jul_201,Jul_201,Jul_203,Jul_204,Jul_205,Jul_206,Jul_207,Jul_208,Jul_209,Jul_210,Jul_211,Jul_212,Aug_213,Aug_214,Aug_215,Aug_216,Aug_217,Aug_218,Aug_219,Aug_220,Aug_221,Aug_222,Aug_223,Aug_224,Aug_225,Aug_226,Aug_227,Aug_228,Aug_229,Aug_230,Aug_231,Aug_232,Aug_233,Aug_234,Aug_235,Aug_236,Aug_237,Aug_238,Aug_239,Aug_240,Aug_241,Aug_242,Aug_243,Sep_244,Sep_245,Sep_246,Sep_247,Sep_248,Sep_249,Sep_250,Sep_251,Sep_252,Sep_253,Sep_254,Sep_255,Sep_256,Sep_257,Sep_258,Sep_259,Sep_260,Sep_261,Sep_262,Sep_263,Sep_264,Sep_265,Sep_266,Sep_267,Sep_268,Sep_269,Sep_270,Sep_271,Sep_272,Sep_273,Oct_274,Oct_275,Oct_276,Oct_277,Oct_278,Oct_279,Oct_280,Oct_281,Oct_282,Oct_283,Oct_284,Oct_285,Oct_286,Oct_287,Oct_288,Oct_289,Oct_290,Oct_291,Oct_292,Oct_293,Oct_294,Oct_295,Oct_296,Oct_297,Oct_298,Oct_299,Oct_300,Oct_301,Oct_302,Oct_303,Oct_304,Nov_305,Nov_306,Nov_307,Nov_308,Nov_309,Nov_310,Nov_311,Nov_312,Nov_313,Nov_314,Nov_315,Nov_316,Nov_317,Nov_318,Nov_319,Nov_320,Nov_321,Nov_322,Nov_323,Nov_324,Nov_325,Nov_326,Nov_327,Nov_328,Nov_329,Nov_330,Nov_331,Nov_332,Nov_333,Nov_334,Dec_335,Dec_336,Dec_337,Dec_338,Dec_339,Dec_340,Dec_341,Dec_342,Dec_343,Dec_344,Dec_345,Dec_346,Dec_347,Dec_348,Dec_349,Dec_350,Dec_351,Dec_352,Dec_353,Dec_354,Dec_355,Dec_356,Dec_357,Dec_358,Dec_359,Dec_360,Dec_361,Dec_362,Dec_363,Dec_364,Dec_365)

Seasonal_cycle_Acctmnsh <-data.frame(
  c(1:366),
  mean_dailytmn_Accsh
)

write.csv(Seasonal_cycle_Acctmnsh, 
          file = "data_for_seasonal_cycle_of_Min tmp of Accra (1990-2018).csv")

ee <- ksmooth(
  c(1:366), 
  mean_dailytmn_Accsh, 
  bandwidth = 15
)
ee.1 <- data.frame(ee) # converting to a dataframe

ggplot(Seasonal_cycle_Acctmnsh, aes(x=c(1:366), y=mean_dailytmn_Accsh)) + 
  geom_line(col="firebrick", lwd=1) + 
  labs(title="Seasonal Cycle of Minimum Temperature (1990-2018)\n Accra",
       x="Days", y=yyy) + 
  geom_line(data = ee.1, aes(x=x, y=y), col="blue", lwd=1) +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Minimum Tmp. of Accra (1990-2018).csv", 
         width=1000, height=750)
dev.off()
sumn8 <- summary(Seasonal_cycle_Acctmnsh)
write.csv(sumn8,
          file = "summary stats for seasonal cycle of min Tmp of Accra.csv")


#plot(Seasonal_cycle_Damtmnfh, lwd=1,ylim=c(14,26) ,
#    type="l", col="firebrick", 
#     main="Seasonal Min Tmp Cycle of Damongo- 1960-2017",
#     xlab="Days", ylab=yyy)

#lines(gg.1, col="blue", lwd=3)
#lines(Seasonal_cycle_Damtmnsh, type="l", col="firebrick")
#lines(ee.1, lwd=3, col="red")
#legend("topright", col=c("blue","red"), 
#       legend = c("1960-1988","1989-2017"),
#       bty = "n", lty = 1, lwd=3)


ggplot(Seasonal_cycle_Acctmnfh, aes(x=Seasonal_cycle_Acctmnfh[,1])) + 
  geom_line(aes(y=Seasonal_cycle_Acctmnfh[,2]), lwd = .25) + 
  geom_line(aes(y=gg.1[,2], col="1960-1989"), lwd=1.25) + 
  geom_line(aes(y=Seasonal_cycle_Acctmnsh[,2]), lwd=.25) +
  geom_line(aes(y=ee.1[,2], col="1990-2018"),lwd=1.25) + 
  scale_colour_manual("Time Period",
                      values = c("1960-1989"="darkblue","1990-2018"="red")) +
  xlab("Day") + ylab(yyy) + 
  ggtitle("Seasonal Cycle of Minimum Temperature (1960-2018)\n Accra") +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 13))

dev.copy(png, filename="Seasonal Cycle of Max Tmp of Accra (1960-89-2018).png", 
         width = 1000, height = 750)

dev.off()



require(dplyr)
Tmin.ann.df_1 <- arrange(
  Tmin.ann.df, Tmin
)

Tmin.ann.df_1$Rank <- 1:length(
  Tmin.ann.df_1[,2]
)

ptile <- function(x){
  x / 100 * (59 + 1)
}

#25th  Percentile
ptile(25)
Tmin_per <- Tmin.ann.df_1[Tmin.ann.df_1$Rank == 15, ]

#50th Percentile
ptile(50)
Tmin.ann.df_1[Tmin.ann.df_1$Rank == 30, ]

#75th Percentile
ptile(75)
Tmin.ann.df_1[Tmin.ann.df_1$Rank <= 45, ]

Tmin_per[2:3,1:3] <- c(Tmin.ann.df_1[Tmin.ann.df_1$Rank == 30, ],
                       Tmin.ann.df_1[Tmin.ann.df_1$Rank == 45, ]
)
Tmin_per[3,1:3] <- Tmin.ann.df_1[Tmin.ann.df_1$Rank == 45, ]

rownames(Tmin_per) <- c("1st Percentile", "2nd Percentile", "3rd Percentile")
head(Tmin_per)
write.csv(Tmin_per, file="Percentile_Accra_Tmin.csv")





#Indexing
#first half
#head(Tmin_fh)
#Tmin_fh.1 <- drop_na(Tmin_fh)
#lng.mn.Tminfh <- mean(Tmin_fh.1$Tmin)
#sd.Tminfh <- sd(Tmin_fh.1$Tmin)
#Tmin_fh_anom.v <- (Tmin_fh.1$Tmin - lng.mn.Tminfh)/sd.Tminfh
#Tmin_fh_anom <- data.frame(Tmin_fh.1$date, Tmin_fh_anom.v)
#write.csv(Tmin_fh_anom, file="Index of Annual Minimum Tmp of Damongo (1960-1988).csv")

#Creating a dummy data.frame from 1977-1988
#Tmin_fh_anom[15:26, ] <- data.frame(seq(as.Date("1977-12-31"), length.out = 12, by="year"), NA)

#ggplot(Tmin_fh_anom, aes(Tmin_fh_anom$Tmin_fh.1.date, Tmin_fh_anom.v, ymin = 0,ymax = Tmin_fh_anom.v)) + 
#  geom_linerange(data = Tmin_fh_anom, aes(colour = ifelse(Tmin_fh_anom.v >0, "Positive", "Negative")),stat = "identity",
#                                                                                                        position = "identity",size=2) + 
#  theme(legend.title = element_text(size = 2, colour = F)) + (scale_x_date(date_breaks="4 year",date_labels="%Y"))  +geom_hline(yintercept=0) + 
#  labs(title="Index of Annual Minimum Tmp of Tamale \n (1960-1988)", x="Years", y="Index")
#dev.copy(png, filename="Index of AAnnuAl Minimum Tmp of Damongo (1960-1988).png", width=1000, height=750)
#dev.off()


#Second half
#head(Tmin_sh)
#Tmin_sh.1 <- drop_na(Tmin_sh)
#lng.mn.Tminsh <- mean(Tmin_sh.1$Tmin)
#sd.Tminsh <- sd(Tmin_sh.1$Tmin)
#Tmin_sh_anom.v <- (Tmin_sh.1$Tmin - lng.mn.Tminsh)/sd.Tminsh
#Tmin_sh_anom <- data.frame(Tmin_sh.1$date, Tmin_sh_anom.v)
#write.csv(Tmin_sh_anom, file="Index of min Tmp of Damongo(1989-2017).csv")

##ggplot(Tmin_sh_anom, aes(Tmin_sh.1$date, Tmin_sh_anom.v, ymin = 0,ymax = Tmin_sh_anom.v)) + 
#  geom_linerange(data = Tmin_sh_anom, aes(colour = ifelse(Tmin_sh_anom.v >0, "Positive", "Negative")),stat = "identity",
#                                                                                                         position = "identity",size=2) + 
#  theme(legend.title = element_text(size = 2, colour = F)) + (scale_x_date(date_breaks="4 years",date_labels="%Y"))  +
#  geom_hline(yintercept=0) + labs(title="Index of Annual Minimum Tmp of Damongo (1989-2017)", x="Years", y="Index")
#dev.copy(png, filename="Inde of Annual Minimum Tmp of Damongo(1989-2017).png", width=1000, height=750)
#dev.off()


#HOW TO ADD A COLUMN ON TOP OF ANOTHER IN R

