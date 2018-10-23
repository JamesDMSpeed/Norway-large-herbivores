#Script til Herbivore data
library(readxl)
Sau <- read_excel("Fra erling/Kopi av sau 1949-2015 pr kommune m areal (003)_GA.xlsx")
View(Kopi_av_sau_1949_2015_pr_kommune_m_areal_003_GA)


BrowGraz <- read_excel("Browsing/BrowvsGraz2015.xlsx")
View(BrowGraz)

BrowGraz0110 <- read_excel("Browsing/BrowvsGraz0110.xlsx")
View(BrowGraz0110)

BrowGraz0310 <- read_excel("Browsing/Browse0210/BrowvsGraz0310.xlsx")
View(BrowGraz0310)



#**********************
#housekeeping####
Sau$knr2017   <- as.factor(Sau$knr2017)
Sau$ART  <-as.factor(Sau$ART)
Sau$fAAR <-as.factor(Sau$AAR)


BrowGraz$fAAR <-as.factor(BrowGraz$AAR)

BrowGraz0310$fAAR <-as.factor(BrowGraz0310$AAR)

#********************************
#Data exploration####

table(Sau$fAAR)
table(Sau$knr2017,Sau$fAAR)


View(Sau[Sau$knr2017=="1503",])


plot(Sau$TOTBAR)


table(BrowGraz$fAAR)
#identify(Sau$TOTBAR)
#View(Sau[617,])


#******************************
#Relationships

boxplot(Sau$TOTBAR~Sau$fAAR)

boxplot(BrowGraz$`Andel G TOT`~BrowGraz$fAAR)
boxplot(BrowGraz$`Andel B TOT`~BrowGraz$fAAR)

boxplot(BrowGraz$`Andel G`~BrowGraz$fAAR)
boxplot(BrowGraz$`Andel B`~BrowGraz$fAAR)
boxplot(log(Sau$TOTBAR+1)~Sau$fAAR)

boxplot(BrowGraz0110$`Andel G`~BrowGraz0110$fAAR)
boxplot(BrowGraz0210$`Andel B`~BrowGraz0210$fAAR)
boxplot(BrowGraz0310$`Andel B`~BrowGraz0310$fAAR)
boxplot(BrowGraz0310$`ABS BROW`~BrowGraz0310$fAAR)

plot(Sau$TOTBAR~Sau$AAR)
lines(lm(Sau$TOTBAR~Sau$AAR))
abline(lm(Sau$TOTBAR~Sau$AAR))

library(ggplot2)
ggplot(data=Sau, aes(x=AAR, y=TOTBAR))+
  geom_smooth(method="lm")

SauSum<-aggregate(data=Sau, TOTBAR~AAR, FUN=sum)
barplot(SauSum$TOTBAR,names.arg = SauSum$AAR)

SauSumTOTBEITE<-aggregate(data=Sau, TOTBEITE~AAR, FUN=sum)
barplot(SauSumTOTBEITE$TOTBEITE,names.arg = SauSumTOTBEITE$AAR)
#ku
#***************************************



ku <- read_excel("Fra erling/ku 1949-2015 pr kommune m areal_GA.xlsx")

View(Kopi_av_ku_1949_2015_pr_kommune_m_areal_GA)
#*******Boxplot
library(ggplot2)
ggplot(data=ku, aes(x=AAR, y=TOTBAR))+
  geom_smooth(method="lm")

KuSum<-aggregate(data=ku, TOTBAR~AAR, FUN=sum)
barplot(KuSum$TOTBAR,names.arg = KuSum$AAR)

KuSumMETBEITAD<-aggregate(data=ku, METBEITAD~AAR, FUN=sum)
barplot(KuSumMETBEITAD$METBEITAD,names.arg = KuSumMETBEITAD$AAR)
