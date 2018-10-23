#Script til Herbivore data

getwd()

library(readxl)
dat <- read_excel("BrowsGraz/BrowvsGraz0310_final.xlsx", 
                  sheet = "Final B and G 0510")
View(dat)


#**********************
#housekeeping####
#Sau$knr2017   <- as.factor(Sau$knr2017)
#Sau$ART  <-as.factor(Sau$ART)
#Sau$fAAR <-as.factor(Sau$AAR)


dat$fAAR <-as.factor(dat$AAR)
dat$LANDKAT <-as.factor(dat$LANDKAT)
levels(dat$LANDKAT)
dat$ALTREG <-as.factor(dat$ALTREG)
table(dat$ALTREG)
table(dat$ALTREG, dat$LANDKAT)

#********************************
#Data exploration####

# Her kan det være lurt å fjerne kommuner (GJØR TIL na) som har hverken B eller G

#table(Sau$fAAR)
#table(Sau$knr2017,Sau$fAAR)


#View(Sau[Sau$knr2017=="1503",])


#plot(Sau$TOTBAR)


#table(BrowGraz$fAAR)
#identify(Sau$TOTBAR)
#View(Sau[617,])


#******************************
#Relationships

boxplot(dat$`Andel B`~dat$fAAR,
        xlab = "Year", ylab = "Proportion browsing")

par(mar=c(5,5,2,2))
boxplot((dat$`ABS BROW`)/1000000~dat$fAAR,
        xlab = "Year", ylab = "Absolute MB browsed\n(ton/1000)",
        las = 2)


#******************************
# Her lager jeg barplot for LANDKAT per år

#******************************


landkat_yr <- aggregate(data = dat,
                        `Andel B` ~ fAAR + LANDKAT,
                        FUN = mean, na.rm = T)
std <- function(x) sd(x)/sqrt(length(x))
landkat_yr_se <- aggregate(data = dat,
                        `Andel B` ~ fAAR + LANDKAT,
                        FUN = std)
landkat_yr$se <- landkat_yr_se$`Andel B`

library(ggplot2)

ggplot(data = landkat_yr, aes(x= fAAR, y = `Andel B`))+
  geom_bar(stat = "identity")+
  xlab("Year")+ylab("Mean proportion browsing")+
  geom_errorbar(aes(ymin = `Andel B` - se, ymax = `Andel B` + se), width = 0.2)+
  facet_wrap(~LANDKAT, ncol = 4)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#******************************
# Her SKAL jeg lage  barplot for ALTREG per år

#******************************

altreg_yr <- aggregate(data = dat,
                        `Andel B` ~ fAAR + ALTREG,
                        FUN = mean, na.rm = T)
#std <- function(x) sd(x)/sqrt(length(x))
altreg_yr_se <- aggregate(data = dat,
                           `Andel B` ~ fAAR + ALTREG,
                           FUN = std)
altreg_yr$se <- altreg_yr_se$`Andel B`

library(ggplot2)

ggplot(data = altreg_yr, aes(x= fAAR, y = `Andel B`))+
  geom_bar(stat = "identity")+
  xlab("Year")+ylab("Mean proportion browsing")+
  geom_errorbar(aes(ymin = `Andel B` - se, ymax = `Andel B` + se), width = 0.2)+
  facet_wrap(~ALTREG, ncol = 3)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#******************************






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
