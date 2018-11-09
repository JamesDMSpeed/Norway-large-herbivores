# Exploring the tree level NFI dataset
# ALK

# LIBRARY####
library(readr)
library(dplyr)

# IMPORT ####

# this is a large file that takes about 1 min to import. 1.3 million rows and 50 columns. 
#dat <- read_delim("T:/vm/alle/Bruker/gunnar/SUSTHERB/NFI/Fra Erling/Tredata 1986-2017 til Gunnar.csv", 
#                  ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
#                  trim_ws = TRUE)
dat <- read.csv2("T:/vm/alle/Bruker/gunnar/SUSTHERB/NFI/Fra Erling/Tredata 1986-2017 til Gunnar.csv")

# Learning the columns: ####
#change cloumn name
colnames(dat)[1] <- "FLATEID"




#*****************
# Data exploration ####
# How many 'flater' is there?
length(unique(dat$FLATEID))
# 13219

# How many trees do we have?
length(unique(dat$TREID))
# ~400 000 trees

# How many species do we have?
# TRESLAG vs TERART
length(unique(dat$TRESLAG)) # 30
(unique(dat$TRESLAG))
# This is a code value for tree species.
length(unique(dat$treart))
unique(dat$treart)
table(dat$treart, dat$TRESLAG)
# This is a character variable with species names, where lots of TESLAG codes have been combined as 'annet'.
# We dont need the TRESLAG column


unique(dat$TILSTAND)
# This dataset only has live trees (see NFI manual p 31)



# How big are the trees?
summary(dat$BRYSTH_DIA)
# trees are min 50mm DBH. No NA's. Biggest tree is 1m DBH
# what is the biggest tree?
View(dat[which.max(dat$BRYSTH_DIA),])
#alm
View(dat[which.max(dat$TREHOYDE),])
#gran
summary(dat$TREHOYDE)

# I think thi is is dm, so the smallest tree is 140cm.
# About 1 mill record with NA. BEREGNET_HOYDE is probably better:
summary(dat$BEREGNET_HOYDE) # No NA's
par(mfrow=c(2,1))
plot(dat$BRYSTH_DIA[1:1000], dat$TREHOYDE[1:1000])
plot(dat$BRYSTH_DIA[1:1000], dat$BEREGNET_HOYDE[1:1000])# It's just a column of 1's. 


# Defining NFI cyckles####
summary(dat$SESONG)
# SESONG is year. I could convert this to be cycle 6-10 instead
#lage en ny kolonne som heter syklus
dat$syklus<-0
dat$syklus[dat$SESONG<=1993]<-6
dat$syklus[dat$SESONG>=1994]<-7
dat$syklus[dat$SESONG>=2000]<-8
dat$syklus[dat$SESONG>=2005]<-9
dat$syklus[dat$SESONG>=2010]<-10
dat$syklus[dat$SESONG>=2015]<-11
table(dat$syklus)

summary(dat$AVS_FLATESENT)
# trees are between 0 and 89 m from the plot centre. 
# Not important to keep that data for our analyses.




# I thing not all plots were surveyed completely:
unique(dat$HEL_DELT_FLATE) 
summary(dat$FLATEDELX) 
length(unique(dat$FLATEDELX)) #I think this has something to do with the proportion of the plots that was surveyed.
hist((dat$FLATEDELX))



# subsetting the datafile to make it more managable

dat <- select(dat, 
              flateID = FLATEID,
              HEL_DELT_FLATE,
              year = SESONG,
              treeID = TREID,
              species = treart,
              DBH = BRYSTH_DIA,
              TreeHeigth = BEREGNET_HOYDE,
              flatedel = FLATEDEL,
              flatedelX = FLATEDELX,
              kommune = KOMNR,
              fylke = FYLNR,
              klavetre_provetre = TRETYPE)
# Mangler bonitet, vegetasjonstype og hogstklasse, men kanskje dette ligegr i 'flatadataene.

              
              

# DBH growth over one cycle ####

# for Rowan:
unique(dat$treart)
rowan <- dat[dat$treart == "Rogn"
             & dat$syklus == 9 | 
               dat$treart == "Rogn"
             & dat$syklus == 10, c(51, 10, 6)
             ]

rowan$syklus <- as.factor(rowan$syklus)
library(reshape2)

rowan2 <- dcast(data = rowan,
               TREID ~ syklus,
               value.var = "BRYSTH_DIA",
               fun.aggregate = mean)

rowan2$diff <- rowan2$`10` - rowan2$`9`

rowan3 <- rowan2[-rowan2$diff <50,]
plot(rowan3$diff)
hist(rowan3$diff)

summary(rowan3$diff)
mean(rowan3$diff, na.rm=T)
sd(rowan3$diff, na.rm=T)

#for osp

summary(dat$SESONG)
# SESONG is year. I could convert this to be cycle 6-10 instead
#lage en ny kolonne som heter syklus
dat$syklus<-0
dat$syklus[dat$SESONG<=1993]<-6
dat$syklus[dat$SESONG>=1994]<-7
dat$syklus[dat$SESONG>=2000]<-8
dat$syklus[dat$SESONG>=2005]<-9
dat$syklus[dat$SESONG>=2010]<-10
dat$syklus[dat$SESONG>=2015]<-11
table(dat$syklus)

summary(dat$AVS_FLATESENT)
# trees are between 0 and 89 m from the plot centre. 
# Not important to keep that data for our analyses.




# I thing not all plots were surveyed completely:
unique(dat$HEL_DELT_FLATE) 
summary(dat$FLATEDELX) 
length(unique(dat$FLATEDELX)) #I think this has something to do with the proportion of the plots that was surveyed.




# subsetting the datafile to make it more managable

dat <- select(dat, 
              flateID = FLATEID,
              HEL_DELT_FLATE,
              year = SESONG,
              treeID = TREID,
              species = treart,
              DBH = BRYSTH_DIA,
              TreeHeigth = BEREGNET_HOYDE,
              flatedel = FLATEDEL,
              flatedelX = FLATEDELX,
              kommune = KOMNR,
              fylke = FYLNR,
              klavetre_provetre = TRETYPE)
# Mangler bonitet, vegetasjonstype og hogstklasse, men kanskje dette ligegr i 'flatadataene.





# DBH growth over one cycle ####

# for osp:
unique(dat$treart)
aspen <- dat[dat$treart == "Osp"
             & dat$syklus == 9 | 
               dat$treart == "Osp"
             & dat$syklus == 10, 
             c(51, 10, 6) # dette er kolonner
             ]
names(dat)
# Dette kan også skrives slik:
aspen <- dat[dat$treart == "Osp"
             & dat$syklus == 9 | 
               dat$treart == "Osp"
             & dat$syklus == 10, 
             c( "syklus" , "BRYSTH_DIA", "TREID") # dette er kolonner
             ]


aspen$syklus <- as.factor(aspen$syklus)
library(reshape2)

aspen2 <- dcast(data = aspen,
                TREID ~ syklus,
                value.var = "BRYSTH_DIA",
                fun.aggregate = mean)

aspen2$diff <- aspen2$`10` - aspen2$`9`

aspen3 <- aspen2[-aspen2$diff <50,]
plot(aspen3$diff)
hist(aspen3$diff)

summary(aspen3$diff)
mean(aspen3$diff, na.rm=T)
sd(aspen3$diff, na.rm=T)

# DBH growth over one cycle ####

# for selje:
unique(dat$treart)
willow <- dat[dat$treart == "Selje"
             & dat$syklus == 9 | 
               dat$treart == "Selje"
             & dat$syklus == 10, c(51, 10, 6)
             ]

willow$syklus <- as.factor(willow$syklus)
library(reshape2)

willow2 <- dcast(data = willow,
                TREID ~ syklus,
                value.var = "BRYSTH_DIA",
                fun.aggregate = mean)

willow2$diff <- willow2$`10` - willow2$`9`

willow3 <- willow2[-willow2$diff <50,]
plot(willow3$diff)
hist(willow3$diff)

summary(willow3$diff)
mean(willow3$diff, na.rm=T)
sd(willow3$diff, na.rm=T)

# Areadatafile ######
library(readr)
Areadata <- read_delim("M:/Mine dokumenter/Moose/Beitetrykk_2017/Browsing/NFI/Trerekruttering 1986-2017 60-80 mmdbh.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)
View(Areadata)
Areadata2 <- select(Areadata, 
                    FLATEID, FYLNR, KOMNR, aar, syklus = takst, Rognprha)
# Selects relavant columns from flatedata
Areadata3 <- Areadata2[Areadata2$syklus==6 | Areadata2$syklus==10, ]
# sletter Areadata og Areadata2
rm(Areadata);rm(Areadata2)

# Get the file with absolute browsing data####
library(readxl)
BrowvsGraz <- read_excel("M:/Mine dokumenter/Moose/Beitetrykk_2017/Browsing/Browsing_final 0410/BrowvsGraz2910_final.xlsx")
View(BrowvsGraz)
# Selects relavant columns from BrowvsGraz
BrowvsGraz2 <- select(BrowvsGraz,
                      KOMNR=knr2017, 
                      aar=AAR,
                      Browsing="ABS BROW")
# sletter BrowsvsGraz
rm(BrowvsGraz)                 
# Make a new variable; calculate syklus 10 - syklus 6####

library(reshape2)

Areadata4 <- dcast(data = Areadata3,
                 FLATEID ~ syklus,
                 value.var = "Rognprha",
                 fun.aggregate = mean)

willow2$diff <- willow2$`10` - willow2$`9`            