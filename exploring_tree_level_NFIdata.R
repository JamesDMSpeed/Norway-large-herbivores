# Exploring the tree level NFI dataset
# ALK

# LIBRARY####
library(readr)
library(dplyr)

# IMPORT ####

# this is a large file that takes about 1 min to import. 1.3 million rows and 50 columns. 
dat <- read_delim("T:/vm/alle/Bruker/gunnar/SUSTHERB/NFI/Fra Erling/Tredata 1986-2017 til Gunnar.csv", 
                  ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                  trim_ws = TRUE)


# Learning the columns: ####

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
summary(dat$TREHOYDE)
# I think thi is is dm, so the smallest tree is 140cm.
# About 1 mill record with NA. BEREGNET_HOYDE is probably better:
summary(dat$BEREGNET_HOYDE) # No NA's
par(mfrow=c(2,1))
plot(dat$BRYSTH_DIA[1:1000], dat$TREHOYDE[1:1000])
plot(dat$BRYSTH_DIA[1:1000], dat$BEREGNET_HOYDE[1:1000])# It's just a column of 1's. 



summary(dat$SESONG)
# SESONG is year. I could convert this to be cycle 6-10 instead





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

              
              