#Norway large herbivore analysis

rm(list=ls())
require(rgdal)
require(raster)
require(classInt)
require(RColorBrewer)
require(rasterVis)
require(gridExtra)
require(MuMIn)
require(nlme)
require(NbClust)


# Set up ------------------------------------------------------------------

#Norway muniicipaliy
norway<-getData('GADM',country='NOR',level=2) #Does not have the updated kommune list (438, should be 422)
norway0<-getData('GADM',country='NOR',level=0) #Norway outline
norwaykom2017<-readOGR(getwd(),'Kommuner_2017',encoding='UTF-8')
nk2<-crop(norwaykom2017,c(-98891, 1114929,6450245,7939986))
#Norway elevation
noralt<-getData('alt',country='NOR')

#Read in data for each species
#Set #NULL! as NA
wildreindeer<-read.csv('villrein.csv',na.strings='#NULL!')
semidomreindeer<-read.csv('tamrein.csv',na.strings='#NULL!')
moose<-read.csv('elg.csv',na.strings='#NULL!',fileEncoding='ISO-8859-1',encoding='ISO-8859-1')
reddeer<-read.csv('hjort.csv',na.strings='#NULL!',fileEncoding='ISO-8859-1',encoding='ISO-8859-1')
roedeer<-read.csv('radyr.csv',na.strings='#NULL!',fileEncoding='ISO-8859-1',encoding='ISO-8859-1')
muskox<-read.csv('moskus.csv',na.strings='#NULL!')
sheep<-read.csv('sau.csv',na.strings='#NULL!')
cattle<-read.csv('ku.csv',na.strings='#NULL!')
heifer<-read.csv('storfe.csv',na.strings='#NULL!')
goat<-read.csv('geit.csv',na.strings='#NULL!')
horse<-read.csv('hest.csv',na.strings='#NULL!')

#Check dimensions of all
dim(wildreindeer)
dim(semidomreindeer)
dim(moose)
dim(reddeer)
dim(roedeer)
dim(muskox)
dim(sheep)
dim(cattle)
dim(heifer)
dim(goat)#Some blank lines
dim(horse)
#Remove blanks
goat<-goat[1:3408,]


#List dataframes
listspp<-list(wildreindeer,semidomreindeer,moose,reddeer,roedeer,muskox,sheep,cattle,heifer,goat,horse)
names(listspp)<-c('wildreindeer','semidomreindeer','moose','reddeer','roedeer','muskox','sheep','cattle','heifer','goat','horse')
lapply(listspp,dim)
#Order the same
listspp<-lapply(listspp,FUN=function(x)x[order(x$knr2017),])
#Check orderings
cbind(listspp$wildreindeer$knr2017,listspp$semidomreindeer$knr2017,listspp$moose$knr2017,listspp$sheep$knr2017,
      listspp$wildreindeer$aar,listspp$semidomreindeer$aar,listspp$moose$aar,listspp$sheep$aar,listspp$muskox$knr2017)
listspp$wildreindeer$knr2017-listspp$muskox$knr2017


#Replace excel null values with 0
listspp<-lapply(listspp,FUN=function(x) {x[is.na(x)]<-0;x})


#Look at total biomass per year
tapply(listspp$moose$TOTBAR,listspp$moose$aar,sum)
plot(tapply(listspp$moose$TOTBAR,listspp$moose$aar,sum),type='b')

#Combine metabolic biomass kgkm2 of all species into one dataframe
#Divide livestock by 365 to change from accumulated metbio to mean
metabolicbiomass<-data.frame(cbind(knr2017=listspp$wildreindeer$knr2017,kommune=listspp$wildreindeer$kommune,Year=listspp$wildreindeer$aar,Wild_reindeer=listspp$wildreindeer$TOTBAR/365,
                                   Semi_domestic_reindeer=listspp$semidomreindeer$TOTBAR/365,
                                   Moose=listspp$moose$TOTBAR,Red_deer=listspp$reddeer$TOTBAR,Roe_deer=listspp$roedeer$TOTBAR,Musk_ox=listspp$muskox$TOTBAR/365,
                                   Sheep=listspp$sheep$TOTBAR/365,Cows=listspp$cattle$TOTBAR/365,Heifer=listspp$heifer$TOTBAR/365,Goat=listspp$goat$TOTBAR/365,Horse=listspp$horse$TOTBAR/365))

head(metabolicbiomass)
summary(metabolicbiomass)
#Sum total
metabolicbiomass$Total<-rowSums(metabolicbiomass[,4:14])
head(metabolicbiomass)
#Sum all cattle
metabolicbiomass$Cattle<-rowSums(metabolicbiomass[,11:12])
#Sum all reindeer
metabolicbiomass$allreindeer<-rowSums(metabolicbiomass[,4:5])
#Sum livestock including semidomestic reindeer
metabolicbiomass$Livestock<-rowSums(metabolicbiomass[,c(5,10:14)])
#Sum wild herbivores
metabolicbiomass$Wildlife<-rowSums(metabolicbiomass[,c(4,6:9)])
#Wild as a proportion of total
metabolicbiomass$WildProp<-metabolicbiomass$Wildlife/metabolicbiomass$Total

#Summed biomass
metbiosum<-data.frame(cbind(knr2017=listspp$wildreindeer$knr2017,kommune=listspp$wildreindeer$kommune,Year=listspp$wildreindeer$aar,Wild_reindeer=listspp$wildreindeer$sumvillrein/365,
                            Semi_domestic_reindeer=listspp$semidomreindeer$Sumtam/365,
                            Moose=listspp$moose$antmkg,Red_deer=listspp$reddeer$antmkg,Roe_deer=listspp$roedeer$antmkg,Musk_ox=listspp$muskox$TOTBAR/365,
                            Sheep=listspp$sheep$TOTBEITE/365,Cows=listspp$cattle$METBEITAD/365,Heifer=listspp$heifer$METBEITEOFF/365,Goat=listspp$goat$TOTBEITE/365,Horse=listspp$horse$TOTBEITE/365))

metbiosum$Cattle<-metbiosum$Cows+metbiosum$Heifer


koms<-c('Vadsø','Vardø','Båtsfjord','Nesseby','Tana')
komnumb<-norwaykom2017$kommunenr[norwaykom2017$NAVN%in%koms]
write.csv(metabolicbiomass[metabolicbiomass$knr2017%in%komnumb,],'FinnmarkKommData.csv')
# Summaries ---------------------------------------------------------------


#Fig 1
yearlysums<-aggregate.data.frame(metbiosum[,4:15],by=list(metbiosum$Year),FUN=sum)

colsR<-brewer.pal(5,'Reds')
greys<-grey.colors(5)
names(yearlysums)<-gsub('_',' ',names(yearlysums))
tiff(width=7,height=5,units='in',res=100,'NorwayHerbivoreTrends.tif')
par(mar=c(5,4,1,9.5))
barplot(t(as.matrix(yearlysums[,c(4:7,2:3,8,13,11,12)]))/sum(listspp$sheep$utmar[listspp$sheep$AAR==2015]),names.arg=yearlysums$Group.1,legend=T,las=2,
        args.legend=list(x=15,y=100,ncol=1,cex=0.8,title='Species'),xlab='Year',
        col=c(colsR[1:5],1,greys[5:1]))
title(ylab=expression('Metabolic biomass kg km'^-2),line=2.5)
dev.off()


#Rewilding
#Add metabiolicbiomasses to Norwegian kommune spatial data
#First reshape to metabolic biomass to wide
metbiowide<-reshape(metabolicbiomass,timevar='Year',direction='wide',idvar='knr2017',drop='kommune')
#Next merge
kommetbio<-merge(norwaykom2017,metbiowide,by.x='KOMMUNENUM',by.y='knr2017')

#Plot
plot(kommetbio,col=kommetbio$Total.2015,border=F)
plot(kommetbio,col=grey(kommetbio$Total.2015/max(kommetbio$Total.2015)),border=F)

r1<-rasterize(kommetbio,noralt,field=kommetbio$Total.2015,fun='mean',na.rm=T)


# Species richness --------------------------------------------------------


#Species richness
kommetbio$speciesrichness.1949<-specnumber(kommetbio@data[,c(11:17,20,21,23)])
kommetbio$speciesrichness.1969<-specnumber(kommetbio@data[,c(45:51,54,55,57)])
kommetbio$speciesrichness.2015<-specnumber(kommetbio@data[,c(130:136,139,140,142)])
colpal<-brewer.pal(9,'YlOrRd')
spplot(kommetbio,c('speciesrichness.1949','speciesrichness.1969','speciesrichness.2015'),cuts=8
       ,names.attr=c(1949,1969,2015),main='Species richness',col.regions=colpal,col=NA)


# Plotting all species and all years --------------------------------------

#Plots
norwayP<-spTransform(norway0,crs(kommetbio))
colpal<-brewer.pal(9,'YlOrRd')
#Total by quantiles
total_qt<-classIntervals(kommetbio$Total.1949,n=9)
spplot(kommetbio,c('Total.1949','Total.1969','Total.2015'),at=total_qt$brks,col.regions=colpal,names.attr=c('1949','1969','2015'),main='Total metabolic biomass',col=NA)
spplot(kommetbio,c('Moose.1949','Moose.1969','Moose.2015'),cuts=8,col.regions=colpal,names.attr=c('1949','1969','2015'),main='Moose metabolic biomass',col=NA)
spplot(kommetbio,c('Red_deer.1949','Red_deer.1969','Red_deer.2015'),cuts=8,col.regions=colpal,names.attr=c('1949','1969','2015'),main='Red deer metabolic biomass',col=NA)
spplot(kommetbio,c('Roe_deer.1949','Roe_deer.1969','Roe_deer.2015'),cuts=8,col.regions=colpal,names.attr=c('1949','1969','2015'),main='Roe deer metabolic biomass',col=NA)
spplot(kommetbio,c('Wild_reindeer.1949','Wild_reindeer.1969','Wild_reindeer.2015'),cuts=8,col.regions=colpal,names.attr=c('1949','1969','2015'),main='wildreindeer metabolic biomass',col=NA)
spplot(kommetbio,c('Semi_domestic_reindeer.1949','Semi_domestic_reindeer.1969','Semi_domestic_reindeer.2015'),cuts=8,col.regions=colpal,names.attr=c('1949','1969','2015'),main='semidomreindeer metabolic biomass',col=NA)
spplot(kommetbio,c('Musk_ox.1949','Musk_ox.1969','Musk_ox.2015'),cuts=8,col.regions=colpal,names.attr=c('1949','1969','2015'),main='muskox metabolic biomass',col=NA)
#Sheep by quantiles due to outlier
sheep_qt <- classIntervals(kommetbio$Sheep.1949, n = 9)
spplot(kommetbio,c('Sheep.1949','Sheep.1969','Sheep.2015'),at=sheep_qt$brks,col.regions=colpal,names.attr=c('1949','1969','2015'),main='sheep metabolic biomass',col=NA)
spplot(kommetbio,c('Goat.1949','Goat.1969','Goat.2015'),cuts=8,col.regions=colpal,names.attr=c('1949','1969','2015'),main='goat metabolic biomass',col=NA)
spplot(kommetbio,c('Horse.1949','Horse.1969','Horse.2015'),cuts=8,col.regions=colpal,names.attr=c('1949','1969','2015'),main='horse metabolic biomass',col=NA)
cattle_qt<-classIntervals(kommetbio$Cattle.1949,n=9)
spplot(kommetbio,c('Cattle.1949','Cattle.1969','Cattle.2015'),at=cattle_qt$brks,col.regions=colpal,names.attr=c('1949','1969','2015'),main='cattle metabolic biomass',col=NA)
#spplot(kommetbio,c('heiferMB.1949','heiferMB.1999','heiferMB.2015'),cuts=8,col.regions=colpal,names.attr=c('1949','1999','2015'),main='heifer metabolic biomass',col=NA)

#All years
allyrs<-c(1949,1959,1969,1979,1989,1999,2009,2015)

tiff(width=10,height=9,units='in',res=100,'TotalMetBio.tif')
spplot(kommetbio,c('Total.1949','Total.1959','Total.1969','Total.1979','Total.1989','Total.1999','Total.2009','Total.2015'),
       at=total_qt$brks,col.regions=colpal,names.attr=allyrs,main=expression('Total metabolic biomass' ~(kg~km^{-2})),col=NA,as.table=T,
       colorkey=list(at=log10(total_qt$brks+1),labels=list(labels=round(total_qt$brks,0.1),at=log10(total_qt$brks+1),cex=0.7)))+#As.table draws from topl ratther than bottoml
layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))
dev.off()

tiff(width=10,height=9,units='in',res=100,'MooseMetBio.tif')
spplot(kommetbio,c('Moose.1949','Moose.1959','Moose.1969','Moose.1979','Moose.1989','Moose.1999','Moose.2009','Moose.2015'),
       cuts=8,col.regions=colpal,names.attr=allyrs,main=expression('Moose metabolic biomass' ~(kg~km^{-2})),col=NA,as.table=T)+#As.table draws from topl ratther than bottoml
  layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))
dev.off()

tiff(width=10,height=9,units='in',res=100,'Red_deerMetBio.tif')
spplot(kommetbio,c('Red_deer.1949','Red_deer.1959','Red_deer.1969','Red_deer.1979','Red_deer.1989','Red_deer.1999','Red_deer.2009','Red_deer.2015'),
       cuts=8,col.regions=colpal,names.attr=allyrs,main=expression('Red deer metabolic biomass' ~(kg~km^{-2})),col=NA,as.table=T)+#As.table draws from topl ratther than bottoml
  layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))
dev.off()

tiff(width=10,height=9,units='in',res=100,'Roe_deerMetBio.tif')
spplot(kommetbio,c('Roe_deer.1949','Roe_deer.1959','Roe_deer.1969','Roe_deer.1979','Roe_deer.1989','Roe_deer.1999','Roe_deer.2009','Roe_deer.2015'),
       cuts=8,col.regions=colpal,names.attr=allyrs,main=expression('Roe deer metabolic biomass' ~(kg~km^{-2})),col=NA,as.table=T)+#As.table draws from topl ratther than bottoml
  layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))
dev.off()

tiff(width=10,height=9,units='in',res=100,'Musk_oxMetBio.tif')
spplot(kommetbio,c('Musk_ox.1949','Musk_ox.1959','Musk_ox.1969','Musk_ox.1979','Musk_ox.1989','Musk_ox.1999','Musk_ox.2009','Musk_ox.2015'),
       cuts=8,col.regions=colpal,names.attr=allyrs,main=expression('Musk ox metabolic biomass' ~(kg~km^{-2})),col=NA,as.table=T)+#As.table draws from topl ratther than bottoml
  layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))
dev.off()

tiff(width=10,height=9,units='in',res=100,'Wild_reindeerMetBio.tif')
spplot(kommetbio,c('Wild_reindeer.1949','Wild_reindeer.1959','Wild_reindeer.1969','Wild_reindeer.1979','Wild_reindeer.1989','Wild_reindeer.1999','Wild_reindeer.2009','Wild_reindeer.2015'),
       cuts=8,col.regions=colpal,names.attr=allyrs,main=expression('Wild reindeer metabolic biomass' ~(kg~km^{-2})),col=NA,as.table=T)+#As.table draws from topl ratther than bottoml
  layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))
dev.off()

tiff(width=10,height=9,units='in',res=100,'Semi_domestic_reindeerMetBio.tif')
spplot(kommetbio,c('Semi_domestic_reindeer.1949','Semi_domestic_reindeer.1959','Semi_domestic_reindeer.1969','Semi_domestic_reindeer.1979','Semi_domestic_reindeer.1989','Semi_domestic_reindeer.1999','Semi_domestic_reindeer.2009','Semi_domestic_reindeer.2015'),
       cuts=8,col.regions=colpal,names.attr=allyrs,main=expression('Semi domestic reindeer metabolic biomass' ~(kg~km^{-2})),col=NA,as.table=T)+#As.table draws from topl ratther than bottoml
  layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))
dev.off()

tiff(width=10,height=9,units='in',res=100,'SheepMetBio.tif')
sheep_qt <- classIntervals(kommetbio$Sheep.1949, n = 9)
spplot(kommetbio,c('Sheep.1949','Sheep.1959','Sheep.1969','Sheep.1979','Sheep.1989','Sheep.1999','Sheep.2009','Sheep.2015'),
       at=sheep_qt$brks,col.regions=colpal,names.attr=allyrs,main=expression('Sheep metabolic biomass' ~(kg~km^{-2})),col=NA,as.table=T,
       colorkey=list(at=log10(sheep_qt$brks+1),labels=list(labels=round(sheep_qt$brks,0.1),at=log10(sheep_qt$brks+1),cex=0.7)))+#As.table draws from topl ratther than bottoml
  layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))
dev.off()


tiff(width=10,height=9,units='in',res=100,'CattleMetBio.tif')
cattle_qt <- classIntervals(kommetbio$Cattle.1949, n = 9)
spplot(kommetbio,c('Cattle.1949','Cattle.1959','Cattle.1969','Cattle.1979','Cattle.1989','Cattle.1999','Cattle.2009','Cattle.2015'),
       at=cattle_qt$brks,col.regions=colpal,names.attr=allyrs,main=expression('Cattle metabolic biomass' ~(kg~km^{-2})),col=NA,as.table=T,
       colorkey=list(at=log10(cattle_qt$brks+1),labels=list(labels=round(cattle_qt$brks,0.1),at=log10(cattle_qt$brks+1),cex=0.7)))+#As.table draws from topl ratther than bottoml
  layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))
dev.off()

tiff(width=10,height=9,units='in',res=100,'GoatMetBio.tif')
Goat_qt <- classIntervals(kommetbio$Goat.1949, n = 7,'equal')
spplot(kommetbio,c('Goat.1949','Goat.1959','Goat.1969','Goat.1979','Goat.1989','Goat.1999','Goat.2009','Goat.2015'),
       at=Goat_qt$brks,col.regions=colpal,names.attr=allyrs,main=expression('Goat metabolic biomass' ~(kg~km^{-2})),col=NA,as.table=T,
       colorkey=list(at=(Goat_qt$brks),labels=list(labels=round(Goat_qt$brks,0.1),at=(Goat_qt$brks),cex=0.7)))+#As.table draws from topl ratther than bottoml
  layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))
dev.off()

tiff(width=10,height=9,units='in',res=100,'HorseMetBio.tif')
Horse_qt <- classIntervals(kommetbio$Horse.1949, n = 9)
spplot(kommetbio,c('Horse.1949','Horse.1959','Horse.1969','Horse.1979','Horse.1989','Horse.1999','Horse.2009','Horse.2015'),
       at=Horse_qt$brks,col.regions=colpal,names.attr=allyrs,main=expression('Horse metabolic biomass' ~(kg~km^{-2})),col=NA,as.table=T,
       colorkey=list(at=log10(Horse_qt$brks+1),labels=list(labels=round(Horse_qt$brks,0.1),at=log10(Horse_qt$brks+1),cex=0.7)))+#As.table draws from topl ratther than bottoml
  layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))
dev.off()



# Summarising wildlife and livestock --------------------------------------


#Wildlife
p1<-spplot(kommetbio,c('Wildlife.1949','Wildlife.1969','Wildlife.2015'),cuts=8,col.regions=colpal,names.attr=c('1949','1969','2015'),main=expression('Wildlife metabolic biomass kg km'^-2),col=NA,
           colorkey=list(labels=list(cex=0.7)))+
  layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))
#Livestock
livestock_qt <- classIntervals(kommetbio$Livestock.1949, n = 8)
livestock_qt$brks[1]<-livestock_qt$brks[1]+0.1
livestock_qt$brks[length(livestock_qt$brks)]<-livestock_qt$brks[length(livestock_qt$brks)]+0.1
p2<-spplot(kommetbio,c('Livestock.1949','Livestock.1969','Livestock.2015'),at=livestock_qt$brks,col.regions=colpal,names.attr=c('1949','1969','2015'),main=expression('Livestock metabolic biomass kg km'^-2),col=NA,
           colorkey=list(at=log10(livestock_qt$brks+1),labels=list(labels=round(livestock_qt$brks,0.1),at=log10(livestock_qt$brks+1),cex=0.7)))+  
  layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))

#% of total biomass made up of livestock
p3<-spplot(kommetbio,c('WildProp.1949','WildProp.1969','WildProp.2015'),cuts=8,col.regions=colpal,names.attr=c('1949','1969','2015'),
           main='Wildlife as a proportion of large herbivores',col=NA,colorkey=list(labels=list(cex=0.7)))+
  layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))


tiff(width=10,height=9,units='in',res=100,'WildLivestockMetBio.tif')
grid.arrange(p1,p2,p3)
dev.off()


#Change in wildlife and livestock
kommetbio$changeinwildprop<-kommetbio$WildProp.2015-kommetbio$WildProp.1949
kommetbio$changeinwildlife<-kommetbio$Wildlife.2015-kommetbio$Wildlife.1949
kommetbio$changeinlivestock<-kommetbio$Livestock.2015-kommetbio$Livestock.1949



# Modelling change in biomas ----------------------------------------------
#Elevation
noraltutm<-projectRaster(noralt,crs=crs(kommetbio),res=1000)


#Average elevation of kommuner
kommetbio<-extract(noraltutm,kommetbio,method='simple',fun=mean,na.rm=T,sp=T)
spplot(kommetbio,'NOR_msk_alt')
#Plot kommune average elevation agains change in proportion of herbivore metbio contributed by herbivores
plot(kommetbio$NOR_msk_alt,kommetbio$changeinwildprop)
plot(kommetbio$NOR_msk_alt,kommetbio$Wildlife.2015-kommetbio$Wildlife.1949,xlab='Elevation',ylab='Change in wildlife biomass')
plot(kommetbio$NOR_msk_alt,kommetbio$Livestock.2015-kommetbio$Livestock.1949,xlab='Elevation',ylab='Change in livestock biomass')

#Landcover
ar50type<-raster('AR50Type100mras')
ar50typep<-projectRaster(ar50type,noraltutm)


#All landuses as cover
builtup<-extract(ar50typep,kommetbio,method='simple',fun=function(x,...)length(x[x==10])/length(x),na.rm=T)
agricultural<-extract(ar50typep,kommetbio,method='simple',fun=function(x,...)length(x[x==20])/length(x),na.rm=T)
forest<-extract(ar50typep,kommetbio,method='simple',fun=function(x,...)length(x[x==30])/length(x),na.rm=T)
otherveg<-extract(ar50typep,kommetbio,method='simple',fun=function(x,...)length(x[x==50])/length(x),na.rm=T)
mires<-extract(ar50typep,kommetbio,method='simple',fun=function(x,...)length(x[x==60])/length(x),na.rm=T)

kommetbio$builtup<-builtup
kommetbio$agricultural<-agricultural
kommetbio$forest<-forest
kommetbio$otherveg<-otherveg
kommetbio$mires<-mires


# #Climate
# #Download 3 worldclim tiles that cover norway
# climdat<-getData('worldclim',res=0.5,var='bio',lat=60,lon=5)
# climdat1<-getData('worldclim',res=0.5,var='bio',lat=70,lon=5)
# climdat2<-getData('worldclim',res=0.5,var='bio',lat=70,lon=40)
# #Merge to one extent
# m1<-merge(climdat,climdat1)
# m2<-merge(m1,climdat2)
# #Crop and mask to Norway
# c1<-crop(m2,noralt)
# norbioclim<-mask(c1,noralt,filename='NorBioClim')

norbioclimdat<-stack('NorBioClim')
norbioclim1<-projectRaster(norbioclimdat,noraltutm)
mst<-extract(norbioclim1[[10]],kommetbio,fun=mean,na.rm=T)
map<-extract(norbioclim1[[12]],kommetbio,fun=mean,na.rm=T)
psea<-extract(norbioclim1[[15]],kommetbio,fun=mean,na.rm=T)

kommetbio$meansumtemp<-mst
kommetbio$meanannprecip<-map
kommetbio$precipseason<-psea

#Plot kommune vegetation cover against herbivores
plot(kommetbio$forest,kommetbio$changeinwildprop)
plot(kommetbio$forest,kommetbio$Wildlife.2015-kommetbio$Wildlife.1949,xlab='Forest Cover',ylab='Change in wildlife biomass')
plot(kommetbio$forest,kommetbio$Livestock.2015-kommetbio$Livestock.1949,xlab='Forest Cover',ylab='Change in livestock biomass')

plot(kommetbio$otherveg,kommetbio$Wildlife.2015-kommetbio$Wildlife.1949,xlab='Snaumark',ylab='Change in wildlife biomass')
plot(kommetbio$otherveg,kommetbio$Livestock.2015-kommetbio$Livestock.1949,xlab='Snaumark',ylab='Change in livestock biomass')

plot(kommetbio$agricultural,kommetbio$Wildlife.2015-kommetbio$Wildlife.1949,xlab='Agriculture',ylab='Change in wildlife biomass')
plot(kommetbio$agricultural,kommetbio$Livestock.2015-kommetbio$Livestock.1949,xlab='Agriculture',ylab='Change in livestock biomass')


#PCA of change in wildlife and livestock with climate data and landcover data
biplot(prcomp(scale(kommetbio@data[,c(148:158)])))



#Plot change in wildlife biomass a
rewildras<-rasterize(kommetbio,noraltutm,field='changeinwildprop')
changelivestockras<-rasterize(kommetbio,noraltutm,field='changeinlivestock')
changewildliferas<-rasterize(kommetbio,noraltutm,field='changeinwildlife')

diverge0 <- function(p, ramp) {
  require(RColorBrewer)
  require(rasterVis)
  if(length(ramp)==1 && is.character(ramp) && ramp %in% 
     row.names(brewer.pal.info)) {
    ramp <- suppressWarnings(colorRampPalette(rev(brewer.pal(11, ramp))))
  } else if(length(ramp) > 1 && is.character(ramp) && all(ramp %in% colors())) {
    ramp <- colorRampPalette(ramp)
  } else if(!is.function(ramp)) 
    stop('ramp should be either the name of a RColorBrewer palette, ', 
         'a vector of colours to be interpolated, or a colorRampPalette.')
  rng <- range(p$legend[[1]]$args$key$at)
  s <- seq(-max(abs(rng)), max(abs(rng)), len=1001)
  i <- findInterval(rng[which.min(abs(rng))], s)
  zlim <- switch(which.min(abs(rng)), `1`=i:(1000+1), `2`=1:(i+1))
  p$legend[[1]]$args$key$at <- s[zlim]
  p$par.settings$regions$col <- ramp(1000)[zlim[-length(zlim)]]
  p
}

p1<-levelplot(changelivestockras,margin=F,main='Change in livestock biomass')
p2<-levelplot(changewildliferas,margin=F,main='Change in wildlife biomass')
diverge0(p1,'RdBu')
diverge0(p2,'RdBu')

#Test drivers of rewildling
lm1<-lm(changeinwildlife~changeinlivestock+meansumtemp+meanannprecip+precipseason+agricultural+forest+otherveg,data=kommetbio@data)
lm2<-step(lm1)
summary(lm2)


#Include kommune centre point xy variables
cents<-coordinates(kommetbio)
spplot(kommetbio,'changeinwildlife')+
  layer(sp.points(SpatialPoints(cents),col=1))

kommetbio$x<-cents[,1]
kommetbio$y<-cents[,2]

#Make dataframe including variables for modelling and xy coordinates
aicdataframe<-data.frame(cbind(scale(kommetbio@data[,148:158]),kommetbio@data[,159:160]))

#Cross correlations between explanatory variables?
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(aicdataframe[,1:11],upper.panel=panel.cor)
#MST correlated with elevation, 

#Model averaging GLM
globmodglm<-glm(changeinwildlife~changeinlivestock+builtup+agricultural+forest+otherveg+meansumtemp+meanannprecip+precipseason,data=aicdataframe,na.action=na.fail)
modsetglm<-dredge(globmodglm,trace=2)
modselglm<-model.sel(modsetglm)
modavgglm<-model.avg(modselglm)
importance(modavgglm)
summary(modavgglm)


#Model with only landcover
globmodglm<-glm(changeinwildlife~changeinlivestock+builtup+agricultural+forest+otherveg+mires,data=aicdataframe,na.action=na.fail)
modsetglm<-dredge(globmodglm,trace=2)
modselglm<-model.sel(modsetglm)
modavgglm<-model.avg(modselglm,fit=T)
importance(modavgglm)
summary(modavgglm)


#Model with only climate
globmodglm<-glm(changeinwildlife~meansumtemp*meanannprecip*precipseason,data=aicdataframe,na.action=na.fail)
modsetglm<-dredge(globmodglm,trace=2)
modselglm<-model.sel(modsetglm)
modavgglm<-model.avg(modselglm,fit=T)
importance(modavgglm)
summary(modavgglm)


#Model averaging GLS - accounting for spatial autocorrelation
globmodgls<-gls(changeinwildlife~changeinlivestock+builtup+agricultural+forest+meansumtemp+meanannprecip+precipseason,
                data=aicdataframe,na.action=na.fail,method='ML',
                correlation=corExp(form=~x+y,nugget=T))
modsetgls<-dredge(globmodgls,trace=2)
modselgls<-model.sel(modsetgls)
modavggls<-model.avg(modselgls)
importance(modavggls)
summary(modavggls)



macdf<-data.frame(summary(modavggls)$coefmat.full[2:nrow(summary(modavggls)$coefmat.full),])
impdf<-data.frame(importance(modavggls))
df2<-merge(macdf,impdf,by='row.names')
df2
plotdf<-df2[order(df2$importance.modavggls.),]
plotdf

tiff('ImpMac.tif',width=6,height=4,units='in',res=100)
par(oma=c(1,8,1,1))
par(mfrow=c(1,2))
par(mar=c(5,0,1,1))
par(xpd=T)
barplot(plotdf$importance.modavggls.,beside=T,horiz=T,names.arg=plotdf$Row.names,las=1,xlab='Importance')
par(mar=c(5,1,1,1))
b1<-barplot(plotdf[,2],horiz=T,col=F,border=F,xlim=c(-0.5,1.2),las=1,xlab='Model averaged coefficients')
points(plotdf[,2],b1,pch=16)
arrows(plotdf[,2]+plotdf[,3],b1,plotdf[,2]-plotdf[,3],b1,code=3,angle=90,length=0.05)
par(xpd=F)
abline(v=0,lty=2)
dev.off()



# Clustering --------------------------------------------------------------

kommetbio$speciesrichness.1969<-specnumber(kommetbio@data[,c(45:51,54,55,57)])
kommetbio$speciesrichness.2015<-specnumber(kommetbio@data[,c(130:136,139,140,142)])

#Kommune 216 (Moskenes has no herbivores in 2015)
kb2<-kommetbio[c(1:215,217:nrow(kommetbio@data)),]

d49<-vegdist(kb2@data[,c(11:17,20,21,23)])
d15<-vegdist(kb2@data[,c(130:136,139,140,142)])
h49<-hclust(d49)
h15<-hclust(d15)
plot(h49)
plot(h15)
c15<-cutree(h15,k=5)
c49<-cutree(h49,k=5)
kb2$c15<-c15
kb2$c49<-c49
spplot(kb2,c('c49','c15'))


#Based on metabolicbiomass df
#Knr 1857 and 1874 have no herbivores in 2009 and 2015
#Knr 1841 has negative SD reindeer in 1959 (check with gunnar)
mb1<-metabolicbiomass[metabolicbiomass$knr2017!=1857 & metabolicbiomass$knr2017!=1874 & metabolicbiomass$knr2017!=1841,]
#Bray distance matrix
dm1<-vegdist(mb1[,c(4:10,13,14,16)])
#Hierarchical clustering
hm1<-hclust(dm1)
plot(hm1,labels=metabolicbiomass$knr2017[metabolicbiomass$knr2017!=1857 & metabolicbiomass$knr2017!=1874 & metabolicbiomass$knr2017!=1841])
#Cut tree
#How many clusters?
#indexchoice<-'cindex'
#methodchoice<-'complete'
#nclust<-NbClust(dm1,method=methodchoice,min.nc=2,max.nc=12,index=indexchoice)
#nclust$Best.nc ###8
mb1$cm1<-cutree(hm1,k=8)

mbcutwide<-reshape(mb1,timevar='Year',direction='wide',idvar='knr2017',drop='kommune')
mbcuttreedf<-merge(kommetbio,mbcutwide,by.x='KOMMUNENUM',by.y='knr2017')

#Number of kommune per year in each cluster
tapply(mb1$cm1,list(mb1$cm1,mb1$Year),length)

#Characterise clusters
herbclusts<-(aggregate.data.frame(mb1[,c(6:9,4,5,10,16,13,14)],by=list(mb1$cm1),FUN='mean'))
herbclusts
barplot(t(herbclusts[,2:11]),legend=T,names.arg=herbclusts$Group.1,las=1,args.legend=list(x=5,y=2500,title='Species'),col=c(colsR[1:5],1,greys[5:1]),
        xlab='Cluster')
title(ylab=expression('Metabolic biomass kg km'^-2),line=2.5)

clusternames<-c('Cattle/Deer','Roe deer','Moose','Cattle/Sheep','Sheep/Reindeer','Sheep/Red deer','S-d reindeer','High sheep')

#With seperate axis for cluster8
tiff(width=7,height=5,units='in',res=100,'NorwayHerbivoreClusters.tif')
funprop<-function(x)x/max(x)
app1<-apply(herbclusts[,2:11],MARGIN=1,FUN=funprop)
par(mar=c(5,5,5,3))
barplot(app1,legend=T,names.arg=herbclusts$Group.1,las=1,args.legend=list(x=12,y=2.7,title='Species',cex=0.8,bg=0),col=c(colsR[1:5],1,greys[5:1]),
        xlab='Cluster',yaxt='n')+
title(ylab=expression('Metabolic biomass kg km'^-2),line=2.5)+
axis(2, at = seq(0, max(colSums(app1)[1:7]), length.out = 5),las=1,
     labels = round(seq(0, max(herbclusts[1:7,2:11]), length.out = 5),-1))+
axis(4, at = seq(0, max(colSums(app1)[8]), length.out = 5),las=1,
     labels = round(seq(0, max(herbclusts[8,2:11]), length.out = 5),-1))+
arrows(8.5,0,8.5,1.3,code=0,lty=2)
dev.off()

#Make dataframe of props
app2<-as.data.frame(t(app1))
app2$Cluster<-1:8

app2c17<-app2
app2c17[8,1:10]<-0

app2c8<-app2
app2c8[1:7,1:10]<-0

       
barchart(Cluster~Moose+Red_deer+Roe_deer+Musk_ox+Wild_reindeer+Semi_domestic_reindeer+Sheep+Cattle+Goat+Horse,data=app2c17
         ,stack=T,par.settings=list(superpose.polygon=list(col=c(colsR[1:5],1,greys[5:1]))),ylab='Cluster',xlab=expression('Metabolic biomass kg km'^-2),
         auto.key=list(space='right'),
         scales=list(x=list(at=seq(0, max(colSums(app1)[1:7]), length.out = 5),labels=round(seq(0, max(herbclusts[1:7,2:11]), length.out = 5),-1))))
p8<-barchart(Cluster~Moose+Red_deer+Roe_deer+Musk_ox+Wild_reindeer+Semi_domestic_reindeer+Sheep+Cattle+Goat+Horse,data=app2c8
             ,stack=T,par.settings=list(superpose.polygon=list(col=c(colsR[1:5],1,greys[5:1]))),ylab='Cluster',xlab=expression('Metabolic biomass kg km'^-2),
             auto.key=list(space='right'),
             scales=list(x=list(at=seq(0, max(colSums(app1)[8]), length.out = 5),labels=round(seq(0, max(herbclusts[8,2:11]), length.out = 5),-1))))

doubleYScale(p17,p8)

#Correspondance plot
p1<-levelplot(mb1$cm1~mb1$Year+as.factor(mb1$knr2017),col.regions=cp1,cuts=7,yaxt='n',scales=list(y=list(at=0),x=list(at=yrs)),
          colorkey=list(lables=list(labels=clusternames)),ylab='Municipality',xlab='Year')

#Plot distribution of clusters
cp1<-brewer.pal(8,'Dark2')
tiff(width=10,height=9,units='in',res=100,'HerbivoreClusterDistribution.tif')
p2<-spplot(mbcuttreedf,c('cm1.1949','cm1.1959','cm1.1969','cm1.1979','cm1.1989','cm1.1999','cm1.2009','cm1.2015'),cuts=7,
       names.attr=c(1949,1959,1969,1979,1989,1999,2009,2015),col=NA,col.regions=cp1,as.table=T,colorkey=list(labels=list(labels=clusternames)))+
  layer(sp.polygons(norwayP,lwd=0.5,col=grey(0.5)))
dev.off()

grid.arrange(p0,p1,p2,ncol=1)


)

# Ordinations -------------------------------------------------------------
# cca1<-cca(metabolicbiomass[metabolicbiomass$knr2017!=1857 & metabolicbiomass$knr2017!=1874 & metabolicbiomass$knr2017!=1841,c(4:10,13,14,16)])
# plot(cca1)
# 
# 
# m1<-merge(metabolicbiomass,kommetbio,by.y='KOMMUNENUM',by.x='knr2017',all.x=T)
# m2<-m1[m1$knr2017!=1857 & m1$knr2017!=1874 & m1$knr2017!=1841,]
# cca1<-cca(m2[,c(4:10,13,14,16)]~m2$Year+m2$agricultural+m2$forest+m2$otherveg+m2$builtup)
# plot(cca1)
# ordiellipse(cca1,m2$Year)

#Add biogeographical region of kommuner to dataset
komtype<-wildreindeer[,c(1,3,9)]
names(komtype)[2]<-'Year'
mx<-merge(metabolicbiomass,komtype)
#Remove missing kommune
mx1<-mx[metabolicbiomass$knr2017!=1857 & metabolicbiomass$knr2017!=1874 & metabolicbiomass$knr2017!=1841,]
#Make factor for year:biogeography
mx1$YearRegion<-paste(mx1$ALTREG,mx1$Year,sep='_')


mds1<-metaMDS(metabolicbiomass[metabolicbiomass$knr2017!=1857 & metabolicbiomass$knr2017!=1874 & metabolicbiomass$knr2017!=1841,c(4:10,13,14,16)])
e1<-envfit(mds1~as.factor(metabolicbiomass$Year[metabolicbiomass$knr2017!=1857 & metabolicbiomass$knr2017!=1874 & metabolicbiomass$knr2017!=1841]))
plot(mds1,display='species',cex=0)
text(mds1,display='species')
plot(e1,label=levels(as.factor(metabolicbiomass$Year)),cex=0.8)
lines(e1$factors$centroids)
#ordiellipse(mds1,metabolicbiomass$Year[metabolicbiomass$knr2017!=1857 & metabolicbiomass$knr2017!=1874 & metabolicbiomass$knr2017!=1841])

yrs<-c(1949,1959,1969,1979,1989,1999,2009,2015)

e2<-envfit(mds1~mx1$YearRegion)
tiff(width=6,height=6,units='in',res=100,'NorwayHerbivoreTrajectoriesNMDS.tif')
par(mar=c(5,5,1,1))
plot(mds1,type='n')
points(mds1$points[mx1$ALTREG=='inlandlow',],cex=0.1,col='green')
points(mds1$points[mx1$ALTREG=='mountain',],cex=0.1,col='brown')
points(mds1$points[mx1$ALTREG=='coast',],cex=0.1,col='blue')
text(mds1,display='species')
lines(e2$factors$centroids[1:8,],col='blue')
text(e2$factors$centroids[1:8,],labels=yrs,col='blue',cex=0.8)
lines(e2$factors$centroids[9:16,],col='green')
text(e2$factors$centroids[9:16,],labels=yrs,col='green',cex=0.8)
lines(e2$factors$centroids[17:24,],col='brown')
text(e2$factors$centroids[17:24,],labels=yrs,col='brown',cex=0.8)
legend('topr',pch=16,col=c('blue','green','brown'),c('Coast','Inland','Mountain'))
dev.off()

plot(mds1,type='n')
ordiarrows(mds1,mx1$knr2017,startmark=1)
