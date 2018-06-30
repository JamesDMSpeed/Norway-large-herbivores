#Norway large herbivore analysis

rm(list=ls())
require(rgdal)
require(raster)
require(classInt)
require(RColorBrewer)
require(rasterVis)
require(gridExtra)
require(MuMIn)

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
sheep_qt <- classIntervals(kommetbio$sheepMB.1949, n = 9)
spplot(kommetbio,c('Sheep.1949','Sheep.1969','Sheep.2015'),at=sheep_qt$brks,col.regions=colpal,names.attr=c('1949','1969','2015'),main='sheep metabolic biomass',col=NA)
spplot(kommetbio,c('Goat.1949','Goat.1969','Goat.2015'),cuts=8,col.regions=colpal,names.attr=c('1949','1969','2015'),main='goat metabolic biomass',col=NA)
spplot(kommetbio,c('Horse.1949','Horse.1969','Horse.2015'),cuts=8,col.regions=colpal,names.attr=c('1949','1969','2015'),main='horse metabolic biomass',col=NA)
cattle_qt<-classIntervals(kommetbio$allcattle.1949,n=9)
spplot(kommetbio,c('allcattle.1949','allcattle.1969','allcattle.2015'),at=cattle_qt$brks,col.regions=colpal,names.attr=c('1949','1969','2015'),main='cattle metabolic biomass',col=NA)
#spplot(kommetbio,c('heiferMB.1949','heiferMB.1999','heiferMB.2015'),cuts=8,col.regions=colpal,names.attr=c('1949','1999','2015'),main='heifer metabolic biomass',col=NA)

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

aicdataframe<-data.frame(scale(kommetbio@data[,148:158]))

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

#Model averaging
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

stack1<-stack(norbioclim1$layer.10,norbioclim1$layer.12,norbioclim1$layer.15)
names(stack1)<-c('meansumtemp','meanannprecip','precipseason')
p1<-raster::predict(stack1,summary(modavgglm))
plot(p1)#Predicts increase along coast. Not increase around Oslo and Trondheim...
spplot(kommetbio,'changeinwildlife')
