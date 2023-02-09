#Plotting data for PIL from fishers FSP
#@silvia Rodriguez Climent
# 01-03-2021; last modified 09/09/2022
#---------------------------------------------------------------------## 
setwd("C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/FSPsmallpelagics2022/")
rm(list=ls())

lib <- function(packages){ 
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
}

species = 'PIL' # Not working for anything but PIL yet.  it makes outputs for sardine still even when the species is set to sprat

# set input, output directories
inp_dir <- file.path(getwd(), paste0("Data/Fishers/",species,"/"))
plot_dir <- file.path(getwd(), paste0("Data/plots/",species,"/"))
out_dir <- file.path(getwd(), "Data/Output/")

# load libraries
packages <- c("ggplot2","data.table","xlsx","openxlsx","dplyr","readxl","stringr","plyr","tidyr","reshape2",
              "maps","mapdata","mapproj","mapplots","lubridate","rgdal","raster","GISTools","ggspatial","XLConnect",
              "openxlsx","sjmisc","viridis","scatterpie","ggspatial","sp")

lib(packages) #install packages through own function


#get all the final plots from all the processors
list.files(out_dir, recursive=FALSE,pattern=paste0(species,"*"))

#enter species------

# ===================================================--
# 0. Load files----
# ===================================================--

# File produced in script: A3. PIL_DataExtraction_Fishers #

pilfish <- read.table(paste(out_dir,paste0("/",species,"_LBfishers_2223.csv"),sep=''),sep=",",header=TRUE,stringsAsFactors = F)
head(pilfish);dim(pilfish) # 472 25

piltl <- read.table(paste(out_dir,paste0("/",species,"_TLfishers_2223.csv"),sep=''),sep=",",header=TRUE,stringsAsFactors = F)
head(piltl);dim(piltl) # 312 13


#######################################################--
# *****************************************************--
# 1. FUNCTIONS ----
# *****************************************************--
#######################################################--

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  proj4string(b_poly) <- proj4string(shp)
  gIntersection(shp, b_poly, byid = TRUE)
}


#######################################################--
# *****************************************************--
#PLOTS ----
# *****************************************************--
#######################################################--

# ===================================================--
# 2.1 Tot catch per month/vessel ----
# ===================================================--

plot1<- as.data.table(pilfish)

Catch <- plot1[,sum(as.numeric(as.character(pil)), na.rm=TRUE), by=c("vessel", "month","year")]
Catch$month2 <- Catch$month

Catch$month2[Catch$month2=="7"] <- "Jul"
Catch$month2[Catch$month2=="8"] <- "Aug"
Catch$month2[Catch$month2=="9"] <- "Sep"
Catch$month2[Catch$month2=="10"] <- "Oct"
Catch$month2[Catch$month2=="11"] <- "Nov"
Catch$month2[Catch$month2=="12"] <- "Dec"
Catch$month2[Catch$month2=="1"] <- "Jan"
Catch$month2[Catch$month2=="2"] <- "Feb"
Catch$month2 <- factor(Catch$month2,levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)  

T_Catch<- ggplot(Catch, aes(month2, V1/1000, group=vessel, col=vessel)) + geom_line(size=1.2) +geom_point()+
  theme_bw(25) + ylab("tonnes") + xlab("month") + 
  theme(legend.position="top",legend.text=element_text(color="black",size=10),legend.title=element_text(color="black",size=12))+
  guides(col = guide_legend(nrow = 2)) 

T_Catch

#save Plots
ggsave(file=paste(plot_dir,paste("/",species,"_fishers_Catch.png",sep=""),sep=""),T_Catch, width=24, height=16, units="cm", dpi=200)


# ===================================================--
# 2.2 Boxplot by month ----
# ===================================================--

boxplot <- pilfish

str(boxplot)
boxplot$pil <- as.numeric(as.character(boxplot$pil))

boxplot$month2 <- boxplot$month

boxplot$month2[boxplot$month2=="7"] <- "Jul"
boxplot$month2[boxplot$month2=="8"] <- "Aug"
boxplot$month2[boxplot$month2=="9"] <- "Sep"
boxplot$month2[boxplot$month2=="10"] <- "Oct"
boxplot$month2[boxplot$month2=="11"] <- "Nov"
boxplot$month2[boxplot$month2=="12"] <- "Dec"
boxplot$month2[boxplot$month2=="1"] <- "Jan"
boxplot$month2[boxplot$month2=="2"] <- "Feb"
boxplot$month2[boxplot$month2=="3"] <- "Mar"
boxplot$month2[boxplot$month2=="3"] <- "Apr"
boxplot$month2[boxplot$month2=="3"] <- "May"

boxplot$month2 <- factor(boxplot$month2,levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May"),ordered=TRUE)  

T_CatchMonth <- ggplot(boxplot, aes(month2, pil/1000, fill=vessel)) + geom_boxplot() +
  theme_bw(25) + ylab("tonnes") + xlab("month") +
  scale_y_continuous(breaks=seq(0,30,5)) + 
  #theme(legend.position=c(0.2,0.8), legend.title=element_blank())+
  theme(legend.position="top",legend.text=element_text(color="black",size=10),legend.title=element_text(color="black",size=12))+
  guides(fill = guide_legend(nrow = 2)) 

T_CatchMonth

# Save plots
ggsave(file=paste(plot_dir,paste("/",species,"_fishers_CatchMonth.png",sep=""),sep=""),T_CatchMonth, width=24, height=16, units="cm", dpi=200)


# ===================================================--
# 3. Lengths (TL) ----
# ===================================================--

head(piltl);dim(piltl)
TotL <- piltl

summary(TotL)
TotL[is.na(TotL)] <- 0
TotL <- as.data.table(TotL)
TLc1 <- TotL[!is.na(N),sum(N), by=.(TL)]
TLc2 <- TotL[!is.na(N),sum(N), by=.(TL,month,vessel)]

stat2 <- TLc1 %>%
  dplyr::summarize(median=median(TL), weighted.mean(TL, V1)) 

stat1 <- TLc2 %>%
  group_by(month,vessel) %>%
  dplyr::summarize(median=median(TL), weighted.mean(TL, V1))


#Plots for all the fishers--
head(TotL)
head(TLc1) #global length distribution
head(TLc2) #by month

## 3.1a Overall LFD----
TLc1 <- subset(TLc1,!TL==0) #without the zeros

all.TL1 <- ggplot(TLc1, aes(TL, V1)) + geom_bar(stat="identity", position="dodge",fill="grey") +
  theme_bw(25) +ylab("N")+xlab("Total length (cm)")+scale_y_continuous(limits=c(0,max(TLc1$V1)+50), expand=c(0,0))+
  theme(legend.position = "none")+ggtitle(paste(species,"all fishers",sep="-"))+
  geom_vline(aes(xintercept=stat2$`weighted.mean(TL, V1)`), col="red", size=1.2)

ggsave(filename=paste(plot_dir,paste(species,"allfishers_TL.png",sep="_"),sep="/"),plot=all.TL1,width=25,height=20,units="cm",dpi=300,type="cairo-png")

# weighted mean
print(weighted.mean(TLc1$TL,TLc1$V1))

## 3.1b LFD by month and vessel----
#put name into months and order them
TLc2 <- subset(TLc2,!TL==0) #without the zeros
summary(TLc2)
TLc3 <- merge(TLc2,stat1,by=c("month","vessel"))

table(TLc3$month)

TLc3$month[TLc3$month=="7"] <- "Jul"
TLc3$month[TLc3$month=="8"] <- "Aug"
TLc3$month[TLc3$month=="9"] <- "Sep"
TLc3$month[TLc3$month=="10"] <- "Oct"
TLc3$month[TLc3$month=="11"] <- "Nov"
TLc3$month[TLc3$month=="12"] <- "Dec"
TLc3$month[TLc3$month=="1"] <- "Jan"
TLc3$month[TLc3$month=="2"] <- "Feb"

TLc3$month <- factor(TLc3$month,levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)  

all.TL2 <- ggplot(TLc3, aes(TL,V1, group=factor(month), fill=factor(month))) +
  geom_bar(stat="identity",position=position_dodge(2),width=0.45) + 
  theme_bw(25) + scale_y_continuous(limits=c(0,max(TLc3$V1)+20), expand=c(0,0)) + 
  ylab("N") + xlab("Total length (cm)")+facet_grid(rows=vars(month),vars(vessel))+
  geom_vline(aes(xintercept=weighted.mean(TL, V1)), col="red", size=1.2) +
  theme(legend.position="none",strip.text.x=element_text(size=12,colour="black"),
        strip.text.y = element_text(size=12),axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 15),axis.title.x = element_text(color = "black", size = 15))
# + ggtitle(paste(species,"All processors"))

ggsave(filename=paste(plot_dir,paste(species,"allfishers_TLMonths(2).png",sep="_"),sep="/"),plot=all.TL2,width=25,height=20,units="cm",dpi=300,type="cairo-png")


scaleFUN <- function(x) sprintf("%.0f", x) 
#Pelagic Marskman is pushing the others, so maybe you have to just plot him alone, or set the N free
#better visulization:m months in columns
all.TL2 <- ggplot(TLc3, aes(TL,V1, group=factor(month), fill=factor(month))) +
  geom_bar(stat="identity",position=position_dodge(2),width=0.45) + 
  theme_bw(25) + scale_y_continuous(limits=c(0,max(TLc3$V1)+20), expand=c(0,0)) + 
  ylab("N") + xlab("Total length (cm)")+facet_grid(rows=vars(vessel),vars(month)) +
  geom_vline(aes(xintercept=weighted.mean(TL, V1)), col="red", size=1.2) +
  theme(legend.position="none",strip.text.x=element_text(size=12,colour="black"),
        strip.text.y = element_text(size=12),axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 15),axis.title.x = element_text(color = "black", size = 15)) +
  scale_x_continuous(labels=scaleFUN)

# + ggtitle(paste(species,"All processors"))
all.TL2

ggsave(filename=paste(plot_dir,paste(species,"allfishers_TLMonths.png",sep="_"),sep="/"),plot=all.TL2,width=25,height=20,units="cm",dpi=300,type="cairo-png")


#scales free
all.TL3 <- ggplot(TLc3, aes(TL,V1, group=factor(month), fill=factor(month))) +
  geom_bar(stat="identity",position=position_dodge(2),width=0.45) + 
  theme_bw(25) + scale_y_continuous(expand=c(0,0)) + 
  ylab("N") + xlab("Total length (cm)")+facet_grid(rows=vars(month),vars(vessel),scales="free_y") +
  geom_vline(aes(xintercept=weighted.mean(TL, V1)), col="red", size=1.2) +
  theme(legend.position="none",strip.text.x=element_text(size=12,colour="black"),
        strip.text.y = element_text(size=12),axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 15),axis.title.x = element_text(color = "black", size = 15))
# + ggtitle(paste(species,"All processors")) 
all.TL3

ggsave(filename=paste(plot_dir,paste(species,"allfishers_TLMonthsfreescal.png",sep="_"),sep="/"),plot=all.TL3,width=25,height=20,units="cm",dpi=300,type="cairo-png")


# ===================================================--
# 4. Slipped and discarded catch ----
# ===================================================--

#4.1 Slipped----
head(pilfish)

slipdf<- as.data.table(pilfish)
slipdf <- slipdf[,sum(as.numeric(as.character(slipped)), na.rm=TRUE), by=c("vessel","month","year")]
slipdf <- subset(slipdf,!V1==0) 

ggplot(slipdf,aes(factor(month),V1,fill=vessel))+
  geom_bar(stat="identity",position="dodge")+
  theme_bw(25) +ylab("Slipping (Kg)")+xlab("Month")+
  scale_y_continuous(limits=c(0,max(slipdf$V1)+500), expand=c(0,0))+
  theme(legend.position = "bottom")+ggtitle("")


#same width of barcols
table(slipdf$month)

slipdf$month[slipdf$month=="7"] <- "Jul"
slipdf$month[slipdf$month=="8"] <- "Aug"
slipdf$month[slipdf$month=="9"] <- "Sep"
slipdf$month[slipdf$month=="10"] <- "Oct"
slipdf$month[slipdf$month=="11"] <- "Nov"
slipdf$month[slipdf$month=="12"] <- "Dec"
slipdf$month[slipdf$month=="1"] <- "Jan"
slipdf$month[slipdf$month=="2"] <- "Feb"

slipdf$month <- factor(slipdf$month,levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)  

head(slipdf)
slip2 <- slipdf %>% 
  complete(month, vessel) %>% 
  ggplot(aes(x = month, y = V1, fill = vessel)) +
  geom_col(position = position_dodge())

slip3 <- slip2+theme_bw(25)+ylab("Slipping (Kg)")+xlab("Month")+
  scale_y_continuous(limits=c(0,max(slipdf$V1)+500),expand=c(0,0))+
  theme(legend.position = "bottom",legend.text=element_text(size=10))

ggsave(filename=paste(plot_dir,paste(species,"slipped.png",sep="_"),sep="/"),plot=slip3,width=25,height=20,units="cm",dpi=300,type="cairo-png")


#4.2 Bycatch----
head(pilfish)
str(pilfish)

byc <- pilfish[,c("vessel","date","lat","lon","depth","pil","birdsbc","sealsbc","dolphinsbc","tunabc","month","year")]

byc <- data.table(byc)

birds <- byc[,sum(as.numeric(as.character(birdsbc)), na.rm=TRUE), by=c("vessel", "month","year")]
birds <- subset(birds,!V1==0)
names(birds)[4] <- "birds"

seals <- byc[,sum(as.numeric(as.character(sealsbc)), na.rm=TRUE), by=c("vessel", "month","year")]
seals <- subset(seals,!V1==0)
names(seals)[4] <- "seals"

dolp <- byc[,sum(as.numeric(as.character(dolphinsbc)), na.rm=TRUE), by=c("vessel", "month","year")]
dolp <- subset(dolp,!V1==0)
names(dolp)[4] <- "dolp"

tuna <- byc[,sum(as.numeric(as.character(tunabc)), na.rm=TRUE), by=c("vessel", "month","year")]
tuna <- subset(tuna,!V1==0)
names(tuna)[4] <- "tuna"

#merge them
a <- merge(birds,seals,all=T)
b <- merge(a,dolp,all=T)
c <- merge(b,tuna,all=T)

library(reshape)
mdata <- melt(c, id=c("vessel","month","year"))

ggplot(mdata, aes(month, value, colour = variable,group=vessel)) + geom_point()

#plot of bycatch
mdata2 <- with(mdata,aggregate(value,list(month=month,variable=variable),sum,na.rm=T))

mdata2$month[mdata2$month=="7"] <- "Jul"
mdata2$month[mdata2$month=="8"] <- "Aug"
mdata2$month[mdata2$month=="9"] <- "Sep"
mdata2$month[mdata2$month=="10"] <- "Oct"
mdata2$month[mdata2$month=="11"] <- "Nov"
mdata2$month[mdata2$month=="12"] <- "Dec"

mdata2$month <- factor(mdata2$month,levels = c("Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)  

#mdata2[17,] <- c("Oct","tuna",0) #add new row to have oct in the plot


bycatch <- ggplot(mdata2, aes(month, x, colour = variable))+geom_point(size=3)##+geom_line(size=1.2)

bycacth2 <-bycatch+ylab("Bycatch (N)")+xlab("Month")+scale_y_continuous(expand=c(0,0))+
  theme(legend.position="top")+ggtitle("")+labs(colour="species")+theme_bw(25)

bycacth3<- bycacth2+scale_y_continuous(expand=c(0,0),breaks=c(0,2,4,6,8,10,12))
bycacth3

ggsave(filename=paste(plot_dir,paste(species,"bycatch.png",sep="_"),sep="/"),plot=bycacth3,width=25,height=20,units="cm",dpi=300,type="cairo-png")


#######################################################--
# *****************************************************--
# MAPS ----
# *****************************************************--
#######################################################--

# ===================================================--
# 5.1 Map with sardine and other catches ----
# ===================================================--

# Load europe coastline
#wmap <- raster::shapefile("C:/Users/SRC01/OneDrive - CEFAS/01. PELTIC/Maps Peltic/Europe//EuropeESRI_high.shp")
#mybox <- matrix(c(-6,50,0,51.5), nrow = 2, ncol = 2, dimnames = list(c("x","y"), c("min","max")))#whole cornish peninsula
mybox <- matrix(c(-6,50,-1,51.5), nrow = 2, ncol = 2, dimnames = list(c("x","y"), c("min","max")))#little zoom cornish peninsula
#mybox <- matrix(c(-4.0,49.75,-2.0,51.0), nrow = 2, ncol = 2, dimnames = list(c("x","y"), c("min","max"))) #Lyme bay

# clip spatialLines
#ec_map <- gClip(wmap, mybox)
#ec <- df_spatial(ec_map)

library(rnaturalearth) # For coastlines
worldcoastlines <- ne_countries(scale = "medium", returnclass = "sf")
  
# ggplot()+
#   geom_sf(data=worldcoastlines,fill='darkseagreen') + coord_sf(xlim = c(-6,-1), ylim = c(49.8, 51.5)) +
#   geom_point(data=df,aes(x=lon,y=lat),col="red",size=2)+
#   theme_bw(20)+ ylab("Latitude")+xlab("Longitude")


# First create dataframe
head(pilfish)
LOG <-pilfish[!is.na("lon"),c("lon", "lat","month", "pil", "ane", "spr", "her", "mac", "hom","vessel")]

str(LOG)
summary(LOG)

LOG$pil[is.na(LOG$pil)] <- 0 
LOG$ane[is.na(LOG$ane)] <- 0 
LOG$spr[is.na(LOG$spr)] <- 0 
LOG$her[is.na(LOG$her)] <- 0 
LOG$mac[is.na(LOG$mac)] <- 0 
LOG$hom[is.na(LOG$hom)] <- 0 

LOG$ane <- as.factor(as.character(LOG$ane))
LOG$ane <- as.numeric(as.factor(LOG$ane))
LOG$her <- as.factor(as.character(LOG$her))
LOG$her <- as.numeric(as.factor(LOG$her))
LOG$mac <- as.factor(as.character(LOG$mac))
LOG$mac <- as.numeric(as.factor(LOG$mac))
LOG$hom <- as.factor(as.character(LOG$hom))
LOG$hom <- as.numeric(as.factor(LOG$hom))

LOG1 <- LOG[!(LOG$lat==0|LOG$lon==0),]

# P_All_MapHaul <-ggplot() + 
#   geom_sf(data=worldcoastlines,fill='darkseagreen') + coord_sf(xlim = c(-6,-1), ylim = c(49.8, 51.5)) +
#   geom_polypath(col="black")+ylab("Lat") + xlab("Lon") + theme_bw(20)+
#   geom_scatterpie(data=LOG1, aes(x=lon, y=lat, group=as.factor(vessel)), 
#                   cols=c("spr", "pil", "ane", "her", "mac", "hom"), 
#                   alpha=0.5,legend_name = "species",pie_scale = 1.5) +
#   facet_wrap(.~month,nrow=3) 
# 
# P_All_MapHaul

# Save map w piechart
# ggsave(filename=paste(plot_dir,paste(species,"MapHauls_Othersp.png",sep="_"),sep="/"),plot=P_All_MapHaul,width=50,height=20,units="cm",dpi=200,type="cairo-png")


# ===================================================--
# 5.2 Sardine map ----
# ===================================================--

# Load europe coastline
#wmap <- raster::shapefile("X:/Current3rdPartyData/GeodataShapefiles/Coastlines/Europe/EuropeESRI_high.shp")#from GIS server (slower)
#wmap <- raster::shapefile("C:/Users/SRC01/OneDrive - CEFAS/01. PELTIC/Maps Peltic/Europe//EuropeESRI_high.shp")
mybox <- matrix(c(-6.5,49.5,-2.0,51.5), nrow = 2, ncol = 2, dimnames = list(c("x","y"), c("min","max")))#whole cornish peninsula
#mybox <- matrix(c(-6.0,49.7,0.0,51.5), nrow = 2, ncol = 2, dimnames = list(c("x","y"), c("min","max")))#whole cornish peninsula
#mybox <- matrix(c(-3.6,47.0,-3.0,51.0), nrow = 2, ncol = 2, dimnames = list(c("x","y"), c("min","max"))) #lyme bay

# clip spatialLines
#ec_map <- gClip(wmap, mybox)
#ec <- df_spatial(ec_map)
#plot(ec_map)

# Isolate data for Sardine
LOG1
table(LOG1$month)
LOG1$month[LOG1$month=="7"] <- "Jul"
LOG1$month[LOG1$month=="8"] <- "Aug"
LOG1$month[LOG1$month=="9"] <- "Sep"
LOG1$month[LOG1$month=="10"] <- "Oct"
LOG1$month[LOG1$month=="11"] <- "Nov"
LOG1$month[LOG1$month=="12"] <- "Dec"
LOG1$month[LOG1$month=="1"] <- "Jan"
LOG1$month[LOG1$month=="2"] <- "Feb"

LOG1$month <- factor(LOG1$month,levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)  

PIL_hauls_pos <- LOG1[!is.na(LOG1$pil),c("lon", "lat", "vessel", "month", "pil")] #project(cbind(IBTS10_h$ShootLong, IBTS10_h$ShootLat), proj="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") 
# remove zeros
#PIL_hauls_pos <- PIL_hauls_pos[which(PIL_hauls_pos$pil!=0),]
# anonimize vessels
PIL_hauls_pos$vessCode <- as.factor(PIL_hauls_pos$vessel)
levels(PIL_hauls_pos$vessCode) <- c("vess1", "vess2","vess3","vess4","vess5","vess6","vess7","vess8","vess9")
table(PIL_hauls_pos$vessCode,PIL_hauls_pos$vessel)


#longs for vessel 4 are off, remove for visualitzation
dim(PIL_hauls_pos)
#PIL_hauls_pos <- subset(PIL_hauls_pos,!(lon>-4 & vessel=="LYONESSE"))

#select vessel
vess1 <- subset(PIL_hauls_pos,vessCode=="vess1") 
#a) one vessel---- Only one vessel provided logbook data by Feb 2023, lyonesse
MapHaulv1 <- ggplot() +
  geom_sf(data=worldcoastlines,fill='darkseagreen') + coord_sf(xlim = c(-6,-1), ylim = c(49.8, 51.5)) +
  geom_polypath(col="black") +
  guides(fill=FALSE) + ylab("Lat") + xlab("Lon") +
  theme_bw(25) +
  geom_point(data=vess1, aes(lon, lat, group=factor(month), size=pil/1000,fill=factor(month), col=factor(month),alpha=0.3))+
#  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  scale_color_discrete(name = "Month", labels=c(levels(vess1$month)))+
  scale_size_continuous(range = c(min(vess1$pil/1000),max(vess1$pil/1000)), name = "Catch (t)")
MapHaulv1

ggsave(filename=paste(plot_dir,paste(species,unique(vess1$vessel),"MapHauls.png",sep="_"),sep="/"),plot=MapHaulv1,width=30,height=20,units="cm",dpi=200,type="cairo-png")


#b) all vessels----
MapHaul <- ggplot() + 
  geom_sf(data=worldcoastlines,fill='darkseagreen') + coord_sf(xlim = c(-6,-1), ylim = c(49.8, 51.5)) +
  geom_point(data=PIL_hauls_pos, aes(lon, lat, group=factor(month),alpha=0.8,size=pil/1000,fill=factor(month), col=factor(month)))+
  geom_polypath(col="black") +
  guides(fill=FALSE) + ylab("Lat") + xlab("Lon") +
  theme_bw(22) +
#  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + 
  scale_color_discrete(name = "Month", labels=c(levels(PIL_hauls_pos$month)))+
  scale_size_continuous(range = c(min(PIL_hauls_pos$pil/1000),max(PIL_hauls_pos$pil/1000)), name = "Catch (t)")+
  facet_grid(~vessCode)+theme(legend.position = "bottom")+
  labs(title="", y="Latitude", x="Longitude")

MapHaul

#c) better visualization----
MapHaul <- ggplot() + 
  geom_sf(data=worldcoastlines,fill='darkseagreen') + coord_sf(xlim = c(-6,-1), ylim = c(49.8, 51.5)) +
  geom_point(data=PIL_hauls_pos, aes(lon, lat,group=factor(month),alpha=0.8,size=pil/1000,fill=factor(month), col=factor(month)))+
  geom_polypath(col="black") +guides(fill=FALSE)+
  theme_bw(22) +
#  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + 
  scale_color_discrete(name = "Month", labels=c(levels(PIL_hauls_pos$month)))+
  scale_size_continuous(name = "Catch (t)",range = c(min(PIL_hauls_pos$pil/1000),(max(PIL_hauls_pos$pil/1000))))+
  facet_grid(~vessCode)+theme(legend.position = "bottom")+
  labs(title="Sardine", y="Latitude", x="Longitude")

MapHaul2 <- MapHaul+scale_size(range=c(0,30))+ scale_size_continuous(name = "Catch (t)")+guides(alpha=FALSE)

# Save map w piechart
ggsave(filename=paste(plot_dir,paste(species,"MapHauls(1).png",sep="_"),sep="/"),plot=MapHaul,width=50,height=20,units="cm",dpi=200,type="cairo-png")
ggsave(filename=paste(plot_dir,paste(species,"MapHauls(2).png",sep="_"),sep="/"),plot=MapHaul2,width=50,height=20,units="cm",dpi=200,type="cairo-png")


#d) by month/vessel----
MapHaul3 <- ggplot() + 
  geom_sf(data=worldcoastlines,fill='darkseagreen') + coord_sf(xlim = c(-6,-1), ylim = c(49.8, 51.5)) +
  geom_polypath(col="black") +guides(fill=FALSE)+
  theme_bw(22) +
  geom_point(data=PIL_hauls_pos, aes(lon, lat,group=factor(month),alpha=0.8,size=pil/1000,fill=factor(month), col=factor(month)))+
#  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + 
  scale_color_discrete(name = "Month", labels=c(levels(PIL_hauls_pos$month)))+
  scale_size_continuous(name = "Catch (t)",range = c(min(PIL_hauls_pos$pil/1000),(max(PIL_hauls_pos$pil/1000))))+
  facet_grid(vessCode~month)+theme(legend.position = "bottom")+
  labs(title="Sardine", y="Latitude", x="Longitude")+guides(alpha=FALSE)

MapHaul3.2 <- MapHaul3+scale_size(range=c(0,30))+ scale_size_continuous(name = "Catch (t)")
ggsave(filename=paste(plot_dir,paste(species,"MapHauls(3).png",sep="_"),sep="/"),plot=MapHaul3.2,width=50,height=25,units="cm",dpi=200,type="cairo-png")


#e) rows
MapHaul4 <- ggplot() + 
  geom_sf(data=worldcoastlines,fill='darkseagreen') + coord_sf(xlim = c(-6,-1), ylim = c(49.8, 51.5)) +
  geom_point(data=PIL_hauls_pos, aes(lon, lat,group=factor(month),alpha=0.8,size=pil/1000,fill=factor(month), col=factor(month)))+
  geom_polypath(col="black") +guides(fill=FALSE)+
  theme_bw(22) +
#  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + 
  scale_color_discrete(name = "Month", labels=c(levels(PIL_hauls_pos$month)))+
  scale_size_continuous(name = "Catch (t)",range = c(min(PIL_hauls_pos$pil/1000),(max(PIL_hauls_pos$pil/1000))))+
  facet_wrap(vessCode~.,nrow=2)+theme(legend.position = "bottom")+
  labs(title="Sardine", y="Latitude", x="Longitude")+guides(alpha=FALSE)
MapHaul4

MapHaul4.2 <- MapHaul4+scale_size(range=c(0,30))+ scale_size_continuous(name = "Catch (t)")
MapHaul4.2

#intentant millorar
MapHaul4.1 <- MapHaul4+ scale_size_continuous(name = "Catch (t)")

MapHaul4.1 <- MapHaul4+scale_size(name="Catch (t)",range = c(3,14))+ 
  guides(color=guide_legend(override.aes = list(size=5)))
MapHaul4.1


ggsave(filename=paste(plot_dir,paste(species,"MapHauls(4).png",sep="_"),sep="/"),plot=MapHaul4.1,width=45,height=45,units="cm",dpi=200,type="cairo-png")
ggsave(filename=paste(plot_dir,paste(species,"MapHauls(5).png",sep="_"),sep="/"),plot=MapHaul4.1,width=32,height=30,units="cm",dpi=200,type="cairo-png")


# # ENVIRONMENT (don't have this info)
# # See if any relationship between widn speed and catch
# SPR_Env <- SPR_LOGc[,.(Date., month, Wind_speed, Tide, PILats)]
# PIL_Env[,Tide:=as.numeric(as.character(Tide))]
# # Scatterplot Wind speed vs catches
# P_WindCatch <- ggplot(PIL_Env, aes(Wind_speed, PILats)) + geom_point()
# # Scatterplots tide vs catches
# P_TideCatch <- ggplot(PIL_Env[!is.na(Tide),], aes(Tide, PILats/1000)) + geom_point() +
#   theme_classic(25) + xlab("Tide (m)") + ylab("Catch (t)") 
# # save 
# ggsave(file=paste0(out_dir, "/PIL_Tide.png"), P_TideCatch, width=30, height=20, units="cm", dpi=200)
# 
