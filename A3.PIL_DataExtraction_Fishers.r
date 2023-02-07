#Extracting data for PIL from fishers FSP
#@silvia Rodriguez Climent
# 09-11-2020; last modified 09/09/2022
#---------------------------------------------------------------------##

setwd("C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/FSPsmallpelagics2022/")
rm(list=ls())


#enter species------
species = 'PIL' # Not working for anything but PIL yet.  it makes outputs for sardine still even when the species is set to sprat
speciesname <- "Sardine"


#.rs.restartR();#xlcFreeMemory();#gc() #see memmory used
#options(java.parameters = "-Xmx1000m")  ## memory set to 2 GB
#install.packages("rlang")

packages <- c("XLConnect", "rJava","ggplot2","data.table","xlsx","openxlsx","dplyr","readxl","stringr","plyr",
              "tidyr","reshape2", "maps","mapdata","mapproj","mapplots","lubridate","rgdal","raster","ggspatial")


lib <- function(packages){ 
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
}


lib(packages)


# library(XLConnect);library(rJava);library(ggplot2);library(data.table);library(xlsx);library(openxlsx);
# library(dplyr);library(readxl);library(stringr);library(plyr);library(tidyr);library(reshape2);library(maps);
# library(mapdata);library(mapproj);library(mapplots);library(lubridate);library(rgdal);library(raster);library(ggspatial)


#######################################################--
# *****************************************************--
# 0. Set up directories ----
# *****************************************************--
#######################################################--

#inp_dir <- file.path(getwd(), "Data/Fishers/")

# set input, output directories
inp_dir <- file.path(getwd(), "Data/Fishers//")
out_dir <- file.path(getwd(), "Data/Output/")
list.files(inp_dir)

#######################################################--
# *****************************************************--
# 2. Read Logbooks (LB)----
# *****************************************************--
#######################################################--

# List of files
list.files(inp_dir)
#temp <- unlist(getSheetNames(paste(inp_dir, "FSP_Database_Fishers2021_vs5.xlsx", sep="/"))) #does not work
#getSheetNames("C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP2021/Data/Fishers/FSP_Database_Fishers2021_vs5.xlsx")
temp <- unlist(getSheetNames(paste(inp_dir, "FSP_Database_Fishers2223.xlsx", sep=""))) 
# This excel spreadsheet was broken as two column names were unseparated, causing an NA column, and longitude was misspelt as longitue. I corrected this in the excel file as well as in the script (which expected longitue)

sheetLOG <- temp[grepl("^LB", temp)]
# Before reading them in, make sure shooting and hauling time are in time format in excel and that there are no comments in those columns.
# create list with all my Logbooks
LOG <- list()
for(i in sheetLOG) LOG[[i]] <- data.table(xlsx::read.xlsx(paste(inp_dir, "FSP_Database_Fishers2223.xlsx", sep="/"), header=TRUE, sheetName = i))
LOG <- LOG[sapply(LOG, function(x) dim(x)[1]) > 0]

# add column with ID
LOGb <- mapply(cbind, LOG, "SampleID"=names(LOG), SIMPLIFY=F)
summary(LOGb)

str(LOGb)
#LOGb$LB_PELAGIC_MARKSMAN$Shoot.time<- as.numeric(LOGb$LB_PELAGIC_MARKSMAN$Shoot.time) #need to be able to join them
#LOGb$LB_CHARLOTTE_CLARE$Shoot.time<- as.numeric(LOGb$LB_CHARLOTTE_CLARE$Shoot.time) #need to be able to join them

# from list to DF
LOGc <- do.call(rbind.data.frame,LOGb)
summary(LOGc)
dim(LOGc[is.na(LOGc$Latitude),])

# convert date
LOGc[,Date:=as.Date(Date, format="%Y-%m-%d")]
# add month
LOGc[,month:=month(Date)]
LOGc[,year:=year(Date)]

LOGc$fishingseason = "2022-2023"
# #add fishing season
# for (i in 1:nrow(LOGc)){
#   {if (LOGc$month[i]%in%c("1","2","3")&LOGc$year[i]=="2021")
#     LOGc$fishingseason[i] <-"2020-2021"}
#   {if (LOGc$month[i]%in%c("7","8","9","10","11","12")&LOGc$year[i]=="2021")
#     LOGc$fishingseason[i] <-"2021-2022"}
#   {if (LOGc$month[i]%in%c("1","2")&LOGc$year[i]=="2022")
#     LOGc$fishingseason[i] <-"2021-2022"}
# }

table(LOGc$month,LOGc$year,LOGc$fishingseason)

#-------------------------------------------#
# Convert lat long into decimal degrees
# For lat long columns: makes sure that there are Degrees, minutes and seconds (if seconds not there just add 00) separated by a space
# If missing lat long, either leave blank space or put NA
# If necessary, add space between degree minutes seconds

head(LOGc);str(LOGc)
table(LOGc$Latitude);table(LOGc$Longitude)

LOGc$LAT1 <- sub("\\s+$", "", gsub('(.{2})', '\\1 ',LOGc$Latitude))
table(LOGc$LAT1)

LOGc$LON1 <- sub("\\s+$", "", gsub('(.{2})', '\\1 ',paste0("0",LOGc$Longitude,sep="")))
table(LOGc$LON1)

##Decimal Degrees = degrees + (min/60) + (sec/3600)
LOGc$lat <-(as.numeric(substr(LOGc$LAT1,1,2))+((as.numeric(substr(LOGc$LAT1,4,5))/60)+(as.numeric(substr(LOGc$LAT1,7,8))/3600)))
LOGc$lon <-(as.numeric(substr(LOGc$LON1,1,2))+((as.numeric(substr(LOGc$LON1,4,5))/60)+(as.numeric(substr(LOGc$LON1,7,8))/3600)))
LOGc$lon <- LOGc$lon*-1
table(LOGc$lat)
table(LOGc$lon)

plot(lat~lon,LOGc)
# library(EchoR)
# coast()


#link with the code that you already had
df <- LOGc

#correct format
head(df);str(df)
#df2$date<-strftime(strptime(df2$date,"%Y-%m-%d"),"%d/%m/%Y")  #changing from one format to another one
#df$prova2 <- openxlsx::convertToDateTime(as.vector(df[,"Shoot.time"]))
table(df$Shoot.time)
str(df$Shoot.time)

df$shoot <- as.POSIXct(x=as.numeric(df$Shoot.time),format="%H:%M",tz="UTC",origin=lubridate::origin)
df$shoot <- substr(df$shoot,13,20)
head(df)
table(df$shoot)


#Map lat and lon correct------

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  proj4string(b_poly) <- proj4string(shp)
  gIntersection(shp, b_poly, byid = TRUE)
}

library(rnaturalearth) # For coastlines
worldcoastlines <- ne_countries(scale = "medium", returnclass = "sf")
    
    
# Load europe coastline
# wmap <- raster::shapefile("C:/Users/SRC01/OneDrive - CEFAS/01. PELTIC/Maps Peltic/Europe//EuropeESRI_high.shp")
#mybox <- matrix(c(-6,50,0,51.5), nrow = 2, ncol = 2, dimnames = list(c("x","y"), c("min","max")))#whole cornish peninsula
mybox <- matrix(c(-6,49.8,-1,51.5), nrow = 2, ncol = 2, dimnames = list(c("x","y"), c("min","max")))#little zoom cornish peninsula
#mybox <- matrix(c(-4.0,49.75,-2.0,51.0), nrow = 2, ncol = 2, dimnames = list(c("x","y"), c("min","max"))) #Lyme bay

# clip spatialLines
#ec_map <- gClip(wmap, mybox)
#ec <- df_spatial(ec_map)

ggplot()+
  geom_sf(data=worldcoastlines,fill='darkseagreen') + coord_sf(xlim = c(-6,-1), ylim = c(49.8, 51.5)) +
  geom_point(data=df,aes(x=lon,y=lat),col="red",size=2)+
  theme_bw(20)+ ylab("Latitude")+xlab("Longitude")

###################################---

#select wanted columns and change the names
df2 <- df[,c("Vessel.Name","Vessel.registration","Skipper","Date","lat","lon","Depth.of.fish..m.",
             "Sardines..kg._retained","Anchovy..kg._retained","Sprats..kg._retained","Herring...kg._retained",
             "Mackerel..kg._retained","Scad..kg._retained","Bycatch..kg.","Slipped.fish..kg.","Dead.fish..kg.",
             "Time.at.sea..total.h.","birds_bycatch","seals_bycatch","dolphins_bycatch","tuna_bycatch","shoot",
             "month","year","fishingseason")]
names(df2)
colnames(df2) <- c("vessel","reg","skipper","date","lat","lon","depth","pil","ane","spr","her","mac","hom",
                   "bycatch","slipped","dead","hsea","birdsbc","sealsbc","dolphinsbc","tunabc","shoot","month",
                   "year","fishingseason")#scad assumed to be HOM

head(df2);dim(df2) #383 25
summary(df2)
#df2[is.na(df2)] <- 0

#remove NA's
row.has.na <- apply(df2, 1, function(x){any(is.na(x))})
sum(row.has.na) #totes les rows tenen NA per tant no les puc eliminar
final.filtered <- df2[!row.has.na,]
summary(final.filtered)

#save it
write.csv(df2,file=paste(out_dir,"/",species,"_LBfishers_2022.csv",sep=""),row.names=F)

#select your season
#season 2021-2022--
df3 <- subset(df2,fishingseason=="2022-2023")
table(df3$month,df3$year)
dim(df3) #328 25
summary(df3)
#df3[!is.na(df3)] <- 0
#saveit
write.csv(df3,file=paste(out_dir,"/",species,"_LBfishers_2223.csv",sep=""),row.names = F)


#######################################################--
# *****************************************************--
# 3. Read Lengths (TL) ----
# *****************************************************--
#######################################################--

list.files(inp_dir)
temp <- unlist(getSheetNames(paste(inp_dir, "FSP_Database_Fishers2223.xlsx", sep="/"))) #trying with .xls version (less memory used), getSheetNames does not work with .xls

sheetTL <- temp[grepl("^TL", temp)]

TL <- list()
for(i in sheetTL) TL[[i]] <- data.table(xlsx::read.xlsx(paste(inp_dir,"FSP_Database_Fishers2223.xlsx", sep="/"),header=TRUE,sheetName = i,stringsAsFactors=F))

# add column with ID
TLb <- mapply(cbind,TL, "SampleID"=names(TL), SIMPLIFY=F)
View(TLb)

#remove the extra column from GALWAD Y MOR
#TLb$TL_GALWADYMOR<- as.data.frame(TLb$TL_GALWADYMOR)
#str(TLb$TL_GALWADYMOR)

#TLb$TL_GALWADYMOR <- TLb$TL_GALWADYMOR[,!(names(TLb$TL_GALWADYMOR)%in%c("SAMPLE.WEIGHT"))]
#TLb$TL_GALWADYMOR<- data.table(TLb$TL_GALWADYMOR)
str(TLb)


# from list to DF
TLc <- do.call(rbind.data.frame,TLb)

# SUBSET
TLc=TLc[TLc$Species==speciesname,]

TLc$Date <- as.Date(TLc$Date,format="%Y-%m-%d")
TLc$month <- month(TLc$Date)
TLc$year <- year(TLc$Date)

colnames(TLc) <- c("vessel","reg","measurer","date","haul","haul_weight_t","sp","TL","N","comm","SampleID","month","year")

table(TLc$month)
table(TLc$year)

table(TLc$month,TLc$year)

#season 2021-22--
table(TLc$month,TLc$year)

#TL19 <- subset(TLc,month%in%c("1","2","3")& year=="2020")
#table(TL19$month,TL19$year)

#TL20 <- subset(TLc,month%in%c("7","8","9","10","11","12")& year=="2020")
#table(TL20$month,TL20$year)

#dim(TL19) #107 13
#dim(TL20) #1761 13

TotL <- TLc
summary(TotL)
TotL[is.na(TotL$TL)] <- 0

# sum up lengths
TLc1 <- TotL[!is.na(N),sum(N), by=.(TL)]
TLc2 <- TotL[!is.na(N),sum(N), by=.(TL,month,vessel)]

# calculate median and weighted average
stat2 <- TLc1 %>%
  dplyr::summarize(median=median(TL), weighted.mean(TL, V1))

stat1 <- TLc2 %>%
  group_by(month,vessel) %>%
  dplyr::summarize(median=median(TL), weighted.mean(TL, V1))


#saveit
write.csv(TotL,file=paste(out_dir,"/",species,"_TLfishers_2223.csv",sep=""),row.names = F)
###########################################################################################################################--

#next script: A4.PIL_Plots_Fishers #-----

########################################### END ##############################################################################-----
