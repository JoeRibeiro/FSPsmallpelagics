
#######################################################--
# *****************************************************--
# FISHERS vs PROCESSORS ----
# *****************************************************--
#######################################################--

# ===================================================--
# 6. Length Comparisons ----
# ===================================================--

#go to the file where I comply all the info for the programme
db_inp <- "C:/Users/JR13/Documents/LOCAL_NOT_ONEDRIVE/FSPsmallpelagics2022/Data/Output/"
list.files(db_inp)

# This section brings in PIL_agg2122.csv which is created at the end of script A5, So this section was out of sequence
db <- read.table(paste(db_inp,"PIL_agg2122.csv",sep=""),sep=",",header=T, stringsAsFactors = F)
head(db);dim(db) #1293 13
summary(db)
db[is.na(db)] <- 0 

table(db$source)
table(db$month,db$year,db$fishing_season)
summary(db); str(db)
db$source <- as.factor(as.character(db$source))

#select this fishing season and remove the zeros
db <- subset(db,!length_cm==0)


#6.1 Length comparisons----
comp<- ggplot(db, aes(length_cm,group=factor(source), fill=factor(source))) + geom_density(alpha=0.25) + 
  theme_bw(22) +facet_wrap(~vessel, scales="free_y")

comp2<- ggplot(db, aes(length_cm,fill=factor(source))) + geom_density(alpha=0.25) + 
  theme_bw(15) +facet_wrap(~vessel)+labs(fill="source")
comp3 <- comp2+theme(legend.position="bottom")

#save plot
#ggsave(file=paste0(plot_dir, "/PIL_LProcFish_2122.png"), comp3, width=24, height=16, units="cm", dpi=200)


tapply(db$length_cm, db$source, summary)


#6.2 common months for processors and fishers-----
table(db$month,db$vessel,db$source)

#galwad-y-mor common months are 10-12
#lyonesse common months are 7-9
#mayflower common months are 7-12
#pelagic marskman common months are 7-12
#resolute common months 7-9
#serene dawn common months are 7-12
#vesta common months are 8
#gm <- subset(db,vessel=="GALWAD-Y-MOR"&month%in%c(10,11,12))
l <- subset(db,vessel=="LYONESSE"&month%in%c(9,11,12))
#mf <- subset(db,vessel=="MAYFLOWER"&month%in%c(7,8,9,10,11,12))
#pm <- subset(db,vessel=="PELAGIC MARKSMAN"&month%in%c(7,8,9,10,11,12))
#r <- subset(db,vessel=="RESOLUTE"&month%in%c(7,8,9))
#sd <- subset(db,vessel=="SERENE DAWN"&month%in%c(7,8,9,10,11,12))
#v <- subset(db,vessel=="VESTA"&month%in%c(8))
cc <- subset(db,vessel=="CHARLOTTE CLARE"&month%in%c(11))

#p <- subset(db,vessel%in%c("ASTHORE","CHARLOTTE CLARE","GOLDEN HARVEST","PRIDE OF CORNWALL","RACHEL ANNE","CONSTANT FRIEND","MARY ANNE"))
p <- subset(db,vessel%in%c("CHARLOTTE CLARE","LYONESSE"))


#db2 <- rbind(gm,l,mf,pm,r,sd,v,p)
db2 <- rbind(l,cc)
table(db2$month,db2$vessel,db2$source)

#comparisions with common months
comp<- ggplot(db2, aes(length_cm,group=factor(source), fill=factor(source))) + geom_density(alpha=0.25) + 
  theme_bw(22) +facet_wrap(~vessel, scales="free_y")

comp2<- ggplot(db2, aes(length_cm,fill=factor(source))) + geom_density(alpha=0.25) + 
  theme_bw(15) +facet_wrap(~vessel)+labs(fill="source")
comp3 <- comp2+theme(legend.position="bottom")

#save plot
#ggsave(file=paste0(plot_dir,"/PIL_LProcFish_2122_samemonths.png"), comp3, width=24, height=16, units="cm", dpi=200)


#6.3 Improving graphs----
ggplot(db2, aes(x=length_cm, fill=source)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")+
  facet_wrap(~vessel,scales="free_y")+ theme_bw(22)


library(plyr)
cdat <- ddply(db2, "source", summarise, rating.mean=mean(length_cm))
cdat

# Overlaid histograms with means
hist1 <- ggplot(db2, aes(x=length_cm, fill=source)) +
  geom_histogram(binwidth=.5, alpha=.25, position="identity") +
  geom_vline(data=cdat, aes(xintercept=rating.mean,  colour=source),
             linetype="dashed", size=1)+theme_bw(22)

#ggsave(filename=paste(plot_dir,paste(species,"Hist_fishproc(1).png",sep="_"),sep="/"),plot=hist1,width=50,height=25,units="cm",dpi=200,type="cairo-png")


# Density plots with means
hist2 <- ggplot(db2, aes(x=length_cm, fill=source)) +
  geom_density(alpha=0.25) +
  geom_vline(data=cdat, aes(xintercept=rating.mean, colour=source),
             linetype="dashed", size=1)+theme_bw(22)+
  theme(legend.position = "none")

#ggsave(filename=paste(plot_dir,paste(species,"Hist_fishproc(2).png",sep="_"),sep="/"),plot=hist2,width=50,height=25,units="cm",dpi=200,type="cairo-png")

tapply(db2$length_cm, db2$source, summary)

##############################################################################################################--
#checks with aggregated data: it worked to see that the previous plots were right
#your data
# db <- db2
# 
# #your species
# species <- "PIL"
# 
# #your variable
# var <- c("fishers","processors")
# var[1]
# #var <- unique(db$species)
# 
# db$sel <- db$source
# 
# #loop (check names are correct in line 27 or change them)
# 
# plot1.TL1 <- list()
# plot1.TL2 <- list()
# plot1.TL3 <- list()
# 
# for(i in 1:length(var)){
#   
#   plot1=db[db$sel==var[i],]
#   
#   names(plot1) <- c("date","vessel","totalcatch_kg","length_cm","N","samplewt_g","sampleID","month","year","source","fishing_season","processor","division","sel")
#   
#   # Length distribution per month
#   plot1_cumMonth <- data.table(table(plot1$length_cm,plot1$month))
#   plot1_cum <- data.table(table(plot1$length_cm))
#   names(plot1_cumMonth) <- c("TL", "Month", "Freq")
#   names(plot1_cum) <- c("TL", "Freq")
#   
#   # weighted average overall and weighted average by month
#   plot1_cumMonth[,TL:=as.numeric(TL)]
#   plot1_cumMonth[,Freq:=as.numeric(Freq)]
#   plot1_cumMonth[,wtMean:=weighted.mean(TL,Freq),by="Month"]
#   
#   plot1_cum[,TL:=as.numeric(TL)]
#   plot1_cum[,wtMean:=weighted.mean(TL,Freq)]
#   
#   #put name into months and order them
#   plot1_cumMonth$Month[plot1_cumMonth$Month=="7"] <- "Jul"
#   plot1_cumMonth$Month[plot1_cumMonth$Month=="8"] <- "Aug"
#   plot1_cumMonth$Month[plot1_cumMonth$Month=="9"] <- "Sep"
#   plot1_cumMonth$Month[plot1_cumMonth$Month=="10"] <- "Oct"
#   plot1_cumMonth$Month[plot1_cumMonth$Month=="11"] <- "Nov"
#   plot1_cumMonth$Month[plot1_cumMonth$Month=="12"] <- "Dec"
#   plot1_cumMonth$Month[plot1_cumMonth$Month=="1"] <- "Jan"
#   plot1_cumMonth$Month[plot1_cumMonth$Month=="2"] <- "Feb"
#   
#   plot1_cumMonth$Month<- factor(plot1_cumMonth$Month,levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)  
#   
#   #save the file
#   write.csv(plot1_cumMonth,paste(plot_dir,"/",var[i],"_cumMonth.csv",sep=""),row.names = F)
#   write.csv(plot1_cum,paste(plot_dir,"/",var[i],"_cum.csv",sep=""),row.names = F)
#   
#   
#   ##Overall LFD--
#   plot1.TL1[[i]] <- ggplot(plot1_cum, aes(TL, Freq)) + geom_bar(stat="identity", position="dodge",fill="grey") +
#     theme_bw(25) +ylab("N")+xlab("Total length (cm)")+ 
#     geom_vline(aes(xintercept=wtMean), col="red", size=1.2,lty=2)+
#     theme(legend.position = "none")+ggtitle(paste(var[i]))+
#     scale_y_continuous(limits=c(0,max(plot1_cum$Freq)+10),expand=c(0,0))
#   
#   ggsave(filename = paste(plot_dir,paste(var[i],species,"aTL.png",sep="_"),sep="/"), 
#          plot = plot1.TL1[[i]], width = 25, 
#          height = 20, units = "cm", dpi = 300, type = "cairo-png") 
#   
#   
#   ##LFD by month--
#   plot1_cumMonth <- na.omit(plot1_cumMonth)
#   plot1.TL2[[i]] <- ggplot(plot1_cumMonth, aes(TL,Freq, group=factor(Month), fill=factor(Month))) + geom_bar(stat="identity") + 
#     theme_bw(25) + scale_y_continuous(limits=c(0,max(plot1_cumMonth$Freq)+10), expand=c(0,0)) + 
#     ylab("N") + xlab("Total length (cm)")+ facet_grid(rows = vars(Month)) + #facet_grid(.~Month) by column
#     geom_vline(aes(xintercept=wtMean), col="red", size=1.2,lty=2) +
#     theme(legend.position="none")+
#     ggtitle(paste(var[i])) 
#   
#   #make text smaller
#   plot1.TL3[[i]] <- plot1.TL2[[i]]+theme(text = element_text(size=rel(5.0)))+ scale_fill_brewer(palette="Set1")
#   
#   ggsave(filename = paste(plot_dir,paste(var[i],species,"bTLbyMonth.png",sep="_"),sep="/"), 
#          plot = plot1.TL3[[i]], width = 25, 
#          height = 20, units = "cm", dpi = 300, type = "cairo-png") 
#   
# }

#############################################END##########################################################################----
