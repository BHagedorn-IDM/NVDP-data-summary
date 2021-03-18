#Delivery strategies

DP <- data.frame(read_xlsx("NVDP Dataset _ Master.xlsx",sheet='Delivery plans (working version'))

DP <- subset(DP,!is.na(Country))
DP$Target_pop_num <- as.numeric(DP$Target_pop_num)

#Change yes/no to binaries for multiplication, then calculate pop by mode
DP$P3_binary <- 0
DP$P20_binary <- 0
DP$del_fac <- 0
DP$del_exfac <- 0
DP$del_nwfac <- 0
DP$del_mb <- 0
DP$del_fp <- 0
DP$del_d2d <- 0
DP$del_unk <- 0
DP$P3_binary[DP$X3_percent_group == "Yes"] = 1
DP$P20_binary[DP$X20_percent_group == "Yes"] = 1
DP$del_fac[DP$Facility_delivery == "Yes"] = 1
DP$del_exfac[DP$Existing_Facility == "Yes"] = 1
DP$del_nwfac[DP$New__Temp_Facility == "Yes"] = 1
DP$del_mb[DP$Mobile_delivery == "Yes"] = 1
DP$del_fp[DP$Fixed_post == "Yes"] = 1
DP$del_d2d[DP$Mobile_Door_campaign == "Yes"] = 1
DP$del_unk[DP$Non_specific_delivery == "Yes"] = 1
DP$pop_fac <- DP$Target_pop_num * DP$del_fac
DP$pop_exfac <- DP$Target_pop_num * DP$del_exfac
DP$pop_nwfac <- DP$Target_pop_num * DP$del_nwfac
DP$pop_mb <- DP$Target_pop_num * DP$del_mb
DP$pop_fp <- DP$Target_pop_num * DP$del_fp
DP$pop_d2d <- DP$Target_pop_num * DP$del_d2d
DP$pop_unk <- DP$Target_pop_num * DP$del_unk

#restructure & melt
DP <- subset(DP,!is.na(Target_pop_num))
#PSA <-ddply(DP,.(Region),summarize,Total_Pop = sum(Target_pop_num))
SB <- DP[,1:3]
SB[,4:10] <- DP[,39:45]
PSM <- melt(SB,id=c("Country","Region","Target_pop_dxn"))
PSM$variable <- as.character(PSM$variable)
PSM$variable[PSM$variable == "pop_fac"] = "Facility"
PSM$variable[PSM$variable == "pop_exfac"] = "Existing Fac"
PSM$variable[PSM$variable == "pop_nwfac"] = "New Fac"
PSM$variable[PSM$variable == "pop_mb"] = "Mobile"
PSM$variable[PSM$variable == "pop_fp"] = "Fixed Post"
PSM$variable[PSM$variable == "pop_d2d"] = "D2D"
PSM$variable[PSM$variable == "pop_unk"] = "Unspecified"
PSM$variable <- factor(PSM$variable,ordered=TRUE,
                levels=c("Facility","Existing Fac","New Fac","Fixed Post","Mobile","D2D","Unspecified"))

#Plot total population by delivery mode
PSB <- ddply(PSM,.(variable,Region),summarize,Total_Pop = sum(value))
p1 <- ggplot(data=PSB,aes(x=variable,y=Total_Pop/1000000)) + geom_bar(stat="identity") + theme_bw() + 
  facet_wrap(~Region,scales="free") + ylab("Pop (millions)") + xlab("") +
  theme(axis.text.x=element_text(angle=45,hjust=1))

PSC <- ddply(PSM,.(variable,Target_pop_dxn),summarize,Total_Pop = sum(value))
PSC <- subset(PSC,Target_pop_dxn != "Children")
p2 <- ggplot(data=PSC,aes(x=variable,y=Total_Pop/1000000)) + geom_bar(stat="identity") + theme_bw() + 
  facet_wrap(~Target_pop_dxn,scales="free") + ylab("Pop (millions)") + xlab("") +
  theme(axis.text.x=element_text(angle=45,hjust=1))

PSD <- ddply(PSM,.(variable,Target_pop_dxn,Region),summarize,Total_Pop = sum(value))
PSD <- subset(PSD,Target_pop_dxn != "Children")
p3 <- ggplot(data=PSD,aes(x=variable,y=Total_Pop/1000000,fill=Region)) + geom_bar(stat="identity") + theme_bw() + 
  facet_wrap(~Target_pop_dxn,scales="free") + ylab("Pop (millions)") + xlab("") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + 
  scale_fill_brewer(palette="Dark2")

TR <- PSM
TR$Target_pop_dxn[TR$Target_pop_dxn=="High_Risk_HCW"] = "HCW"
TR$Target_pop_dxn[TR$Target_pop_dxn=="Children"] = "Other"
TR$Target_pop_dxn[TR$Target_pop_dxn=="Students"] = "Other"
TR$Target_pop_dxn[TR$Target_pop_dxn=="Teachers"] = "Essential_Workers"
TR$Target_pop_dxn[TR$Target_pop_dxn=="Refugees"] = "Migr_Nmdc_Refug"
TR$Target_pop_dxn[TR$Target_pop_dxn=="Migrant_Nomadic"] = "Migr_Nmdc_Refug"
PSE <- ddply(TR,.(variable,Target_pop_dxn),summarise,Total_Pop=sum(value))
p4 <- ggplot(data=PSE,aes(x=variable,y=Total_Pop/1000000,fill=Target_pop_dxn)) + 
  geom_bar(stat="identity") + theme_bw() + 
  ylab("Pop (millions)") + xlab("") +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + 
  scale_fill_brewer(palette="Dark2") #+ facet_wrap(~Target_pop_dxn,scales="free")


pdf("Delivery Plots.pdf",width=9,height=6)
plot(p1)
plot(p2)
plot(p3)
plot(p4)
dev.off()
