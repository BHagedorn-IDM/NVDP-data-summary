#Human resources

setwd("C:/Users/brittanyha/OneDrive - Bill & Melinda Gates Foundation/Brittany Hagedorn/COVID tools/Modeling needs Phase 3/NVDP set 1")
GN <- data.frame(read_xlsx("NVDP Dataset _ Master.xlsx",sheet='General info (working version)'))

GN$SupportRatio <- GN$Vx_Team_Size/GN$Vaccinators_Per_Team

# ggplot(data=subset(GN,!is.na(Vx_Team_Size)),aes(x=as.factor(Vx_Team_Size))) + 
#   geom_bar() + theme_bw() + 
#   ylab("# of Countries") + xlab("Team Size") +
#   theme(axis.text.x=element_text(angle=45,hjust=1))

# ggplot(data=subset(GN,!is.na(Vx_Team_Size)),aes(x=Vx_Team_Size,y=SupportRatio)) + 
#   geom_jitter() + theme_bw() + 
#   ylab("Support Ratio (Non/Vacc)") + xlab("Team Size") +
#   theme(axis.text.x=element_text(angle=45,hjust=1))

p4 <- ggplot(GN,aes(x=log10(Total_Target_Pop),y=Vaccinator_num_need)) +geom_point() + theme_bw() + 
  xlab("Population (1000s)") + ylab("# Vaccinators Needed") + 
  geom_text(aes(label=Code,hjust=-0.3,vjust=.3),alpha=.5,size=2.5) +
  scale_x_continuous(breaks=c(3,4,5,6,7,8),
                     labels=c(1,10,100,1000,10000,100000))
  
p5 <- ggplot(GN,aes(x=Vaccinator_num_existing,y=Vaccinator_num_need)) +geom_point() + theme_bw() + 
  xlab("# Vaccinators Existing") + ylab("# Vaccinators Needed") + 
  geom_text(aes(label=Code,hjust=-0.3,vjust=.3),alpha=.5,size=2.5) + xlim(0,30000) + ylim(0,30000) +
  geom_abline(intercept=0,slope=1)

GNS <- ddply(GN,.(Vx_Team_Size,Vaccinators_Per_Team),summarize,NumberCountries=n())
GNS <- subset(GNS,!is.na(Vx_Team_Size))
GNS <- subset(GNS,!is.na(Vaccinators_Per_Team))
p1 <- ggplot(GNS,aes(x=factor(Vaccinators_Per_Team),y=factor(Vx_Team_Size),fill=NumberCountries))+
  geom_tile() + theme_bw() +
  scale_x_discrete(expand=c(0,0),breaks=c(1,2,3,4,5)) +
  scale_y_discrete(expand=c(0,0),breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)) +
  xlab("Vaccinators per Team") + ylab("Team Size (total)")

GN$Vx_PerTeam_PerDay <- as.numeric(GN$Vx_PerTeam_PerDay)
p2 <- ggplot(subset(GN,!is.na(Vx_PerTeam_PerDay)),
             aes(x=SupportRatio,y=Vx_PerTeam_PerDay/Vaccinators_Per_Team,color=WHO_Region)) + 
  geom_point() + theme_bw() + 
  geom_text(aes(label=Code,hjust=-0.3,vjust=.3),alpha=.5,size=2.5) +
  xlab("Support Staff per Vaccinator") + ylab("Vaccinations per Vaccinator per Day")

p3 <- ggplot(subset(GN,!is.na(Vx_PerTeam_PerDay)),aes(x=Vx_Team_Size,y=Vx_PerTeam_PerDay,color=WHO_Region)) + 
  geom_point() + theme_bw() + 
  geom_text(aes(label=Code,hjust=-0.3,vjust=.3),alpha=.5,size=2.5) +
  xlab("Team Size") + ylab("Vaccinations per Team per Day")

GS <- subset(GN,!is.na(Country))
SB <- GS[,1:3]
SB$MD <- GS$MD_Vaccinators
SB$Nurse <- GS$Nurse_Vaccinators
SB$CHW <- GS$CHW_Vaccinator
SB$Midwife <- GS$Midwife_Vaccinators
SB$Dentist <- GS$Dentist_Vaccinators
SB$Pharma <- GS$Pharmacist_Vaccinators
SB$LabTech <- GS$LabTech_Vaccinators
SB$Retiree <- GS$Retiree_Vaccinators
SB$Student <- GS$Student_Vaccinators
SB$Civilian <- GS$Civilian_Vaccinators

SBM <- melt(SB,id=c("Country","Code","WHO_Region"))
SBM$variable <- factor(SBM$variable,ordered=TRUE,
                       levels=c("Nurse","MD","CHW","Midwife","Denist","Retiree","Student","Pharma","LabTech","Civilian"))
SBM$value[SBM$value == "Yes"] = 1
SBM$value[is.na(SBM$value)] = 0
SBM$value <- as.numeric(SBM$value)
SBM <- subset(SBM,!is.na(value))
SBM <- subset(SBM,!is.na(variable))
SBS <- ddply(SBM,.(WHO_Region,variable),summarize,CountCountries=sum(value))

p6 <- ggplot(SBS,aes(x=variable,y=CountCountries,fill=WHO_Region)) + 
  geom_bar(stat="identity") + theme_bw() + 
  ylab("Number of Countries") + xlab("") +
  theme(axis.text.x=element_text(angle=45,hjust=1))+ 
  scale_fill_brewer(palette="Dark2") + facet_wrap(~WHO_Region)

pdf("HumanResource Plots.pdf",width=7,height=5)
plot(p1)
plot(p2)
plot(p3)
plot(p4)
plot(p5)
plot(p6)
dev.off()  

