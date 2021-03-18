#Summary Statistics

setwd("C:/Users/brittanyha/OneDrive - Bill & Melinda Gates Foundation/Brittany Hagedorn/COVID tools/Modeling needs Phase 3/NVDP set 1")
AD <- data.frame(read_xlsx("NVDP Dataset _ Master.xlsx",sheet='General info (working version)'))

#Number of pages
p1 <- ggplot(data=AD, aes(x=log10(Total_Target_Pop),y=Plan_length)) + 
  geom_point(aes(color=WHO_Region,shape=WHO_Region)) + theme_bw() +
  geom_text(aes(label=Code,hjust=-0.3,vjust=.3),alpha=.5,size=2.5)+
  #geom_label_repel(aes(label=Country))+
  scale_color_brewer(palette="Dark2")+
  xlab("Target Population (1,000s)") + ylab("Pages in NVDP") + 
  scale_x_continuous(breaks=c(3,4,5,6,7,8),
                     labels=c(1,10,100,1000,10000,100000))
plot(p1)

# ggplot(data=AD, aes(x=log10(Total_Target_Pop),y=Plan_length/Total_Target_Pop*1000)) + 
#   geom_point() + theme_bw() +
#   xlab("log base 10 (target population)") + ylab("Pages per 1k Pop in NVDP")
# ggplot(data = AD, aes(x=Plan_length, color = WHO_Region, fill = WHO_Region)) + 
#   geom_histogram(binwidth = 10) + theme_bw()

#IPC
p2 <- ggplot(data=AD, aes(x=log10(Total_Target_Pop),y=Wastage_rate)) + 
  geom_point(aes(color=WHO_Region,shape=WHO_Region)) + theme_bw() +
  scale_color_brewer(palette="Dark2")+
  geom_text(aes(label=Code,hjust=-0.3,vjust=.3),alpha=.5,size=2.5)+
  #geom_label_repel(aes(label=Country))+
  xlab("Target Population (1,000s)") + ylab("Wastage rate") + 
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(breaks=c(3,4,5,6,7,8),
                     labels=c(1,10,100,1000,10000,100000))
plot(p2)

p3 <- ggplot(data = AD, aes(x=IPC_count, fill = WHO_Region, color=WHO_Region)) + 
  geom_histogram(binwidth = 1, alpha=.5) + scale_fill_brewer(palette = "Dark2") + 
  scale_color_brewer(palette="Dark2")+
  facet_wrap(~WHO_Region) + theme_bw()

pdf("Summary Plots.pdf",width=9,height=6)
plot(p1)
plot(p2)
plot(p3)
dev.off()