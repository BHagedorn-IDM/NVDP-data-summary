#summaries of priority populations

ds <- data.frame(read_xlsx("NVDP Dataset _ Master.xlsx",sheet='Delivery plans (working version'))
ds$Target_pop_dxn <- str_to_lower(ds$Target_pop_dxn, locale="en")
#ds$Target_pop_dxn <- paste(ds$Target_pop_dxn,ds$OlderPop)
ds$Target_pop_dxn <- as.factor(ds$Target_pop_dxn)
ds$Target_pop_num <- as.numeric(ds$Target_pop_num)
ds$Priority_Phase <- as.numeric(ds$Priority_Phase)
ds <- subset(ds,Region!="NA")

pd <- ddply(ds,.(Target_pop_dxn,Priority_Phase),summarize,Num_Countries = n())
pd <- subset(pd,Target_pop_dxn != "NA")
pd <- subset(pd,Priority_Phase != "NA")
p1 <- ggplot(data=pd,aes(x=Priority_Phase,y=Num_Countries)) + geom_col() + theme_bw() +
  facet_wrap(~Target_pop_dxn) + #+ ylim(0,45) + xlim(0,10) + 
  scale_x_continuous(breaks=seq(0,max(pd$Priority_Phase),2))

ps <- ddply(ds,.(Country),summarize,Num_Phases = max(Priority_Phase))
ps <- ddply(ps,.(Num_Phases),summarize,Num_Countries=n())
ps <- subset(ps,Num_Phases != "NA")
p2a <- ggplot(data=ps,aes(x=Num_Phases,y=Num_Countries)) + geom_col() + theme_bw() +
  scale_x_continuous(breaks=seq(1,max(ps$Num_Phases))) + 
  xlab("Number of Phases") + ylab("Number of Countries")

ps <- ddply(ds,.(Country,Region),summarize,Num_Phases = max(Priority_Phase))
ps <- ddply(ps,.(Num_Phases,Region),summarize,Num_Countries=n())
ps <- subset(ps,Num_Phases != "NA")
p2b <- ggplot(data=ps,aes(x=Num_Phases,y=Num_Countries)) + geom_col() + theme_bw() +
  scale_x_continuous(breaks=seq(1,max(ps$Num_Phases))) + 
  xlab("Number of Phases") + ylab("Number of Countries") + facet_wrap(~Region)

pa <- ddply(ds,.(Country),summarize,Num_TargetPops = n())
pa <- ddply(pa,.(Num_TargetPops),summarize,Num_Countries=n())
p3a <- ggplot(data=pa,aes(x=Num_TargetPops,y=Num_Countries)) + geom_col() + theme_bw() +
  labs(title="Target Population Complexity") + 
  scale_y_continuous(breaks=seq(0,max(pa$Num_Countries),5)) +
  xlab("Number of Target Populations") + ylab("Number of Countries")

pa <- ddply(ds,.(Country,Region),summarize,Num_TargetPops = n())
pa <- ddply(pa,.(Num_TargetPops,Region),summarize,Num_Countries=n())
p3b <- ggplot(data=pa,aes(x=Num_TargetPops,y=Num_Countries)) + geom_col() + theme_bw() +
  labs(title="Target Population Complexity") + 
  scale_y_continuous(breaks=seq(0,max(pa$Num_Countries),5)) +
  xlab("Number of Target Populations") + ylab("Number of Countries") + facet_wrap(~Region)

#Consider adding pop size in 3%, 20%, etc.

pdf("Targeting Plots.pdf",width=9,height=6)
plot(p1)
plot(p2a)
plot(p2b)
plot(p3a)
plot(p3b)
dev.off()