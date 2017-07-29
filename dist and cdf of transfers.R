############## DISTRIBUTION AND cdf OF TRANSFERS
############## BRIAN HEALY
############## LAST MODIFIED: 6.18.17

a <- data.frame(Sent = c(1:10))
S1 <- a %>% group_by(Sent) %>% summarize(count = n()) %>% mutate(freq = NA, Condition = "1S", count = NA)

S2 <- a %>% group_by(Sent) %>% summarize(count = n()) %>% mutate(freq = NA, Condition = "2S", count = NA)

bins <- bind_rows(S1, S2)

####MTURK

temp1 <- Sender_ALLMTURK %>% select(Sent, condition) %>% mutate(Sent=Sent/10) %>%
  group_by(Sent, condition) %>% rename(Condition =condition) %>%
  summarize(count = n()) 
temp1 <- temp1 %>% group_by(Condition) %>% mutate(freq = count/sum(count)) 
                   

temp1 <- bind_rows(temp1,bins) %>% mutate(Experiment = "MTurk") 
temp1$Condition[temp1$Condition == "S1S"] <- "1S"
temp1$Condition[temp1$Condition == "S2S"] <- "2S"

###LAB FISRST ROUND

temp2 <- senderALL_lab1P %>% select(Sent, Condition) %>% group_by(Sent, Condition) %>%
  summarize(count = n()) 
temp2 <- temp2 %>% group_by(Condition) %>% mutate(freq = count/sum(count))
  
temp2$Condition[temp2$Condition == "S1S"] <- "1S"
temp2$Condition[temp2$Condition == "S2S"] <- "2S"
                                                
temp2 <- bind_rows(temp2,bins) %>% mutate(Experiment = "Lab First Round")

### LAB ALL ROUNDS

temp3 <- Data_SenderAll  %>% select(Sent, Condition) %>% group_by(Sent, Condition) %>%
  summarize(count = n()) 
temp3$Condition[temp3$Condition == "S1S"] <- "1S"
temp3$Condition[temp3$Condition == "S2S"] <- "2S"
temp3 <- temp3 %>% group_by(Condition) %>% mutate(freq = count/sum(count)) 
                                                  
temp3 <- bind_rows(temp3,bins) %>% mutate( Experiment = "Lab All Rounds")

#COMBINE

transfers1 <- bind_rows(temp1,temp2, temp3) 

#SET FACET ORDER

transfers1$Experiment <- factor(transfers1$Experiment, levels= c("Lab First Round", "Lab All Rounds", "MTurk"))

#GRAPH

distTrans_all <- ggplot(transfers1, aes(x=factor(Sent), y = freq)) + expand_limits(y=.8) +
  geom_bar(stat = "identity", aes(fill=Condition), position = "dodge", size=1) + theme_classic() +
  ggtitle("Figure 2A: Distribution of Transfers") + 
  facet_grid(~Experiment) + theme(strip.background = element_rect(colour="white"), 
                                  strip.text = element_text(size=rel(1)),
                                  panel.spacing = unit(1.2, "lines"),
                                  plot.title = element_text(hjust = 0.5)) + 
  labs(x = "", y = "Proportion",fill="")+ scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1))+ scale_x_discrete(breaks = c(0,2,4,6,8,10))+
  geom_text(aes(label = "", group=Condition), position=position_dodge(width=1), vjust = -.25)+ 
  scale_fill_manual(values = c("#fb9a99","#1f78b4"))

print(distTrans_all)

#### CONDITIONAL TRANSFER DIST ######

##OFFER 6 
####MTURK

temp1 <- Sender_ALLMTURK %>% filter(Promise == 60) %>% select(Sent, condition) %>% mutate(Sent=Sent/10) %>%
  group_by(Sent, condition) %>% rename(Condition =condition) %>%
  summarize(count = n()) 
temp1 <- temp1 %>% group_by(Condition) %>% mutate(freq = count/sum(count)) 


temp1 <- bind_rows(temp1,bins) %>% mutate(Experiment = "MTurk") 
temp1$Condition[temp1$Condition == "S1S"] <- "1S"
temp1$Condition[temp1$Condition == "S2S"] <- "2S"

###LAB FISRST ROUND

temp2 <- senderALL_lab1P %>% filter(Promise == 6) %>% select(Sent, Condition) %>% group_by(Sent, Condition) %>%
  summarize(count = n()) 
temp2 <- temp2 %>% group_by(Condition) %>% mutate(freq = count/sum(count))

temp2$Condition[temp2$Condition == "S1S"] <- "1S"
temp2$Condition[temp2$Condition == "S2S"] <- "2S"

temp2 <- bind_rows(temp2,bins) %>% mutate(Experiment = "Lab First Round")

### LAB ALL ROUNDS

temp3 <- Data_SenderAll  %>% filter(Promise == 6) %>% select(Sent, Condition) %>% group_by(Sent, Condition) %>%
  summarize(count = n()) 
temp3$Condition[temp3$Condition == "S1S"] <- "1S"
temp3$Condition[temp3$Condition == "S2S"] <- "2S"
temp3 <- temp3 %>% group_by(Condition) %>% mutate(freq = count/sum(count)) 

temp3 <- bind_rows(temp3,bins) %>% mutate( Experiment = "Lab All Rounds")

#COMBINE

transfers6 <- bind_rows(temp1,temp2, temp3) 


transfers6$Experiment <- factor(transfers6$Experiment, levels= c("Lab First Round", "Lab All Rounds", "MTurk"))

levels(transfers6$Experiment)[levels(transfers6$Experiment)=="Lab First Round"]  <- "Lab First Round \n n1= 4; n2=20" 
levels(transfers6$Experiment)[levels(transfers6$Experiment)=="Lab All Rounds"]  <- "Lab All Rounds \n n1= 32; n2=154"
levels(transfers6$Experiment)[levels(transfers6$Experiment)=="MTurk"]  <- "MTurk \n n1= 8; n2=36"


## The title should be: "Distribution of transfers, conditional on offer 6".

distTrans_all6 <- ggplot(transfers6, aes(x=factor(Sent), y = freq)) + expand_limits(y=.8) +
  geom_bar(stat = "identity", aes(fill=Condition), position = "dodge", size=1) + theme_classic() +
  ggtitle("Figure 5: Distribution of Transfers, Conditional on Offer 6") +
  facet_grid(~Experiment) + theme(strip.background = element_rect(colour="white"), 
                                  strip.text = element_text(size=rel(1)),
                                  panel.spacing = unit(1.2, "lines"),
                                  plot.title = element_text(hjust = 0.5)) + 
  labs(x = "", y = "Proportion",fill="")+ scale_y_continuous(breaks = c(0,.2,.4,.6,.8,1))+ scale_x_discrete(breaks = c(0,2,4,6,8,10))+
  geom_text(aes(label = "", group=Condition), position=position_dodge(width=1), vjust = -.25)+ 
  scale_fill_manual(values = c("#fb9a99","#1f78b4"))

print(distTrans_all6)
# CDF

temp1 <- Sender_ALLMTURK %>% select(Sent, condition) %>% mutate(Experiment = "MTurk", Sent = Sent/10) %>% 
  rename(Condition = condition)
temp2 <- senderALL_lab1P %>% select(Sent, Condition) %>% mutate(Experiment = "Lab First Round")
temp3 <- Data_SenderAll  %>% select(Sent, Condition) %>% mutate(Experiment = "Lab All Rounds")
transfers <- bind_rows(temp1,temp2, temp3)
transfers$Experiment <- factor(transfers$Experiment, levels= c("Lab First Round", "Lab All Rounds", "MTurk"))

TransferCDF <- ggplot(transfers, aes(x=Sent, colour = Condition)) + theme_classic() 
TransferCDF <- TransferCDF + stat_ecdf(size = 1) + scale_x_continuous(breaks = c(0,2,4,6,8,10))+ 
  scale_y_continuous(breaks = c(0,.20,.40,.60,.80,1.00)) + ggtitle("Figure 2B: CDF of Transfers") +
  labs(x = "Transfer", y="Proportion") + coord_cartesian(xlim = c(0,10), ylim = c(0,1)) +
  facet_grid(~Experiment) + theme(strip.background = element_rect(colour="white"), 
                                  strip.text = element_blank(),panel.spacing = unit(1.2, "lines"),
                                  plot.title = element_text(hjust = 0.5)) + 
  scale_colour_manual(values = c("#fb9a99","#1f78b4"))
print(TransferCDF)

