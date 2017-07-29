######### DISTRIBUTION OF RECEIVER AND SENDER BELIEFS
######### BY BRIAN HEALY
######### LAST MODIFIED 6.28.17

######### 1S5 vs 2S 6 ########
a <- data.frame(Belief = c(0:10))
S1 <- a %>% group_by(Belief) %>% summarize(count = n()) %>% mutate(Proportion = NA, Condition = "1S 5", count = NA)

S2 <- a %>% group_by(Belief) %>% summarize(count = n()) %>% mutate(Proportion = NA, Condition = "2S 6", count = NA)

bins <- bind_rows(S1, S2)


# LAB ALL ROUNDS
tempDplyr5 <- receiver_1S_allRounds %>% rename(Belief= beliefR, Promise = Offer) %>%
  filter(Promise  == 5) %>%
  group_by(Belief) %>% summarize(count = n()) %>% 
  mutate(Proportion = round(count/sum(count), digits = 2)) %>% 
  mutate(Condition = "1S 5")

#-> Lab All Round 2S
Beliefs_2S_1P <- select(receiver_2S_allRounds, belief1stA, MessageA) %>%
  rename(Belief = belief1stA, Promise = MessageA )

Beliefs_2S_1P1 <- select(receiver_2S_firstRound, belief1stB, MessageB) %>%
  rename(Belief = belief1stB, Promise = MessageB )

Beliefs_2S_1P <- bind_rows(Beliefs_2S_1P, Beliefs_2S_1P1)

tempDplyr6 <- Beliefs_2S_1P %>% group_by(Promise) %>% 
  filter(Promise  == 6) %>% 
  group_by(Belief) %>% summarize(count = n()) %>% 
  mutate(Proportion = round(count/sum(count), digits = 2)) %>% 
  mutate(Condition = "2S 6")

prop_beliefs_All <- bind_rows(tempDplyr5,tempDplyr6)

prop_beliefs_All <- bind_rows(prop_beliefs_All,bins) %>% mutate(Experiment = "Lab All Rounds") 

ggplot(prop_beliefs_All, aes(x = Belief, y=Proportion)) + 
  geom_bar(aes(fill = factor(Condition)), stat="identity", position= position_dodge(width=.9), width=.8)+
  coord_cartesian(ylim = c(0,.8)) +
  theme_classic() + scale_x_continuous(breaks= c(0,1,2,3,4,5,6,7)) +
  ggtitle("Receiver Beliefs Conditional on 1S5 and 2S6 \nLab All Rounds n1(5)= 185; n2(6)=91")+
  theme(axis.title.x=element_blank(), axis.title.y = element_blank(), 
        plot.title = element_text(hjust = 0.5))+ scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#fb8072","#1f78b4")) + guides(fill=guide_legend(title=element_blank()))


# MTURK 1S 5

tempDplyr <- receiver_1S %>% group_by(randPromiseValue) %>% rename(Promise = randPromiseValue, Belief= Belief1st) %>%
  filter(Promise  == 50) %>%
  group_by(Belief) %>% summarize(count = n()) %>% 
  mutate(Belief=Belief/10, Proportion = round(count/sum(count), digits = 2),Condition = "1S 5")

#-> Mturk Data 2S 6
Beliefs_2S_MT <- select(receiver_2S, Belief1stA, randPromiseValueA) %>%
  rename(Belief = Belief1stA, Promise = randPromiseValueA )

Beliefs_2S_MT1 <- select(receiver_2S, Belief1stB, randPromiseValueB) %>%
  rename(Belief = Belief1stB, Promise = randPromiseValueB )

Beliefs_2S <- bind_rows(Beliefs_2S_MT, Beliefs_2S_MT1)

tempDplyr2 <- Beliefs_2S %>% filter(Promise  == 60) %>%
  group_by(Belief) %>% summarize(count = n()) %>% 
  mutate(Belief=Belief/10, Proportion = round(count/sum(count), digits = 2),Condition = "2S 6")

prop_beliefs_MTurk <- bind_rows(tempDplyr2,tempDplyr)
prop_beliefs_MTurk <- bind_rows(prop_beliefs_MTurk,bins) %>% mutate(Experiment = "MTurk") 

ggplot(prop_beliefs_MTurk, aes(x = Belief, y=Proportion)) + 
  geom_bar(aes(fill = factor(Condition)), stat="identity", position= position_dodge(width=.9), width=.8)+
  coord_cartesian(ylim = c(0,1)) +
  theme_classic() + scale_x_continuous(breaks= c(0,1,2,3,4,5,6,7)) +
  ggtitle("Receiver Beliefs Conditional on 1S5 and 2S6 \nMTurk n1(5)= 144; n2(6)=29")+
  theme(axis.title.x=element_blank(), axis.title.y = element_blank(), 
        plot.title = element_text(hjust = 0.5))+ scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#fb9a99","#1f78b4")) + guides(fill=guide_legend(title=element_blank()))

#-> Lab First Round 1S 5
tempDplyr3 <- receiver_1S_firstRound %>% rename(Belief= beliefR, Promise = Offer) %>%
  filter(Promise  == 5) %>%
  group_by(Belief) %>% summarize(count = n()) %>% 
  mutate(Proportion = round(count/sum(count), digits = 2)) %>% 
  mutate(Condition = "1S 5")

#-> Lab First Round 2S 6
Beliefs_2S_1P <- select(receiver_2S_firstRound, belief1stA, MessageA) %>%
  rename(Belief = belief1stA, Promise = MessageA )

Beliefs_2S_1P1 <- select(receiver_2S_firstRound, belief1stB, MessageB) %>%
  rename(Belief = belief1stB, Promise = MessageB )

Beliefs_2S_1P <- bind_rows(Beliefs_2S_1P, Beliefs_2S_1P1)

tempDplyr4 <- Beliefs_2S_1P %>% group_by(Promise) %>% 
  filter(Promise  == 6) %>% 
  group_by(Belief) %>% summarize(count = n()) %>% 
  mutate(Proportion = round(count/sum(count), digits = 2)) %>% 
  mutate(Condition = "2S 6")

prop_beliefs_1P <- bind_rows(tempDplyr3,tempDplyr4)
prop_beliefs_1P <- bind_rows(prop_beliefs_1P,bins) %>% mutate(Experiment = "Lab First Round") 

prop_beliefs <- bind_rows(prop_beliefs_1P,prop_beliefs_All,prop_beliefs_MTurk)

prop_beliefs$Experiment <- factor(prop_beliefs$Experiment, levels= c("Lab First Round", "Lab All Rounds", "MTurk"))

levels(prop_beliefs$Experiment)[levels(prop_beliefs$Experiment)=="Lab First Round"]  <- "Lab First Round \n n2(5)= 27; n2(6)=20" 
levels(prop_beliefs$Experiment)[levels(prop_beliefs$Experiment)=="Lab All Rounds"]  <- "Lab All Rounds \n n2(5)= 185; n2(6)=91"
levels(prop_beliefs$Experiment)[levels(prop_beliefs$Experiment)=="MTurk"]  <- "MTurk \n n2(5)= 144; n2(6)=29"


ggplot(prop_beliefs, aes(x = Belief, y=Proportion)) + 
  geom_bar(aes(fill = Condition), stat="identity", position= position_dodge(width=.8), width=.8)+
  coord_cartesian(ylim = c(0,.8)) + facet_grid(~Experiment)+
  theme_classic()  + scale_x_continuous(breaks= c(0,2,4,6,8,10)) +
  ggtitle("Figure 6A: Receiver Beliefs Conditional on 1S Offer 5 and 2S Offer 6")+
  theme(strip.background = element_rect(colour="white"), axis.title.x = element_blank(),
          panel.spacing = unit(1.2, "lines"),
          plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) + guides(fill=guide_legend(title=element_blank()))

##### Sender Beliefs

# -> MTurk for 5 1S 

CDF5_1S_MT <- sender_1S %>% select(Belief2nd, Promise) %>% filter(Promise == 50) %>% 
  rename(Belief = Belief2nd) %>% mutate(Promise = Promise/10) %>% 
  group_by(Belief) %>% summarise(count=n())
  
CDF5_1S_MT <- CDF5_1S_MT %>% mutate(Belief = Belief/10, Proportion = count/sum(count),Condition = "1S 5")

# MTURK 6 2S

CDF6_2S_MT <- sender_2S %>% select(Belief2nd, Promise) %>% filter(Promise == 60) %>% 
  rename(Belief = Belief2nd) %>% group_by(Belief) %>% summarise(count=n()) 

CDF6_2S_MT <- CDF6_2S_MT %>%  mutate(Proportion = count/sum(count), 
                                     Belief=Belief/10,
                                     Condition = "2S 6")

MT_2nd <- bind_rows(CDF5_1S_MT,CDF6_2S_MT)

MT_2nd <- bind_rows(MT_2nd,bins) %>% mutate(Experiment = "MTurk")

# -> Lab First Round for 5 1S 
CDF5_1S_1P <- receiver_1S_firstRound %>% select(beliefS, Offer) %>% filter(Offer == 5) %>% 
  rename(Promise = Offer, Belief = beliefS)  %>% group_by(Belief) %>% summarise(count=n()) 
CDF5_1S_1P <- CDF5_1S_1P %>%  mutate(Proportion = count/sum(count), Condition = "1S 5") 

CDF5_1S_1P <- bind_rows(CDF5_1S_1P,bins) %>% mutate(Experiment = "Lab First Round")
# -> Lab First Round for 6 2S

CDF6_2S_1P <- sender_2S_firstRound %>% select(belief2nd, Promise) %>% filter(Promise == 6) %>%
  rename(Belief = belief2nd) %>% group_by(Belief) %>% summarise(count=n())
CDF6_2S_1P <- CDF6_2S_1P %>%  mutate(Proportion = count/sum(count), Condition = "2S 6")

CDF6_2S_1P <- bind_rows(CDF6_2S_1P,bins) %>% mutate(Experiment = "Lab First Round")

# -> Lab All Rounds for 5 1S 
CDF5_1S_All <- receiver_1S_allRounds %>% select(beliefS, Offer) %>% filter(Offer == 5) %>% 
  rename(Promise = Offer, Belief = beliefS) %>% group_by(Belief) %>% summarise(count=n()) 

CDF5_1S_All <- CDF5_1S_All %>% mutate(Proportion = count/sum(count), Condition = "1S 5")

CDF5_1S_All <- bind_rows(CDF5_1S_All,bins) %>% mutate(Experiment = "Lab All Rounds") 

###### Lab All Rounds for 6 2S

CDF6_2S_All <-  sender_2S_allRounds %>% select(belief2nd, Promise) %>% filter(Promise == 6) %>%
  rename(Belief = belief2nd) %>% group_by(Belief) %>% summarise(count=n())  
CDF6_2S_All <- CDF6_2S_All %>%  mutate(Proportion = count/sum(count), Condition = "2S 6")

CDF6_2S_All <- bind_rows(CDF6_2S_All,bins) %>% mutate(Experiment = "Lab All Rounds") 

beliefs2nd <- bind_rows(CDF5_1S_1P, CDF5_1S_All, MT_2nd, CDF6_2S_All,
                        CDF6_2S_1P)

beliefs2nd$Experiment <- factor(beliefs2nd$Experiment, levels= c("Lab First Round", "Lab All Rounds", "MTurk"))

levels(beliefs2nd$Experiment)[levels(beliefs2nd$Experiment)=="Lab First Round"]  <- "Lab First Round \n n1(5)= 27; n2(6)=20" 
levels(beliefs2nd$Experiment)[levels(beliefs2nd$Experiment)=="Lab All Rounds"]  <- "Lab All Rounds \n n1(5)= 185; n2(6)=154"
levels(beliefs2nd$Experiment)[levels(beliefs2nd$Experiment)=="MTurk"]  <- "MTurk \n n1(5)= 149; n2(6)=36"


#GRAPHS
# -> PROPORTIONAL 

ggplot(beliefs2nd, aes(x = Belief, y=Proportion)) + 
  geom_bar(aes(fill = Condition), stat="identity", position= position_dodge(width=.8), width=.8)+
  coord_cartesian(ylim = c(0,.8)) + facet_grid(~Experiment)+
  theme_classic() + scale_x_continuous(breaks= c(0,2,4,6,8,10)) +
  ggtitle("Figure 6B: Sender Beliefs Conditional on 1S Offer 5 and 2S Offer 6")+
  theme(strip.background = element_rect(colour="white"), 
        panel.spacing = unit(1.2, "lines"),
        plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) + guides(fill=guide_legend(title=element_blank()))


########### 2S5 vs 2S6 ######

#-> Lab All Round 2S
Beliefs_2S_1P <- select(receiver_2S_allRounds, belief1stA, MessageA) %>%
  rename(Belief = belief1stA, Promise = MessageA )

Beliefs_2S_1P1 <- select(receiver_2S_firstRound, belief1stB, MessageB) %>%
  rename(Belief = belief1stB, Promise = MessageB )

Beliefs_2S_1P <- bind_rows(Beliefs_2S_1P, Beliefs_2S_1P1)

tempDplyr6 <- Beliefs_2S_1P %>% group_by(Promise) %>% 
  filter(Promise == 6) %>% 
  group_by(Belief) %>% summarize(count = n()) %>% 
  mutate(Proportion = round(count/sum(count), digits = 2))
  
tempDplyr6 <- bind_rows(tempDplyr6, bins) %>%  mutate(Condition = "Offer 6")

tempDplyr5 <- Beliefs_2S_1P %>% group_by(Promise) %>% 
  filter(Promise  == 5) %>% 
  group_by(Belief) %>% summarize(count = n()) %>% 
  mutate(Proportion = round(count/sum(count), digits = 2))
  
tempDplyr5 <- bind_rows(tempDplyr5, bins) %>% mutate(Condition = "Offer 5")

prop_beliefs_All <- bind_rows(tempDplyr5,tempDplyr6) %>% mutate(Experiment = "Lab All Rounds") 

ggplot(prop_beliefs_All, aes(x = Belief, y=Proportion)) + 
  geom_bar(aes(fill = factor(Condition)), stat="identity", position= position_dodge(width=.9), width=.8)+
  coord_cartesian(ylim = c(0,1)) +
  theme_classic() + scale_x_continuous(breaks= c(0,1,2,3,4,5,6,7)) +
  ggtitle("Receiver Beliefs Conditional on 1S5 and 2S6 \nLab All Rounds n1(5)= 185; n2(6)=91")+
  theme(axis.title.x=element_blank(), axis.title.y = element_blank(), 
        plot.title = element_text(hjust = 0.5))+ scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#fb8072","#1f78b4")) + guides(fill=guide_legend(title=element_blank()))



#-> Mturk Data 2S
Beliefs_2S_MT <- select(receiver_2S, Belief1stA, randPromiseValueA) %>%
  rename(Belief = Belief1stA, Promise = randPromiseValueA )

Beliefs_2S_MT1 <- select(receiver_2S, Belief1stB, randPromiseValueB) %>%
  rename(Belief = Belief1stB, Promise = randPromiseValueB )

Beliefs_2S <- bind_rows(Beliefs_2S_MT, Beliefs_2S_MT1)

tempDplyr2 <- Beliefs_2S %>% filter(Promise == 60) %>%
  group_by(Belief) %>% summarize(count = n()) %>% 
  mutate(Belief=Belief/10, Proportion = round(count/sum(count), digits = 2)) 

tempDplyr2 <- bind_rows(tempDplyr2, bins) %>% mutate(Condition = "Offer 6")

tempDplyr <- Beliefs_2S %>% filter(Promise == 50) %>%
  group_by(Belief) %>% summarize(count = n()) %>% 
  mutate(Belief=Belief/10, Proportion = round(count/sum(count), digits = 2)) 

tempDplyr <- bind_rows(tempDplyr, bins) %>%  mutate(Condition = "Offer 5")

prop_beliefs_MTurk <- bind_rows(tempDplyr2,tempDplyr) %>% mutate(Experiment = "MTurk") 

ggplot(prop_beliefs_MTurk, aes(x = Belief, y=Proportion)) + 
  geom_bar(aes(fill = factor(Condition)), stat="identity", position= position_dodge(width=.9), width=.8)+
  coord_cartesian(ylim = c(0,1)) +
  theme_classic() + scale_x_continuous(breaks= c(0,1,2,3,4,5,6,7)) +
  ggtitle("Receiver Beliefs Conditional on 1S5 and 2S6 \nMTurk n1(5)= 144; n2(6)=29")+
  theme(axis.title.x=element_blank(), axis.title.y = element_blank(), 
        plot.title = element_text(hjust = 0.5))+ scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c("#fb8072","#1f78b4")) + guides(fill=guide_legend(title=element_blank()))

#-> Lab First Round 2S
Beliefs_2S_1P <- select(receiver_2S_firstRound, belief1stA, MessageA) %>%
  rename(Belief = belief1stA, Promise = MessageA )

Beliefs_2S_1P1 <- select(receiver_2S_firstRound, belief1stB, MessageB) %>%
  rename(Belief = belief1stB, Promise = MessageB )

Beliefs_2S_1P <- bind_rows(Beliefs_2S_1P, Beliefs_2S_1P1)

tempDplyr4 <- Beliefs_2S_1P %>% group_by(Promise) %>% 
  filter(Promise  == 6) %>% 
  group_by(Belief) %>% summarize(count = n()) %>% 
  mutate(Proportion = round(count/sum(count), digits = 2)) 

tempDplyr4 <- bind_rows(tempDplyr4, bins) %>%  mutate(Condition = "Offer 6")

tempDplyr3 <- Beliefs_2S_1P %>% group_by(Promise) %>% 
  filter(Promise == 5) %>% 
  group_by(Belief) %>% summarize(count = n()) %>% 
  mutate(Proportion = round(count/sum(count), digits = 2))

tempDplyr3 <- bind_rows(tempDplyr3, bins) %>%  mutate(Condition = "Offer 5")

prop_beliefs_1P <- bind_rows(tempDplyr4,tempDplyr3) %>% mutate(Experiment = "Lab First Round") 

prop_beliefs <- bind_rows(prop_beliefs_1P,prop_beliefs_All,prop_beliefs_MTurk)

prop_beliefs$Experiment <- factor(prop_beliefs$Experiment, levels= c("Lab First Round", "Lab All Rounds", "MTurk"))

levels(prop_beliefs$Experiment)[levels(prop_beliefs$Experiment)=="Lab First Round"]  <- "Lab First Round \n n2(5)= 27; n2(6)=20" 
levels(prop_beliefs$Experiment)[levels(prop_beliefs$Experiment)=="Lab All Rounds"]  <- "Lab All Rounds \n n2(5)=28 ; n2(6)=91"
levels(prop_beliefs$Experiment)[levels(prop_beliefs$Experiment)=="MTurk"]  <- "MTurk \n n2(5)= 272; n2(6)=29"


ggplot(prop_beliefs, aes(x = Belief, y=Proportion)) + 
  geom_bar(aes(fill = Condition), stat="identity", position= position_dodge(width=.9), width=.8)+
  coord_cartesian(ylim = c(0,1)) + facet_grid(~Experiment)+
  theme_classic()  + scale_x_continuous(breaks= c(0,2,4,6,8,10)) +
  ggtitle("Receiver Beliefs for Two Senders Conditional on Offer 5 and 6")+
  theme(strip.background = element_rect(colour="white"), axis.title.x = element_blank(),
        panel.spacing = unit(1.2, "lines"),
        plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) + guides(fill=guide_legend(title=element_blank()))

##### Sender Beliefs



# MTURK 6 2S

CDF6_2S_MT <- sender_2S %>% select(Belief2nd, Promise) %>% filter(Promise == 60) %>% 
  rename(Belief = Belief2nd) %>% group_by(Belief) %>% summarise(count=n()) 

CDF6_2S_MT <- CDF6_2S_MT %>%  mutate(Proportion = count/sum(count), 
                                     Belief=Belief/10)

CDF6_2S_MT <- bind_rows(CDF6_2S_MT,bins) %>% mutate(Experiment = "MTurk", Condition = "Offer 6")

CDF5_2S_MT <- sender_2S %>% select(Belief2nd, Promise) %>% filter(Promise == 50) %>% 
  rename(Belief = Belief2nd) %>% group_by(Belief) %>% summarise(count=n()) 

CDF5_2S_MT <- CDF5_2S_MT %>%  mutate(Proportion = count/sum(count), 
                                     Belief=Belief/10)

CDF5_2S_MT <- bind_rows(CDF5_2S_MT,bins) %>% mutate(Experiment = "MTurk", Condition = "Offer 5")

MT_2nd <- bind_rows(CDF5_2S_MT,CDF6_2S_MT)

# -> Lab First Round for 6 2S

CDF6_2S_1P <- sender_2S_firstRound %>% select(belief2nd, Promise) %>% filter(Promise == 6) %>%
  rename(Belief = belief2nd) %>% group_by(Belief) %>% summarise(count=n())
CDF6_2S_1P <- CDF6_2S_1P %>%  mutate(Proportion = count/sum(count))

CDF6_2S_1P <- bind_rows(CDF6_2S_1P,bins) %>% mutate(Experiment = "Lab First Round", 
                                                    Condition = "Offer 6")

CDF5_2S_1P <- sender_2S_firstRound %>% select(belief2nd, Promise) %>% filter(Promise == 5) %>%
  rename(Belief = belief2nd) %>% group_by(Belief) %>% summarise(count=n())
CDF5_2S_1P <- CDF5_2S_1P %>%  mutate(Proportion = count/sum(count))

CDF5_2S_1P <- bind_rows(CDF5_2S_1P,bins) %>% mutate(Experiment = "Lab First Round", 
                                                    Condition = "Offer 5")
FIRSTPERIOD_2nd <- bind_rows(CDF5_2S_1P, CDF6_2S_1P)

###### Lab All Rounds for 6 2S

CDF6_2S_All <-  sender_2S_allRounds %>% select(belief2nd, Promise) %>% filter(Promise == 6) %>%
  rename(Belief = belief2nd) %>% group_by(Belief) %>% summarise(count=n())  
CDF6_2S_All <- CDF6_2S_All %>%  mutate(Proportion = count/sum(count))

CDF6_2S_All <- bind_rows(CDF6_2S_All,bins) %>% mutate(Experiment = "Lab All Rounds",
                                                      Condition = "Offer 6") 


CDF5_2S_All <-  sender_2S_allRounds %>% select(belief2nd, Promise) %>% filter(Promise == 5) %>%
  rename(Belief = belief2nd) %>% group_by(Belief) %>% summarise(count=n())  
CDF5_2S_All <- CDF5_2S_All %>%  mutate(Proportion = count/sum(count))

CDF5_2S_All <- bind_rows(CDF5_2S_All,bins) %>% mutate(Experiment = "Lab All Rounds", 
                                                      Condition = "Offer 5")
LABALL_2nd <- bind_rows(CDF6_2S_All, CDF5_2S_All)



beliefs2nd <- bind_rows(LABALL_2nd,FIRSTPERIOD_2nd,MT_2nd)



beliefs2nd$Experiment <- factor(beliefs2nd$Experiment, levels= c("Lab First Round", "Lab All Rounds", "MTurk"))

levels(beliefs2nd$Experiment)[levels(beliefs2nd$Experiment)=="Lab First Round"]  <- "Lab First Round \n n1(5)= 28; n2(6)=20" 
levels(beliefs2nd$Experiment)[levels(beliefs2nd$Experiment)=="Lab All Rounds"]  <- "Lab All Rounds \n n1(5)= 172; n2(6)=154"
levels(beliefs2nd$Experiment)[levels(beliefs2nd$Experiment)=="MTurk"]  <- "MTurk \n n1(5)= 262; n2(6)=36"

ggplot(beliefs2nd, aes(x = Belief, y=Proportion)) + 
  geom_bar(aes(fill = Condition), stat="identity", position= position_dodge(width=.9), width=.8)+
  coord_cartesian(ylim = c(0,.8)) + facet_grid(~Experiment)+
  theme_classic() + scale_x_continuous(breaks= c(0,2,4,6,8,10)) +
  ggtitle("Sender Beliefs For Two Senders Conditional Offer 5 and 6")+
  theme(strip.background = element_rect(colour="white"), 
        panel.spacing = unit(1.2, "lines"),
        plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = c("#fb9a99", "#1f78b4")) + guides(fill=guide_legend(title=element_blank()))

