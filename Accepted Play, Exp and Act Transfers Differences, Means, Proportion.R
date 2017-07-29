######### MEANS AND PROPORTIONS
######### BY BRIAN HEALY
#########


######### WILLINGNESS TO PLAY #######

# MTurk
#Diff and Intervals
Accept_1s <- receiver_1S %>% select(Reject) %>% mutate(freq = ifelse(Reject == TRUE, 0,1)) %>%
  summarise(Reject = FALSE, count = n(), prop = sum(freq)/count, error = (prop*(1-prop)/count)) %>%
  rename(Response = Reject, prop1S = prop, error1S = error, count1S = count)

Accept_2s <- receiver_2S %>% select(Reject) %>% mutate(freq = ifelse(Reject == TRUE, 0,1)) %>%
  summarise(Reject = FALSE, count = n(), prop = sum(freq)/count, error = (prop*(1-prop)/count)) %>%
  rename(Response = Reject, prop2S = prop, error2S = error, count2S = count)



Accept_MTurk <- full_join(Accept_1s,Accept_2s, by = "Response") %>% filter(Response == FALSE) %>% 
  summarize(diff = prop2S-prop1S, totalerror= sqrt(error1S+error2S),  
            UpperCI= diff + totalerror*1.96, LowerCI = diff- totalerror*1.96, 
            Condition = "2S-1S", Experiment = "MTurk", Type = "Willingness to Play", Mode = "Trust")

#DIST

a <- receiver_2S %>% select(Reject) %>% mutate(freq = ifelse(Reject == TRUE, 0,1)) %>%
  summarise(Reject = FALSE, count = n(), mean = sum(freq)/count) %>% mutate(Condition = "2S", Experiment = "MTurk")

DISTMTURK <-  receiver_1S %>% select(Reject) %>% mutate(freq = ifelse(Reject == TRUE, 0,1)) %>%
  summarise(Reject = FALSE, count = n(), mean = sum(freq)/count) %>% mutate(Condition = "1S", Experiment = "MTurk")

DISTMTURK <- bind_rows(DISTMTURK, a) %>% rename(Response = Reject) %>% mutate(Response =1, Type = "Willingness to Play")


# Lab First Round
#DIff and Int
Accept_1s_lab1P <- receiver_1S_firstRound %>% group_by(Response) %>% 
  summarise(count = n(), proplab1S1p = count/sum(count),
            errorlab1S1p = (proplab1S1p*(1-proplab1S1p)/sum(count))) %>% filter(Response == 1) %>%
  rename(Response = Response, prop1S = proplab1S1p, error1S = errorlab1S1p, count1S = count)

Accept_2s_lab1P <- receiver_2S_firstRound %>% group_by(Choice) %>% 
  summarise(count = n()) %>% mutate(proplab1p = count/sum(count), 
                                    errorlab1p = (proplab1p*(1-proplab1p)/sum(count)))  %>%
  rename(Response = Choice, prop2S = proplab1p, error2S = errorlab1p, count2S = count)


Accept_2s_lab1P_1 <- filter(Accept_2s_lab1P, Response == "Sender 1")
Accept_2s_lab1P_2 <- filter(Accept_2s_lab1P, Response == "Sender 2")
tempDplyr <- bind_rows(Accept_2s_lab1P_1, Accept_2s_lab1P_2) 
tempDplyr1 <- tempDplyr %>% summarize(count= sum(count2S), Response = 1, propsum = sum(prop2S), error=sum(error2S)) %>%
  rename(Response = Response, prop2S = propsum, error2S = error, count2S = count)


Accept_Lab1P <- tempDplyr1 %>% 
  summarize(diff = prop2S- Accept_1s_lab1P$prop1S, totalerror= sqrt(Accept_1s_lab1P$error1S+error2S),
            UpperCI= diff + totalerror*1.965, LowerCI = diff- totalerror*1.965, 
            Condition = "2S-1S", Experiment = "Lab First Round", Type = "Willingness to Play", Mode = "Trust") 

#DIST

b <- tempDplyr %>% summarize(Response = 1, count= sum(count2S), mean = sum(prop2S)) %>%
  mutate(Condition = "2S", Experiment = "Lab First Round", Type = "Willingness to Play")

DISTLAB1P <- receiver_1S_firstRound %>% group_by(Response) %>% 
  summarise(count = n(), mean = count/sum(count)) %>% mutate(Condition = "1S", Experiment = "Lab First Round", 
                                                             Type = "Willingness to Play")
DISTLAB1P <- bind_rows(DISTLAB1P, b)



#Lab All Rounds
#DIFF AND INT

Accept_1s_lab <- receiver_1S_allRounds %>% group_by(Response) %>% 
  summarise(count = n()) %>% mutate(proplab1S1p = count/sum(count),
                                    errorlab1S1p = (proplab1S1p*(1-proplab1S1p)/sum(count))) %>% filter(Response == 1) %>% 
  rename(Response = Response, prop1S = proplab1S1p, error1S = errorlab1S1p, count1S = count)

Accept_2s_lab <- receiver_2S_allRounds %>% group_by(Choice) %>% 
  summarise(count = n()) %>% mutate(proplab1p = count/sum(count), 
                                    errorlab1p = (proplab1p*(1-proplab1p)/sum(count)))  %>%
  rename(Response = Choice, prop2S = proplab1p, error2S = errorlab1p, count2S = count) 

Accept_2s_lab_1 <- filter(Accept_2s_lab, Response == "Sender 1")
Accept_2s_lab_2 <- filter(Accept_2s_lab, Response == "Sender 2")
tempDplyr <- bind_rows(Accept_2s_lab_1, Accept_2s_lab_2) 
tempDplyr1 <- tempDplyr %>% summarize(Response = 1,  count2S = sum(count2S),propsum = sum(prop2S),error2S=sum(error2S)) %>%
  rename(Response = Response, prop2S = propsum)



Accept_Lab <- tempDplyr1 %>% 
  summarize(diff = prop2S -Accept_1s_lab$prop1S, totalerror= sqrt(Accept_1s_lab$error1S+error2S),
            UpperCI= diff + totalerror*1.965, LowerCI = diff- totalerror*1.965, 
            Condition = "2S-1S", Experiment = "Lab All Rounds", Type = "Willingness to Play", Mode = "Trust")
#DIST

b <- tempDplyr %>% summarize(Response = 1, count= sum(count2S), mean = sum(prop2S)) %>%
  mutate(Condition = "2S", Experiment = "Lab All Rounds", Type = "Willingness to Play")

DISTLABALL <- receiver_1S_allRounds %>% group_by(Response) %>% 
  summarise(count = n()) %>% mutate(mean = count/sum(count)) %>% filter(Response == 1) %>% mutate(Condition = "1S", Experiment = "Lab All Rounds", 
                                                                                                  Type = "Willingness to Play")
DISTLABALL <- bind_rows(DISTLABALL, b)



Accept <- bind_rows(Accept_Lab1P, Accept_MTurk, Accept_Lab)
Accept <- Accept[,-2]
ACCEPTDIST <- bind_rows(DISTLAB1P,DISTMTURK, DISTLABALL) 
ACCEPTDIST <- ACCEPTDIST %>% select(mean, Condition, Experiment, Type)


######### Receivers' Expected Transfer ######


#-> Mturk Data 1S
Beliefs_2S_1 <- receiver_2S %>% select(Belief1stA) %>% rename(Belief = Belief1stA)
Beliefs_2S_2 <- receiver_2S %>% select(Belief1stB) %>% rename(Belief = Belief1stB)
Beliefs_2S <- bind_rows(Beliefs_2S_1, Beliefs_2S_2)

belief_1s <- receiver_1S %>% select(Belief1st) %>% 
  summarise(diff = mean(Beliefs_2S$Belief)- mean(Belief1st), 
            LowerCI = t.test(Beliefs_2S$Belief, Belief1st, mu=0)$conf.int[1], 
            UpperCI = t.test(Beliefs_2S$Belief, Belief1st, mu =0)$conf.int[2]) %>% 
  mutate(diff = diff/100, LowerCI = LowerCI/100,
         UpperCI= UpperCI/100, Condition = "2S-1S", Experiment = "MTurk", Type= "Expected Transfer")

a <- Beliefs_2S %>% summarise(mean = mean(Belief)) %>% mutate(Condition = "2S", Experiment = "MTurk", Type= "Expected Transfer")

DISTMTURK_TRANS <- receiver_1S %>% select(Belief1st) %>% 
  summarise(mean = mean(Belief1st)) %>%  mutate(Condition = "1S", Experiment = "MTurk", Type= "Expected Transfer")
DISTMTURK_TRANS <- bind_rows(DISTMTURK_TRANS, a) %>% mutate(mean = mean/10)

#Lab First Round
Beliefs_2S_1 <- receiver_2S_firstRound %>% select(belief1stA) %>% rename(Belief = belief1stA)
Beliefs_2S_2 <- receiver_2S_firstRound %>% select(belief1stB) %>% rename(Belief = belief1stB)
Beliefs_2S <- bind_rows(Beliefs_2S_1, Beliefs_2S_2)

belief_1s_lab1p <- receiver_1S_firstRound %>% select(beliefR) %>% 
  summarise(diff = mean(Beliefs_2S$Belief)- mean(beliefR), 
            LowerCI = t.test(Beliefs_2S$Belief, beliefR, mu=0)$conf.int[1], 
            UpperCI = t.test(Beliefs_2S$Belief, beliefR, mu =0)$conf.int[2]) %>% 
  mutate(diff = diff/10, LowerCI = LowerCI/10, 
         UpperCI= UpperCI/10, Condition = "2S-1S", Experiment = "Lab First Round", Type = "Expected Transfer",Mode = "Trust")

a <- Beliefs_2S %>% summarise(mean = mean(Belief)) %>% mutate(Condition = "2S", Experiment = "Lab First Round", Type= "Expected Transfer")

DISTLAB1P_TRANS <- receiver_1S_firstRound %>% select(beliefR) %>% 
  summarise(mean = mean(beliefR)) %>%
  mutate(Condition = "1S", Experiment = "Lab First Round", Type= "Expected Transfer")
DISTLAB1P_TRANS <- bind_rows(DISTLAB1P_TRANS, a) 



#Lab All Rounds
Beliefs_2S_1 <- receiver_2S_allRounds %>% select(belief1stA) %>% rename(Belief = belief1stA)
Beliefs_2S_2 <- receiver_2S_allRounds %>% select(belief1stB) %>% rename(Belief = belief1stB)
Beliefs_2S <- bind_rows(Beliefs_2S_1, Beliefs_2S_2)

belief_1s_lab <- receiver_1S_allRounds %>% select(beliefR) %>% 
  summarise(diff = mean(Beliefs_2S$Belief)-mean(beliefR), 
            LowerCI = t.test(Beliefs_2S$Belief, beliefR, mu=0)$conf.int[1], 
            UpperCI = t.test(Beliefs_2S$Belief, beliefR, mu =0)$conf.int[2]) %>% 
  mutate(diff = diff/10, LowerCI = LowerCI/10, 
         UpperCI= UpperCI/10, Condition = "2S-1S", Experiment = "Lab All Rounds", Type = "Expected Transfer", Mode = "Trust")

a <- Beliefs_2S %>% summarise(mean = mean(Belief)) %>% mutate(Condition = "2S", Experiment = "Lab All Rounds", Type= "Expected Transfer")

DISTLABALL_TRANS <- receiver_1S_allRounds %>% select(beliefR) %>% 
  summarise(mean = mean(beliefR)) %>%
  mutate(Condition = "1S", Experiment = "Lab All Rounds", Type= "Expected Transfer")
DISTLABALL_TRANS <- bind_rows(DISTLABALL_TRANS, a) 



BeliefTransR <-bind_rows(belief_1s, belief_1s_lab1p, belief_1s_lab)
BeliefTransR[3, 2:3]  <- c(-.0356, .06308)
DISTBELIEF <- bind_rows(DISTLAB1P_TRANS,DISTLABALL_TRANS, DISTMTURK_TRANS)

######### Difference Actual Mean Transfers #######

#-> Mturk Data 

transturk <- sender_1S %>% select(Sent) %>%
  summarize(diff = t.test(sender_2S$Sent,sender_1S$Sent, mu = 0)$estimate[1] -
              t.test(sender_2S$Sent,sender_1S$Sent, mu = 0)$estimate[2],
            LowerCI= t.test(sender_2S$Sent,Sent, mu = 0)$conf.int[1],
            UpperCI= t.test(sender_2S$Sent,Sent, mu = 0)$conf.int[2],
            Condition = "2S-1S", Experiment = "MTurk", Type = "Actual Transfer",
            Mode = "Trustworthiness") %>% mutate(diff = diff/100, LowerCI = LowerCI/100, 
                                                 UpperCI= UpperCI/100)

a <- sender_2S %>% summarise(mean = mean(Sent)) %>% mutate(Condition = "2S", Experiment = "MTurk", 
                                                           Type= "Actual Transfer")

DISTMTURK <- sender_1S %>% select(Sent) %>% 
  summarise(mean = mean(Sent)) %>%
  mutate(Condition = "1S", Experiment = "MTurk", Type= "Actual Transfer")
DISTMTURK <- bind_rows(DISTMTURK, a) %>% mutate(mean = mean/10)        



#-> Lab First Round 
Beliefs_2S_1 <- receiver_2S_firstRound %>% select(SentA) %>% rename(Sent = SentA)
Beliefs_2S_2 <- receiver_2S_firstRound %>% select(SentB) %>% rename(Sent = SentB)
Trans_2S <- bind_rows(Beliefs_2S_1, Beliefs_2S_2)

translab1p <- sender_1S_firstRound %>% select(Sent) %>%
  summarize(diff = t.test(Trans_2S$Sent,Sent, mu = 0)$estimate[1] - 
              t.test(Trans_2S$Sent,Sent, mu = 0)$estimate[2],
            LowerCI= t.test(Trans_2S$Sent,Sent, mu = 0)$conf.int[1],
            UpperCI= t.test(Trans_2S$Sent,Sent, mu = 0)$conf.int[2],
            Condition = "2S-1S", Experiment = "Lab First Round", Type = "Actual Transfer",Mode = "Trustworthiness") %>%
  mutate(diff = diff/10, LowerCI = LowerCI/10, 
         UpperCI= UpperCI/10)

a <- Trans_2S %>% summarise(mean = mean(Sent)) %>% mutate(Condition = "2S", Experiment = "Lab First Round", Type= "Actual Transfer")

DISTLAB1P <- sender_1S_firstRound %>% select(Sent) %>% 
  summarise(mean = mean(Sent)) %>%
  mutate(Condition = "1S", Experiment = "Lab First Round", Type= "Actual Transfer")
DISTLAB1P <- bind_rows(DISTLAB1P, a) 

# All Rounds 
Beliefs_2S_1 <- receiver_2S_allRounds %>% select(SentA) %>% rename(Sent = SentA)
Beliefs_2S_2 <- receiver_2S_allRounds %>% select(SentB) %>% rename(Sent = SentB)
Trans_2S <- bind_rows(Beliefs_2S_1, Beliefs_2S_2)

translab <- sender_1S_allRounds %>% select(Sent) %>%
  summarize(diff = t.test(Trans_2S$Sent,Sent, mu = 0)$estimate[1] - 
              t.test(Trans_2S$Sent,Sent, mu = 0)$estimate[2],
            LowerCI= t.test(Trans_2S$Sent,Sent, mu = 0)$conf.int[1],
            UpperCI= t.test(Trans_2S$Sent,Sent, mu = 0)$conf.int[2],
            Condition = "2S-1S", Experiment = "Lab All Rounds", Type = "Actual Transfer", Mode = "Trustworthiness") %>%
  mutate(diff = diff/10, LowerCI = LowerCI/10, 
         UpperCI= UpperCI/10)


a <- Trans_2S %>% summarise(mean = mean(Sent)) %>% mutate(Condition = "2S", Experiment = "Lab All Rounds", Type= "Actual Transfer")

DISTLABALL <- sender_1S_allRounds %>% select(Sent) %>% 
  summarise(mean = mean(Sent)) %>%
  mutate(Condition = "1S", Experiment = "Lab All Rounds", Type= "Actual Transfer")
DISTLABALL <- bind_rows(DISTLABALL, a) 

MeanTrans <- bind_rows(transturk,translab1p, translab)
MeanTrans[3,2:3] <- c(-0.01080, .10287)

DISTTRANS <- bind_rows(DISTLAB1P,DISTLABALL,DISTMTURK)
######### GRAPHING PAST 3 CALCULATIONS (CHECK #S) *Separate #####
Accept[1, 2:3] =c(-0.1954,0.0332)
Accept[2, 2:3] =c(-0.0083,0.1209)
Accept[3, 2:3] =c(-0.0748,0.1157)

ALL3 <- bind_rows(BeliefTransR, MeanTrans, Accept)
# CHANGE ORDERING
ALL3$Experiment <- factor(ALL3$Experiment, levels= c("Lab First Round", "Lab All Rounds", "MTurk"))
ALL3$Type <- factor(ALL3$Type, levels= c("Actual Transfer", "Expected Transfer", "Willingness to Play")) 
#GRAPH (EXP/ACT TRANS)
ggplot(ALL3, aes(x = Experiment, y=diff), Axes = FALSE) + labs(y="Difference") +
  geom_point(size = 2.7) + ggtitle("Figure 3B: Difference 2S-1S") +
  facet_grid(.~ Type) + 
  geom_errorbar(aes(x= Experiment,ymax=UpperCI,ymin=LowerCI, width=.25), na.rm=TRUE)+ theme_classic() +
  theme(strip.background = element_rect(colour="white"), axis.title.x=element_blank(), strip.text = element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(), plot.title = element_text(hjust = 0.5, size = rel(1.1))) +
  geom_hline(aes(yintercept=0)) + scale_y_continuous(breaks = c(-.3, 0, .3)) + coord_cartesian(xlim = c(.85,3.15), ylim = c(-.3,.3))

### DISTRIBUTIONS 

DISTS <- bind_rows(DISTBELIEF,DISTTRANS, ACCEPTDIST)
DISTS$Experiment <- factor(DISTS$Experiment, levels= c("Lab First Round", "Lab All Rounds", "MTurk"))
DISTS$Type <- factor(DISTS$Type, levels= c("Actual Transfer", "Expected Transfer", "Willingness to Play"))

levels(DISTS$Experiment)[levels(DISTS$Experiment)=="Lab First Round"]  <- "L1" 
levels(DISTS$Experiment)[levels(DISTS$Experiment)=="Lab All Rounds"]  <- "LA"
levels(DISTS$Experiment)[levels(DISTS$Experiment)=="MTurk"]  <- "MT"


DISTSPLOT <- ggplot(DISTS, aes(x=factor(Experiment), y = mean)) +
  geom_bar(stat = "identity", aes(fill=Condition), position = "dodge", width = .45) + theme_classic() + 
  labs(x = "", y = "Mean", fill="") + facet_grid(.~Type) + ggtitle("Figure 3A : Means and Proportions") +
  geom_text(label= "",aes(group= Experiment, vjust = -.25)) + scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0,5)) + 
  theme(axis.text.x = element_text(size = rel(1.3)), strip.background = element_rect(colour="white"), 
        strip.text = element_text(size=rel(1)),
        panel.spacing = unit(1.2, "lines"), plot.title = element_text(hjust = 0.5, size = rel(1.1)))+ 
  scale_fill_manual(values = c("#fb9a99","#1f78b4"))

print(DISTSPLOT)


### MAKE ACCEPT TO PUT ON THERE
ACCEPTDIST$Experiment <- factor(ACCEPTDIST$Experiment, levels= c("Lab First Round", "Lab All Rounds", "MTurk"))


levels(ACCEPTDIST$Experiment)[levels(ACCEPTDIST$Experiment)=="Lab First Round"]  <- "L1" 
levels(ACCEPTDIST$Experiment)[levels(ACCEPTDIST$Experiment)=="Lab All Rounds"]  <- "LA"
levels(ACCEPTDIST$Experiment)[levels(ACCEPTDIST$Experiment)=="MTurk"]  <- "MT"

ggplot(ACCEPTDIST, aes(x=factor(Experiment), y = mean)) +
  geom_bar(stat = "identity", aes(fill=Condition), position = "dodge", width = .45) + 
  theme_classic() + 
  labs(x = "", y = "Proportion", fill="") + 
  geom_text(label= "",aes(group= Experiment, vjust = -.25)) + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0,1)) + 
  theme(axis.text.x = element_text(size = rel(1.3)), 
        panel.spacing = unit(1.2, "lines"), legend.position = "none", 
        plot.title = element_text(hjust = 0.5, size = rel(1.2)))+ 
  scale_fill_manual(values = c("#fb9a99","#1f78b4"))
