######### MEAN BELIEFS AND CONFIDENCE INTERVALS
######### BY Brian Healy
######### LAST MODIFIED 6.21.17

#Sender Belief CONF INT


#-> Mturk Data 1S
tempDplyr <- sender_1S %>% group_by(Promise) %>% filter(Promise >= 50, Promise <= 60) %>% 
  summarise(MeanBelief = mean(Belief2nd), LowerCI = (t.test(Belief2nd)$conf.int[1])/10, UpperCI = (t.test(Belief2nd)$conf.int[2])/10) %>% 
  mutate(Promise = Promise/10, MeanBelief = MeanBelief/10, Condition = "1S", Experiment = "MTurk")

#-> Mturk Data 2S
tempDplyr2 <- sender_2S %>% group_by(Promise) %>% filter(Promise == 60) %>% 
  summarise(MeanBelief = mean(Belief2nd), LowerCI = (t.test(Belief2nd)$conf.int[1])/10,
            UpperCI = (t.test(Belief2nd)$conf.int[2])/10) %>% 
  mutate(Promise = Promise/10, MeanBelief = MeanBelief/10, Condition = "2S", Experiment = "MTurk")

#-> Lab First Round 1S
tempDplyr3 <- sender_1S_firstRound %>% group_by(Promise) %>% filter(Promise >= 5, Promise <= 6) %>% 
  summarise(MeanBelief = mean(belief2nd), LowerCI = (t.test(belief2nd)$conf.int[1]), 
            UpperCI = (t.test(belief2nd)$conf.int[2])) %>% 
  mutate(Promise, Condition = "1S", Experiment = "Lab First Round")

#-> Lab First Round 2S
tempDplyr4 <- sender_2S_firstRound %>% group_by(Promise) %>% filter(Promise == 6) %>% 
  summarise(MeanBelief = mean(belief2nd), LowerCI = (t.test(belief2nd)$conf.int[1]),
            UpperCI = (t.test(belief2nd)$conf.int[2])) %>% 
  mutate(Promise, Condition = "2S", Experiment = "Lab First Round")

# Lab ALL ROUNDS
tempDplyr5 <- sender_1S_allRounds %>% group_by(Promise) %>% filter(Promise >= 5, Promise <= 6) %>% 
  summarise(MeanBelief = mean(belief2nd), LowerCI = (t.test(belief2nd)$conf.int[1]), 
            UpperCI = (t.test(belief2nd)$conf.int[2])) %>% 
  mutate(Promise, Condition = "1S", Experiment = "Lab All Rounds")

#-> Lab ALL Round 2S
tempDplyr6 <- sender_2S_allRounds %>% group_by(Promise) %>% filter(Promise == 6) %>% 
  summarise(MeanBelief = mean(belief2nd), LowerCI = (t.test(belief2nd)$conf.int[1]), 
            UpperCI = (t.test(belief2nd)$conf.int[2])) %>% 
  mutate(Promise, Condition = "2S", Experiment = "Lab All Rounds")


Sbelief_by_offer <- bind_rows(tempDplyr,tempDplyr2,tempDplyr3,tempDplyr4, tempDplyr5, tempDplyr6) %>% 
  mutate(Promise1 = Promise)

Sbelief_by_offer$Experiment <- factor(Sbelief_by_offer$Experiment, 
                                      levels= c("Lab First Round", "Lab All Rounds", "MTurk"))

Sbelief_by_offer[3,1] = 7
Sbelief_by_offer[6,1] = 7
Sbelief_by_offer[9,1] = 7

Sbelief_by_offer[1,3:4] <- c(4.26846,	4.65772) #Bootstrap for mturk 1S5
Sbelief_by_offer[2,2:4] <- c(4.12500,	2.25000, 5.37500) #Bootstrap for 1S6 MTURK
Sbelief_by_offer[3,2:4] <- c(5.27778,	4.69444, 5.75000) #Bootstrap for MTurk 2S6

Sbelief_by_offer[4,2:4] <- c(4.0000,	3.4815,	4.3704) #Bootstrap 1S5 Lab First Round
Sbelief_by_offer[5,3:4] <- c(1.5000,	6.0000) #Bootstrap Correction for 1S6 Lab First Round
Sbelief_by_offer[6,2:4] <- c(4.6000,	4.0500,	4.9000) #Bootstrap for 2S6 Lab First Round

Sbelief_by_offer[7,3:4] <- c(3.4550,	4.1471) ##Bootstrap 1S5 ALL ROUNDS
Sbelief_by_offer[8,2:4] <- c(3.7500,	2.8710,	4.7895) #Bootstrap 1S6 ALL ROUNDS
Sbelief_by_offer[9,2:4] <- c(4.2403,	3.8882,	4.4839) #Bootstrap 2S6 ALL ROUNDS

plot1 <- ggplot(Sbelief_by_offer, aes(x = Promise, y=MeanBelief)) +  
  geom_point(aes(colour = Condition, shape = factor(Promise1)), size = 2.7)+ 
  facet_grid(~Experiment)+ coord_cartesian(ylim=c(0,6), xlim = c(4,8)) +
  geom_errorbar(aes(x=Promise,ymax=UpperCI,ymin=LowerCI, width=.5),na.rm=TRUE)+ theme_classic() +
  theme(strip.background = element_rect(colour="white"), 
        plot.title = element_text(hjust = 0.5)) +
  scale_shape_manual(values = c(16, 15)) + labs(x= "", y="Mean", colour = "", shape="") +
  scale_x_continuous(breaks = c(5,6,7), labels=c("1S 5", "1S 6", "2S 6")) +
  scale_color_manual(values = c("#fb9a99","#1f78b4")) + ggtitle("Figure SI.1B: Senders' Second Order Beliefs")

print(plot1)

### RECEIVER BELIEFS

tempDplyr <- receiver_1S %>% group_by(randPromiseValue) %>% filter(randPromiseValue >= 50, randPromiseValue<= 60) %>% 
  summarise(MeanBelief = mean(Belief1st), LowerCI = (t.test(Belief1st)$conf.int[1])/10,
            UpperCI = (t.test(Belief1st)$conf.int[2])/10) %>% rename(Promise = randPromiseValue) %>%
  mutate(Promise = Promise/10, MeanBelief = MeanBelief/10, Condition = "1S", Experiment = "MTurk")


#-> Mturk Data 2S
Beliefs_2S_MT <- select(receiver_2S, Belief1stA, randPromiseValueA) %>%
  rename(Belief = Belief1stA, Promise = randPromiseValueA )

Beliefs_2S_MT1 <- select(receiver_2S, Belief1stB, randPromiseValueB) %>%
  rename(Belief = Belief1stB, Promise = randPromiseValueB )

Beliefs_2S <- bind_rows(Beliefs_2S_MT, Beliefs_2S_MT1)

tempDplyr2 <- Beliefs_2S %>% group_by(Promise) %>% filter(Promise == 60) %>% 
  summarise(MeanBelief = mean(Belief), LowerCI = (t.test(Belief)$conf.int[1])/10, 
            UpperCI = (t.test(Belief)$conf.int[2])/10) %>%
  mutate(Promise = Promise/10, MeanBelief = MeanBelief/10, Condition = "2S", Experiment = "MTurk")



#-> Lab First Round 1S
tempDplyr3 <- receiver_1S_firstRound %>% group_by(Offer) %>% filter(Offer >= 5, Offer <= 6) %>% 
  summarise(MeanBelief = mean(beliefR), LowerCI = (t.test(beliefR)$conf.int[1]), 
            UpperCI = (t.test(beliefR)$conf.int[2])) %>% 
  rename(Promise =Offer) %>%
  mutate(MeanBelief = MeanBelief, Condition = "1S", Experiment = "Lab First Round")

#-> Lab First Round 2S
Beliefs_2S_1P <- select(receiver_2S_firstRound, belief1stA, MessageA) %>%
  rename(Belief = belief1stA, Promise = MessageA )

Beliefs_2S_1P1 <- select(receiver_2S_firstRound, belief1stB, MessageB) %>%
  rename(Belief = belief1stB, Promise = MessageB )

Beliefs_2S_1P <- bind_rows(Beliefs_2S_1P, Beliefs_2S_1P1)

tempDplyr4 <- Beliefs_2S_1P %>% group_by(Promise) %>% filter(Promise == 6) %>% 
  summarise(MeanBelief = mean(Belief), LowerCI = (t.test(Belief)$conf.int[1]), 
            UpperCI = (t.test(Belief)$conf.int[2])) %>% 
  mutate(MeanBelief = MeanBelief, Condition = "2S", Experiment = "Lab First Round")

# LAB ALL ROUNDS 
#-> Lab 1S
tempDplyr5 <- receiver_1S_allRounds %>% group_by(Offer) %>% filter(Offer  >= 5, Offer <= 6) %>% 
  summarise(MeanBelief = mean(beliefR), LowerCI = (t.test(beliefR)$conf.int[1]), 
            UpperCI = (t.test(beliefR)$conf.int[2])) %>% 
  rename(Promise = Offer) %>%
  mutate(MeanBelief = MeanBelief, Condition = "1S", Experiment = "Lab All Rounds")

#-> Lab 2S
Beliefs_2S_ALL <- select(receiver_2S_allRounds, belief1stA, MessageA) %>%
  rename(Belief = belief1stA, Promise = MessageA )

Beliefs_2S_ALL1 <- select(receiver_2S_allRounds, belief1stB, MessageB) %>%
  rename(Belief = belief1stB, Promise = MessageB )

Beliefs_2S_ALL <- bind_rows(Beliefs_2S_ALL, Beliefs_2S_ALL1)

tempDplyr6 <- Beliefs_2S_ALL %>% group_by(Promise) %>% filter(Promise == 6) %>% 
  summarise(MeanBelief = mean(Belief), LowerCI = (t.test(Belief)$conf.int[1]), 
            UpperCI = (t.test(Belief)$conf.int[2])) %>% 
  mutate(MeanBelief = MeanBelief, Condition = "2S", Experiment = "Lab All Rounds")

Rbelief_by_offer <- bind_rows(tempDplyr,tempDplyr2,tempDplyr3,tempDplyr4, tempDplyr5, tempDplyr6) %>% 
  mutate(Promise1 = Promise)


Rbelief_by_offer$Experiment <- factor(Rbelief_by_offer$Experiment, 
                                      levels= c("Lab First Round", "Lab All Rounds", "MTurk"))
Rbelief_by_offer[3,1] = 7
Rbelief_by_offer[6,1] = 7
Rbelief_by_offer[9,1] = 7

Rbelief_by_offer[1,3:4] <- c(4.24306,	4.64583) #Bootstrap for mturk 1S5
Rbelief_by_offer[2,3:4] <- c(1.85714,	4.85714) #Bootstrap for 1S6 MTURK
Rbelief_by_offer[3,3:4] <- c(3.68966, 5.10345) #Bootstrap for MTurk 2S6

Rbelief_by_offer[4,3:4] <- c(3.1481, 4.2963) #Bootstrap 1S5 Lab First Round
Rbelief_by_offer[5,3:4] <- c(4,	5) #Bootstrap Correction for 1S6 Lab First Round
Rbelief_by_offer[6,2:4] <- c(4.4500, 3.1053,	5.0000) #Bootstrap for 2S6 Lab First Round

Rbelief_by_offer[7,3:4] <- c(2.9524,	3.8444) ##Bootstrap 1S5 ALL ROUNDS
Rbelief_by_offer[8,2:4] <- c(3.4063,	2.7941,	4.0909) #Bootstrap 1S6 ALL ROUNDS
Rbelief_by_offer[9,2:4] <- c(3.6623,	3.3413,	3.9931) #Bootstrap 2S6 ALL ROUNDS


plot1 <- ggplot(Rbelief_by_offer, aes(x = Promise, y=MeanBelief)) +  
  geom_point(aes(colour = Condition, shape = factor(Promise1)), size = 2.7)+ 
  facet_grid(~Experiment)+ coord_cartesian(ylim=c(0,6), xlim = c(4,8)) +
  geom_errorbar(aes(x=Promise,ymax=UpperCI,ymin=LowerCI, width=.5),na.rm=TRUE)+ theme_classic() +
  theme(strip.background = element_rect(colour="white"), 
        plot.title = element_text(hjust = 0.5)) +
  scale_shape_manual(values = c(16, 15)) + labs(x= "", y="Mean", colour = "", shape="") +
  scale_x_continuous(breaks = c(5,6,7), labels=c("1S 5", "1S 6", "2S 6")) +
  scale_color_manual(values = c("#fb9a99","#1f78b4")) + ggtitle("Figure SI.1A: Receivers' First Order Beliefs")

print(plot1)



