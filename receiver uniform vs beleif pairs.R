#-> Mturk Data 1S
tempDplyr <- sender_1S %>% group_by(Promise) %>% filter(Promise >= 50, Promise <= 60) %>% 
  summarise(MeanTransfer = mean(Sent), LowerCI = (t.test(Sent)$conf.int[1])/10, UpperCI = (t.test(Sent)$conf.int[2])/10) %>% 
  mutate(Promise = Promise/10, MeanTransfer = MeanTransfer/10, Condition = "1S", Experiment = "MTurk")

#-> Mturk Data 2S
tempDplyr2 <- sender_2S %>% group_by(Promise) %>% filter(Promise >= 50, Promise <= 70) %>% 
  summarise(MeanTransfer = mean(Sent), LowerCI = (t.test(Sent)$conf.int[1])/10, UpperCI = (t.test(Sent)$conf.int[2])/10) %>% 
  mutate(Promise = Promise/10, MeanTransfer = MeanTransfer/10, Condition = "2S", Experiment = "MTurk")

#-> Lab First Round 1S
tempDplyr3 <- sender_1S_firstRound %>% group_by(Promise) %>% filter(Promise >= 5, Promise <= 6) %>% 
  summarise(MeanTransfer = mean(Sent), LowerCI = (t.test(Sent)$conf.int[1]), UpperCI = (t.test(Sent)$conf.int[2])) %>% 
  mutate(Promise, MeanTransfer = MeanTransfer, Condition = "1S", Experiment = "Lab First Round")

#-> Lab First Round 2S
tempDplyr4 <- sender_2S_firstRound %>% group_by(Promise) %>% filter(Promise >= 5, Promise <= 7) %>% 
  summarise(MeanTransfer = mean(Sent), LowerCI = (t.test(Sent)$conf.int[1]), UpperCI = (t.test(Sent)$conf.int[2])) %>% 
  mutate(Promise, MeanTransfer = MeanTransfer, Condition = "2S", Experiment = "Lab First Round")

# Lab ALL ROUNDS
tempDplyr5 <- sender_1S_allRounds %>% group_by(Promise) %>% filter(Promise >= 5, Promise <= 6) %>% 
  summarise(MeanTransfer = mean(Sent), LowerCI = (t.test(Sent)$conf.int[1]), UpperCI = (t.test(Sent)$conf.int[2])) %>% 
  mutate(Promise, MeanTransfer = MeanTransfer, Condition = "1S", Experiment = "Lab All Rounds")

#-> Lab First Round 2S
tempDplyr6 <- sender_2S_allRounds %>% group_by(Promise) %>% filter(Promise >= 5, Promise <= 7) %>% 
  summarise(MeanTransfer = mean(Sent), LowerCI = (t.test(Sent)$conf.int[1]), UpperCI = (t.test(Sent)$conf.int[2])) %>% 
  mutate(Promise, MeanTransfer = MeanTransfer, Condition = "2S", Experiment = "Lab All Rounds")


trans_by_offer <- bind_rows(tempDplyr,tempDplyr2,tempDplyr3,tempDplyr4, tempDplyr5, tempDplyr6) %>% mutate(Promise1 = Promise)

trans_by_offer$Experiment <- factor(trans_by_offer$Experiment, levels= c("Lab First Round", "Lab All Rounds", "MTurk"))


trans_by_offer[1,1] = 2
trans_by_offer[2,1] = 3


trans_by_offer[6,1] = 2
trans_by_offer[7,1] = 3


trans_by_offer[11,1] = 2
trans_by_offer[12,1] = 3

trans_by_offer[1,2:4] <- c(0.8403/0.3333, 0.7778/0.3333,	0.8958/0.3333) #5 M 1S
trans_by_offer[2,2:4] <- c(0.5714/0.2857,	0.1429/0.2857,	0.8571/0.2857) #6 M 1S
trans_by_offer[3,2:4] <- c(0.7279/0.3333,	0.6496/0.3333,	0.7845/0.3333) #5 M 2S
trans_by_offer[4,2:4] <- c(0.6207/0.2857,	0.4483/0.2857,	0.7931/0.2857)  #6 M 2S
trans_by_offer[5,2:4] <- c(0.3200/0.2500,	0.1600/0.2500,	0.5556/0.2500)   #7 M 2S

trans_by_offer[6,2:4] <- c(0.7037/0.3333,	0.5185/0.3333,	0.8519/0.3333) #5 1P 1S
trans_by_offer[7,2:4] <- c(0.2500/0.2857,	0.0000,	1.0000/0.2857) #6 1P 1S
trans_by_offer[8,2:4] <- c(0.7857/0.3333,	0.4138/0.3333,	0.9231/0.3333) #5 1P 2S
trans_by_offer[9,2:4] <- c(0.6500/0.2857,	0.3636/0.2857,	0.8824/0.2857) #6 1P 2S
trans_by_offer[10,2:4] <-c(0.5000/0.2500,	0.2308/0.2500,	0.7692/0.2500) #7 1P 2S

trans_by_offer[11,2:4] <- c(0.5514/0.3333,  0.4269/0.3333,	0.6649/0.3333) #5 LA 1S
trans_by_offer[12,2:4] <- c(0.2500/0.2857,	0.1250/0.2857,	0.4333/0.2857) #6 LA 1S
trans_by_offer[13,2:4] <- c(0.6047/0.3333,	0.4878/0.3333,	0.6971/0.3333) #5 LA 2S
trans_by_offer[14,2:4] <- c(0.3766/0.2857,	0.2774/0.2857,	0.4855/0.2857) #6 LA 2S
trans_by_offer[15,2:4] <- c(0.1811/0.2500,	0.1066/0.2500,	0.2880/0.2500) #7 LA 2S


plot1 <- ggplot(trans_by_offer, aes(x = Promise, y=MeanTransfer)) +  
  geom_point(aes(colour = Condition, shape = factor(Promise1)), size = 2.7)+
  scale_x_continuous(breaks = c(2,3,5,6,7)) + geom_hline(yintercept=1)+
  facet_grid(~Experiment) + coord_cartesian(ylim = c(0,4), xlim = c(1,8)) + scale_y_continuous(breaks = c(1:6))+
  geom_errorbar(aes(x=Promise,ymax=UpperCI,ymin=LowerCI, width=.5),na.rm=TRUE)+ theme_classic() +
  theme(strip.background = element_rect(colour="white"), 
        plot.title = element_text(hjust = 0.5)) +
  scale_shape_manual(values = c(16, 15, 17)) + labs(x= "", y="Mean", shape="", colour="") +
  scale_color_manual(values = c("#fb9a99", "#1f78b4")) + ggtitle("Mean Receiver Belief Pairs Relative to Uniform")

print(plot1)

