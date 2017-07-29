############## DISTRIBUTION AND cdf OF OFFERS
############## BRIAN HEALY
############## LAST MODIFIED: 6.18.17

a <- data.frame(Promise = c(1:10))
S1 <- a %>% group_by(Promise) %>% summarize(count = n()) %>% mutate(freq = NA, Condition = "1S", count = NA)

S2 <- a %>% group_by(Promise) %>% summarize(count = n()) %>% mutate(freq = NA, Condition = "2S", count = NA)

bins <- bind_rows(S1, S2)

### MTURK

Offerdist <-  sender_1S %>% group_by(Promise) %>%
  summarize(count = n()) %>% mutate(freq = count/sum(count), Condition = "1S",
                                    Promise=Promise/10)

Offerdist2 <-  sender_2S %>%  group_by(Promise) %>%  summarize(count = n()) %>%
  mutate(freq = count/sum(count), Promise=Promise/10, Condition = "2S")

tempDplyr <- bind_rows(Offerdist, Offerdist2, bins) %>%
  mutate(Experiment = "MTurk")
#combines 1S and 2S dataframes

distPromises <- ggplot(tempDplyr, aes(x=factor(Promise), y = freq)) + expand_limits(y=1) +
  geom_bar(stat = "identity", aes(fill=Condition), position = "dodge") + theme_classic() + labs(x = "Offer", y = "Frequency") + 
  geom_text(aes(label = "", group=Condition), position=position_dodge(width=.9), vjust = -.25) + 
  ggtitle("MTurk - Distribution of Promises") 

print(distPromises)


### First Round

Offerdist3 <-  sender_1S_firstRound %>%  
  group_by(Promise) %>%
  summarize(count = n()) %>%
  mutate(freq = count/sum(count), Condition = "1S")

Offerdist4 <-  sender_2S_firstRound %>% group_by(Promise) %>%
  summarize(count = n()) %>% mutate(freq = count / sum(count), Condition = "2S")

tempDplyr2 <- bind_rows(Offerdist3, Offerdist4, bins) %>% mutate(Experiment = "Lab First Round") #combines 1S and 2S dataframes

distPromises_lab1p <- ggplot(tempDplyr2, aes(x=factor(Promise), y = freq)) + expand_limits(y=1) +
  geom_bar(stat = "identity", aes(fill=Condition), position = "dodge") + theme_classic() + labs(x = "Offer", y = "Frequency") + 
  geom_text(aes(label = "", group=Condition), position=position_dodge(width=1), vjust = -.25) + 
  ggtitle("Distribution of Promises - Lab First Round") + theme(legend.position="none")

print(distPromises_lab1p)
#Lab ALL ROUNDS

Offerdist5 <-  sender_1S_allRounds %>%  group_by(Promise) %>% summarize(count = n()) %>%
  mutate(freq = count/sum(count), Condition = "1S")

Offerdist6 <-  sender_2S_allRounds %>% group_by(Promise) %>%
  summarize(count = n()) %>% mutate(freq = count / sum(count), Condition = "2S")

tempDplyr3 <- bind_rows(Offerdist5, Offerdist6, bins) %>% mutate(Experiment = "Lab All Rounds") #combines 1S and 2S dataframes


offer <- bind_rows(tempDplyr, tempDplyr2, tempDplyr3) 



offer$Experiment = factor(offer$Experiment, levels=c('Lab First Round','Lab All Rounds','MTurk'))



ggplot(offer, aes(x=factor(Promise), y = freq)) + expand_limits(y=.8) +
  geom_bar(stat = "identity", aes(fill=Condition), position = "dodge", size=1) + theme_classic() +
  ggtitle("Figure 1A: Distribution of Offers") +
  facet_grid(~Experiment) + theme(strip.background = element_rect(colour="white"), 
                                    strip.text = element_text(size=rel(1)),
                                    panel.spacing = unit(1.2, "lines"),
                                    plot.title = element_text(hjust = 0.5)) + 
  labs(x = "", y = "Proportion", fill="Proportion")+ scale_y_continuous(breaks = c(0,.2,.4,.6,.8))+
  scale_x_discrete(breaks = c(0,2,4,6,8,10))+
  geom_text(aes(label = "", group=Condition), position=position_dodge(width=1), vjust = -.25)+ 
  scale_fill_manual(values = c("#fb9a99","#1f78b4"))



print(distPromises_all)

temp1 <- Sender_ALLMTURK %>% select(Promise, condition) %>% mutate(Experiment = "MTurk", Promise = Promise/10) %>% 
  rename(Condition = condition)
temp2 <- senderALL_lab1P %>% select(Promise, Condition) %>% mutate(Experiment = "Lab First Round")
temp3 <- Data_SenderAll  %>% select(Promise, Condition) %>% mutate(Experiment = "Lab All Rounds")
offers <- bind_rows(temp1,temp2, temp3) %>% mutate(variable = "cdf")

offers$Experiment <- factor(offers$Experiment, levels= c("Lab First Round", "Lab All Rounds", "MTurk"))

promiseCDF <- ggplot(offers, aes(x=Promise, colour = Condition)) + theme_classic() 
promiseCDF <- promiseCDF + stat_ecdf(size = 1) + scale_x_continuous(breaks = c(0,2,4,6,8,10))+ 
  scale_y_continuous(breaks = c(0,.20,.40,.60,.80,1.00)) + ggtitle("Figure 1B: CDF of Offers") +
  labs(x = "Offer", y="Proportion", colour="") + coord_cartesian(xlim = c(0,10), ylim = c(0,1)) +
  facet_grid(~Experiment) + theme(strip.background = element_rect(colour="white"), 
                                  strip.text = element_blank(),panel.spacing = unit(1.2, "lines"),
                                  plot.title = element_text(hjust = 0.5)) + 
  scale_colour_manual(values = c("#fb9a99","#1f78b4"))
print(promiseCDF)

