######### Change in Frequency of Offers 2s-1s  
######### BY BRIAN HEALY
######### LOAST MODIFIED 6.28.17

#MTurk 
Offerdist <-  sender_1S %>% group_by(Promise) %>%
  summarize(count = n()) %>%
  mutate(freq = count/sum(count), Promise=Promise/10) %>% rename(freq1S = freq) %>%
  mutate(error1S = (freq1S*(1-freq1S)/count)) #CLEARED



Offerdist2 <-  sender_2S %>%
  group_by(Promise) %>%
  summarize(count = n()) %>%
  mutate(freq = count / sum(count), Promise = Promise/10) %>% rename(freq2S = freq) %>%
  mutate(error2S = (freq2S*(1-freq2S)/(2*count)))


tempDplyr <- full_join(Offerdist, Offerdist2, by = "Promise") %>% filter(Promise >= 5, Promise <= 7) 

offer_Turk <- tempDplyr %>% group_by(Promise) %>%
  summarize(diff = freq2S-freq1S, totalerror= sqrt(error1S+error2S), 
            UpperCI= diff + totalerror*1.965, LowerCI = diff- totalerror*1.965,
            Condition = "2S-1S", Experiment = "MTurk") %>% mutate(Promise1= c("Offer 5", "Offer 6", "Offer 7"))
#Lab first Round
Offerdist <-  sender_1S_firstRound %>% group_by(Promise) %>%
  summarize(count = n()) %>%
  mutate(freq = count/sum(count)) %>% rename(freq1S = freq)

Offerdist <- rbind(Offerdist, c(7,0,0))

Offerdist2 <-  sender_2S_firstRound %>%  group_by(Promise) %>%
  summarize(count = n()) %>%
  mutate(freq = count / sum(count)) %>%
  rename(freq2S = freq)

tempDplyr <- full_join(Offerdist, Offerdist2, by = "Promise") %>% filter(Promise >= 5, Promise <= 7) %>%
  mutate(error1S = (freq1S*(1-freq1S)/count.x), error2S = (freq2S*(1-freq2S)/count.y))
tempDplyr[3,6] = 0

offer_lab1p <- tempDplyr %>% group_by(Promise) %>%
  summarize(diff = freq2S-freq1S, totalerror= sqrt(error1S+error2S), 
            UpperCI= diff + totalerror*1.96, LowerCI = diff- totalerror*1.96,
            Condition = "2S-1S", Experiment = "Lab First Round")  %>% 
  mutate(Promise1= c("Offer 5", "Offer 6", "Offer 7"))
#Lab ALL ROUNDS
Offerdist <-  sender_1S_allRounds %>% group_by(Promise) %>%
  summarize(count = n()) %>%
  mutate(freq = count/sum(count)) %>% rename(freq1S = freq)

Offerdist2 <-  sender_2S_allRounds %>%  group_by(Promise) %>%
  summarize(count = n()) %>%
  mutate(freq = count / sum(count)) %>%
  rename(freq2S = freq)

tempDplyr <- full_join(Offerdist, Offerdist2, by = "Promise") %>% filter(Promise >= 5, Promise <= 7) %>%
  mutate(error1S = (freq1S*(1-freq1S)/count.x), error2S = (freq2S*(1-freq2S)/count.y))

offer_laball <- tempDplyr %>% group_by(Promise) %>%
  summarize(diff = freq2S-freq1S, totalerror= sqrt(error1S+error2S), 
            UpperCI= diff + totalerror*1.96, LowerCI = diff- totalerror*1.96,
            Condition = "2S-1S", Experiment = "Lab All Rounds")  %>%
  mutate(Promise1= c("Offer 5", "Offer 6", "Offer 7"))

freqoffer <- bind_rows(offer_Turk,offer_lab1p,offer_laball)

freqoffer[1,4:5] = c(0.0022, -0.1582)
freqoffer[2,4:5] = c(0.0943, 0.0084)
freqoffer[3,4:5] = c(0.0789, 0.0177)
freqoffer[4,4:5] = c(-0.1122, -0.5157)
freqoffer[5,4:5] = c(0.3262, 0.0092	)
freqoffer[6,4:5] = c(0.2980,0.0804)
freqoffer[7,4:5] = c(-0.1813,-0.4069)
freqoffer[8,4:5] = c(0.2268,0.0738)
freqoffer[9,4:5] = c(0.2518, 0.1353)


freqoffer$Experiment <- factor(freqoffer$Experiment, levels= c("Lab First Round", "Lab All Rounds", "MTurk"))

levels(freqoffer$Experiment)[levels(freqoffer$Experiment)=="Lab First Round"]  <- "L1" 
levels(freqoffer$Experiment)[levels(freqoffer$Experiment)=="Lab All Rounds"]  <- "LA"
levels(freqoffer$Experiment)[levels(freqoffer$Experiment)=="MTurk"]  <- "MT"

ggplot(freqoffer, aes(x = factor(Experiment), y=diff), Axes = FALSE) + labs(y="Difference", shape="") +
  geom_point(aes(shape = factor(Promise)),size = 2.4) +
  ggtitle("Figure 2: Change in Frequency of Offers: 2S-1S") +
  facet_grid(.~ factor(Promise1)) +
  coord_cartesian(ylim = c(-.6,.6), xlim = c(-1,5))+
  geom_errorbar(aes(x= Experiment,ymax=UpperCI,ymin=LowerCI, width=.35), na.rm=TRUE)+
  theme_classic() +
  scale_y_continuous(breaks = c(5,6,7), labels=c("L1", "LA", "MT")) +
  theme(strip.background = element_rect(colour="white"), strip.text = element_text(size=rel(1.08)),
        axis.title.x=element_blank(), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))+ 
  geom_hline(aes(yintercept=0)) + scale_y_continuous(breaks = c(-.6, -.4,-.2, 0, .2,.4, .6)) +
  scale_shape_manual(values = c(16, 15, 17))
 
