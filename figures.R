#load packages!
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(knitr)
library(cowplot)

#helpful data subsets
fec_subset <- subset(cpue, matcode %in% c("mCpue", "fCpue"))
no_larv <- subset(cpue, matcode != "lden")
month_names <- c("5" = "May", "6"="June", "7"="July")
### Figure 1
## stacked barplot, all maturities, over total catch for the net type

#1A. gill nets
oneA <- ggplot(data=subset(no_larv, big_net == "GIL"), aes(x= site, y= biggest_perc, fill= matcode)) + 
  geom_bar(position="stack", stat="identity", alpha=0.7)+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "bottom", axis.title.y= element_text(size=8),
        plot.title= element_text(size=9, hjust=0.5)) +
  scale_y_continuous(limits = c(0,.3))+
  labs(x=" ", y="CPUE %", 
       title = "Maturity Class CPUE by Site/Total CPUE (Gill Nets)", fill="Maturity Code") +
  scale_fill_hue(labels=c("Fecund Female", "Fecund Male", "Non-Fecund Adult", "Juvenile"), l=30)
oneA

#1B. hoop nets

oneB <- ggplot(data=subset(no_larv, big_net == "HOP"), aes(x= site, y= biggest_perc, fill= matcode)) + 
  geom_bar(position="stack", stat="identity", alpha=0.7)+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "bottom", axis.title.y= element_text(size=8),
        plot.title= element_text(size=9, hjust=0.5)) +
  scale_y_continuous(limits = c(0,.3))+
  labs(x=" ", y="CPUE %", 
       title = "Maturity Class CPUE by Site/All CPUE (Hoop Nets)", fill="Maturity Code") +
  scale_fill_hue(labels=c("Fecund Female", "Fecund Male", "Non-Fecund Adult", "Juvenile"), l=30)
oneB

grid.arrange(oneB, oneA, ncol=1)

# Figure 2-run through for gill, hoop and then juvenile alone on minnow, larval always stays the same
#larval
# check scale limit because sometimes I change it for the last graph
l_den <- ggplot(data=subset(cpue, matcode == "lDen"), aes(x= site, y= big_perc)) + 
  geom_bar(stat="identity", fill = "darkseagreen", alpha= 0.5)+
  scale_y_continuous(limits = c(0,.4))+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "none", 
        axis.title.y= element_text(size=8),
        plot.title= element_text(size=9, hjust=0.5)) +
  geom_hline(yintercept=.11, linetype = "longdash") +
  labs(title = "Larval Density by Site/ Larval Total Density", x=" ", y= "Larval Density %", Fill="" )
l_den

#juvenile
j_perc_min <- ggplot(data=subset(cpue, matcode == "jCpue" & big_net == "MIN"), aes(x= site, y= big_perc)) + 
  geom_bar(stat="identity", fill = "lightblue", alpha= 0.75)+
  scale_y_continuous(limits = c(0,.25))+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "none", 
        axis.title.y= element_text(size=8),
        plot.title= element_text(size=8, hjust=0.5)) +
  geom_hline(yintercept=.11, linetype = "longdash") +
  labs(title = "Juvenile CPUE by Site/ Juvenile Total CPUE (Minnow Traps)", 
       x=" ", y="CPUE %", Fill="" )
j_perc_min

#adult nf
nf_perc_hop <- ggplot(data=subset(cpue, matcode == "nfCpue" & big_net == "HOP"), aes(x= site, y= big_perc)) + 
  geom_bar(stat="identity", fill = "brown1", alpha=0.5)+
  scale_y_continuous(limits = c(0,.6))+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "none", 
        axis.title.y= element_text(size=8),
        plot.title= element_text(size=8, hjust=0.5)) +
  geom_hline(yintercept=.11, linetype = "longdash") +
  labs(title = "Non-Fecund Adult CPUE by Site/ Non-Fecund Adult Total CPUE (Hoop Nets)", x=" ", y="CPUE %", Fill="" )
nf_perc_hop

#adult male
m_perc_hop <- ggplot(data=subset(cpue, matcode == "mCpue" & big_net == "HOP"), aes(x= site, y= big_perc)) + 
  geom_bar(stat="identity", fill = "blue", alpha=0.5)+
  scale_y_continuous(limits = c(0,.6))+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "none", 
        axis.title.y= element_text(size=8),
        plot.title= element_text(size=8, hjust=0.5)) +
  geom_hline(yintercept=.11, linetype = "longdash") +
  labs(title = "Fecund Male CPUE by Site/ Fecund Male Total CPUE (Hoop Nets)", x=" ", y="CPUE %", Fill="" )
m_perc_hop

#adult female
f_perc_hop <- ggplot(data=subset(cpue, matcode == "fCpue" & big_net == "HOP"), aes(x= site, y= big_perc)) + 
  geom_bar(stat="identity", fill = "darkorchid1", alpha=0.5)+
  scale_y_continuous(limits = c(0,.6))+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "none", 
        axis.title.y= element_text(size=8),
        plot.title= element_text(size=8, hjust=0.5)) +
  geom_hline(yintercept=.11, linetype = "longdash") +
  labs(title = "Fecund Female CPUE by Site/ Fecund Female Total CPUE (Hoop Nets)", x=" ", y="CPUE %", Fill="" )
f_perc_hop

grid.arrange(f_perc_hop, j_perc_hop, m_perc_hop, l_den, nf_perc_hop, nrow=3, ncol=2)

### Figure 3
## Show gill net catch by depth and minnow trap (juveniles only for minnow)
#adults in gil
threeA <- ggplot(data=subset(cpue, big_net == "GIL"), aes(x= net, y= biggest_perc, fill= matcode)) + 
  geom_bar(position="stack", stat="identity", alpha=0.4)+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "bottom", 
        axis.title.y= element_text(size=8),
        plot.title= element_text(size=9, hjust=0.5)) +
  labs(x=" ", y="CPUE %", 
       title = "Maturity Class CPUE by Depth/Total CPUE (Gill Nets)", fill="Maturity Class") +
  geom_hline(yintercept=.33, linetype = "longdash") +
  scale_fill_hue(labels=c("Fecund Female", "Fecund Male", "Non-Fecund Adult", "Juvenile"), l=45)
threeA

#juveniles in minnow-show depth breakdown
threeB <- ggplot(data=subset(cpue, matcode == "jCpue" & big_net == "MIN"), aes(x= net, y= big_perc)) + 
  geom_bar(stat="identity", fill = "deeppink4", alpha= 0.75)+
  scale_y_continuous(limits = c(0,.25))+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "none", 
        axis.title.y= element_text(size=8),
        plot.title= element_text(size=8, hjust=0.5)) +
  geom_hline(yintercept=.13, linetype = "longdash") +
  scale_x_discrete(labels=c("1m", "2m", "5m", "10m", "15m", "20m", "25m", "30m"))+
  labs(title = "Juvenile CPUE by Minnow Trap/ Total Juvenile Minnow Trap CPUE", 
       x=" ", y="CPUE %", Fill="" )
threeB

### Figure 4
# Percent of total CPUE by month, broken out by maturity code-ie all gil cpue, show how it is distributed amongst the months
# Gill-all
fourA <- ggplot(data=subset(cpue, big_net == "GIL"), aes(x= site, y= biggest_perc, fill= matcode)) + 
  facet_wrap(~month, labeller = as_labeller(month_names)) +
  geom_bar(position="stack", stat="identity", alpha=0.75)+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "bottom", 
        axis.title.y= element_text(size=8),
        axis.text.x= element_text(size=8, angle=45, hjust=1),
        plot.title= element_text(size=9, hjust=0.5)) +
  labs(x=" ", y="CPUE %", 
       title = "Maturity Class CPUE by Site/Total CPUE (Gill Nets)", fill="Maturity Class") +
  scale_fill_hue(labels=c("Fecund Female", "Fecund Male", "Non-Fecund Adult", "Juvenile"), l=20)
fourA

#Gill-fecund
fourB <- ggplot(data=subset(fec_subset, big_net == "GIL"), aes(x= site, y= biggest_perc_fec, fill= matcode)) + 
  facet_wrap(~month, labeller = as_labeller(month_names)) +
  geom_bar(position="stack", stat="identity", alpha=0.75)+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "bottom", 
        axis.title.y= element_text(size=8),
        axis.text.x= element_text(size=8, angle=45, hjust=1),
        plot.title= element_text(size=9, hjust=0.5)) +
  labs(x=" ", y="CPUE %", 
       title = "Fecund CPUE by Site/Total Fecund CPUE (Gill Nets)", fill="Maturity Class") +
  scale_fill_hue(labels=c("Fecund Female", "Fecund Male"), l=20)
fourB

#Hoop-all
fourC <- ggplot(data=subset(cpue, big_net == "HOP"), aes(x= site, y= biggest_perc, fill= matcode)) + 
  facet_wrap(~month, labeller = as_labeller(month_names)) +
  geom_bar(position="stack", stat="identity", alpha=0.75)+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "bottom", 
        axis.title.y= element_text(size=8),
        axis.text.x= element_text(size=8, angle=45, hjust=1),
        plot.title= element_text(size=9, hjust=0.5)) +
  labs(x=" ", y="CPUE %", 
       title = "Maturity Class CPUE by Site/Total CPUE (Hoop Nets)", fill="Maturity Class") +
  scale_fill_hue(labels=c("Fecund Female", "Fecund Male", "Non-Fecund Adult", "Juvenile"), l=20)
fourC

#Hoop-fecund
fourD <- ggplot(data=subset(fec_subset, big_net == "HOP"), aes(x= site, y= biggest_perc_fec, fill= matcode)) + 
  facet_wrap(~month, labeller = as_labeller(month_names)) +
  geom_bar(position="stack", stat="identity", alpha=0.75)+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "bottom", 
        axis.title.y= element_text(size=8),
        axis.text.x= element_text(size=8, angle=45, hjust=1),
        plot.title= element_text(size=9, hjust=0.5)) +
  labs(x=" ", y="CPUE %", 
       title = "Fecund CPUE by Site/Total Fecund CPUE (Hoop Nets)", fill="Maturity Class") +
  scale_fill_hue(labels=c("Fecund Female", "Fecund Male"), l=20)
fourD

### Figure 5
## Stacked barplot of big% by maturity code with larval below 

five <- ggplot(data=subset(fec_subset, big_net == "GIL"), aes(x= site, y= big_perc, fill= matcode)) + 
  geom_bar(position="stack", stat="identity", alpha=0.4)+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "bottom", 
        axis.title.y= element_text(size=8),
        plot.title= element_text(size=9, hjust=0.5)) +
  labs(x=" ", y="CPUE %", 
       title = "Maturity Class CPUE by Site/Maturity Class CPUE (Gill Nets)", fill="Maturity Class") +
  geom_hline(yintercept=.22, linetype = "longdash") +
  scale_y_continuous(limits = c(0,.5))+
  scale_fill_hue(labels=c("Fecund Female", "Fecund Male"), l=70)
five

grid.arrange(larv_den, five, ncol=1, heights=c(1,2))

### Figure 6
## Side by side comparison of male and female
# gill net
sixA <- ggplot(data=subset(fec_subset, big_net == "GIL"), aes(x= matcode, y= biggest_perc_fec)) + 
  geom_bar(position="stack", fill = "royalblue", stat="identity", alpha=0.75)+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "bottom", 
        axis.title.y= element_text(size=10),
        axis.text.x= element_text(size=10, angle=45, hjust=1),
        plot.title= element_text(size=10, hjust=0.5)) +
  labs(x=" ", y="CPUE %", 
       title = "Fecund Male vs. Female CPUE (Gill Nets)", fill="Maturity Class") +
  scale_x_discrete(labels=c("Female CPUE", "Male CPUE"))+
  scale_fill_hue(labels=c("Fecund Female", "Fecund Male"), l=80)
sixA

#hoop net
sixB <- ggplot(data=subset(fec_subset, big_net == "HOP"), aes(x= matcode, y= biggest_perc_fec)) + 
  geom_bar(position="stack", fill = "violetred4", stat="identity", alpha=0.75)+
  theme(panel.background= element_rect(fill="white", color="black"),
        legend.position = "bottom", 
        axis.title.y= element_text(size=10),
        axis.text.x= element_text(size=10, angle=45, hjust=1),
        plot.title= element_text(size=10, hjust=0.5)) +
  labs(x=" ", y="CPUE %", 
       title = "Fecund Male vs. Female CPUE (Hoop Nets)", fill="Maturity Class") +
  scale_x_discrete(labels=c("Female CPUE", "Male CPUE"))+
  scale_fill_hue(labels=c("Fecund Female", "Fecund Male"), l=80)
sixB

