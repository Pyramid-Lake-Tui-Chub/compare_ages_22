#load packages!
library(dplyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(knitr)
library(cowplot)
library(data.table)

#reshape and split original dataset
allages_nets_CPUE <- melt(allages_nets_CPUE, id.vars=c("site", "month", "net"))
numb <- allages_nets_CPUE[1:2172,]
cpue <- allages_nets_CPUE[2173:4344,]

##add names
names(numb)[4:5] <- c("matcode", "total")
names(cpue)[4:5] <- c("matcode", "cpue")

#remove NAs
numb <- na.omit(numb)
cpue <- na.omit(cpue)

#remove total adult rows
numb <- subset(numb, matcode != "adult#")
cpue <- subset(cpue, matcode != "aCpue")

#make a big nets column
#write a left function and then make new column to add
left = function (string,char) {
  substr(string, 1,char)
}
big_net <- c(left(cpue$net[1:1160], 3), left(cpue$net[1161:1232], 2))
cpue <- cbind(cpue, big_net)
names(cpue)[5] <- c("ind_cpue")
#make total cpue and densities for calculations -just doing cpue for now
#little net and big net
totals <- aggregate(ind_cpue~matcode + net, cpue, sum)
totals_lil_2 <-aggregate(ind_cpue~net, cpue, sum)
totals_big_month_2 <-aggregate(ind_cpue~big_net + month, cpue, sum)
totals_lil_month <- aggregate(ind_cpue~matcode + net + month, cpue, sum)
totals_big_month <- aggregate(ind_cpue~matcode + big_net + month, cpue, sum)
totals_big <- aggregate(ind_cpue~matcode + big_net, cpue, sum)
totals_biggest <-aggregate(ind_cpue~big_net, cpue, sum)
  
names(totals)[3] <- c("lil_cpue")
names(totals_lil_2)[2] <- c("lil_2_cpue")
names(totals_big_month_2)[3] <- c("big_month_2_cpue")
names(totals_lil_month)[4] <- c("lil_month_cpue")
names(totals_big_month)[4] <- c("big_month_cpue")
names(totals_big)[3] <- c("big_cpue")
names(totals_biggest)[2] <- c("biggest_cpue")

# create a vector with the totals in length proportion to that in the cpue dataset
# add to cpue dataset
cpue<- merge(x=cpue, y=totals, by = c('matcode', 'net'))
cpue<- merge(x=cpue, y=totals_big_month_2, by = c('month', 'big_net'))
cpue <- merge(x=cpue, y=totals_lil_month, by = c('matcode', 'net', "month"))
cpue <- merge(x=cpue, y=totals_big_month, by = c('matcode', 'big_net', "month"))
cpue <- merge(x=cpue, y=totals_big, by=c("matcode", "big_net"))
cpue <- merge(x=cpue, y=totals_biggest, by=c("big_net"))
cpue <- merge(x=cpue, y=totals_lil_2, by=c("net"))


cpue <- cpue %>% mutate(biggest_perc=ind_cpue/biggest_cpue)
cpue <- cpue %>% mutate(big_month_2_perc=ind_cpue/big_month_2_cpue)
cpue <- cpue %>% mutate(big_perc=ind_cpue/big_cpue)
cpue <- cpue %>% mutate(lil_perc=ind_cpue/lil_cpue)
cpue <- cpue %>% mutate(lil_month_perc=ind_cpue/lil_month_cpue)
cpue <- cpue %>% mutate(big_month_perc=ind_cpue/big_month_cpue)
cpue <- cpue %>% mutate(lil_2_perc=ind_cpue/lil_2_cpue)

# percentage of fecund individuals
fec_subset <- subset(cpue, matcode %in% c("mCpue", "fCpue"))
fec_total <- aggregate(ind_cpue.x~big_net, fec_subset, sum)
fec_subset <- merge(x=fec_subset, y=fec_total, by=c("big_net"))
fec_subset <- fec_subset %>% mutate(biggest_perc_fec=ind_cpue.x/ind_cpue.y)




