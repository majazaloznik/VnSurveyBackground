#################################
## Vietnam Household Living Standards Survey
################################
#######################################
library(foreign)
library(dplyr)

## household memebrs
muc1a.2010 <- read.dta("data/muc1a.dta", 
                       convert.factors=FALSE)
# working in agriculture 
muc4a1 <- read.dta("data/muc4a1.dta", 
                    convert.factors=FALSE)

## merge both
vhlss.2010 <- inner_join( muc1a.2010, muc4a1)
province.names <- read.csv("data/tinh.csv", header=FALSE)

## wieghts
wt10 <- read.dta("data/wt10.dta",
                 convert.factors=FALSE)
wt10 <- unique(wt10)
## two househodls weights are duplicated in the weights.. so it looks like tehre are 9402 hh weights,
## but there aren't really.. this removes 2 hh = 6 people from data set


## then there are 5 households in teh data set that don't have weights as well, 
## see them here: 
#  wt10[is.na(wt10$wt9),]
wt10 <- wt10[!is.na(wt10$wt9),]
## so we have to remove those 18 people as well. 

## so an inner join is  how we get rid of the people without weights - but we've lost 7 hh or 24 people
vhlss.2010 <- inner_join(vhlss.2010, wt10)

## no of households
vhlss.2010 %>%
group_by(tinh, huyen, xa, diaban, hoso) %>%
  summarise(n=n()) %>% nrow()

## who is asked the agriculture question? over fives:   
max(vhlss.2010[is.na(vhlss.2010$m4ac1a),]$m1ac5)

## age groups: age is m1ac5 (and big age groups)
vhlss.2010 <- vhlss.2010 %>%
  filter(!is.na(m4ac1b)) %>%
  mutate(age.group = cut(m1ac5, breaks=c(seq(5,105,10)), right=FALSE, include.lowest=TRUE)) %>%
  mutate(big.age.group = cut(m1ac5, breaks=c(5,25,55, 105), right=FALSE, include.lowest=TRUE)) 


## Agri people

vhlss.2010.agri <- 
  vhlss.2010 %>%
  filter(m4ac1b==1 | m4ac3 %in% c(61, 62, 63, 92) | m4ac4 %in% c(61, 62, 63, 92))

SUM.vhlss.2010.agri <- 
  vhlss.2010.agri %>%
  dplyr::group_by(m1ac2, age.group) %>%
  dplyr::summarise(n=sum(wt9), n.uW=n())  %>%
  dplyr::ungroup()%>%
  dplyr::mutate(freq=n/sum(n), freq.uW = n.uW/sum(n.uW))

## All people (over 5)

SUM.vhlss.2010.all <- 
  vhlss.2010 %>%
  dplyr::group_by(m1ac2, age.group) %>%
  dplyr::summarise(n=sum(wt9), n.uW=n())  %>%
  dplyr::ungroup()%>%
  dplyr::mutate(freq=n/sum(n), freq.uW = n.uW/sum(n.uW))


###plots

lower <- 5
upper <- 95
labz <- c(paste(seq(lower, upper - 10, by = 10),
                seq(lower + 10 - 1, upper - 1, by = 10),
                sep = "-"),
          paste(upper, "+", sep = ""))
oldpar <-  par(no.readonly=T)

## 2.2. plot all agri pop vs all pop (over 15)

par(mfrow=c(1,2))
par(mar=c(2,0,0,2.5))
barplot(rep(0,10), horiz = TRUE,
        xlim=c(-0.12,0), axes = FALSE,
        col="black", density=10)
axis(1, at=seq(0, -0.3, -0.05), labels=seq(0, 30, 5))
for (i in seq(0, -0.1, -0.05)){
  lines(c(i,i), c(0,11.7), lty=2, col="gray")
}

barplot(-SUM.vhlss.2010.agri[SUM.vhlss.2010.agri$m1ac2==1,]$freq, horiz = TRUE,
  axes = FALSE, add=TRUE,
        col="black", density=10)

for (i in 1:11) {
  lines(-rep(SUM.vhlss.2010.all[SUM.vhlss.2010.all$m1ac2==1,]$freq[i],2), 
        c(i-1+(0.2*i), i+(0.2*i)), 
        col="red", lwd=2)
  lines(c(0, -SUM.vhlss.2010.all[SUM.vhlss.2010.all$m1ac2==1,]$freq[i]),
        rep((i-1+(0.2*i)+ i+(0.2*i))/2,2), col="red")
}

par(mar=c(2,2.5,0,0))
barplot(rep(0,10), horiz = TRUE,
        xlim=c(0,0.12), names.arg = labz, las=2, axes=FALSE,
        col="black", density=10)
axis(1, at=seq(0, 0.3, 0.05), labels=seq(0, 30, 5))
for (i in seq(0, 0.1, 0.05)){
  lines(c(i,i), c(0,11.7), lty=2, col="gray")
}
barplot(SUM.vhlss.2010.agri[SUM.vhlss.2010.agri$m1ac2==2,]$freq, horiz = TRUE,
 names.arg = labz, las=2, axes=FALSE, add=TRUE,
        col="black", density=10)

for (i in 1:11) {
  lines(rep(SUM.vhlss.2010.all[SUM.vhlss.2010.all$m1ac2==2,]$freq[i],2), 
        c(i-1+(0.2*i), i+(0.2*i)), 
        col="red", lwd=2)
  lines(c(0, SUM.vhlss.2010.all[SUM.vhlss.2010.all$m1ac2==2,]$freq[i]),
        rep((i-1+(0.2*i)+ i+(0.2*i))/2,2), col="red")
}

dev.copy2eps(file="figures/VHLSSAgriDistAll.eps", family="ComputerModern",
             width=7, height=3.7)

xx <- rbind(
  SUM.vhlss.2010.agri[SUM.vhlss.2010.agri$m1ac2==1,]$freq*100,
  SUM.vhlss.2010.agri[SUM.vhlss.2010.agri$m1ac2==2,]$freq*100,
  SUM.vhlss.2010.all[SUM.vhlss.2010.all$m1ac2==1,]$freq*100,
  SUM.vhlss.2010.all[SUM.vhlss.2010.all$m1ac2==2,]$freq*100)
colnames(xx) <- labz

require(xtable)
xtable(xx)


## 2.2. plot all agri pop vs all pop sam scale
## 
## ratio
rat <- sum(SUM.vhlss.2010.agri$n)/sum(SUM.vhlss.2010.all$n)

par(mfrow=c(1,2))
par(mar=c(2,0,0,2.5))
barplot(rep(0,10), horiz = TRUE,
        xlim=c(-0.11,0), axes = FALSE,
        col="black", density=10)
axis(1, at=seq(0, -0.3, -0.05), labels=seq(0, 30, 5))
for (i in seq(0, -0.1, -0.05)){
  lines(c(i,i), c(0,11.7), lty=2, col="gray")
}

barplot(-SUM.vhlss.2010.agri[SUM.vhlss.2010.agri$m1ac2==1,]$freq*rat, horiz = TRUE,
        axes = FALSE, add=TRUE,
        col="black", density=10)

for (i in 1:11) {
  lines(-rep(SUM.vhlss.2010.all[SUM.vhlss.2010.all$m1ac2==1,]$freq[i],2), 
        c(i-1+(0.2*i), i+(0.2*i)), 
        col="red", lwd=2)
  lines(c(0, -SUM.vhlss.2010.all[SUM.vhlss.2010.all$m1ac2==1,]$freq[i]),
        rep((i-1+(0.2*i)+ i+(0.2*i))/2,2), col="red")
}


par(mar=c(2,2.5,0,0))
barplot(rep(0,10), horiz = TRUE,
        xlim=c(0,0.11), names.arg = labz, las=2, axes=FALSE,
        col="black", density=10)
axis(1, at=seq(0, 0.3, 0.05), labels=seq(0, 30, 5))
for (i in seq(0, 0.1, 0.05)){
  lines(c(i,i), c(0,11.7), lty=2, col="gray")
}
barplot(SUM.vhlss.2010.agri[SUM.vhlss.2010.agri$m1ac2==2,]$freq*rat, horiz = TRUE,
        names.arg = labz, las=2, axes=FALSE, add=TRUE,
        col="black", density=10)

for (i in 1:11) {
  lines(rep(SUM.vhlss.2010.all[SUM.vhlss.2010.all$m1ac2==2,]$freq[i],2), 
        c(i-1+(0.2*i), i+(0.2*i)), 
        col="red", lwd=2)
  lines(c(0, SUM.vhlss.2010.all[SUM.vhlss.2010.all$m1ac2==2,]$freq[i]),
        rep((i-1+(0.2*i)+ i+(0.2*i))/2,2), col="red")
}

dev.copy2eps(file="figures/VHLSSAgriDistAllRelative.eps", family="ComputerModern",
             width=7, height=3.7)

xx <- rbind(
  c(SUM.vhlss.2010.agri[SUM.vhlss.2010.agri$m1ac2==1,]$freq*100, 0),
  c(SUM.vhlss.2010.agri[SUM.vhlss.2010.agri$m1ac2==2,]$freq*100, 0),
  c(SUM.vhlss.2010.agri[SUM.vhlss.2010.agri$m1ac2==1,]$freq*100*rat, 0),
  c(SUM.vhlss.2010.agri[SUM.vhlss.2010.agri$m1ac2==2,]$freq*100*rat, 0),
  SUM.vhlss.2010.all[SUM.vhlss.2010.all$m1ac2==1,]$freq*100,
  SUM.vhlss.2010.all[SUM.vhlss.2010.all$m1ac2==2,]$freq*100,
  c(SUM.vhlss.2010.agri[SUM.vhlss.2010.agri$m1ac2==1,]$freq*rat,0)/SUM.vhlss.2010.all[SUM.vhlss.2010.all$m1ac2==1,]$freq*100,
  c(SUM.vhlss.2010.agri[SUM.vhlss.2010.agri$m1ac2==2,]$freq*rat,0)/SUM.vhlss.2010.all[SUM.vhlss.2010.all$m1ac2==2,]$freq*100)
colnames(xx) <- labz

require(xtable)
xtable(xx)
sum(c(SUM.vhlss.2010.agri[SUM.vhlss.2010.agri$m1ac2==1,]$freq*100*rat,
      SUM.vhlss.2010.agri[SUM.vhlss.2010.agri$m1ac2==2,]$freq*100*rat))


############ same but REGIONS as well 
## Agri people

SUM.vhlss.2010.agri.REG <- 
  vhlss.2010.agri %>%
  dplyr::group_by(tinh, m1ac2, big.age.group) %>%
  dplyr::summarise(n=sum(wt9), n.uW=n())  %>%
  dplyr::mutate(freq=n/sum(n), freq.uW = n.uW/sum(n.uW))

## All people (over 5)

SUM.vhlss.2010.all.REG <- 
  vhlss.2010 %>%
  dplyr::group_by(tinh, m1ac2, big.age.group) %>%
  dplyr::summarise(n=sum(wt9), n.uW=n())  %>%
  dplyr::mutate(freq=n/sum(n), freq.uW = n.uW/sum(n.uW))

Reg.vhlss.2010.summary <- full_join(SUM.vhlss.2010.all.REG, 
                         SUM.vhlss.2010.agri.REG, 
                         by=c("tinh"="tinh", "m1ac2"="m1ac2", "big.age.group"="big.age.group")) %>%
  group_by(tinh) %>%
  mutate(prop.farmers = n.y/sum(n.y))


require(tidyr)

Reg.farmers <- Reg.vhlss.2010.summary %>%
  select(tinh, m1ac2, big.age.group, prop.farmers) %>%
  unite(sex.age, m1ac2,big.age.group) %>%
  spread( prop.farmers, key=sex.age, drop=TRUE) %>%
  arrange(.[[7]])
par(mfrow=c(1,1))
par(mar=c(2.2,2.2,0.5,.2), xpd=TRUE)  
barplot(t(as.matrix(Reg.farmers[1:61,c(4,2,3,6,5,7)])), horiz = TRUE,
        col= c("gray20", "gray50", "gray70", "pink", "pink2", "pink4"), space=0.5,
        names.arg = unlist(Reg.farmers[1:61,1]), las=2, axes=FALSE)
axis(1)
legend(0.2, 97,legend=c("a","b","c","c","b","a"), fill = c("gray20", "gray50", "gray70", "pink", "pink2", "pink4"),
       horiz = TRUE,bty = "n", x.intersp=5)

dev.copy2eps(file="figures/VHLSSRegions.eps", family="ComputerModern",
             width=8, height=12)

sum(Reg.vhlss.2010.summary[Reg.vhlss.2010.summary$tinh==34,]$n.uW.y)
sum(Reg.vhlss.2010.summary[Reg.vhlss.2010.summary$tinh==34,]$n.uW.x)
mean(unlist(Reg.farmers[,7]), na.rm=TRUE)
mean(unlist(Reg.farmers[,4]), na.rm=TRUE)


sort(unique(unlist(Reg.farmers[1:61,1])))



## split into two and add province names

left_join(Reg.farmers[1:30,1], province.names, by = c("tinh" = "V1")) %>%
  select(V2) %>%
  unlist() %>%
  as.character() -> names1
sapply(names1, function(x) substr(x, 2, nchar(x)-9)) -> names1

par(mfrow=c(1,1))
par(mar=c(2.2,7.7,2.5,.2), xpd=TRUE)  

barplot(t(as.matrix(Reg.farmers[1:30,c(4,2,3,6,5,7)])), horiz = TRUE,
        col= c("gray20", "gray50", "gray70", "pink", "pink2", "pink4"), space=0.5,
        names.arg = names1, las=2, axes=FALSE)
axis(1)
legend(0., 51,legend=c("over 55", "25-54", "under 25", "under 25", "25-54", "over 55"),
       fill = c("gray20", "gray50", "gray70", "pink", "pink2", "pink4"),
       horiz = TRUE,bty = "n", x.intersp=1)
dev.copy2eps(file="figures/VHLSSRegions1.eps", family="ComputerModern",
             width=12, height=8)

##second one 
left_join(Reg.farmers[31:61,1], province.names, by = c("tinh" = "V1")) %>%
  select(V2) %>%
  unlist() %>%
  as.character() -> names2
sapply(names2, function(x) substr(x, 2, nchar(x)-9)) -> names2
names2[4] <- "Ha Noi"
names2[21] <- "Hai Phong"
par(mfrow=c(1,1))
par(mar=c(2.2,7.7,2.5,.2), xpd=TRUE)  

barplot(t(as.matrix(Reg.farmers[31:61,c(4,2,3,6,5,7)])), horiz = TRUE,
        col= c("gray20", "gray50", "gray70", "pink", "pink2", "pink4"), space=0.5,
        names.arg = names2, las=2, axes=FALSE)
axis(1)
c("over 55", "25-54", "under 25", "under 25", "25-54", "over 55")
legend(0., 51,legend=c("over 55", "25-54", "under 25", "under 25", "25-54", "over 55"),
       fill = c("gray20", "gray50", "gray70", "pink", "pink2", "pink4"),
       horiz = TRUE,bty = "n", x.intersp=1)
dev.copy2eps(file="figures/VHLSSRegions2.eps", family="ComputerModern",
             width=12, height=8)
