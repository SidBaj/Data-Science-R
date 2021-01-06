#EDx Micromasters Data Science
install.packages("gapminder")
install.packages("dslabs")
install.packages(tidyverse)
install.packages("tidytext")
install.packages("textdata")
install.packages('caTools')
install.packages('matrixStats')
install.packages("Rborist")
library(gapminder)
library(tidyverse)
library(ggplot2)
library(dslabs)
library(ggrepel)
library(ggthemes)
library(gtools)
library(dplyr)
library(rvest)
library(broom)
library(dslabs)
library(textdata)
library(lubridate)
library(tidytext)
library(Lahman)
library(HistData)
library(caret)
library(e1071)
library(maps)
require(caTools)
library(matrixStats)
library(Rborist)
library(randomForest)

#Course 2:Data Visulization with ggplot2
# Week1
#Basic Statistical Analysis to Understand the distribution
library(dslabs)
prop.table(table(heights$sex)) #Proportion
unique(heights$height) #Gives the unique values
table(unique(heights$height)) #Frequency table of unique values
length(heights$height) #Length
table(heights$height) #Simple Frequency table
mh <-filter(heights , sex=='Male') #Selecting heights of Males only
#Scaling the distribution of Male Heights
m <- scale(mh$height)
#Calculating proportion of values within 2 std of mean
v <- mean(abs(m)<2)
#Normal Distribution
n <- dnorm(mh$height,mean(mh$height),sd(mh$height))
plot(mh$height,n)
#Cummalative Distribution
c <- pnorm(mh$height,mean(mh$height),sd(mh$height))
plot(mh$height,c)
#Plotting both cdf and pdf on a graph
df <- data.frame(n,c,mh$height)
ggplot(data=df)+
  geom_line(mapping=aes(x=mh$height,y=n))+
  geom_line(mapping=aes(x=mh$height,y=c))
#Eg-To find % of people with height > 75in
1 -pnorm(75,mean(mh$height),sd(mh$height))

#Quantiles and the qnorm function
qu <- quantile(mh$height,0.5) #To find the median/mean value (observd)
qu1 <- qnorm(0.5,mean(mh$height),sd(mh$height)) #Predicted by CDF
#Getting the theoretical values of Quartiles
p <- c(0.25,0.5,0.75)
per <- quantile(mh$height,p)
per1 <- qnorm(p,mean(mh$height),sd(mh$height))
plot(per,per1)
abline(0,1)
#Comparing theoretical and practical values of percentiles
pr <- quantile(mh$height,seq(0.01,0.99,0.05)) #Observed
pr1 <- qnorm(seq(0.01,0.99,0.05),mean(mh$height),sd(mh$height))
plot(pr,pr1)
abline(0,1)

#Week 2
#Using ggplot2 on murders dataset: 1st Method
data(murders)
ggplot(data=murders)+
  geom_point(aes(x=population,y=(total/population)*(10^7),color=region),size=1.5)+
  geom_text(aes(x=population,y=(total/population)*(10^7),label=abb),nudge_x = 0.075,nudge_y = 0.075)+
  geom_line(aes(x=population,y=mean(total/population)*(10^7)),lty=2)+
  #scale_discrete_manual(aes(x=population,y=(total/population)*(10^7),guide=guide_legend(title = "Region of the US"),values=c("Orange","Blue","Green","Black")))+
  scale_x_log10()+
  scale_y_log10()+
  xlab("Logarithm of Population")+
  ylab("Logarithm of No of people died per 1Cr")+
  ggtitle("US Murders Dataset")

#2nd Method:
rate = intercept=log10((sum(murders$total)/sum(murders$population))*10^6)
p1 <- murders %>% ggplot(aes(x=population/10^6,y=total,label=abb,col=region))
p2 <- p1 +geom_point() + geom_hline(yintercept = mean(murders$total))
p3 <- p2 + geom_text_repel(color="Black")
p4 <- p3 + scale_x_log10() + scale_y_log10() + geom_abline(intercept = rate,lty=2)
p5 <- p4 + scale_color_discrete(name="Regions of US")
p6 <- p5 +xlab("Population") +ylab("Total Murders in the State")+ggtitle("US Gun Murders By State")
#Loading ggthemes package
install.packages("ggthemes")
library(ggthemes) #To change the theme
p7 <- p6 +theme_economist()
install.packages("ggrepel")
#Labels do not overlap

#Examples of the heights dataset Hist in ggplot
ht <-heights%>%
  filter(sex=="Female")%>%
  ggplot(aes(x=height))
ht5<- ht +geom_histogram(fill="Black",col="White")

#Density Plot
ht1 <-heights%>%
  filter(sex=="Female")%>%
  ggplot(aes(x=height))
ht4 <- ht1 +geom_density()

#QQ plot
fm <-filter(heights,sex=="Female")
prm <- c(mean(fm$height),sd(fm$height))
ht2 <- heights%>%
  filter(sex=="Female")%>%
  ggplot(aes(sample=height))
ht3 <- ht2 +geom_qq(dparams = prm) +geom_abline()

#How to add plots next to each other
install.packages("gridExtra")
library(gridExtra)
grid.arrange(ht3,ht4,ht5,ncol=3)

#Week3
#Intro to dplyr
#summarize()
su <-heights%>%
  filter(sex=="Female")%>%
  summarize(m=mean(height),s=sd(height),mi=min(height),mx=max(height))
#Using dot operator
su %>% .$m

#Using the group_by()
murders %>%
  group_by(region)%>%
  summarise(meanmurderrate=(sum(total)/sum(population))*10^6,meanpop=mean(population))

iris %>%
  group_by(Species)%>%
  summarise(Mean_Length=mean(Sepal.Length),Mean_Width=mean(Sepal.Width),Petal_Length=mean(Petal.Length),Petal_Width=mean(Petal.Width))%>%
  .$Mean_Length

#Using the arrange function()
mu <- murders
mu['DeathsperMillion'] <- (mu$total/mu$population)*(10^6)
mu %>% arrange(desc(DeathsperMillion))%>%head(10)

mtcars %>%
  arrange(desc(mpg,cyl)) %>% head()

iris %>%
  group_by(Species)%>%
  arrange(desc(Petal.Length)) %>% top_n(25)

murders %>%
  group_by(region)%>%
  arrange(region,desc(population)) 
summarise(meanpop=mean(population))

#Week4: Gapminder
#arrange(),facet_wrap(),geom_text(),reorder(),logit(),limit(),breaks()
gapminder %>%
  filter(year==2016)%>%
  select(life_expectancy,country)%>%
  arrange(desc(life_expectancy))

filter(gapminder,year %in% c(1962,2010))%>%
  ggplot(aes(y=life_expectancy,x=fertility,col=continent))+
  facet_wrap(.~year) +geom_point() #Using the facet function for multiple plots

pl2 <- filter(gapminder,year==2010)%>%
  ggplot(aes(x=life_expectancy,y=fertility,col=continent))+
  geom_point()

#Using the facet_wrap()
iris %>%
  ggplot(aes(x=Sepal.Length,y=Petal.Length,col=Species))+
  facet_wrap(.~Species) +geom_point()

mtcars %>%
  ggplot(aes(x=disp,y=mpg,col=cyl))+
  facet_wrap(.~cyl) + geom_point()+
  ggtitle("Miles per Gallon vs. Displacement")

#Fertility rate of India over the years
#Comparing gdp of different countries over time
gapminder %>%
  filter(country %in%c("India","China"))%>%
  ggplot(aes(x=year,y=population/10000000,col=country)) +geom_line()

#With text labels
countries = c("South Korea","Germany")
labels <- data.frame(country = countries, x = c(1975, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")

#Per Capita income of countries in $per day: Histogram
gapminder%>%
  ggplot(aes(x=(gdp/population)/365))+
  geom_histogram(binwidth = 1,fill="Grey",col="black")+
  scale_x_continuous(trans = "log2")+
  ggtitle("Per Capital GDP")

#Boxplot of per capita daily income stratified by region
p <- gapminder%>%
  filter(year==2010)%>%
  mutate(dpd = gdp/population/365,region=reorder(region,dpd,FUN=median))

p %>%ggplot(aes(y=dpd,region,fill=continent))+
  theme(axis.text.x = element_text(angle=90,hjust=1))+
  geom_boxplot()+geom_point()

#Side by side comparison of west and developing world
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
#Income distribution of West vs. Developing world
c1 <- gapminder %>% filter(year==1970 & !is.na(gdp)) %>% .$country
c2 <- gapminder%>% filter(year==2010 & !is.na(gdp)) %>% .$country
c3 <- intersect(c1,c2)  

gapminder %>%
  filter(year %in% c(1970,2010),country %in% c3)%>%
  mutate(dollar_per_day = (gdp/population/365),regn = ifelse(region %in% west,"West","Rest"))%>%
  ggplot(aes(y=dollar_per_day,region,fill=factor(year)))+
  geom_boxplot()+
  scale_y_continuous(trans="log2")+
  theme(axis.text.x = element_text(angle=90,hjust=1))

gm <- gapminder %>%
  filter(year %in% c(1970,2010) &country %in% c3)%>%
  mutate(dollar_per_day = (gdp/population/365),regn = ifelse(region %in% west,"West","Developing"))%>%
  ggplot(aes(y=dollar_per_day,region,fill=factor(year)))+
  geom_boxplot()+
  scale_y_continuous(trans="log2")+
  theme(axis.text.x = element_text(angle=90,hjust=1))

afr <- c("Eastern Africa","Northern Africa")

gap <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% afr ~"North Africa",
    .$region %in% c("Eastern Asia","South-Eastern Asia") ~ "Asia",
    .$region =="Southern Asia" ~"Southern Asia",
    .$region %in% c("Central America","South America","Caribbean") ~"Latin America",
    .$continent == "Africa" &.$region !="Northern Africa" ~"Subsaharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))
gap <- gap %>%
  filter(year == 2010 & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(country,group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))

gap %>% arrange(income)
gap %>% ggplot(aes(income, infant_survival_rate,label=country ,color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) +geom_point()

gap1 <- gapminder %>%
  filter(year==1970)%>%
  select(country,population,gdp,continent)%>%
  mutate(pop=log2(population))%>%
  arrange(desc(gdp))%>%
  top_n(20)

gap1%>%
  filter(!is.na(gdp))%>%
  mutate(country = reorder(country,gdp,FUN = mean))%>%
  ggplot(aes(gdp,country,fill=continent)) +geom_col()



gapminder %>%
  mutate(country=reorder(country,population,FUN=median))%>%
  ggplot(aes(population,country)) +geom_col()

heights %>% ggplot(aes(sex, height)) + geom_jitter(width = 0.2,alpha=0.4)

#Slope Chart
gapminder %>%
  filter(year %in% c(1970,2010),!is.na(life_expectancy),country %in% c("India","China","Pakistan","United States","France","Finland"))%>%
  ggplot(aes(y=life_expectancy,x=year,col=country))+geom_point()+geom_line()


t1 <-titanic_train %>%
  filter(!Fare==0 &Survived==0)%>%
  group_by(Survived)%>%
  ggplot(aes(x=Fare))+geom_boxplot()+scale_x_continuous(trans="log2")


t2 <-titanic_train %>%
  filter(!Fare==0 &Survived==1)%>%
  group_by(Survived)%>%
  ggplot(aes(x=Fare))+geom_boxplot()+scale_x_continuous(trans="log2")

titanic_train%>%
  filter(Fare > 0)%>%
  ggplot(aes(Survived, Pclass)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log10") +
  geom_jitter(alpha = 0.2,width = 0.2)

t <- titanic_train %>%
  filter(Pclass==3 &  Survived==0)

titanic%>%
  ggplot(aes(Age,y=..count..,fill=Survived))+geom_density(alpha=0.2)+facet_grid(Sex~Pclass)

stars%>%
  ggplot(aes(x=temp,y=magnitude,color=type))+geom_point()

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  filter(year %in% c(1880,2018))%>%select(temp_anomaly)

p <- temp_carbon %>%
  filter(!is.na(temp_anomaly))%>%
  ggplot(aes(x=year,y=temp_anomaly))+geom_line()

temp_carbon %>%
  filter(!is.na(ocean_anomaly),!is.na(land_anomaly),!is.na(temp_anomaly))%>%
  ggplot(aes(x=year,y=temp_anomaly))+geom_line(col='Black')+
  geom_line(aes(x=year,y=ocean_anomaly),col='Blue')+geom_line(aes(x=year,y=land_anomaly),col="Red")

greenhouse_gases %>%
  ggplot(aes(x=year,y=concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free")+
  geom_vline(xintercept = 1850)
ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

greenhouse_gases %>%
  filter(year==2000)%>%
  select(gas,concentration)

temp_carbon%>%
  filter(!is.na(carbon_emissions))%>%
  select(year,carbon_emissions)%>%
  ggplot(aes(x=year,y=carbon_emissions))+
  geom_line()

temp_carbon%>%
  filter(year %in% c(2014,1960))%>%
  select(carbon_emissions)

historic_co2%>%
  ggplot(aes(x=year,y=co2,col=source))+xlim(-3000,2018)+geom_line()+geom_vline(
    xintercept = 1850
  )
c <- gapminder$country
data("gapminder")
g1 <- gapminder %>%
  filter(country %in% c("India","Pakistan","China","United States","United Kingdom"),!is.na(lifeExp))%>%
  ggplot(aes(x=year,y=pop,col=country)) +geom_line()


#Course 3 Probability
#Discrete Variables probability
set.seed(1)
set.seed(1, sample.kind="Rounding") 
#Monte Carlo simulation for discrete variables/probabilities
#Without replacement
beads <- rep(c("Red","Blue"),c(20,80))
samp <- sample(beads,10)
re <- replicate(20000,sample(beads,10))
#With replacement
sa <- sample(beads,10,replace = TRUE)
#Permutation and Combinations
#Creating a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(Suit=suits,Numbers=numbers)
deck <- paste(deck$Numbers,deck$Suit)
#Creating different outcomes
king <- paste("King",suits) #Outcomes of drawing a king
mean(deck %in% king) #Probability of drawing a king
queen <- paste("Queen",suits) #Outcomes of drawing a Queen
mean(deck %in% queen) #Probability of drawing a Queen
install.packages("gtools")
library(gtools)
#Probability of drawing two consecutive kings
total <- permutations(52,2,v=deck)
k <- total[,1]
s <- total[,2]
prob <- mean(k %in% king & s %in% king)/mean(k %in% king)
#Birthday problem
#Probability of atleast 2 people sharing birthdays for a group of 50 people
bday <- replicate(1000,any(duplicated(sample(1:365,50,replace = TRUE))))
#Calculating the above probability for different group sizes
n <- seq(1:100)
dd <- function(n,B){
  f <- replicate(B,{
    b <- sample(1:365,n,replace = TRUE)
    any(duplicated(b))
  })
  mean(f)
}
plt <- sapply(n,dd)
plot(n,plt)
#Calculating the result for different values of monte carlo simulations(B)
B1 <- 10^seq(1,5,len=100)
comp_prob <- function(B1,n=22){
  f1 <- replicate(B1,{
    b1<- sample(1:365,n,replace = TRUE)
    any(duplicated(b1))
  })
  mean(f1)
  
}
sims <- sapply(B1,comp_prob)
plot(log10(B1),sims,type = "l") #Plotting the results vs. No of sims
#Monty Hall Problem
B2 <- 10000
switch <- replicate(B2,{
  doors <- as.character(1:3)
  prize <- sample(c("Car","Goat","Goat"))
  prize_door <- doors[prize=="Car"]
  my_pick <- sample(doors,1)
  show <- sample( doors[!doors %in% c(my_pick,prize_door)],1)
  switch <- doors[!doors %in% c(my_pick,show)]
  switch == prize_door
})
mean(switch)

#Example
#Odds of winning a game
outcome <- c(0,1)
b <- permutations(2,r=10,v=outcome,repeats.allowed = TRUE,set = FALSE)
mean(rowSums(b)>=6)

#Doing the same thing by 10,000 monte carlo simulations
#Doing this for different series lengths (from 1 to 25 by 2)
#Assuming the first game is won by the team
results <- function(n){
  rep <- replicate(10000,{
    samp <- sample(c(0,1),n-1,replace=T,prob = c(0.5,0.5))
    sum(samp)>=(n-1)/2
  })
  mean(rep)
}
n <- seq(1,25,2)
pr <- sapply(n,results)
plot(n,pr)
#Assignment questions EDx
library(gtools)
library(tidyverse)
j <- c("Jamaica", "Jamaica", "Jamaica")
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
oly <- replicate(10000,{
  k <- sample(runners,3)
  all(k %in% j)
})
r <- seq(2,12,1)
a <- function(r){
  b <- combinations(r,2)
  nrow(b)
}
b <- sapply(r,a)
esoph %>%
  filter(tobgp=="30+" & alch)%>%
  summarise(sum(ncases),sum(ncontrols))


#Continuous Variables probability
#Proportion of male student above 70.5 inches
library(dslabs)
pro <- function(n){
  heights %>%
    filter(sex=="Male")%>%
    summarise(mean(height<=n))
}
#Plotting proportions for different heights (CDF)
n <- seq(50,83,0.5)
pr <- sapply(n,pro)
plot(n,pr)
#Using the pnorm() function for CDF
ht <- heights %>% filter(sex=="Female")%>% .$height
ht <- sort(ht,decreasing = FALSE)
avg <- mean(ht)
s <- sd(ht)
p <- pnorm(ht,avg,s)
df <- data.frame(ht,p)
df%>%
  ggplot(aes(x=ht,y=p))+
  geom_point()
#Using dnorm() for PDF
ht <- heights %>% filter(sex=="Female")%>% .$height
ht <- sort(ht,decreasing = FALSE)
avg <- mean(ht)
s <- sd(ht)
d <- dnorm(ht,avg,s)
df <- data.frame(ht,d)
df%>%
  ggplot(aes(x=ht,y=d))+
  geom_point()
#Plotting both PDF & CDF on single graph
df1 <- data.frame(ht,p,d)
df1 %>%
  ggplot()+
  geom_point(aes(ht,p),color="Red")+
  geom_point(aes(ht,d))
#Using rnorm() for Monte-Carlo Simulation
r <- rnorm(length(ht),avg,s)
hist(r)
#Using qnorm() to get exact values for different quantiles
sw <- seq(0,1,0.025)
q <- qnorm(sw,avg,s)

#Probability of the casino losing money.
#Monte Carlo on Sampling model
sam <- replicate(10000,{
  s <- sample(c(-1,1),prob=c(9/19,10/19),size = 1000,replace = TRUE)
  sum(s)
  
})
mean(sam<=0)
d <- dnorm(sam,mean(sam),sd(sam))
df <- data.frame(d,sam)
df%>%
  ggplot(aes(df,..density..))+
  geom_line(aes(x=sam,y=d),color="Blue",lwd=1)+
  geom_histogram(aes(sam))

#Probability of winning on 10,000 bets on green using CLT
p_green <- 2/38
p_not_green <- 36/38
s <- sample(c(17,-1),size=10000,replace = TRUE,prob=c(p_green,p_not_green))

#Probability of winning on 10,000 bets on green using 10,000 Monte-Carlos
p_green <- 2/38
p_not_green <- 36/38
mcs <- replicate(10000,{
  s1 <- sample(c(17,-1),size=100,replace = TRUE,prob=c(p_green,p_not_green))
  mean(s1>0)
  
})
1 - pnorm(0,mean(mcs),sd(mcs))

se <- sqrt(44)*(abs(-0.25-1)*sqrt(0.2*0.8))
score <- sample(c(-0.25,1),prob=c(0.8,0.2),size=44,replace = TRUE)
m <- ((0*0.75)+(0.25))

set.seed(21)
b <- seq(0.25,0.95,0.05)
mct <- function(b){
  replicate(10000,{
    s <- sample(c(-0.25,1),prob=c(1-b,b),size=44,replace = TRUE)
    sum(s)
  })
}

#The BIG SHORT
#Sampling model for determining loan defaults
#Giving out 1000 loans of 200,000$ each & default rate = 2%
loans <- sample(c(-200000,0),size = 1000,prob = c(0.02,0.98),replace = TRUE)
exp_value <- 1000*((0.02)*(-200000)+(0.98)*0)
se <- sqrt(1000)*(abs(-200000)*sqrt(0.02*0.98))
#Probability for determining interest rates to minimise defaults
#Case1: Probability of loss as a function of different interest rates
rat <- seq(0,0.035,0.001)
default <- function(rat){
  rates <- replicate(10000,{
    sa <- sample(c(-200000,rat*180000),prob=c(0.02,0.98),size=1000,replace = TRUE)
    mean(sa)
  })
  mean(rates<0)
  
}
def_ault <- sapply(rat,default)
#Case2: Probability of profit as a function of different interest rates
payback <- function(rat){
  rates <- replicate(10000,{
    sa <- sample(c(-200000,rat*180000),prob=c(0.02,0.98),size=1000,replace = TRUE)
    mean(sa)
  })
  mean(rates>0)
  
}
pay_back <- sapply(rat,payback)
#Plotting the results
data.frame(rat,def_ault,pay_back)%>%
  ggplot()+
  geom_point(aes(rat,def_ault),color="Red")+
  geom_point(aes(rat,pay_back),color="Green")


#Insurance Problem!!
set.seed(29)
profits <- function(premium=3268){
  r <- replicate(10000,{
    p <- 0.015 + sample(seq(-0.01,0.01,length=100),1)
    s <- sample(c(-150000,premium),size=1000,prob=c(p,1-p),replace = TRUE)
    sum(s)
  })
  mean(r< -10^6)
}
prft <- sapply(seq(3200,3250,1),profits)
df <- data.frame(seq(3200,3250,1),prft)

#Course 4: Inference and Modeling
#Monte-Carlo Simulation to confirm CLT
n <- seq(10,200,10)
p <- 0.6
props <- function(n=100){
  pr <- replicate(1000,{
    s <- sample(c(0,1),size=n,replace=TRUE,prob=c(1-p,p))
    mean(s)-0.6
  })
  pr
}
se_sim <- sapply(n, props)
se_calc <- sqrt(p*(1-p))/sqrt(n)
data.frame(n,se_sim,se_calc)%>%
  ggplot()+
  geom_point(aes(x=n,y=se_sim),color="Red")+
  geom_point(aes(x=n,y=se_calc),color="Blue")

#Monte-Carlo Simulation of Confidence Intervals
inside <- replicate(10000,{
  s <- sample(c(0,1),size=100,replace=TRUE,prob=c(0.2,0.8))
  m <- mean(s)
  std <- sqrt(m*(1-m)/100)
  between(0.8,m-(1.96*std),m+ (1.96*std))
})

#Creating 95% confidence intervals for the spread for 2016 US polls
#How many of those intervals actually got the correct value i.e 2.1%
polls <- polls_us_election_2016 %>%
  filter(state=='U.S.' & enddate>=2016-10-31)%>%
  group_by(pollster)%>%
  filter(n()>6)%>%
  mutate(spread=rawpoll_clinton-rawpoll_trump)%>%
  mutate(me=2*1.96*sqrt(rawpoll_clinton*(100-rawpoll_clinton)/samplesize))%>%
  mutate(lower=spread-me,upper=spread+me)%>%
  mutate(gotit=ifelse(2.1>=lower & 2.1<=upper,TRUE,FALSE))%>%
  select(spread,samplesize,pollster,grade,me,lower,upper,gotit)

#Plot of all the 13 pollster's histograms of spreads
polls %>%
  group_by(pollster)%>%
  ggplot()+
  geom_point(aes(x=spread,y=pollster))+
  geom_vline(aes(xintercept=2.1))
#Aggreate of all the polls to calculate the spread(POLL AGGREGATION)
agg <- polls %>% ungroup() %>% select(samplesize,spread,pollster)
d_hat <- agg %>% summarise(d=sum(spread*samplesize)/sum(samplesize))%>% .$d  
p_hat <- (100+d_hat)/2
se_d <- 2*qnorm(0.975)*sqrt(p_hat*(100-p_hat)/sum(agg$samplesize)) 
#Aggregate by pollster
poll_agg <- polls %>%
  group_by(pollster)%>%
  summarise(se=2*sqrt(p_hat*(100-p_hat)/sum(samplesize)),d=sum(spread*samplesize)/sum(samplesize))%>%
  mutate(p=(100+d)/2)
#Heights dataset assgn
males <- heights %>%filter(sex=='Male')%>% .$height
male_sample <- sample(males,size=50,replace = TRUE)  

#Bayesian/Posterior Probability
#The Observed Poll Data ~(d,sigma)
polls <- polls_us_election_2016 %>%
  filter(state=='U.S.' & enddate>="2016-10-31" &
           (grade %in% c("A+","","A","A-","B+") | is.na(grade)))%>%
  mutate(spread=rawpoll_clinton/100-rawpoll_trump/100)
one_poll_per_pollster <- polls %>% group_by(pollster)%>%
  filter(enddate==max(enddate))%>%
  ungroup()
results <- one_poll_per_pollster %>%
  summarise(avg=mean(spread),se= sd(spread)/sqrt(length(spread)))
#Historical Data ~(mu=0,tau=0.035)
#Posterior mean = B*mu + (1-B)*Y; B = sigma^2/sigma^2+tau^2
mu <- 0
tau <- 0.035
Y <- results$avg
sigma <- results$se
B <- sigma^2/(sigma^2 + tau^2)
pos_mean <- (B*mu)+(1-B)*Y
pos_se <- sqrt(1/(1/sigma^2+1/tau^2))
#95% credible interval
pos_mean + c(-1.96,1.96)*pos_se
1-pnorm(0,pos_mean,pos_se)
#Predicting the Electoral college
results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", "state") &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))
results <- results %>% arrange(desc(abs(avg)))
results <- left_join(results,results_us_election_2016,by="state")
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))
#Monte-Carlo Simulation of Election Night Results with general bias of 3%
g_bias <- 0.03
Clinton_EV <- replicate(1000,{
  results %>% mutate(sigma=sqrt(sd^2/n+g_bias^2),
                     B = sigma^2/(sigma^2+tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt(1/(1/tau^2+1/sigma^2)),
                     simulated_result <- rnorm(length(posterior_mean),posterior_mean,posterior_se),
                     clinton=ifelse(simulated_result>0,electoral_votes,0))%>%
    summarize(clinton=sum(clinton))%>%
    .$clinton+7
})
mean(Clinton_EV>269)

#Chi-Square Test
totals <- research_funding_rates%>%
  select(-discipline)%>%
  summarise_all(funs(sum))%>%
  summarise(yes_men=awards_men,no_men=applications_men-awards_men,
            yes_women=awards_women,no_women=applications_women-awards_women)
#Creating a two by two contigency table
# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
#Cross-Contingency table
cross_tab <- tibble(awarded = c("no", "yes"),
                    men = c(totals$no_men ,totals$yes_men),
                    women = c(totals$no_women , totals$yes_women))

#Performing the chi_sq test
cross_tab%>%select(-awarded)%>%chisq.test()
odds_men <- cross_tab$men[2]/cross_tab$men[1]
odds_women <- cross_tab$women[2]/cross_tab$women[1]

#Brexit polling assignment
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)
brex <- brexit_polls %>% mutate(p_hat=(1+spread)/2)
june_polls <- brex%>%
  mutate(se_x_hat<- sqrt(p_hat*(1-p_hat)/samplesize),sp=2*se_x_hat)%>%
  mutate(l=spread-qnorm(0.975)*sp,h=spread+qnorm(0.975)*sp,hit=ifelse(-.038>=l & -.038<=h,TRUE,FALSE))%>%
  select(poll_type,hit)

june_polls %>% group_by(poll_type)%>%summarise(N=sum(samplesize),spread=sum(spread*samplesize)/N,p_hat=(1+spread)/2)
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

##Course 5## Data Wrangling
##Reshaping data 
#gather(),spread(),seperate(),unite()
dt <- read.csv("C:\\Users\\lalit\\Downloads\\datasets_180_408_data.csv")
gap <- gapminder %>% select(country,year,gdpPercap)
wid <- gap %>% spread(key=year,value=gdpPercap)
tidy <- wid %>% gather("year","gdpPerCap",'1952':'2007')

age_group <- c(20,30,40,50)
my_time <- c(3,7,8,9)
my_participants <- c(25,32,21,63)
your_time <- c(5,6,3,1)
your_participants <- c(45,33,46,76)
dataf <- data.frame(cbind(age_group,my_time,my_participants,your_time,your_participants))

co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

co2_tidy <- co2_wide %>% gather(month,co2,-year)
tmp <- gather(admissions,key,value,admitted:applicants)

data(admissions)
dat <- admissions %>% select(-applicants)
tmp2 <- tmp %>% unite(column_name,c(key,gender))

#Joining Data ## left_join(),right_join(),inner_join(),full_join(),semi_join(),anti_join()
# Set Operators ## intersect(),union(),setdiff(),setequal()
t1 <- mtcars[1:10,]
t2 <- mtcars[8:20,]
rownames(intersect(t1,t2))

#Example##
f1 <- c(2,3,4)
f2 <- c(3,4,5)
full_join(f1,f2)

##Example Dataset ##
install.packages("Lahman")
library(Lahman)
top <- Batting %>% filter(yearID==2016)%>%top_n(10,HR)
hitt <- AwardsPlayers %>% filter(yearID==2016)

#Web Scraping ##
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
page <- read_html(url)
t <- html_nodes(page,"table")
t <- t[2]
d <- html_table(t)
d <- d %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
#Example#
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
guacamole <- list(recipe, prep_time, ingredients)
#Assignment##
urrl <- "http://www.stevetheump.com/Payrolls.htm"
dt <- read_html(urrl)
nd <- html_nodes(dt,"table")
html_text(nd[8])
tabl<- html_table(nd[8])
tab_1 <- html_table(nd[[10]])
tab_2 <- html_table(nd[[19]])
tab_1 <- tab_1[2:30,]
tab_2 <- tab_2[2:31,]
tab_2 <- tab_2 %>% setNames(c("Team","Payroll"))

#Example2#
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
ht <- read_html(url)
tbls <- html_nodes(ht,"table")
n <- html_table(tbls[5],fill=TRUE)

## ##
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
tab_1<- html_table(nodes[[10]])
tab_1 <- tab_1[2:31,2:4]
tab_1 <- setNames(object=tab_1,c("Team","Payroll","Average"))
tab_2<- html_table(nodes[[19]])
tab_2 <- tab_2[2:31,] %>% setNames(c("Team","Payroll","Average"))

#String Processing basics##
#Load the US Murders Dataset from the webpage##
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]]%>%
  setNames(c("state", "population", "total", "murder_rate"))

commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))
murders_raw <- murders_raw %>% 
  mutate(population=str_replace_all(population,",",""),total=parse_number(total))

murders_new <- murders_raw %>% mutate_at(2:3, parse_number)

#String Processing ## Part2##
library(dslabs)
data("reported_heights")
not_defined <- function(x,smallest = 50, largest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > largest
  ind
}

problems <- reported_heights %>% 
  filter(not_defined(height)) %>%
  .$height
length(problems)

df <- reported_heights %>% mutate(ht=suppressWarnings(as.numeric(height))) %>%
  filter(is.na(ht) | ht > 84 | ht < 50) %>% .$height

##Regex## str_detect(),str_subset(),str_replace(),str_rempve()
str_subset(reported_heights$height,"cm")
str_view(reported_heights$height,"cm|feet")
str_subset(reported_heights$height,"\\d")
str_detect(reported_heights$height,"inches") %>% sum()

##Regex## Anchors and quantifiers##
##Anchors ^ start $end ##
pattern <- "^\\d{1,2}$"
m <- c("23","1","456")
p2 <- "^(\\d)(')(\\d)$"
h1 <- c("5'8","6'2","5'1","3'4")
str_replace(h1,p2,replacement ="\\1 \\2" )
str_extract(h1,p2)

##Additional Quantifiers ##
p4 <- "^[4-7]\\s*'\\s*\\d{1,2}$"
pat <- "^([4-7])\\s*[.|,|\\s+]\\s*(\\d*)$"
problems_solved <- df %>% str_replace("feet|ft|foot","'")%>%
  str_replace("inches|in|\"","") %>%
  str_replace(pat,"\\1'\\2") %>%
  str_remove_all(" ")%>%
  str_subset(p4)

#Pattern with groups () , str_match(), str_match(),str_extract()
yes <- c("5,6","6,8")
pat <- "^([4-7])\\s*[.|,|\\s+]\\s*(\\d*)$"
str_subset(df,pat)
str_replace(df,pat,"\\1'\\2")
#extract(),separate(),str_trim()
pr <- data.frame(x=problems_solved)
pr %>% separate(x,c("feet","inches"),sep="'")
pr %>% extract(x,c("feet","inches"),regex ="(\\d)'(\\d*)")

##Renaming with recode()##
names <- USArrests %>% filter(str_length(rownames(USArrests))>12) %>% rownames(USArrests)
new_names <- recode(names,"Massachusetts"='MS')                                                                   


##Date and Time Mining##
#Date#
ymd_hms(now())
dates <- sample(polls_us_election_2016$startdate,10) %>% sort()
month <- months(dates)
day(dates)
year(dates)
#Time
Sys.time()
now()
now() %>% minute()
now()%>%second()
now()%>%hour()

##TextMining## unnest_tokens()
install.packages("tidytext")
install.packages("textdata")
library(tidytext)

#Trump twitter text analysis##

data("trump_tweets")
##Where did the tweets come from?##
trump_tweets %>% count(source) %>% arrange(desc(n))
##Creating the dataset##
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)
#Visualizing the dataset##
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

##unnest_token()##
example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example %>% unnest_tokens(wrd,text,token="sentences") #For breaking into sentences
example %>% unnest_tokens(wrd,text,token="words") ##For breaking into sentences##

#LookaheadRedex## (?=),(?!)
strng <- c("aqic","aqrr")
pat <- "q(?=i)"
str_subset(strng,pat)
pat1 <- "q(?!i)"
str_subset(strng,pat1)

##Lookbehind## (?<=),(?<!)
pat2 <- "(?<=q)s"
str_subset("ghqs",pat2)

pat4 <- "(?<!a)w"
str_subset(c("eraw","adswwfdg"),pat4)

##grep(),grepl(),sub(),gsub()

##Trump tweets text mining##
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
tweet_words <- campaign_tweets %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word,text,token="regex",pattern=pattern)%>%
  filter(!word %in% stop_words$word & !str_detect(word,"^\\d+$")) %>% mutate(word=str_replace(word,"^'",""))
##Getting the top 10 words by source##Android/iPhone
sourcetweets <- tweet_words %>% count(word,source) %>% arrange(desc(n))
#Given a word, how many time was it tweeted from an iPhone and an Android phone##
wordsourcewise <- sourcetweets %>% spread(key = source,value=n,fill=0)
##1.Given a word, what were the odds that it was tweeted from an Android phone as opposed to an iPhone##O
##2. Given an android phone, how likely was it that the the respective word was tweeted as opposed to other words##OR
android_words_odds <- wordsourcewise %>% filter(Android != 0 & iPhone !=0) %>% mutate(Anodds=Android/iPhone) %>%
  mutate(Anoddsratio = (Android/iPhone)/( (sum(Android)-Android)/ (sum(iPhone)-iPhone) )     )  %>%
  arrange(desc(Anoddsratio))

iphone_words_odds <- wordsourcewise %>% filter(Android != 0 & iPhone !=0) %>% mutate(iodds=iPhone/Android) %>%
  mutate(ioddsratio = (iPhone/Android)/( (sum(iPhone)-iPhone)/ (sum(Android)-Android) )     )  %>%
  arrange(desc(ioddsratio))
#Sentiment analysis of the tweets##
sentiments
get_sentiments("bing")
get_sentiments("nrc") %>% select(word,sentiments)

#Odds of sentiments according to the source## Android vs. iPhone##
nrc <- get_sentiments("nrc") %>% select(word,sentiment)
sentiment_counts <- tweet_words %>% left_join(nrc,by="word") %>% count(source,sentiment) %>% spread(source,n) %>% 
  filter(sentiment != "none") %>% 
  mutate(Android_odds = Android/iPhone, Word_odds_android = Android/(sum(Android)-Android)) %>% arrange(desc(Word_odds_android))

#Odds Ratio##
library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

#Visualization##
log_or %>% mutate(sentiment=reorder(sentiment,log_or),) %>% ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 

#Important datetime functions()##
#weekdays(),rounddate(),month()

##Datetime assessment##
weekdays(brexit_polls$enddate) %>% table()
data("movielens")
movielens %>% mutate(date = year(as_datetime(timestamp))) %>% count(date) %>% arrange(desc(n))

#Project Gutenbreg Assessment##
install.packages("gutenbergr")
library(gutenbergr)
words <- gutenberg_download(1342) %>% unnest_tokens(word,text) %>% filter(!word %in% stop_words$word & !str_detect(word,"\\d"))
afinn <- get_sentiments("afinn") 
words %>% inner_join(afinn,by="word") %>% filter(value==4) %>% nrow()

##Project Hurricane Maria##
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
system("cmd.exe", input = paste("start", fn))
txt <- pdftools::pdf_text(fn)
x <- txt[9] %>% str_split(pattern = "\n")
s <- x[[1]] %>% str_trim()
str_which(s,"SEP")
str_split(s[2],"\\s+",simplify = T) %>% length()
str_which(s,"Total")
str_count(s,"\\d+")
df <- s %>% str_split("\\s+",simplify = T)
df <- df[1:34,1:5] %>% as.data.frame(df)
name <- c("SEP","a","b","c","d")
colnames(df) <- name
df <- df[3:34,1:5]
df <- df %>% filter(as.numeric(SEP) < 31) %>% mutate(SEP=as.character(SEP))
str_split_fixed(str_remove_all(s,"[^\\d\\s]"),"\\s+",n=6)[,1:5]
df %>% ggplot()+geom_point(aes(x=SEP,y=a),color="Blue") + geom_point(aes(x=SEP,y=b),color="Green") + 
  geom_point(aes(x=SEP,y=c),color="Red") + geom_vline(aes(xintercept = 20))

##Course 7##
##Regression##
?Teams()
head(Teams)
Teams %>% filter(yearID %in% 1961:2001) %>% mutate(AB=X2B/G,R=X3B/G) %>% select(AB,R) %>% cor()

##Galtons Dataset##
data(galton)
library(galton)
data("GaltonFamilies")
set.seed(1989)
female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
r <- cor(female_heights$mother,female_heights$daughter)
slope <- r*sd(female_heights$daughter)/sd(female_heights$mother)

##Linear Models#
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
##RSS##
rss <- function(beta0,beta1,data){
  resid <- galton_heights$son - (beta0 + beta1*galton_heights$father)
  return(sum(resid^2))
}
b = seq(0,1,len=nrow(galton_heights))
results <- data.frame(beta1=b,rss=sapply(X=b,FUN=rss,beta0=25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID)%>%
  summarise(s=mean(singles),b=mean(bb))%>%
  select(playerID,s,b)

df <- inner_join(bat_02,bat_01,by="playerID")

##do(), broom()##
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)
dat %>% group_by(HR) %>% do(tidy(lm(R~BB,data=.),conf.int=TRUE)) %>%
  filter(term=="BB")%>%ggplot(aes(x=HR,y=estimate,ymin=conf.low,ymax=conf.high)) + geom_point()+geom_errorbar()

##Excercise##
set.seed(1)
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

##Exercise##
set.seed(1)
t <- Teams %>%filter(yearID %in% 1961:2018)%>% group_by(yearID) %>% select(HR,BB,R) %>%
  do(tidy(lm(R~BB+HR,data=.),conf.int=T)) %>% ungroup() %>% filter(term=="BB") %>%
  select(yearID,term,estimate)
t %>% ggplot(aes(x=yearID,y=estimate)) +geom_point()+geom_smooth(method="lm")

##Assessment 1## Linear Regression
model <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G,runs_per_game=R/G,homeruns_per_game=HR/G)  %>% 
  lm(avg_attendance~runs_per_game+homeruns_per_game+W+yearID,data=.)

data <- Teams %>% filter(yearID %in% 2002) %>% mutate(homeruns_per_game=HR/G,runs_per_game=R/G,ag=attendance/G) %>% 
  select(homeruns_per_game,runs_per_game,W,yearID,ag)

attendance <- data.frame(predicted = predict(model,data),actual= data$ag)
##Scatterplot between actual and predicted attendance per game in 2002##
attendance %>% ggplot(aes(x=actual,y=predicted)) + geom_point() + geom_smooth(method="lm")

##Correlation is not causation##
admissions %>% group_by(gender) %>% 
  summarise(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>% 
  do(tidy(chisq.test(.)))
##Monte Carlo Simulations of p-value##
x <- rnorm(100,10,1)
y = rnorm(100,15,1)
r <- replicate(1000000,{
  sample_x <- sample(x,size=20,replace = TRUE)
  sample_y <- sample(y,size=20,replace=TRUE)
  p <- tidy(lm(sample_x~sample_y),conf.int=T) %>%
    filter(term=="sample_y")%>% select(p.value)
  p
})

##Confounding Assessment##
library(dslabs)
data("research_funding_rates")
research_funding_rates
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")

## Course 8## Machine Learning ##
##Heights Dataset##
x <- heights$height
y <- heights$sex
i <- createDataPartition(y,times=1,p=0.5,list=FALSE)
train_set <- heights[i,]
test_set <- heights[-i,]

y_hat <- sample(c("Male","Female"),nrow(test_set),replace = TRUE) %>%
  factor(levels=levels(test_set$sex))##Random Sampling
mean(y_hat==test_set$sex)

y_HAT <- ifelse(train_set$height>65,"Male","Female") %>%
  factor(levels=levels(test_set$sex))

sequence <- seq(61,70)
acc_train <- function(x){
  y_hat <- ifelse(train_set$height>x,"Male","Female")
  mean(y_hat==train_set$sex)
}

accuracy_train <- sapply(sequence,acc_train) 
acc_test <- function(x){
  y_hat <- ifelse(test_set$height>x,"Male","Female")
  mean(y_hat==test_set$sex)
}
accuracy_test <- sapply(sequence,acc_test) 
ac <- data.frame(train_acc=accuracy_train,test_acc=accuracy_test,cutoff=sequence)

##Confusion Matrix##
##Recall = TP/(TP+FN) , Precision = TP/(TP+FP)
confusionMatrix(data=y_HAT,reference = test_set$sex)
F_meas(data=y_HAT,reference = factor(test_set$sex))

##F-1 Score ## 
acc_train <- function(x){
  y_hat <- ifelse(train_set$height>x,"Male","Female")
  mean(y_hat==train_set$sex)
}

accuracy_train <- sapply(sequence,acc_train) 
acc_test <- function(x){
  y_hat <- ifelse(test_set$height>x,"Male","Female")
  mean(y_hat==test_set$sex)
}
accuracy_test <- sapply(sequence,acc_test) 
ac <- data.frame(train_acc=accuracy_train,test_acc=accuracy_test,cutoff=sequence)

cutoff <- seq(61, 70)
F_1 <- function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
}

library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

new_dat <- dat %>% mutate(y_hat=ifelse(type=="online","Male","Female"),y_hat=factor(y_hat))

library(caret)
set.seed(2, sample.kind="Rounding")
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y = iris$Species
test_index <- createDataPartition(iris,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
trainsp <- factor(train$Species,levels = c("virginica","versicolor"))

sw <- seq(min(test$Sepal.Width),max(test$Sepal.Width),by=0.1)
sl <- seq(min(test$Sepal.Length),max(test$Sepal.Length),by=0.1)
pw <- seq(min(train$Petal.Width),max(train$Petal.Width),by=0.1)
pl <- seq(min(train$Petal.Length),max(train$Petal.Length),by=0.1)

SL <- function(x){
  y_hat <- ifelse(train$Petal.Length>x,'virginica', 'versicolor')
  y_hat <- factor(y_hat,levels = c("virginica","versicolor"))
  mean(y_hat==trainsp)
}
S2 <- function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica","versicolor")
  y_hat <- factor(y_hat,levels=c("virginica","versicolor"))
  mean(y_hat==trainsp)
}
swidth <- sapply(sw,SL) %>% max()
slength <- sapply(sl, SL) %>% max()
pwidth <- sapply(pw, SL) %>% max()
plength <- sapply(pl, SL) %>% max()  

y_hat <- ifelse(test$Petal.Width>1.6 | test$Petal.Length>4.7,'virginica','versicolor')

set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)


Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


##Linear Regression##
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

model <- train_set %>% lm(son~father,data=.)
y_hat <- predict(model,test_set)

set.seed(1) # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

rmse <- function(n) 
  replicate(n,{
    y <- dat$y
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat[-test_index,]
    test_set <- dat[test_index,]
    model <- lm(y~x,data=train_set)
    y_hat <- predict(model,test_set)
    sqrt(mean((y_hat- test_set$y)^2))
    
  })  

f <- function(n){
  me <- mean(rmse(n))
  se <- sd(rmse(n))
  l <- list(me,se)
  return(l)
}

n <- c(100)
sapply(n,f)

##Logistic Regression## glm(family='binomial')
heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point()

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)

heights %>% 
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean(sex == "Female")) %>%
  ggplot(aes(x, prop)) +
  geom_point() + 
  geom_abline(intercept = lm_fit$coef[1], slope = lm_fit$coef[2])

glm_fit <- train_set %>% mutate(y=as.numeric(sex=='Female')) %>%
  glm(y~height,data=.,family='binomial')

p_hat_logit <- predict(glm_fit,newdata=test_set,type='response')
y_hat_logit <- ifelse(p_hat_logit>0.5,"Female","Male") %>% factor()

set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1=z, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
delta <- seq(0, 3, len = 25)
res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit_glm <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat_glm <- ifelse(predict(fit_glm, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat_glm == dat$test$y)
  confusionMatrix(data=y_hat_glm,reference = dat$test$y)$overall['Accuracy']
})
qplot(delta,res)

##Bin Smoothing## ksmooth()
span <- 7
fit <- with(polls_2008,ksmooth(day, margin, x.points = day, kernel="normal", bandwidth =span))
polls_2008 %>% mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") + 
  geom_line(aes(day, smooth), color="red")

#Local Regression## loess()
library(purrr)
library(pdftools)
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% mutate(smooth = predict(fit, as.numeric(date)), day=yday(date),year=as.character(year(date))) %>%
  ggplot() +
  geom_line(aes(day, smooth,col=year), lwd = 2)

##Matrices## matrix(),dim(),t()
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)

x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]
grid <- matrix(x[3,], 28, 28)
image(1:28, 1:28, grid)

# flip the image back
image(1:28, 1:28, grid[, 28:1])

#Summing row values in matrix rowsums() rowSd()
avg <- rowSums(x)
rowMeans(x)
data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")
apply(x,1,sd)
sds <- apply(x,2,sd)
##Filtering matrices on conditions## 
colsd <- apply(x,2,sd)
new_x <- x[,colsd>60]
##sweep() to perform rowise or colwise ops
mymat <- matrix(data=c(1:10),5,2)
sweep(mymat,2,colSums(mymat)) %>% sweep(2,1.581139,FUN="/")

x <- matrix(rnorm(1000),100,10)
dim(x)[1]
x <- sweep(x,1,1:nrow(x),FUN="+")
x <- x+ seq(nrow(x))

class(mnist_27)

#KNN Model## dist() in used to find distance b/w points and predictors ##knn3()
if(!exists("mnist")) mnist <- read_mnist()
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]
distance <- dist(x)
as.matrix(distance)[1:3,1:3]
##Example##
library(dslabs)
data(tissue_gene_expression)
tis <- as.matrix(dist(tissue_gene_expression$x))
image(tis)

##KNN Example## knn3()
x=as.matrix(mnist_27$train[,2:3])
y = mnist_27$train$y
knn_model <- knn3(y~x_1+x_2,k=5,data=mnist_27$train)
y_hat_knn <- predict(knn_model,mnist_27$test,type = "class")
confusionMatrix(data=y_hat_knn,reference = mnist_27$test$y)

##Sample example on logistic##
log_model <- glm(y~.,data=mnist_27$train,family = "binomial")
p_hat <- predict(log_model,mnist_27$test,type="response")
y_hat <- ifelse(p_hat>0.5,7,2) %>% factor()
confusionMatrix(data=y_hat,reference = mnist_27$test$y)

##KNN Example##
set.seed(1,sample.kind = "Rounding")
y <- heights$sex
x <- heights$height
md <- function(){
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

mydata <- md()
neigh <- seq(1,101,3)

f1 <- sapply(neigh,function(d){
  kn <- knn3(y~x,data=mydata$train,k=d)
  yy <- predict(kn,mydata$test,type="class")
  f1 <- F_meas(data=yy,reference = mydata$test$y)
  return(f1)
})

##Another KNN Example###
set.seed(1,sample.kind = "Rounding")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
md <- function(){
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

mydata <- md()
neigh <- seq(1,11,2)

f1 <- sapply(neigh,function(d){
  kn <- knn3(y~.,data=mydata$train,k=d)
  yy <- predict(kn,mydata$test,type="class")
  acc <- mean(yy== mydata$test$y)
  return(acc)
})

kn <- knn3(y~.,data=mydata$train,k=1)
yy <- predict(kn,mydata$test,type="class")
acc <- mean(yy,reference = mydata$test$y)
return(acc)

##Cross Validation## colttest()
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()
x_subset <- x[ ,statistically_significant]
##t-test()##
#install.packages("BiocManager")
#BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y) ## Column-wise p-value
statistically_significant <- which(tt$p.value<0.01)
##Running the predictions with significant predictors##
splitrule <- trainControl(method = "cv",number = 10)
fit <- train(x_subset, y,method='glm',trainControl=splitrule)
fit1 <- train(x_subset, y,method='glm',trControl=splitrule)
ggplot(fit)

##Tuning##
train(tissue_gene_expression$x,tissue_gene_expression$y,method='knn',tuneGrid=data.frame(k=seq(1,7,2)),trControl=trainControl(verboseIter=T,number=17,method = "cv"))


##Bootstrapping##
library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)
x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

##Estimating 75th quantile by 10,000 MCS##
set.seed(1,sample.kind = "Rounding")
data <- replicate(10000,{
  q <- rnorm(100,0,1)
  return(quantile(q,0.75))
})
##Bootstrap sample##
set.seed(1,sample.kind = "Rounding")
y <- rnorm(100,0,1)
bots <- createResample(y,10000)

ss <- sapply(bots,function(i){
  qnt <- quantile(y[i],0.75)
  qnt
})

##Naive Bayes##
library("caret")
data("heights")
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

# Estimating averages and standard deviations
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params

# Estimating the prevalence
pi <- train_set %>% summarize(pi=mean(sex=="Female")) %>% pull(pi)
pi <- 0.5

# Getting an actual rule
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))
jd <- data.frame(height=x,given_male_p_height=f0,given_female_p_height=f1,given_height_p_female=p_hat_bayes)
jd %>% ggplot(aes(x=height,y=p_hat_bayes))+geom_line() + geom_point()
predictions <- ifelse(p_hat_bayes>0.5,"Female","Male") %>% factor()
sensitivity(data=predictions,reference = test_set$sex)

##QDA and LDA Example##
data("tissue_gene_expression")

# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
df <- data.frame(x,y)

model <- train(y~.,data=df,method="lda",preProcess="center")
model$finalModel

t(model$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

set.seed(1993) #set.seed(1993, sample.kind="Rounding") if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

modelq <- train(y~.,data=df,method="qda")

t(modelq$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
df <- data.frame(x,y)
model <- train(y~.,data=df,method="lda",preProcess="center")

##Regression Trees## rpart() package()
data("olive")
olive <- select(olive, -area)

# Predict region using KNN
library(caret)
fit <- train(region ~ .,  method = "rpart", 
             tuneGrid = data.frame(cp= seq(0, 0.05, len=25)), 
             data = olive)


d <- knn3(y~x,tissue_gene_expression,k=3)
train(tissue_gene_expression$x,tissue_gene_expression$y,method='knn',tuneGrid=data.frame(k=seq(1,7,2)))

##Using rpart() package##
data("polls_2008")
train_rpart <- rpart(margin~.,data=polls_2008,control=rpart.control(cp=0)) #rpart package##
polls_2008 %>% mutate(y_hat=predict(train_rpart)) %>%
  ggplot()+geom_point(aes(day,margin))+geom_line(aes(day,y_hat),col="Red")
rmse <- sqrt(mean((predict(train_rpart,polls_2008)-polls_2008$margin)^2))

tr <- train(margin~.,data=polls_2008,method="rpart",tuneGrid=data.frame(cp=seq(0,0.1,len=20)),trControl=trainControl(method = "cv",verboseIter = T),metric="RMSE",maximize=F)

##Another example## rpart() and train()##
##getModelinfo() to study tuning parameter##
mtc <- select(mtcars,c("mpg","hp","wt","qsec"))
tunn <- train(mpg~.,data=mtc,method="rpart",tuneGrid=data.frame(cp=0.02),trControl=trainControl(method = "cv"))
modl <- mtc %>% lm(mpg~.,data=.)
RMSE(predict(modl,mtc),obs=mtc$mpg)

rtree <- rpart(mpg~.,data=mtc,control=rpart.control(cp=0.02)) #rpart##
y_hat <- predict(rtree,mtc)
RMSE(pred = y_hat,obs=mtc$mpg)

##Random Forest##
rf <- randomForest::randomForest(mpg~.,data=mtc,importance=T)

##Examples##
library(rpart)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y~x,data=dat) #Regression Tree
plot(fit)
text(fit)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x,y_hat))

library(randomForest)
fit <- randomForest(y~x,data=dat)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
plot(fit)

getModelInfo("glm")
modelLookup("knn")

##Parameter Tuning##
getModelInfo("glm")
modelLookup("glm")

train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = T)

#Example#
data("tissue_gene_expression")
set.seed(1991,sample.kind = "Rounding")
t <- data.frame(x=tissue_gene_expression$x,y=tissue_gene_expression$y)
c = data.frame(mtry=seq(50,200,25))
mod <- train(y~.,data=t,method="rpart",tuneGrid=c,control = rpart.control(minsplit = 0))
ggplot(mod)
plot(mod$finalModel)
text(mod$finalModel)
#Random Forest##
mod <- train(y~.,data=t,method="rf",tuneGrid=c,nodesize=1)
varImp(mod)

##Titanic dataset##
install.packages("titanic")
library(titanic)    # loads titanic_train data frame
# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

set.seed(42,sample.kind="Rounding")
i <- createDataPartition(titanic_clean$Survived,p=0.2,list=F)
training <- titanic_clean %>% slice(-i)
test <- titanic_clean %>% slice(i)
table(titanic_clean$Survived)

##Random guessing##
set.seed(3,sample.kind="Rounding")
ans <- sample(c(0,1),size=length(test$Survived),replace = T) %>% factor()
mean(ans==test$Survived)

##Survived proportion by sex##
test %>% select(Survived,Sex) %>% group_by(Sex) %>% summarise(m=mean(Survived==1))
preds <- ifelse(test$Sex=="female",1,0) %>% factor()
mean(preds==test$Survived)

##Survived proportion by Pclass##
options(digits = 3)
test %>% select(Survived,Pclass,Sex) %>% group_by(Pclass,Sex) %>% summarise(m=mean(Survived==1)) %>% filter(m>0.5)
pred <- ifelse(test$Sex=="female" & test$Pclass != "3",1,0) %>% factor()
mean(test$Survived==pred)

##Only sex model##
test %>% select(Survived,Sex) %>% group_by(Sex) %>% summarise(m=mean(Survived==1))
preds <- ifelse(test$Sex=="female",1,0) %>% factor()
F_meas(data=preds,reference = test$Survived)

##Only Class model##
test %>% select(Survived,Pclass) %>% group_by(Pclass) %>% summarise(m=mean(Survived==1))
ps <- ifelse(test$Pclass=="1",1,0) %>% factor()
F_meas(data=ps,reference = test$Survived)


#Sex and Class model##
test %>% select(Survived,Pclass,Sex) %>% group_by(Pclass,Sex) %>% summarise(m=mean(Survived==1)) %>% filter(m>0.5)
pred <- ifelse(test$Sex=="female" & test$Pclass != "3",1,0) %>% factor()
mean(test$Survived==pred)
F_meas(data=pred,reference = test$Survived)

#lda model##
new <- select(test,Fare)
set.seed(1,sample.kind="Rounding")
LDA <- train %>% select(Fare,Survived) %>% train(Survived~.,data=.,method="qda")
mean(predict(LDA,new)==test$Survived)

##qda model##
new <- select(test,Fare)
set.seed(1,sample.kind="Rounding")
LDA <- train %>% select(Fare,Survived) %>% train(Survived~.,data=.,method="lda")
mean(predict(LDA,new)==test$Survived)

##logistic regression##
set.seed(1,sample.kind="Rounding")
logis <- glm(Survived~.,data=training,family = "binomial")
p_hat <- predict(logis,test,type = "response")
y_hat <- ifelse(p_hat>0.5,1,0)%>% factor()
mean(y_hat==test$Survived)
confusionMatrix(data=y_hat,reference = test$Survived)

##KNN using train() to tune k-values## Bootstrapped knn model##
set.seed(6,sample.kind = "Rounding")
k_param <- train(Survived~.,data=training,tuneGrid=data.frame(k=seq(3, 51, 2)),method="knn")
ggplot(k_param)
k_param$bestTune                 

res <- predict(k_param,test)
mean(res==test$Survived)

##Cross-Validated knn model##
set.seed(8,sample.kind = "Rounding")
k_param_cv <- train(Survived~.,data=training,tuneGrid=data.frame(k=seq(3, 51, 2)),method="knn",trControl=
                      trainControl(method = "cv",number=10,verboseIter=T))
ggplot(k_param_cv)
k_param$bestTune                 

res <- predict(k_param_cv,test)
mean(res==test$Survived)

##Classification tree## Bootstrapped
set.seed(10,sample.kind = "Rounding")
tree <- train(Survived~.,data=training,tuneGrid=data.frame(cp=seq(0, 0.05, 0.002)),method="rpart")
ggplot(tree) #CP vs. Accuracy##
tree$bestTune    #Best cp value##
fm <- tree$finalModel #Final Model##
plot(fm)
text(fm)
res <- predict(tree,test)
mean(res==test$Survived)

new_test_data <- data.frame(Sex=c("male","female","female","male","female","female","male"),
                            Pclass=c("0","2","3","0","3","1","1"),Age=c(28,0,0,5,0,17,17),
                            Fare=c(0,0,8,0,25,0,0),SibSp=c(0,0,0,4,0,2,2),Parch=c(0,0,0,0,0,0,0),
                            FamilySize=c(1,1,1,5,1,3,3),Embarked=c('S','C','Q','S','Q','C','C'))
new_test_data <- new_test_data %>% mutate(Sex=as.character(Sex),Pclass=as.integer(Pclass),Age=as.numeric(Age),Fare=as.numeric(Fare),SibSp=as.integer(SibSp),Parch=as.integer(Parch),
                                          FamilySize=as.numeric(FamilySize),Embarked=as.factor(Embarked))

res <- predict(tree,new_test_data)


##Random Forest##
set.seed(14,sample.kind = "Rounding")
RF <- train(Survived~.,data=training,tuneGrid=data.frame(mtry=seq(1:7)),method="rf",ntree=100)
ggplot(RF) #CP vs. Accuracy##
RF$bestTune    #Best cp value##
fm <- RF$finalModel #Final Model##
plot(fm)
text(fm)
res <- predict(RF,test)
mean(res==test$Survived)


##Mnist example##
set.seed(123,sample.kind = "Rounding")
mnist <- read_mnist()
index <- sample(nrow(mnist$train$images),10000)
x <- mnist$train$images[index,]
colnames(x) <- 1:ncol(x)
y <- factor(mnist$train$labels[index])

t_index <- sample(nrow(mnist$test$images),10000)
x_test <- mnist$test$images[t_index,]
colnames(x_test) <- 1:ncol(x_test)
y_test <- factor(mnist$test$labels[t_index])

#Removing columns/predictors with 0 variance##
##nearZeroVar() gives predictors with almost 0 variance##
sds <- colSds(x)
qplot(sds, bins = 256)
nzv <- nearZeroVar(x)
length(nzv)
col_index <- setdiff(1:ncol(x),nzv)

#KNN Model##
n <- 1000
b <- 5
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(x[,col_index],y,
                   method = "knn",
                   tuneGrid = data.frame(k = c(3,5,7)),
                   trControl = control)
fit_knn <- knn3(x[ ,col_index], y,  k = 3)

y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]

cm$byClass[,1:2]
library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))
train_rf <-  train(x[, col_index], y,
                   method = "Rborist",
                   nTree = 50,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune

fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]

##Random Forest##
library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y,  ntree = 50)
imp <- importance(rf)
image(matrix(imp, 28, 28))
p_max <- predict(fit_knn, x_test[,col_index])

#Ensemble##
p_rf <- predict(rf, x_test)$census

##Example##
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
new_models <- models[ind]
data("mnist_27")

fits <- lapply(new_models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 
acc <- sapply(fits, function(fit) min(fit$results$Accuracy))
ind <- which(acc>0.8)

res <- sapply(fits,function(fits){
  predict(fits,mnist_27$test)
})

resl <- apply(res,1,function(x) names(which.max(table(x)))) ##Rowwise most common element
resl <- factor(resl)
confusionMatrix(resl,mnist_27$test$y)

##Movie recommendation example##
data("movielens")
movielens %>% 
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")
set.seed(755)
test_index <- createDataPartition(y = movielens$rating, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

##Movielens##
movielens %>% group_by(movieId) %>% summarise(m=count(userId)) %>% top_n(10)

year_wise_ratings <- movielens %>% group_by(year) %>% summarise(ratings=length((rating))) %>% arrange(desc(ratings))
movies_after_1993 <- movielens %>% 
  filter(year >= 1993) %>%
  group_by(title) %>%
  summarize(n = n(), years = 2018 - first(year),
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

movies_after_1993 %>% ggplot(aes(x=rate,y=rating)) + geom_point() + geom_smooth() + geom_hline(aes(yintercept=4.01))

time_rating <- movielens %>% mutate(date=as_datetime(timestamp),newdate=round_date(date,unit="day")) %>% group_by(newdate) %>%
  summarise(rt = mean(rating))

time_rating %>% ggplot(aes(x=newdate,y=rt)) + geom_point() + geom_smooth()

##Ratings by genre#
movielens %>% select(-date) %>% group_by(genres) %>% summarise(n=n(),m=mean(rating),s=sd(rating)) %>% filter(n>1000) %>%
  arrange(desc(m))

#Regularization##
set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
n <- round(2^rnorm(1000, 8, 1))
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))
overall <- mean(sapply(scores, mean))
alphas <- seq(10,250)
rmse <- sapply(alphas, function(alpha){
  score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
  sqrt(mean((score_reg - schools$quality)^2))
})
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

##Matrix Factorization##
set.seed(1987,sample.kind = "Rounding")
#if using R 3.6 or later, use `set.seed(1987, sample.kind="Rounding")` instead
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

s <- svd(y)
ss_y <- sapply(1:ncol(y),function(x){
  z <- as.numeric(y[,x])
  sum(z^2)
})
ss_yv <- apply((y%*%s$v)^2, 2, sum)
d <- data.frame(y=ss_y,yv=ss_yv,c=1:ncol(y))
d %>% ggplot()+geom_point(aes(x=c,y,col="Red"))+geom_point(aes(x=c,yv))


##Course 9##
##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

nrow(edx)
ncol(edx)
unique(edx$rating)
table(factor(edx$rating))
length(unique(factor(edx$movieId)))
length(unique(factor(edx$userId)))

edx %>% filter(genres %in% c("Drama","Comedy","Thriller","Romance")) %>% 
  group_by(genres) %>%summarise(n=length(movieId))

edx %>% filter(genres=="Drama") %>% select(rating)%>%
  nrow()

edx %>% group_by(title) %>% summarise(r=length(userId)) %>%
  arrange(desc(r)) %>% head()

rats <- edx %>% mutate(ratings=rating) %>% select(ratings) %>%
  group_by(ratings) %>% summarise(n=n())

ra <- data.frame(rats)
ra %>% filter(ratings != c(1.0,2.0,3.0,4.0,5.0)) %>% sum(n)
