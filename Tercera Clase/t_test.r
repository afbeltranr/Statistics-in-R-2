# url1pop <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
# filename <- basename(url)
# download(url, destfile=filename)
# x <- unlist( read.csv(filename) )


library(dplyr)
dat <- read.csv("femaleMiceWeights.csv")

control <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist  
treatment <- filter(dat, Diet== "hf") %>% select(Bodyweight) %>% unlist

ttest <- t.test(treatment, control)
ttest

N <- length(treatment)

obs <- mean(treatment) - mean(control)

se <- sqrt(var(treatment)/N + var(control)/N)



tstat <- obs/se 

2*(1 - pnorm(tstat))

population <- read.csv("femaleControlsPopulation.csv")

population <- unlist(population)

n <- 10000
nulls <- vector("numeric",n)
for (i in 1:n){
  
  control <- sample(population,25)
  treatment <- sample(population,25)
  se <- sqrt(var(treatment)/25 + var(control)/25)
  nulls[i] <- (mean(treatment) - mean(control)) / se
}  

mypar()

qqnorm(nulls)
qqline(nulls)

hist(nulls)

?qt
?qchisq
?qf
 f <- rf(1000, df1= 3, df2 = 2)
hist(f) 
