hsb2<-read.table("https://stats.idre.ucla.edu/stat/data/hsb2.csv", sep=",", header=T)
attach(hsb2)

ses <- factor(ses) 
levels(ses) <- c("low","medium","high")
female <- factor(female)
levels(female) <- c("male","female")

tapply(write, ses, mean) 

tapply(write, ses, sd)


# ANOVA en una via

a1 <- aov(write ~ ses) 
summary(a1)


pairwise.t.test(write, ses, p.adj = "none")


TukeyHSD(a1)
