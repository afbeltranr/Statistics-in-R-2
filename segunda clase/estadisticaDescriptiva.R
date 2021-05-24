exp <- c(47,52,52,57,58,58,60,65,66,66,71,71,72,73,96)
median(exp)
summary(exp)
library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)

library(readr)
msleep <- read_csv("msleep_ggplot2.csv")
View(msleep)
library(dplyr)

carni <- dplyr::filter(msleep, vore == 'carni') %>% select(sleep_total) 
omni <- dplyr::filter(msleep, vore == 'omni') %>% select(sleep_total) 



estudiantes <- c('Carlos Amado','Daniel Martinez','Gabriel Ballen','Isabella Blanco','Javier Suesca','Juliana Maldonado','Juliana Penaloza','Laura Cardenas','Leidy Bohorquez','Magda Isabel','Wendy Gonzalez', 'Lizeth Pineda')
 
sample(estudiantes,1) 
  
