y <- c(1,2,3)
y2 <- c('Juan','Juliana','Lizeth','Magda')
sum(y)
z <- sum(y)
?sum
sum(y,TRUE) # ? Aqui paso algo raro con la suma al remover los NA 
sum(y,FALSE)
sum(na.rm = FALSE, y)
sum(FALSE, y)

multiplica <- function(num1, num2){ 
  
  res <- num1*num2
  return(res)
}

resultado2 <- multiplica(1,2)
