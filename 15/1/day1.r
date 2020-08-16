
##FIRST
it<-readLines("./input.txt")

opn<-strsplit(it,'\\(')

cls<-strsplit(it,'\\)')

sum(nchar(cls[[1]][grep('\\(',cls[[1]])]))-sum(nchar(opn[[1]][grep('\\)',opn[[1]])]))

##SECOND
cond <- 0
position <- 0

updown <- function(x){
  switch(x,
         "(" = 1,
         ")" = -1
         )
}

while(cond>=0){
  position <- position+1
  cond <- cond + updown(substring(it, position, position))
}

position

