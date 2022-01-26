test_vector <- 1:10

sredniafor <- function(vector){
  amount <- 0
  sum <- 0
  for (v in vector){
    amount <- amount + 1
    sum <- sum + v
  }
  return(sum/amount)
}


sredniafor(test_vector)

sredniawhile <- function(vector){
  cnt <- 1
  sum <- 0
  while (cnt <= length(vector)){
    sum <- sum + vector[cnt]
    cnt <- cnt + 1
  }
  return(sum/length(vector))
}

sredniawhile(test_vector)


sredniarepeat <- function(vector){
  cnt <- 1
  sum <- 0
  repeat{
    sum <- sum + vector[cnt]
    cnt <- cnt + 1
    if (cnt > length(vector)){
      cnt <- cnt - 1
      break
    }
  }
  return(sum/cnt)
}

sredniarepeat(test_vector)

srednia_ucinana <- function(vector, k){
  new_vector <- vector[(k+1):(length(vector)-k)]
  print(new_vector)
  sredniafor(new_vector)
}

srednia_ucinana(test_vector,1)


srednia_winsor <- function(vector, k){
  message('new_v:',new_vector)
  message('k:',k)
  new_vector <- vector[(k+1):(length(vector)-k)]
  message('new_v:',new_vector)
  sum <- 0
  for (v in new_vector){
    sum <- sum + v
  }
  
  message('min:',min(new_vector))
  message('max:',max(new_vector))
  return((sum+k*min(new_vector)+k*max(new_vector))/(length(new_vector)+2*k))
  
}
srednia_winsor(test_vector, 4)


medianaWLGS <- function(vector, bool){
  len <- length(vector)
  if (bool==1){
    vector <- sort(vector)
  }
  if (len %% 2 == 0){
    return((vector[(len/2)]+vector[((len/2)+1)])/2)
  } else {
    return(vector[(len/2)+1])
  }
}

medianaWLGS(test_vector, 1)
median(test_vector)

rozstep <- function(vector){
  return((max(vector)) - (min(vector)))
}

rozstep(test_vector)


wariancja <- function(vector){
  sr <- mean(vector)
  message('srednia:',sr)
  sum <- 0
  for (v in vector){
    sum <- sum + ((v-sr)**2)
  }
  return(sum/(length(vector)-1))
}

wariancja(test_vector)
var(test_vector)

odsd <- function(vector){
  return(wariancja(vector)**(0.5))
}

odsd(test_vector)
sd(test_vector)


odch_przecietne <- function(vector){
  sr <- mean(vector)
  sum <- 0
  for (v in vector){
    sum <- sum + abs(v - sr)
  }
  return(sum/length(vector))
}

odch_przecietne(test_vector)


kwartyl_dolny <- function(vector){
  len <- length(vector)
  vector <- sort(vector)
  if (len %% 2 == 1){
    return(medianaWLGS(vector[1:((len/2)+1)],0))
  }
  else{
    return(medianaWLGS(vector[1:(len/2)],0))
  }
}

kwartyl_dolny(test_vector)
fivenum(test_vector)
message('Kwartyl dolny zestawu 1:11 to 3... chyba...')


kwartyl_gorny <- function(vector){
  len <- length(vector)
  vector <- sort(vector)
  if (len %% 2 == 1){
    return(medianaWLGS(vector[((len/2)+2):(length(vector))],0))
  }
  else{
    return(medianaWLGS(vector[((len/2)+1):(length(vector))],0))
  }
}
kwartyl_gorny(test_vector)
fivenum(test_vector)

iqrWLGS <- function(vector){
  return(kwartyl_gorny(vector)-kwartyl_dolny(vector))
}

iqrWLGS(test_vector)
IQR(test_vector) #widocznie IQR nie jest po prostu roznica Q3-Q1

library(microbenchmark)
testVec <- 1:1000
print(microbenchmark(
  aver_lib = mean(testVec) # lib function
), signif = 4)

print(microbenchmark(
  wlgs = srednia(testVec) # lib function
), signif = 4)



# Tragedia po krotce