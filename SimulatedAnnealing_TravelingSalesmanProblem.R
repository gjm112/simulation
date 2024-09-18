state <- read.csv("https://raw.githubusercontent.com/jasperdebie/VisInfo/master/us-state-capitals.csv")

f <- function(temp){
tot <- sum((temp[-nrow(temp),"latitude"]- temp[-1,"latitude"])^2 + (temp[-nrow(temp),"longitude"]- temp[-1,"longitude"])^2)
return(tot)
}

perm <- best <- 1:50
ttt <- 1000
for (i in 1:1000){print(f(state[best,]))
cand <- sample(perm,50,replace = FALSE)
dif <-  f(state[cand,]) - f(state[best,])  
p <- exp(-dif/ttt)
if (dif <= 0){best <- cand}
if (dif > 0 & runif(1) < p){best <- cand}

plot(state$longitude[best], state$latitude[best], type = "l")
points(state$longitude[best], state$latitude[best], pch = 16)
}

ttt <- 100
for (i in 1:100000){print(f(state[best,]))
  cand <- sample(perm,50,replace = FALSE)
  dif <-  f(state[cand,]) - f(state[best,])  
  p <- exp(-dif/ttt)
  if (dif <= 0){best <- cand}
  if (dif > 0 & runif(1) < p){best <- cand}
  
  plot(state$longitude[best], state$latitude[best], type = "l")
  points(state$longitude[best], state$latitude[best], pch = 16)
}


#A better way to generate candidates
ttt <- 1000
for (i in 1:100000){print(f(state[best,]))
  ind <- sample(perm,2,replace = FALSE)
  cand <- best
  cand[ind] <- best[c(ind[2],ind[1])]
  dif <-  f(state[cand,]) - f(state[best,])  
  p <- exp(-dif/ttt)
  if (dif <= 0){best <- cand}
  if (dif > 0 & runif(1) < p){best <- cand}
  
  plot(state$longitude[best], state$latitude[best], type = "l", main = f(state[best,]) )
  points(state$longitude[best], state$latitude[best], pch = 16)
  
}

ttt <- 100
for (i in 1:100000){print(f(state[best,]))
  ind <- sample(perm,2,replace = FALSE)
  cand <- best
  cand[ind] <- best[c(ind[2],ind[1])]
  dif <-  f(state[cand,]) - f(state[best,])  
  p <- exp(-dif/ttt)
  if (dif <= 0){best <- cand}
  if (dif > 0 & runif(1) < p){best <- cand}
  
  plot(state$longitude[best], state$latitude[best], type = "l", main = f(state[best,]) )
  points(state$longitude[best], state$latitude[best], pch = 16)
}

ttt <- 10
for (i in 1:10000){print(f(state[best,]))
  ind <- sample(perm,2,replace = FALSE)
  cand <- best
  cand[ind] <- best[c(ind[2],ind[1])]
  dif <-  f(state[cand,]) - f(state[best,])  
  p <- exp(-dif/ttt)
  if (dif <= 0){best <- cand}
  if (dif > 0 & runif(1) < p){best <- cand}
  
  plot(state$longitude[best], state$latitude[best], type = "l", main = f(state[best,]) )
  points(state$longitude[best], state$latitude[best], pch = 16)
}


#Final 
best <- 1:50
for (ttt in c(100:1)){
for (i in 1:100){print(f(state[best,]))
  print(ttt)
  ind <- sample(perm,2,replace = FALSE)
  cand <- best
  cand[ind] <- best[c(ind[2],ind[1])]
  dif <-  f(state[cand,]) - f(state[best,])  
  p <- exp(-dif/ttt)
  if (dif <= 0){best <- cand}
  if (dif > 0 & runif(1) < p){best <- cand}
  
  plot(state$longitude[best], state$latitude[best], type = "l", main = f(state[best,]) , sub = ttt)
  points(state$longitude[best], state$latitude[best], pch = 16)
  
}
}


#Final 
best <- c(2,1,3:8,10:50,9)
for (ttt in c(100:1)){
  for (i in 1:10000){print(f(state[best,]))
    print(ttt)
    ind <- sample(2:49,2,replace = FALSE)
    cand <- best
    cand[ind] <- best[c(ind[2],ind[1])]
    dif <-  f(state[cand,]) - f(state[best,])  
    p <- exp(-dif/ttt)
    if (dif <= 0){best <- cand}
    if (dif > 0 & runif(1) < p){best <- cand}
    
    plot(state$longitude[best], state$latitude[best], type = "l", main = f(state[best,]) , sub = ttt)
    points(state$longitude[best], state$latitude[best], pch = 16)
    
  }
}

library(maps)
library(mapdata)
library(ggplot2)
df <- state[best,]
usa <- map_data('usa')
ggplot(data=usa, aes(x=long, y=lat)) + 
  geom_polygon(fill='lightblue') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map') + 
  coord_fixed(1.3) + geom_line(aes(y = latitude, x = longitude), data = df)

df <- state
usa <- map_data('usa')
ggplot(data=usa, aes(x=long, y=lat)) + 
  geom_polygon(fill='lightblue') + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map') + 
  coord_fixed(1.3) + geom_line(aes(y = latitude, x = longitude), data = state)


plot(state$longitude, state$latitude, type = "l")
points(state$longitude, state$latitude, pch = 16)

plot(state$longitude[best], state$latitude[best], type = "l")
points(state$longitude[best], state$latitude[best], pch = 16)




plot_usmap() +
  
 
state[best,]

f(state[best,])

