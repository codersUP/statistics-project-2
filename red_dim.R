data <- read.csv(file = 'CommViolPredUnnormalizedData.csv', stringsAsFactors=FALSE)

data$Community.name <- NULL
data$state <- NULL

for(c in colnames(data)){
  col <- data[[c]]
  
  for(i in 1:length(col)) {
    col[i] <- as.numeric(as.character(col[i]))
  }
  
  col_d <- as.double(as.character(col))
  col_withot_na <- na.omit(col_d)
  m <- mean(col_withot_na)
  
  for(i in 1:length(col)) {
    if(is.na(col[i])){
      col[i] <- m
    }
  }
  
  col <- as.double(as.character(col))
  
  data[[c]] <- col
}

# para todas las variables

# componentes principales
tp <- cor(data)
symnum(tp)

acp <- prcomp(data, scale=TRUE)
summary(acp)
plot(acp)

acp$rotation


#estandarizando los datos
data.std <- scale(data)

#clúster jerárquico completo
d <- dist(data.std, method = 'euclidean') #matriz de distancias con la distancia euclidiana
fit <- hclust(d, method = 'complete') #ajuste completo
plot(fit)
d2 <- as.dendrogram(fit)

#dibuja rectangulos rojos alrededor de los 8 cluster
rect.hclust(fit, k=8, border="red")

set.seed(0)

# cantidad de clusters a seleccionar
mydata <- data.std
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# k means
data.std <- data.frame(data.std)

fit.k1 <- kmeans(data.std, 8)
fit.k1


# para los datos con las variables seleccionadass
dataFiltered <- data[,c("medIncome","PctPopUnderPov","PctUnemployed","ViolentCrimesPerPop","nonViolPerPop")]

# componentes principales
tp <- cor(dataFiltered)
symnum(tp)

acp <- prcomp(dataFiltered, scale=TRUE)
summary(acp)
plot(acp)

acp$rotation

#estandarizando los datos
dataFiltered.std <- scale(dataFiltered)

#clúster jerárquico completo
d <- dist(dataFiltered.std, method = 'euclidean') #matriz de distancias con la distancia euclidiana
fit <- hclust(d, method = 'complete') #ajuste completo
plot(fit)
d2 <- as.dendrogram(fit)

#dibuja rectangulos rojos alrededor de los 6 cluster
rect.hclust(fit, k=6, border="red")

set.seed(0)

# cantidad de clusters a seleccionar
mydata <- dataFiltered.std
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# k means
dataFiltered.std <- data.frame(dataFiltered.std)

fit.k1 <- kmeans(dataFiltered.std, 6)
fit.k1

plot(dataFiltered.std, col=fit.k1$cluster)
points(fit.k1$centers, col=1:12, pch=6, lwd=2)
