euro <- read.csv("euro.csv")

## ÌÞ³ýNAÐÐ
euro <- subset(euro, goal!="NA")

euro1 <- aggregate(euro[,-1], list(euro$team), mean)

mean_data <- colMeans(euro1[,-1])

for (i in 1:nrow(euro1)){
    euro1[i,-1] = euro1[i,-1]/mean_data
}

cluster <- kmeans(euro1, 2)
