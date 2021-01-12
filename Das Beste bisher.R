
##principal component analyse

#install.packages("psych")
set.seed(240297)
library(psych)
library(readxl)
gaming = read_excel(
  "C:/Users/Florian/Desktop/WS 20-21/Bachelorarbeit/Anfrage 3 What drives Gamers to buy virtual goods/format.xlsx",
  na = "NA",
  sheet = 1
)
gaming_raw = na.omit(gaming)
g_scaled = scale(gaming_raw)

results = prcomp(gaming_raw, scale. = TRUE)
results$rotation[,1:7]



eig = (focus$sdev)^2

variance = eig*100/sum(eig)
variance

cumvar = cumsum(variance)
cumvar
#Die summe der Varianzen der ersten 7 Principal components ist 58.6 % 
#was eigentlich exakt dem Wert der Studie entspricht.

kmodel = kmeans(g_scaled, 5)
kmodel


#choosing k 

k = list()
for(i in 1:10){
  k[[i]] = kmeans(g_scaled, i)
}
k

betweenss_toss = list()
for (i in 1:10) {
  betweenss_toss[[i]] = k[[i]]$betweenss/k[[i]]$totss
  
}

plot(1:10, betweenss_toss, type = "b", ylab = "Between SS / Total SS", xlab = "k")

#install.packages("mclust")


library(GPArotation)
#install.packages("Rcsdp")
library(Rcsdp)
fa.results <- fa(g_scaled, nfactors=7, n.iter = 8, rotate="varimax",
                 scores=TRUE, fm="alpha", oblique.scores=FALSE, max.iter=25)
str(fa.results)
fa.results$loadings

fa.diagram(fa.results ,fa.results$loadings, cut = 0.2, sort = TRUE)

fa.results$scores

gamer_scores = fa.results$scores


dist_player = dist(gamer_scores, method = "euclidean")
dist_players = sqrt(dist_player*dist_player)
dist_players

wardCluster = hclust(dist_players, method = "ward.D")
wardCluster



#Visualisierung der CLuster mit verschiedenen k
library(factoextra)
library(ggplot2)
clust = cutree(wardCluster, k=5)
#fviz_cluster(list(data = gamer_scores, cluster = clust))

#Der Output des Cluster plot soll kann aufgrund der schieren anzahl an Beobachtungen 
#und vorallem Variablen bzw. Faktoren nicht interpretiert werden.
#Grob ist erkennbar, dass sich verschiedene Cluster bilden, allerdings ist in der
#Mitte ein ziemliches Chaos was vorallem aber dadurch ensteht, dass der Plot mehr-
#dimensional zu verstehen ist (aufgrund der Beziehungen von 7 Faktoren zueinender)
#hier aber nur auf eine zwei-dimensionale Fläche projeziert wird. 

#Dendogram
library(dendextend)
dend_players <- as.dendrogram(wardCluster)

dend_5 <- color_branches(dend_players, k = 5)

plot(dend_5)

#Alleine aus dem Dendogram wird nicht ersichtlich was die beste Clusteranzahl ist
#Allerdings lässt sich bereits eine "schöne" Cluster-struktur für k=5(daher auch farbig)
#erkennen. 

#Das gesuchte k wird hier mit dem Ellbow-plot ermittelt.

#elbow method

##############
library(purrr)

# Use map_dbl to run many models with varying value of k
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = gamer_scores, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

elbow_df

#Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
geom_line() +
  scale_x_continuous(breaks = 1:10)



##Problem, der Elbowplot zeigt nicht mehr das was er sollte + nur bestimmte seeds
#lassen es 


# Create a new data frame appending the cluster assignment


###############
kmeans_gamer <- kmeans(gamer_scores, centers = 5, nstart = 20)

clust_gamer <- print(kmeans_gamer)
str(clust_gamer)

clust_gamer$cluster

library(dplyr)
cl_gaming <- mutate(gaming_raw, cluster = clust_gamer$cluster)
cl_gaming
count(cl_gaming, cluster)

kmeans_gamer$centers

str(kmeans_gamer)
gamingList = as.data.frame(kmeans_gamer$centers)




library(fmsb)

spider_web = data.frame(
  social = c(1.2, -1.2, gamingList$alpha1),
  functional = c(1.2, -1.2, gamingList$alpha2),
  hedonic = c(1.2,-1.2, gamingList$alpha6),
  quality = c(1.2,-1.2, gamingList$alpha5),
  economic = c(1.2,-1.2, gamingList$alpha7),
  identification = c(1.2,-1.2, gamingList$alpha3),
  emotional = c(1.2,-1.2, gamingList$alpha4),
  row.names = c("max", "min", "performers", "social aesthetes", "economic aesthetes", "egocentrics", "satisfaction seekers")
)



spider_web

# Define fill colors
colors_fill = c(scales::alpha("gray", 0.1),
                scales::alpha("gold", 0.1),
                scales::alpha("tomato", 0.2),
                scales::alpha("skyblue", 0.2),
                scales::alpha("green", 0.1)
)

# Define line colors
colors_line = c(scales::alpha("darkgray", 0.9),
                scales::alpha("gold", 0.9),
                scales::alpha("tomato", 0.9),
                scales::alpha("royalblue", 0.9),
                scales::alpha("green", 0.9)
)

# Create plot
radarchart(spider_web, 
           seg = 5,  # Number of axis segments
           title = "Cluster comparison",
           pcol = colors_line,
           pfcol = colors_fill,
           plwd = 4)

# Add a legend
legend(x=1.2, 
       y=1.35, 
       legend = rownames(spider_web[-c(1,2),]), 
       bty = "n", pch=20 , col = colors_line, cex = 1.05, pt.cex = 3)
###################

