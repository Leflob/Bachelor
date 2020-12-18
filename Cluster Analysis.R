#Datenauswertung "What Drives Gamers to Buy Virtual Goods? A Comparative Study of European Gamers"
library(readxl)
gaming = read_excel("C:/Users/Florian/Desktop/WS 20-21/Bachelorarbeit/Anfrage 3 What drives Gamers to buy virtual goods/format.xlsx", na="NA", sheet = 2)
gaming

###########
#Schritt 1 - Idee
###########
#Fasse die 27 Variablen in die Kategorien der Studie zusammen


social = subset(gaming, select = c(x19, x26, x27, x18, x20, x17, x24))
functional = subset(gaming, select = c(x8, x13, x16, x14, x7))
hedonic = subset(gaming, select = c(x10, x9, x15))
quality = subset(gaming, select = c(x23, x22, x21))
economic = subset(gaming, select = c(x5, x6, x12, x11))
identification = subset(gaming, select = c(x3, x4))
emotional = subset(gaming, select = c(x1, x25, x2))



social
functional
hedonic
quality
economic
identification
emotional

data = data.frame(social, functional, hedonic, quality, economic, identification, emotional)

gaming_c = as.matrix(data)


#logischerweise ändert das nichts an den Clustern die R findet -.-

#fÃ¼r die Kategorien könnte man den mean jeder Person aller Fragen einer Kategorien ermitteln und 
#von dort aus mit dem mean weiter rechnen. 

#wie bekomme ich den mean für jede Person in den Fragekategorien?
#In excel?

#####
gaming_mean = read_excel("C:/Users/Florian/Desktop/WS 20-21/Bachelorarbeit/Anfrage 3 What drives Gamers to buy virtual goods/format.xlsx", na="NA", sheet = 3)
gaming_mean 



#hirarchical Clustering
###########

dist_players = dist(gaming_mean)
dist_players

hc_players = hclust(dist_players, method = "ward.D2")
hc_players

clusters_k2 = cutree(hc_players, k=5)
clusters_k2

library(dplyr)

gamers_k2_complete = mutate(gaming_mean, cluster = clusters_k2)

gamers_k2_complete

#plot(gamers_k2_complete)

# Count the cluster assignments
count(gamers_k2_complete, cluster)
#####

#Methode "Complete"
###########
#Die Werte sehen schon etwas besser verteilt aus, allerdings ergeben sich immernoch deutliche Diskepanzen.
#Vorallem Cluster 5 und 1 weichen stark von der Studie ab

##VerÃ¤ndert man die anzahl der Cluster k=x so fÃ¤llt auf das ab einer Cluster Anzahl von 3
##R immer ein CLuster "unterbesetzt". Bei k=3,4,5,6 (vmtl. auch den Weiteren) ist der letzte CLuster immer 
##mit 29 Beobachtungen gefüllt. Bei k=6 sehen die Zuteilungen, denen der Studie schon sehr Ã¤hnlich.
## Es stört jedoch das Cluster Nr.6 mit 29 Beobachtungen.

##Woher kommt das und warum macht R das?


gamers_k2_complete$cluster == 1

cluster1 = subset(gamers_k2_complete, cluster == 1)

cluster1


social1 = sum(cluster1$social)
functional1 = sum(cluster1$functional)
hedonic1 = sum(cluster1$hedonic)
quality1 = sum(cluster1$quality)
economic1 = sum(cluster1$economic)
identification1 = sum(cluster1$identification)
emotional1 = sum(cluster1$emotional)

social1/139
functional1/139
hedonic1/139
quality1/139
economic1/139
identification1/139
emotional1/139

# highest= identification
#lowest = hedonic
#In diesem Cluster sind Leute, die die Identifikation als sehr wichtig empfinden 
#und denen das "hedonic oder besser der visuelle Aspekt des Spiels sehr unwichtig ist. 
#Ein solche Gruppe findet sich in der Studie hingegen nicht wieder. 


### Beim Clustern läuft irgend etwas schief
###Das komische ist, dass das Schaubild des Clusters/ der Cluster relativ gut aussieht (zumindest bei k=4)



#####


#Methode "Ward.D2"  
###########


cluster1 = subset(gaming_k_model, cluster == "2")
cluster1


social1 = sum(cluster1$social)
functional1 = sum(cluster1$functional)
hedonic1 = sum(cluster1$hedonic)
quality1 = sum(cluster1$quality)
economic1 = sum(cluster1$economic)
identification1 = sum(cluster1$identification)
emotional1 = sum(cluster1$emotional)

social1/147
functional1/147
hedonic1/147
quality1/147
economic1/147
identification1/147
emotional1/147


#Es stellt sich heraus, dass auch bei Ward.D2 immer ein Cluster ab einer Anzahl von 
#4 Clustern und mehr mit 42 Beobachtungen gefüllt wird. Analysiert man die Fälle in 
#Cluster 5 so fällt auf, dass es sich um Personen handelt, die i.d.R zu allen Fragen 
#nur niedrige werte, also ablehnung bekundet haben handelt.
#####

#Non hirarchical clustering (k-means)
###########
library(dplyr)

gaming_k = kmeans(gaming_mean, centers = 5)
gaming_k
#print(gaming_k)

gaming_k_model <- mutate(gaming_mean, cluster = gaming_k$cluster)
gaming_k_model

count(gaming_k_model, cluster)

library(factoextra)
fviz_cluster(list(data = gaming_mean, cluster = gaming_k$cluster))
#plot(gaming_k_model)

#Ideale anzahl der Cluster
###############################

km2 <- kmeans(gaming_mean, 2, nstart = 20)
km3 <- kmeans(gaming_mean, 3, nstart = 20)
km4 <- kmeans(gaming_mean, 4, nstart = 20)
km5 <- kmeans(gaming_mean, 5, nstart = 20)
km6 <- kmeans(gaming_mean, 6, nstart = 20)
km7 <- kmeans(gaming_mean, 7, nstart = 20)

wss <- c(sum(km2$withinss), sum(km3$withinss), sum(km4$withinss),
         + sum(km5$withinss), sum(km6$withinss), sum(km7$withinss))
names(wss) <- 2:7
barplot(wss)



#k-means Clustern wÃ¼rde in diesem Fall wohl 2 Cluster empfehlen, wie kommt man auf 5 ?

#####


#Analyse
################################

#km3
analyse_gaming <- prcomp(gaming_mean, scale = TRUE)
plot(predict(analyse_gaming)[, 1:2], type = "n")
text(predict(analyse_gaming)[, 1:2], rownames(analyse_gaming), col = gaming_k_model$cluster)
points(predict(analyse_gaming, gaming_k_model$centers)[, 1:2], col = 1:3, pch = 3,
       + cex = 3)

#km4
analyse_gaming <- prcomp(gaming_mean, scale = TRUE)
plot(predict(analyse_gaming)[, 1:2], type = "n")
text(predict(analyse_gaming)[, 1:2], rownames(analyse_gaming), col = km4$cluster)
points(predict(analyse_gaming, km4$centers)[, 1:2], col = 1:3, pch = 3,
       + cex = 3)

#km5

analyse_gaming <- prcomp(gaming_mean, scale = TRUE)
plot(predict(analyse_gaming)[, 1:2], type = "n")
text(predict(analyse_gaming)[, 1:2], rownames(analyse_gaming), col = km5$cluster)
points(predict(analyse_gaming, km5$centers)[, 1:2], col = 1:3, pch = 3,
       + cex = 3)

#km6 

analyse_gaming <- prcomp(gaming_mean, scale = TRUE)
plot(predict(analyse_gaming)[, 1:2], type = "n")
text(predict(analyse_gaming)[, 1:2], rownames(analyse_gaming), col = km6$cluster)
points(predict(analyse_gaming, km6$centers)[, 1:2], col = 1:3, pch = 3,
       + cex = 3)


#Ab fÃ¼nf macht es nach den Darstellungen her keinen Sinn mehr, 4 cluster sind 
#nach den Plots eine valide Option mit der hÃ¶chsten Anzahl an clustern

#install.packages("plot.kmeans.R")



####################
#weiter mit Datacamp
####################


library(tidyverse)

# Plot the positions of the players and color them using their cluster
ggplot(gamers_k2_complete, aes(x = x1, y = x2, color = factor(cluster))) +
  geom_point()



# Generate hclust for complete, single & average linkage methods
hc_complete <- hclust(dist_players, method = "complete")
hc_single <- hclust(dist_players, method = "single")
hc_average <- hclust(dist_players, method = "average")
hc_complete
hc_single
hc_average

# Plot & Label the 3 Dendrograms Side-by-Side
# Hint: To see these Side-by-Side run the 4 lines together as one command
par(mfrow = c(1,3))
plot(hc_complete, main = 'Complete Linkage')
plot(hc_single, main = 'Single Linkage')
plot(hc_average, main = 'Average Linkage')


#install.packages("dendextend")
#
#install.packages("factoextra")

library(factoextra)
library(dendextend)
library(dplyr)

##########################
#kmeans model mit Datacamp
##########################


# Build a kmeans model
model_km2 <- kmeans(gaming_mean, centers = 5)

# Extract the cluster assignment vector from the kmeans model
clust_km2 <- print(model_km2)

# Create a new data frame appending the cluster assignment
lineup_km2 <- mutate(gaming_mean, cluster = model_km2$cluster)
lineup_km2
# Plot the positions of the players and color them using their cluster
ggplot(lineup_km2,aes(x = X, y = Y, color = factor(cluster))) + 
  geom_point() 
lineup_km2

#R hat ein Problem mit der Darstellung von mehereren Variablen hier. Die 
#Frage ist, was muss ich der x und y- Achse für ein Objekt zuweisen, damit ich 
#einen Plot bekomme vergleichbar mit dem Fußballfeld aus Datacamp


####################



library(purrr)

# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = gaming_k_model, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

elbow_df

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:7)



####################

#Silouette
###################
#Die Werte die am nächsten bei 1 sind sind die Besten. 
#heir würde tatsächlich k=5 nach dieser Methode das beste k sein.


library(cluster)
pam_k3 <- pam(gaming_mean, k = 3)
pam_k3$silinfo$widths

library(purrr)
sil_width <- map_dbl(2:10,  function(k)
  {  model <- pam(x = gaming_mean, k = k) 
  model$silinfo$avg.width
  })
sil_df <- data.frame(  k = 2:10,  sil_width = sil_width)

print(sil_df)

plot(sil_df)



#vielleicht über die Cluster center mehr Erkenntnis
##########

clusters = cutree(hclust(dist(gaming_mean)), k=5) # get 5 clusters

# function to find medoid in cluster i
clust_center = clust.centroid = function(i, dat, clusters) {
  ind = (clusters == i)
  colMeans(dat[ind,])
}
clust_center










