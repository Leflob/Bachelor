#Hier sollen die Schritte der Studie in chronologischer Reihenfolge nachvollzogen werden

#Die Studie beginnt mit der einteilung der Fragen in "Überkategorien"
#Diesen Schritt lasse ich an dieser Stelle vorerst aus. 
#Ich gehe weiter mit den Ergebnissen der Analyse der Studie vor.
#In Excel habe ich eine Tabelle erstellt, welche die Antworten der Befragten (1:5) summiert.
#Dabei werden natürlich nur jeweils die Antworten der Fragen summiert, die sich in der selben 
#Überkategorie z.B. social befinden. Danach teile ich durch die Anzahl der Fragen in einer Kategorie und erhalte
#den Mittelwert der aufschluss darüber geben sollte ob eine Peroson den Fragen des 
#Bereichs social zustimmt oder nicht. 
set.seed(1337)
library(readxl)
gaming_mean = read_excel("C:/Users/Florian/Desktop/WS 20-21/Bachelorarbeit/Anfrage 3 What drives Gamers to buy virtual goods/format.xlsx", na="NA", sheet = 3)
gaming_mean 

#Die Frage ist nun wie viele verschiedene Typen von Personen sinnvoller weise aus der Untersuchung 
#geclustert werden sollten. 
#hierfür wurde hirarchical clustering mit "Wards Proceudure" angewand
#genau wie in der Studie ist die Distance = euclidean

dist_players = dist(gaming_mean, method = "euclidean")
dist_players

wardCluster = hclust(dist_players, method = "ward.D2")
wardCluster


#Visualisierung der CLuster mit verschiedenen k
clust = cutree(wardCluster, k=2)
fviz_cluster(list(data = gaming_mean, cluster = clust))


#Dendogram
library(dendextend)
dend_players <- as.dendrogram(wardCluster)

dend_5 <- color_branches(dend_players, k = 5)

plot(dend_5)

#Alleine aus dem Dendogram wird nicht ersichtlich was die beste Clusteranzahl ist

#In der Studie wird ausgeführt, dass:
#We entered the factor loadings (created in the factor analysis) into the 
#hierarchical clustering to determine the ideal number of clusters.
#Wie füge ich die factor loadings in das cluster ein?
#Ich habe bereits die factor loadings berücksichtigt, da ich das Excel-sheet
#entsprechend angepasst habe. Wie die Autoren allerdings dadurch auf das 
#entsprechende k kommen, leuchtet mir nicht ein.

#Ich werde daher mit der Silouette Methode ermitteln welches das gesuchte k ist. 

#Silouette
###################
#Die Werte die am nächsten bei 1 sind sind die Besten. 

library(cluster)
pam_k3 <- pam(gaming_mean, k = 2)
pam_k3$silinfo$widths

library(purrr)
sil_width <- map_dbl(2:10,  function(k)
{  model <- pam(x = gaming_mean, k = k) 
model$silinfo$avg.width
})
sil_df <- data.frame(  k = 2:10,  sil_width = sil_width)

print(sil_df)

plot(sil_df)

#Von dem Plot her wären k = 2 wohl die Beste Cluster-Einteilung. Allerdings
#müssen hier (wie das bei der Ermittlung des k immer der Fall ist) die realen 
#und konzeptionellen Umstände berücksichtigt werden. 
#In diesem Fall würden 2 Cluster wohl eher weniger Aufschluss über die Personen 
#liefern, da man sich erhofft von den Erkenntnissen aus einer Analyse von Käufern 
#bestimmte Nutzerverhalten zu analysieren. Etwas spezifischere Einteilungen wären
#daher ratsam.

#Der nächst höhere Ausschlag ist bei k=5 zu finden. Also auch bei der Cluster anzahl, 
#welche die Studie (wie auch immer) ebenfalls herausgefunden hat.


#Nun muss die Cluster-Zugehörigkeit und die Zentren gefunden werden.
#Dazu non-hirarchical clustering, hier: k-means clustering



# Build a kmeans model
set.seed(1337)
model_km2 <- kmeans(gaming_mean, centers = 5, nstart = 1)


# Extract the cluster assignment vector from the kmeans model
clust_km2 <- print(model_km2)

# Create a new data frame appending the cluster assignment
gaming_km2 <- mutate(gaming_mean, cluster = model_km2$cluster)
gaming_km2

#Count 
count(gaming_km2, cluster)

# Plot the positions of the players and color them using their cluster
fviz_cluster(list(data = gaming_mean, cluster = model_km2$cluster))

#Untersuche die Cluster

cluster1 = subset(gaming_km2, cluster == "3")
cluster1


social1 = sum(cluster1$social)
functional1 = sum(cluster1$functional)
hedonic1 = sum(cluster1$hedonic)
quality1 = sum(cluster1$quality)
economic1 = sum(cluster1$economic)
identification1 = sum(cluster1$identification)
emotional1 = sum(cluster1$emotional)

social1
functional1
hedonic1
quality1
economic1
identification1
emotional1



#ich muss die CLuster Zentren heruasfinden und dann jeweils den Abstand
#der mean der Kategorien zu dem jeweiligen zentrum berechnen
