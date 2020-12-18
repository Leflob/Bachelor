#Hier sollen die Schritte der Studie in chronologischer Reihenfolge nachvollzogen werden

#Die Studie beginnt mit der Einteilung der Fragen in "Überkategorien"
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
library(factoextra)
library(ggplot2)
clust = cutree(wardCluster, k=5)
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
#Die Werte die am n?chsten bei 1 sind, sind die Besten. 

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


#elbow method
################

fviz_nbclust(gaming_mean, FUN = hcut, method = "wss")

#keine eindeutige Bestimmung hierüber möglich
#mit der Methode von Datacamp ließe sich hingegen folgender Plot erzeugen:

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

#Wenn auch hier wieder das Argument der konzeptionellen Umstaende beruecksichtigt wird und
#dementsprechend k = 2 ignoriert wird, so ist k = 5 die beste Alternative.


#####
#Gap statistics Method

#gap_stat <- clusGap(gaming_mean, FUN = hcut, nstart = 1000, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)


#Nun muss die Cluster-Zugehörigkeit und die Zentren gefunden werden.
#Dazu non-hirarchical clustering, hier: k-means clustering

####Untersuche die Stärke der Clustering Struktur mit agnes

hc2 = agnes(gaming_mean, method = "ward")
hc2$ac

pltree(hc2, cex = 0.6, hang = -1, main = "Dendogram of agnes")


# Build a kmeans model

#set.seed(0100805)
###################
model_km2 <- kmeans(gaming_mean, centers = 5, nstart = 1)


# Extract the cluster assignment vector from the kmeans model
clust_km2 <- print(model_km2)

# Create a new data frame appending the cluster assignment
library(dplyr)
gaming_km2 <- mutate(gaming_mean, cluster = model_km2$cluster)
gaming_km2

#Count 
count(gaming_km2, cluster)

# Plot the positions of the players and color them using their cluster
#fviz_cluster(list(data = gaming_mean, cluster = model_km2$cluster))

#Untersuche die Cluster

cl1 = subset(gaming_km2, cluster == "1")
cl1
cl2 = subset(gaming_km2, cluster == "2")
cl2
cl3 = subset(gaming_km2, cluster == "3")
cl3
cl4 = subset(gaming_km2, cluster == "4")
cl4
cl5 = subset(gaming_km2, cluster == "5")
cl5

##############

#1
social1 = mean(cl1$social)
functional1 = mean(cl1$functional)
hedonic1 = mean(cl1$hedonic)
quality1 = mean(cl1$quality)
economic1 = mean(cl1$economic)
identification1 = mean(cl1$identification)
emotional1 = mean(cl1$emotional)

#2
social2 = mean(cl2$social)
functional2 = mean(cl2$functional)
hedonic2 = mean(cl2$hedonic)
quality2 = mean(cl2$quality)
economic2 = mean(cl2$economic)
identification2 = mean(cl2$identification)
emotional2 = mean(cl2$emotional)

#3
social3 = mean(cl3$social)
functional3 = mean(cl3$functional)
hedonic3 = mean(cl3$hedonic)
quality3 = mean(cl3$quality)
economic3 = mean(cl3$economic)
identification3 = mean(cl3$identification)
emotional3 = mean(cl3$emotional)

#4
social4 = mean(cl4$social)
functional4 = mean(cl4$functional)
hedonic4 = mean(cl4$hedonic)
quality4 = mean(cl4$quality)
economic4 = mean(cl4$economic)
identification4 = mean(cl4$identification)
emotional4 = mean(cl4$emotional)

#5
social5 = mean(cl5$social)
functional5 = mean(cl5$functional)
hedonic5 = mean(cl5$hedonic)
quality5 = mean(cl5$quality)
economic5 = mean(cl5$economic)
identification5 = mean(cl5$identification)
emotional5 = mean(cl5$emotional)
#################

#ich muss die CLuster Zentren heruasfinden und dann jeweils den Abstand
#der mean der Kategorien zu dem jeweiligen zentrum berechnen

#Zusätzlich Clustert R immer nur paarweise. In dem Programm der Studie oder des Buches
#ist aber aus dem Dendogram ersichtlich, dass hier auch 3 Untersuchungen in einem Cluster
#stecken können. -> Wie kann man das in R?


#####
#Spiderweb-plot
############

#install.packages("fmsb")
library(fmsb)


spider_web = data.frame(
  social = c(5, 0, social1, social2, social3,social4,social5),
  functional = c(5, 0, functional1, functional2, functional3, functional4, functional5),
  hedonic = c(5,0, hedonic1, hedonic2, hedonic3, hedonic4, hedonic5),
  quality = c(5,0, quality1, quality2, quality3, quality4, quality5),
  economic = c(5,0, economic1, economic2, economic3, economic4, economic5),
  identification = c(5,0, identification1, identification2, identification3, identification4, identification5),
  emotional = c(5,0, emotional1, emotional2, emotional3, emotional4, emotional5),
  row.names = c("max", "min", "Cl1", "CL2", "CL3", "CL4", "CL5")
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
###################