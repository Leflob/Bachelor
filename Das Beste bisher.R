#Die Studie beginnt mit einer Angemessenheitsfeststellung der Sample.
#Dieser Schritt ist hier nicht relevant und wird einfach angenommen.

#Factor Analysis
##################
#
#Einer dient der exploratory und einer der confirmatory factor analysis.
#Da die Studie dies nicht tut, werden die Methode zum Vergleich auch auf den gesamte Datensatz angewand

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

# Basic descriptive statistics
describe(gaming_raw)
summary(gaming_raw)

#Correlationsmatrix

lowerCor(gaming_raw)

#Eigenvalue-One Criterion
#####

# Calculate the correlation matrix
gaming_full<- cor(gaming_raw, use = "pairwise.complete.obs")

# use of the correlation matrix to calculate eigenvalues
eigenvals1 <- eigen(gaming_full)

# Look at the eigenvalues returned
eigenvals1$values


#SCREE-PLOT
#Faktoren mit Eigenwerten >1 sind ein gängiges Kriterium zur Auswahl der Anzahl
#von Faktoren.
scree(gaming_full, factors = FALSE, main = "ganzer Datensatz")
######


#Principal component analyse PCA
#####
results = prcomp(gaming_raw, scale. = TRUE)
results$rotation[,1:7]

eig = (results$sdev)^2

variance = eig*100/sum(eig)
variance

cumvar = cumsum(variance)
cumvar
#Die summe der Varianzen der ersten 7 Principal components ist 58.6 % 
#was exakt dem Wert der Studie entspricht.
#####


#Factor analysis
#####
library(GPArotation)
#install.packages("Rcsdp")
library(Rcsdp)
fa.results <- fa(g_scaled, nfactors=7, n.iter = 8, rotate="varimax",
                 scores=TRUE, fm="alpha", oblique.scores=FALSE, max.iter=25)
str(fa.results)
fa.results$loadings


fa.diagram(fa.results ,fa.results$loadings, cut = 0.2, sort = TRUE) #Welche Variable gehört zu welchem Faktor


fa.results$scores

gamer_scores = fa.results$scores #Im Weiteren wird nur noch mit den scores gearbeitet
#####


#Cluster Analyse
#hirarchical Clustering
#####
#install.packages("mclust")
dist_player = dist(gamer_scores, method = "euclidean") #Distanzmethode euklidischer Abstand
dist_players = sqrt(dist_player*dist_player)
dist_players

wardCluster = hclust(dist_players, method = "ward.D2") #Algorithmus Wards procedure
wardCluster


tail(wardCluster$height)
#Prozentuale Änderung

#1
(31.14325/24.35811)-1
#2
(24.35811/23.29069)-1
#3
(23.29069/22.01693)-1

#4
(22.01693/18.99776)-1
#5
(18.99776/17.82225)-1

#die letzten 5 Werte verliern noch recht viel Höhe. Ab dem 6ten Cluster ist der 
#Höhenverlust unter 1, vielleicht ist das ein Idikator (agglomeration schedule?)
#####


#Ideale Clusteranzahl
#####
#elbow method (per Hand)
library(purrr)
library(ggplot2)
library(factoextra)
# Use map_dbl to run many models with varying value of k
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = gamer_scores, centers = k)
  model$tot.withinss
})


elbow_df <- data.frame(             # Generate a data frame containing both k and tot_withinss
  k = 1:10,
  tot_withinss = tot_withinss
)
#elbow_df

ggplot(elbow_df, aes(x = k, y = tot_withinss)) +    #Plot the elbow plot
  geom_line() +
  scale_x_continuous(breaks = 1:10)
#leichter knick bei 5 allerdings auch bei 3


#Weitere statistische Methoden 
library(factoextra)
#elbow plot (automatisch)
fviz_nbclust(gamer_scores, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")
#leichter knick bie 5 und 3 (s.o.)


# Silhouette method
fviz_nbclust(gamer_scores, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
#Maximum bei 5

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(gamer_scores, kmeans, nstart = 25,  method = "gap_stat", nboot = 50) +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Gap statistic method")
#ebenfalls Maximum bei 5
#####


#Visualisierungsversuch der Cluster 
#####
#Mit diesem Package können auch mehrdimensionale Variablen auf zweidimensionalem 
#Raum dargestellt werden. Ob es hier sinnvoll ist, ist fraglich.

library(ggplot2)
clust = cutree(wardCluster, k=5)
fviz_cluster(list(data = gamer_scores, cluster = clust))
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
plot(dend_players)
#Alleine aus dem Dendogram wird nicht ersichtlich was die beste Clusteranzahl ist
#mit k=5 (eingefärbt) lässt sich aber die errechnete Struktur ideal visualisieren
#####


#kmeans
#####
set.seed(99085)
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


#visualisierung


set.seed(99085)
library(animation)
animation = as.data.frame(gamer_scores)
kmeans.ani(animation[3:4], centers = 5, 
           hints = c("Move centers!", "Find cluster?"), pch = 1:5, col = 1:5)
#####


#Auswertung
#####
#visualisierung anhand eines Spiderweb-Plot
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

#Wer kauft was?

library(readxl)
dat = read_excel(
  "C:/Users/Florian/Desktop/WS 20-21/Bachelorarbeit/Anfrage 3 What drives Gamers to buy virtual goods/format.xlsx",
  na = "NA",
  sheet = 2, range = "A1:AB1097", col_names = TRUE)
data= na.omit(dat)

pur = subset(data, select = c("pur"))
pur
data_raw = subset(data, select = c("x1","x2","x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26","x27"))
data_raw



library(GPArotation)
#install.packages("Rcsdp")
library(Rcsdp)
fa.results <- fa(data_raw, nfactors=7, n.iter = 8, rotate="varimax",
                 scores=TRUE, fm="alpha", oblique.scores=FALSE, max.iter=25)

data_scores = fa.results$scores #Im Weiteren wird nur noch mit den scores gearbeitet



library(dplyr)
pur_gaming <- mutate(gaming_raw, cluster = clust_gamer$cluster, pur)
pur_gaming
count(cl_gaming, cluster)


cl1 <- subset(pur_gaming, cluster == "1")
c1 = subset(cl1, select = "pur")

cl2 <- subset(pur_gaming, cluster == "2")
c2 = subset(cl2, select = "pur")

cl3 <- subset(pur_gaming, cluster == "3")
c3 = subset(cl3, select = "pur")

cl4 <- subset(pur_gaming, cluster == "4")
c4 = subset(cl4, select = "pur")

cl5 <- subset(pur_gaming, cluster == "5")
c5 = subset(cl5, select = "pur")

#Häufigkeiten
#c1 %>% count(pur)
(56/(56+116))*100
100-(56/(56+116))*100

#c2 %>% count(pur)
(52/(52+127))*100
100-(52/(52+127))*100

#c3 %>% count(pur)
(78/(78+181))*100
100-(78/(78+181))*100

#c4 %>% count(pur)
(59/(59+152))*100
100-(59/(59+152))*100

#c5 %>% count(pur)
(83/(83+136))*100
100-(83/(83+136))*100

#####

  
  
#####


#Ganzer Datensatz (Stichporbenartig)
#####

library(GPArotation)
#install.packages("Rcsdp")
library(Rcsdp)
fa.results <- fa(gaming_raw, nfactors=7, n.iter = 8, rotate="varimax",
                 scores=TRUE, fm="alpha", oblique.scores=FALSE, max.iter=25)
str(fa.results)
fa.results$loadings

fa.diagram(fa.results ,fa.results$loadings, cut = 0.2, sort = TRUE)

fa.results$scores

gamer_scores = fa.results$scores


dist_player = dist(gamer_scores, method = "euclidean")
dist_players = sqrt(dist_player*dist_player)
dist_players

wardCluster = hclust(dist_players, method = "ward.D2")
wardCluster


tail(wardCluster$height)

#direkter vergleich der TOSS
#geteilter Datensatz
k = list()
for(i in 1:30){
  k[[i]] = kmeans(g_scaled, i)
}
k

betweenss_toss = list()
for (i in 1:30) {
  betweenss_toss[[i]] = k[[i]]$betweenss/k[[i]]$totss
  
}

plot(1:30, betweenss_toss, type = "b", ylab = "Between SS / Total SS", xlab = "k")

#ganzer Datensatz
k = list()
for(i in 1:30){
  k[[i]] = kmeans(gaming_raw, i)
}
k

betweenss_toss = list()
for (i in 1:30) {
  betweenss_toss[[i]] = k[[i]]$betweenss/k[[i]]$totss
  
}

plot(1:30, betweenss_toss, type = "b", ylab = "Between SS / Total SS", xlab = "k")
#####



#Teilen des DAtensatzes in Exploratory (EFA) und Confirmatory (CFA)
######
# Establish two sets of indices to split the dataset
N <- nrow(gaming_raw)
indices <- seq(1, N)
indices_EFA <- sample(indices, floor((.5 * N)))
indices_CFA <- indices[!(indices %in% indices_EFA)]

# Use those indices to split the dataset into halves for EFA and CFA
gaming_EFA <- gaming_raw[indices_EFA,]
gaming_CFA <- gaming_raw[indices_CFA,]


# Calculate the correlation matrix
gaming_EFA_cor <- cor(gaming_EFA, use = "pairwise.complete.obs")

# use of the correlation matrix to calculate eigenvalues
eigenvals <- eigen(gaming_EFA_cor)

# Look at the eigenvalues returned
eigenvals$values


# Calculate the correlation matrix first
gaming_EFA_cor <- cor(gaming_EFA, use = "pairwise.complete.obs")

#SCREE-PLOT
#Faktoren mit Eigenwerten >1 sind ein gängiges Kriterium zur Auswahl der Anzahl
#von Faktoren.
scree(gaming_EFA_cor, factors = FALSE, main = "geteilter Datensatz")
#####
