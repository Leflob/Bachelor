#US Arrest

data = USArrests
data = na.omit(data)

data = scale(data)
head(data)

d_ist = dist(data, method = "euclidean")

hc1 = hclust(d_ist, method = "ward.D2")
hc1

plot(hc1, cex = 0.6, hang = -1)


hc2 = agnes(data, method = "ward")
hc2$ac
