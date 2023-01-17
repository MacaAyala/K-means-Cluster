#grafico de dispersion con los datos
library(ggplot2)
ggplot() + geom_point(aes(x = YearsExperience, y = Salary), data = Salary_Data, alpha = 0.5) + ggtitle('Conjunto de Datos')



#normalizar las puntuaciones
df <- scale(Salary_Data)
head(Salary_Data)


#calcular la matriz de distacias
library(factoextra)
m.distancia <- get_dist(df, method = "euclidean") #el método aceptado también puede ser: "maximum", "manhattan", "canberra", "binary", "minkowski", "pearson", "spearman" o "kendall"
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

#estimar el número de clústers
#Elbow, silhouette o gap_stat  method
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")
fviz_nbclust(df, kmeans, method = "gap_stat")

#funcion para calcular 30 estimadores de cluster diferentes
library(NbClust)
resnumclust<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)


#calcular los dos clústers
k2 <- kmeans(df, centers = 2, nstart = 25)
k2
str(k2)


#plotear los cluster
fviz_cluster(k2, data = df)
fviz_cluster(k2, data = df, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE) #ellipse.type= "t", "norm", "euclid"
fviz_cluster(k2, data = df, ellipse.type = "norm")
fviz_cluster(k2, data = df, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())


#dendograma
res2 <- hcut(df, k = 2, stand = TRUE)
fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF"))

#pasar los cluster al df inicial para trabajar en el nuevo lienzo
library(dplyr)
Salary_Data %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

df <- Salary_Data
df
df$clus<-as.factor(k2$cluster)
df

df <- Salary_Data
df <- scale(df)
df<- as.data.frame(df)
df$clus<-as.factor(k2$cluster)
df

library(tidyr)
df$clus<-factor(df$clus)
data_long <- gather(df, caracteristica, valor, YearsExperience :Salary , factor_key=TRUE)
data_long

#grafica el nuevo df
ggplot(data_long, aes(as.factor(x = caracteristica), y = valor,group=clus, colour = clus)) + 
  stat_summary(fun = mean, geom="pointrange", size = 1)+
  stat_summary(geom="line")
geom_point(aes(shape=clus))