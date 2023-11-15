getwd()
library(ggplot2)
library(ggcorrplot)
library(factoextra)
library(FactoMineR)
install.packages("corrplot")
library(corrplot)


datos <- read.delim("acidos grasos/datos_PCA.txt",
                    header = TRUE, row.names = 1)

#análsisi exploratorio de los datos, análisis de correlación
cor_matrix <- cor(datos[,-1])
head.matrix(cor_matrix)
#visualiza el análisis de correlación 
ggcorrplot(cor_matrix)
corrplot(cor_matrix, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45)

#análisis de componentes principales PCA

respca <- prcomp(datos[,-1], scale. = TRUE) #paquete de R
respca2 <- PCA(datos[,-1], scale.unit = TRUE, graph = FALSE) #paquete FactoMineR
#el dato dentro de los corchetes me permite
#que no use los datos de la primer columna para el análisis
#This standardization to the same scale avoids some variables to become dominant just because of their large measurement units.

head(respca)
summary(respca)
summary(respca2)
print(respca)
print(respca2)

#visualiza los resultados de manera gráfica

eigenvalues <- respca2$eig #los eigenvalores corresponden a la cantidad
                           #variación explicada por cada PC
head(eigenvalues)
barplot(eigenvalues[, 2], names.arg = 1:nrow(eigenvalues),
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col = "steelblue")
lines(1:nrow(eigenvalues), eigenvalues[, 2], type = "b", pch = 19, col="red")

fviz_eig(respca2, addlabels = TRUE) #paquete factoextra

#variables factor map
print(respca2$var$contrib)

plot(respca2, choix = "var")

fviz_pca_var(respca2, col.var = "contrib",
             repel = TRUE) +
  scale_color_gradient2(low = "blue", mid = "white",
                        high = "red", midpoint = 10) +
  theme_bw()

fviz_contrib(respca2, choice = "var")

#Gráfica de los individuos

print(respca2$ind$contrib)

fviz_pca_ind(respca2, col.ind = "cos2") +
  scale_color_gradient2(low="blue", mid = "white",
                        high = "red", midpoint = 0.65) +
  theme_minimal()

respca2$grupo <- datos$grupo

fviz_pca_ind(respca2, addEllipses = TRUE,
             ellipse.level = 0.95)

#Gráfica biplot variables e individuos

fviz_pca_biplot(respca2, col.var = "black",
                col.ind = "red",
                repel = TRUE) +
  labs(x = "PC1 (45.6%)", y = "PC2 (23.9%)")

datos$grupo <- as.factor(datos$grupo)

fviz_pca_biplot(respca2, habillage = datos$grupo,
                addEllipses = TRUE,
                col.var = "red", alpha.var = "contrib",
                label = "var",
                repel = TRUE) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "PC1 (45.6%)", y = "PC2 (23.9%)") +
  theme_minimal()

  
  
#estos comandos me dan diferentes graficas de mis datos

fviz_pca_ind(respca)
fviz_contrib(respca, choice = "var")
fviz_contrib(respca, choice = "ind")



