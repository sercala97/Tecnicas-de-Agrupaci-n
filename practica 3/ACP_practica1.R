#################################################################
######### MASTER EN DATA SCIENCE PARA FINANZAS - CUNEF ##########
#################################################################
####### TÉCNICAS DE ANÁLISIS ESTADÍSTICO EN DATA SCIENCE I ######
#################################################################
####### PRÁCTICA 1 DE ANÁLISIS DE COMPONENTES PRINCIPALES #######
#################################################################

.libPaths()
## cargamos factoextra, factomineR y la librería iris, de la que quitamos "Species" para hacer el ACP

library(factoextra)

library(FactoMineR)
data(iris)
summary(iris)

acp= PCA(iris[,-5], graph=T) 



# extraemos los autovalores y observamos la varianza explicada

acp$eig # con FacotMineR
get_eig(acp) #con factoextra


# hacemos el screeplot

fviz_eig(acp, addlabels=TRUE, hjust = -0.3)+
        labs(title="Scree plot / Gráfico de sedimentación", x="Dimensiones", y="% Varianza explicada")
        theme_minimal()

# Relación de las variables con los CCPP

var=get_pca_var(acp) #factoextra
var

acp # resultados del ACP en FactoMineR

# coordenadas y contribuciones de las variables
var$coord #coordenadas de las observaciones (ind) o variables (var)
var$contrib  # contribuciones (en %) de las variables a los CCPP. La contribución de una variable (var) 
                # a un CP dado es (en %): (var.cos2 * 100) / (total cos2 del componente).
var$cor # correlaciones de las observaciones (ind) o variables (var)
var$cos2 # representa la calidad de la representación para las variables sobre el mapa factorial. 
        # Se calcula como el cuadrado de las coordenadas: var.cos2 = var.coord * var.coord

var$cos2
var$coord^2

x=var$coord[,1]
y=var$coord[1,]

sum(x^2)
sum(y^2)

z=var$cos2[,1] # cos2 del CP1
sum(z) # cos2 total del CP1 (Autovalor del CP1)
z[1]/sum(z)*100 # contribución de la primera variable a la explicación del CP1

#Con factominer
acp$var$coord
acp$var$cor
acp$var$cos2
acp$var$contrib


# Gráfico por defecto de las variables en el espacio de dos CCPP ...
fviz_pca_var(acp, col.var = "steelblue")

## ... del que modificamos los colores para visualizar mejor la contribución de la variable en el eje principal

fviz_pca_var(acp, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel=TRUE) +
                              labs(title="Mapa de ejes principales")+
        theme_minimal()

fviz_pca_var(acp, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Contribución de las variables a cada uno de los ejes principales
## eje 1
fviz_contrib(acp, choice="var", axes = 1 )+
        labs(title = "Contribuciones a la Dim 1")
## eje 2
fviz_contrib(acp, choice="var", axes = 2 )+
        labs(title = "Contribuciones a la Dim 2")

## ambos ejes
fviz_contrib(acp, choice="var", axes = 1:2)+
        labs(title = "Contribuciones a las dos dimensiones")

####

# Extraccción de resultados para individuos

ind = get_pca_ind(acp)
ind

#Coordenadas de los individuos / observaciones en el plano de los ejes principales
head(ind$coord) 
## Dice bastante poco, así que será mejor representarlos en un mapa 2D

# 1.  repel = TRUE para evitar solapamientos
# 2. Control automatico del color de los indiv mediante la opción cos2, calidad de los individuos sobre el mapa de ejes;
#       emplear sólo puntos, mejor;
# 3. Emplear el gradiente de color

#cos2 representa la comunalidad de la variable en el factor, el cuadrado de la carga factorial; su suma por factores es uno 

fviz_pca_ind(acp, repel = T, col.ind = "cos2")+
        scale_color_gradient2(low="blue", mid="white",
                              high="red", midpoint=0.6)+
        theme_minimal()

## parece observarse disposiciones de las observaciones por grupos; procedemos a identificarlos
# Grupos por colores, opción "" habillage=iris$Species ""
# Para evitar información redundante, eliminamos los identificadores de las observaciones: geom = "point"

acp_g= fviz_pca_ind(acp, geom = "point",
                  habillage=iris$Species, addEllipses=TRUE,
                  ellipse.level= 0.95)+
                  labs(title = "Puntuaciones de las observaciones en las dimensiones")+
                  theme_minimal()
print(acp_g) 

## Por último, realizamos un gráfico conjunto de observaciones y variables, etiquetando sólo éstas:

fviz_pca_biplot(acp,  label="var", habillage=iris$Species,
                addEllipses=TRUE, ellipse.level=0.95) +
        labs(title = "Gráfico conjunto de observaciones y variables - Biplot")+
        theme_minimal()
