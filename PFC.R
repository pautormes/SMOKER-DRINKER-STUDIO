install.packages("pheatmap")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("caret")
install.packages("tree")
install.packages("dummies")
library(dplyr)
library(ggplot2)
library(plotly)
library(pheatmap)
library(rpart)
library(rpart.plot)
library(caret)
library(tree)
library(readr)
install.packages("fastDummies")
library(fastDummies)


tabla_completa <- read_csv("smoking_driking_dataset_Ver01.csv")


# Creamos una tabla sin caracteres str -----------------------------------------------

tabla2 <- tabla_completa %>%
  mutate(sex=case_when(
    sex=="Female"~1,
    TRUE~0
  ), DRK_YN=if_else(DRK_YN=="Y",1,0))

tabla2<- tabla2 %>% 
  mutate(
    IMC = weight/(height/100)^2
  )

tabla2 <- subset(tabla2, select = )
print(df)


# Dummies  ----------------------------------------------------------------

# Crear variables dummy para las columnas categóricas
tabla_dummies <- dummy_cols(tabla2, select_columns = c("SMK_stat_type_cd"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)

# Mostrar las primeras filas del data frame resultante
head(tabla_dummies)

# Calculamos la correlación y mapa de calor -----------------------------------------------

correlacion<-cor(tabla2) 

pheatmap(correlacion, 
         color = colorRampPalette(c("blue", "white", "red"))(50), 
         cellheight = 15,
         cellwidth = 20,
         cluster_rows = FALSE, 
         cluster_cols = FALSE, 
         display_numbers = TRUE)

determinante<-det(correlacion)
print(determinante)


# media de peso agrupado por sexo y si es bebedor o no
ggplotly(
tabla_completa %>%
  select(sex, weight, DRK_YN) %>%
  group_by(sex, DRK_YN) %>%
  summarise(media_weight = mean(weight)) %>% 
  ggplot(
    mapping = aes(x = sex, y = media_weight, fill = DRK_YN)
  ) + 
  geom_col(position="dodge2") +
  theme_classic()
)


# media de hemoglobina agrupado por sexo y si es bebedor o no
tabla_completa %>%
  select(sex, hemoglobin, DRK_YN) %>%
  group_by(sex, DRK_YN) %>%
  summarise(media_hemoglobina = mean(hemoglobin))
# media de colesterol_LDL agrupado por sexo y si es bebedor o no
tabla_completa %>%
  select(sex, LDL_chole, DRK_YN) %>%
  group_by(sex, DRK_YN) %>%
  summarise(media_colesterol_LDL = mean(LDL_chole))
# media de colesterol_HDL agrupado por sexo y si es bebedor o no
tabla_completa %>%
  select(sex, HDL_chole, DRK_YN) %>%
  group_by(sex, DRK_YN) %>%
  summarise(media_colesterol_HDL = mean(HDL_chole))

# Por si queremos crear un nueva columna juntando DRK y SMK

df <- tabla_completa %>%
  mutate(Smoking_Drinking_Status = case_when(
    SMK_stat_type_cd == 3 & DRK_YN == "Y" ~ "Fuma y Bebe",
    SMK_stat_type_cd == 3 & DRK_YN == "N" ~ "Fuma y No Bebe",
    SMK_stat_type_cd == 1 & DRK_YN == "Y" ~ "No Fuma y Bebe",
    SMK_stat_type_cd == 1 & DRK_YN == "N" ~ "No Fuma y No Bebe",
    SMK_stat_type_cd == 2 & DRK_YN == "Y" ~ "Ha dejado de fumar y Bebe",
    SMK_stat_type_cd == 2 & DRK_YN == "N" ~ "Ha dejado de fumar y No Bebe",
    TRUE ~ "Otro"
  ))
head(df)


# Creamos la tabla de las x -----------------------------------------------


tabla_x_fumar<-tabla2[,-23]
matriz_correlacion_x_fumar<-cor(tabla_x_fumar)
pheatmap(matriz_correlacion_x_fumar, 
         color = colorRampPalette(c("blue", "white", "red"))(50), 
         cellheight = 15,
         cellwidth = 20,
         cluster_rows = FALSE, 
         cluster_cols = FALSE, 
         display_numbers = TRUE)

determinante_x_fumar<-det(matriz_correlacion_x_fumar)
print(determinante_x_fumar)


# Calculamos VIF ----------------------------------------------------------

# Iterar sobre cada columna creando un bucle

for (i in colnames(tabla_x_fumar)) {
  # Separar las variables predictoras (X) y la variable dependiente (y)
  X <- tabla_x_fumar[, colnames(tabla_x_fumar) != i, drop = FALSE]
  y <- tabla_x_fumar[, colnames(tabla_x_fumar) == i, drop = FALSE]
  
  # Ajustar el modelo de regresión lineal
  modelo_n <- lm(as.formula(paste(i, "~ .")), data = data.frame(y, X))
  
  # Calcular R^2
  r_cuadrado <- summary(modelo_n)$r.squared
  
  # Calcular VIF
  VIF <- 1 / (1 - r_cuadrado)
  
  # Imprimir VIF
  print(paste("VIF for", i, ":", VIF))
}

# Eliminamos la columna de weight

tabla_x_fumar<-tabla_x_fumar[,-4]

for (i in colnames(tabla_x_fumar)) {
  # Separar las variables predictoras (X) y la variable dependiente (y)
  X <- tabla_x_fumar[, colnames(tabla_x_fumar) != i, drop = FALSE]
  y <- tabla_x_fumar[, colnames(tabla_x_fumar) == i, drop = FALSE]
  
  # Ajustar el modelo de regresión lineal
  modelo_n <- lm(as.formula(paste(i, "~ .")), data = data.frame(y, X))
  
  # Calcular R^2
  r_cuadrado <- summary(modelo_n)$r.squared
  
  # Calcular VIF
  VIF <- 1 / (1 - r_cuadrado)
  
  # Imprimir VIF
  print(paste("VIF for", i, ":", VIF))
}

# Eliminamos la columna de LDL_chole

tabla_x_fumar<-tabla_x_fumar[,-13]
for (i in colnames(tabla_x_fumar)) {
  # Separar las variables predictoras (X) y la variable dependiente (y)
  X <- tabla_x_fumar[, colnames(tabla_x_fumar) != i, drop = FALSE]
  y <- tabla_x_fumar[, colnames(tabla_x_fumar) == i, drop = FALSE]
  
  # Ajustar el modelo de regresión lineal
  modelo_n <- lm(as.formula(paste(i, "~ .")), data = data.frame(y, X))
  
  # Calcular R^2
  r_cuadrado <- summary(modelo_n)$r.squared
  
  # Calcular VIF
  VIF <- 1 / (1 - r_cuadrado)
  
  # Imprimir VIF
  print(paste("VIF for", i, ":", VIF))
}
print(r_cuadrado)


# Separamos los datos entre train y test ----------------------------------

trainIndex <- createDataPartition(tabla2$SMK_stat_type_cd, p = 0.75, list = FALSE)
data_train <- tabla2[trainIndex, ]
data_test <- tabla2[-trainIndex, ]

X_train <- data_train[, -which(names(data_train) == "SMK_stat_type_cd")]
y_train <- data_train$SMK_stat_type_cd
X_test <- data_test[, -which(names(data_test) == "SMK_stat_type_cd")]
y_test <- data_test$SMK_stat_type_cd

# Creamos el modelo de arbol

control <- rpart.control(7)

clf <- rpart(SMK_stat_type_cd ~ ., data = data_train, method = "class",control = control)

# Realizar predicciones en el conjunto de prueba

y_pred <- predict(clf, newdata = X_test, type = "class")

# Calcular la precisión del modelo
accuracy <- sum(y_pred == y_test) / length(y_test)
print(paste('Accuracy of the Decision Tree classifier:', accuracy*100))

rpart.plot(clf, type = 2, extra = 104, fallen.leaves = TRUE, cex = 0.6)


# Vemos si los resultados mejoran con el modelo PCA -----------------------

# Lo hacemos primero con la tabla sin las columnas que hemos eliminado tras el VIF

modelo <- prcomp(tabla_x_fumar, center = TRUE, scale. = TRUE)
# Mostrar el modelo
summary(modelo)
X_pca <- modelo$x[,]
X_pca_tabla <- data.frame(X_pca)


correlacion_PCA<-cor(X_pca_tabla)


X_finales <- X_pca_tabla[, c("PC1", "PC2", "PC3", "PC4","PC5", "PC6", "PC7","PC8", "PC9", "PC10","PC11", "PC12", "PC13","PC14", "PC15", "PC16","PC17", "PC18", "PC19","PC20", "PC21", "PC22")]
tabla_Entera<-X_finales
tabla_Entera$Fumadores<-tabla2$"SMK_stat_type_cd"
tabla_Entera


# Aplicamos de nuevo el modelo del árbol a partir de las X finales --------

y <- tabla2$SMK_stat_type_cd
X <- X_finales

set.seed(15)  # Fijamos la semilla para obtener resultados reproducibles
trainIndex <- createDataPartition(y, p = 0.75, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Entrenar el modelo de árbol de decisión
clf <- rpart(y_train ~ ., data = X_train, method = "class", control = rpart.control(maxdepth = 10))

# Predecir en el conjunto de prueba
y_pred <- predict(clf, X_test, type = "class")

# Calcular la precisión del modelo
accuracy <- sum(y_pred == y_test) / length(y_test)
print(paste('Accuracy of the Decision Tree classifier:', accuracy * 100))

# Creamos el modelo de arbol

control <- rpart.control(10)

clf <- rpart(SMK_stat_type_cd ~ ., data = data_train, method = "class",control = control)

# Realizar predicciones en el conjunto de prueba

y_pred <- predict(clf, newdata = X_test, type = "class")

# Calcular la precisión del modelo
accuracy <- sum(y_pred == y_test) / length(y_test)
print(paste('Accuracy of the Decision Tree classifier:', accuracy*100))


# Ahora volvemos a calcular el PCA pero esta vez con la tabla con  --------

modelo <- prcomp(tabla2, center = TRUE, scale. = TRUE)
# Mostrar el modelo
summary(modelo)
X_pca <- modelo$x[,]
X_pca_tabla <- data.frame(X_pca)


correlacion_PCA<-cor(X_pca_tabla)


X_finales <- X_pca_tabla[, c("PC1", "PC2", "PC3", "PC4","PC5", "PC6", "PC7","PC8", "PC9", "PC10","PC11", "PC12", "PC13","PC14", "PC15", "PC16","PC17", "PC18", "PC19","PC20", "PC21", "PC22")]
tabla_Entera<-X_finales
tabla_Entera$Fumadores<-tabla2$"SMK_stat_type_cd"
tabla_Entera

# Aplicamos de nuevo el modelo del árbol a partir de las X finales con todas las columnas --------

y <- tabla2$SMK_stat_type_cd
X <- X_finales

set.seed(10)  # Fijamos la semilla para obtener resultados reproducibles
trainIndex <- createDataPartition(y, p = 0.75, list = FALSE)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Entrenar el modelo de árbol de decisión
clf <- rpart(y_train ~ ., data = X_train, method = "class", control = rpart.control(maxdepth = 10))

# Predecir en el conjunto de prueba
y_pred <- predict(clf, X_test, type = "class")

# Calcular la precisión del modelo
accuracy <- sum(y_pred == y_test) / length(y_test)
print(paste('Accuracy of the Decision Tree classifier:', accuracy * 100))

# Obtener los valores singulares del modelo
auto_valores <- modelo$sdev^2

# Proporción de varianza explicada por cada componente
proporcion_varianza <- auto_valores / sum(auto_valores)
proporcion_varianza


# Obtener la proporción de varianza explicada acumulada
proporcion_varianza_acumulada <- cumsum(proporcion_varianza)
proporcion_varianza_acumulada

# Obtener los componentes absolutos
componentes_absolutas <- abs(modelo$rotation)
componentes_absolutas

# Normalizar la segunda componente
componente_2_normalizada <- componentes_absolutas[, 22] / sum(componentes_absolutas[, 2])
componente_2_normalizada

# Sumar la proporción de varianza explicada por las dos primeras componentes principales
varianza_explicada_2_componentes <- sum(proporcion_varianza[1:22])
varianza_explicada_2_componentes