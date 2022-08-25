rm(list=ls())

library(tidyverse)
library(caret)

# Importacion de los datos 
df_train_x <- read_csv("C:/Users/zewei/Desktop/Trabajo final preprocesamiento/training_set_features.csv")
df_train_y <- read_csv("C:/Users/zewei/Desktop/Trabajo final preprocesamiento/training_set_labels.csv")
df_test_x <-  read_csv("C:/Users/zewei/Desktop/Trabajo final preprocesamiento/test_set_features.csv")



# Separamos en valores numericos, (imputado_knn de python y preprocesado, para el caso numerico) y categoricos
df_train_x_num <- read_csv("C:\\Users\\zewei\\Desktop\\Trabajo final preprocesamiento\\imputacionknn_datos_training")
df_train_x_num <- df_train_x_num[,-(1:2)] # Elimino la 2  columnas que no son utiles.
df_train_cat <- select_if(df_train_x,is.character)
# Eliminamos las 2 columnas con un alto índices de NA :
df_train_cat <- df_train_cat[,1:10]

# Imputamos los valores categoricos por la moda 
moda_cat<- function(vector_cat){
  ifelse(is.na(vector_cat),as.character(names(which.max(table(vector_cat)))),vector_cat) 
  
}

df_train_cat <- apply(df_train_cat,2,moda_cat)
df_train_cat <- as.data.frame(df_train_cat)

# Junto todo en un mismo dataframe de training:
df_train_x <- cbind(df_train_x_num,df_train_cat)



############################################### Lo mismo para el dataframe de testing (imputado_knn de python y preprocesado, parte numérica)



df_test_num <- read_csv("C:\\Users\\zewei\\Desktop\\Trabajo final preprocesamiento\\imputacionknn_datos_testing")
df_test_num <- df_test_num[,-(1:2)] # Elimino la 2 columnas que corresponde a los índices de las filas
df_test_cat <- select_if(df_test_x,is.character)
df_test_cat <- df_test_cat[,1:10] # Elimino las 2 columnas con un alto indice de NAs 

# Limpiamos los na y revoconvertimos a dataframes 
df_test_cat <- apply(df_test_cat,2,moda_cat)
df_test_cat <- as.data.frame(df_test_cat)

# Junto todo en un mismo dataframe de training:
df_test_x<- cbind(df_test_num,df_test_cat)

# Hasta aquí el proceso de limpieza de datos del dataframe 

# Algoritmo de random forest :
# La idea es dividir el dataset de entrenamiento en tantas partes como arboles tenga el bosque  
# Si escogemos 50 arboles ==> dividir el dataset en 50 partes y para cada parte entrenar el modelo 
# correspondiente a un árbol, con rpart
# El resultado final como en este caso nos piden probabilidades, que es un valor continuo (no es un voto para cada categoria como el baggin)
# lo promediaremos con la media, por lo que los pesos serán los mismos para clasificador

# Dataframe: el dataset, tiene que incluir en la última columna la variable a predecir, o bien h1n1_vaccine o bien 
# seasonal_vaccine
# N_trees: el número de árboles quiera poner, pondremos por defecto 50 
# Formula tiene que ser o bien h1n1_vaccine~. o bien seasonal_vaccine ~. 
df_train_x <- cbind.data.frame(df_train_x,df_train_y[2])
library(rpart)

random_forest <- function (dataframe, n_trees=50,formula ){
  
  
  
  
  df_lista <- list()
  clasificador_lista <- list()
  predicciones_lista <- list()
  
  # Divido el dataset en n_trees partes iguales 
  for (i in 1:n_trees){
    n_filas <-  sample (c(1:nrow(dataframe)),1500)
    sample_split <- sample(c(10:20),1)
    sample_depth <- sample(c(15:25),1)
    
    df_lista[[i]] <- dataframe[n_filas,]
    clasificador_lista[[i]] <- rpart(formula= formula, df_lista[[i]],
                                     control = rpart.control(xval = 10,minsplit = sample_split,
                                                             maxdepth = sample_depth,))
    
    predicciones_lista[[i]]<- predict(clasificador_lista[[i]],newdata = df_test_x)
    
  }
  # Convierto las predicciones en un dataframe para posteriormente 
  # sumar por filas y dividir entre el número de arboles (hacer la media )
  predicciones <- as.data.frame(predicciones_lista)
  
  resultados_1 <-apply(predicciones,1,sum)
  resultados_1 <- resultados_1/n_trees
  
  
}

# Obtenemos la variable h1n1_vaccine con el random fores anterior 
h1n1_vaccine <- random_forest(df_train_x,n_trees = 125,h1n1_vaccine~.)

# Eliminamos la columna h1n1_vaccine ya que en el test no aparece dicha variable  y no lo podemos usar como training 
df_train_x <- df_train_x[,-34]
df_train_x <- cbind.data.frame(df_train_x, df_train_y[3])
seasonal_vaccine <- random_forest(df_train_x,100,seasonal_vaccine~.)

respondent_id <- c(26707:53414)



solucion <- cbind.data.frame(respondent_id,h1n1_vaccine ,seasonal_vaccine)
write_csv(solucion,"random_forest8")
