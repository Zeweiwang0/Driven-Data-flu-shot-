import pandas as pd 
import numpy as np

# Lectura de datos en primer lugar 
df_train_x = pd.read_csv(r"C:/Users/zewei/Desktop/Trabajo final preprocesamiento/training_set_features.csv")
df_train_y = pd.read_csv(r"C:/Users/zewei/Desktop/Trabajo final preprocesamiento/training_set_labels.csv")
df_test_x = pd.read_csv(r"C:/Users/zewei/Desktop/Trabajo final preprocesamiento/test_set_features.csv")


from sklearn.impute import KNNImputer 
df_train_num = df_train_x.select_dtypes(include= "number")
imputador = KNNImputer(n_neighbors=35)

df_train_imputado = imputador.fit_transform(df_train_num)
df_train_imputado = pd.DataFrame(df_train_imputado)
df_train_imputado.to_csv("imputacionknn_datos_training")

# Pasamos a los datos de testing 
df_test_x_num = df_test_x.select_dtypes(include="number")
df_test_x_num = imputador.fit_transform(df_test_x_num)

df_test_x_num= pd.DataFrame(df_test_x_num)
df_test_x_num.to_csv("imputacionknn_datos_testing")
