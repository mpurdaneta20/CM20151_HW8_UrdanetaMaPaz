library(ggplot2)
library(caret)
library(plyr)
library(kernlab)
library(fRegression)

#Leer los datos de training y test
#Es importante recalcar que para que se ejecute este codigo es importante establecer la direccion donde se ubican los archivos training y test 

Training <- read.csv("C:/Users/Paz U/Desktop/CM20151HW8_URDANETA/INTENTO9/training_set.csv")
Test <- read.csv("C:/Users/Paz U/Desktop/CM20151HW8_URDANETA/INTENTO9/test_set.csv")

Training$eventos_cod <- revalue(Training$eventos, c("Ninguno"="1", "Lluvia"="2","Niebla"="3","Nieve"="4","Lluvia-Nieve"="5","Niebla-Lluvia"="6","Lluvia-Tormenta"="7","Niebla-Lluvia-Nieve"="8","Niebla-Nieve"="9"))
Test$eventos_cod <- revalue(Test$eventos, c("Ninguno"="1", "Lluvia"="2","Niebla"="3","Nieve"="4","Lluvia-Nieve"="5","Niebla-Lluvia"="6","Lluvia-Tormenta"="7","Niebla-Lluvia-Nieve"="8","Niebla-Nieve"="9"))

#Descripcion de variables

#Col 2. cod_calendario: variable binaria que determina un evento especial dentro del calendario
#Col 3. conteo_ordenes: Variable numerica que indica el numero de pedidos (Variable que se espera estimar)
#Col 4. conteo_restaurantes: Variable numerica que indica el numero de restaurantes 
#Col 5. temp_max:
#Col 6. temp_min:
#Col 7. precipitacion:
#Col 9. eventos_cod: Variable categorica que indica tiempo climatico. Toma el valor de:
  #1- Ninguno
  #2- Lluvia
  #3- Niebla 
  #4- Nieve
  #5- Lluvia-Nieve 
  #6- Lluvia_Niebla
  #7- Lluvia -Tormenta
  #8- Niebla-Lluvia-Nieve
  #9- Niebla-Nieve


## Ajustemos un de regresión lineal múltiple!
## $$ conteo_ordenes = B_0 + B_1 cod_calendario + B_2 conteo_restaurantes + B_3 temp_max + B_4 temp_min + B_5 precipitacion + B_6 eventos_cod $$

Training$conte_temp <- Training$conteo_restaurantes*Training$temp_max
Test$conte_temp <- Test$conteo_restaurantes*Test$temp_max

Training$temptemp <- (Training$conteo_restaurantes)**2
Test$temptemp <- (Test$conteo_restaurantes)**2


#Relvancia de los parametros, usando la tabla ANOVA - analisis de la varianza, para definir que todos los Betas sean relevantes de lo contrario se incurre en el problema de variable redundante o tal vez se pueda incurrir en el problema de variable omitida que incurre a su vez en un problema de estimadores sesgados, inconsistentes e ineficientes, Este tipo de estimadores no resultan buenos estimadores generando una mala estimacion del y (Yhat)
relevancia <- aov(conteo_ordenes ~ cod_calendario+conteo_restaurantes+conte_temp+eventos_cod+temptemp,data=Training)
summary(relevancia)

#Permite mediante un estadistico F llevar acabo la prueba de hipotesis que tiene como h_0 que no hay sesgo de especificacion (Variable omitida o redundante). Prueba de RESET ramsey
resetTest(conteo_ordenes ~ cod_calendario+conteo_restaurantes+conte_temp+eventos_cod+temptemp,data=Training)

#El modelo esta bien especificado (Multiplicadores de lagrange)
modelo<- lm(conteo_ordenes ~ cod_calendario+conteo_restaurantes+conte_temp+eventos_cod,data=Training)

## plantear el modelo usando train de caret usando el metodo robust lineal model
modFit<- train(conteo_ordenes ~ cod_calendario+conteo_restaurantes+conte_temp+eventos_cod+temptemp,method = "rlm",data=Training)

## imprimir los parámetros del modelo final
print(modFit)

## Comparar la predicción en nuestro conjunto de entrenamiento en el dataset de prueba.
pred <- predict(modFit, Test)

# agregar predicción al dataset
predict_test <- Test
predict_test$pred <- pred


# graficar la predicción, colorear por año.
ggplot(predict_test) + geom_point(aes(fecha,pred))

fecha <- Test$fecha
conteo_ordenes <- as.integer(predict_test$pred)

pred.data <- data.frame(fecha,conteo_ordenes)
rownames(pred.data) <- NULL

write.table(pred.data, file = "pred_Urdaneta_9.csv", sep = ",",qmethod = "double",row.names = FALSE)
