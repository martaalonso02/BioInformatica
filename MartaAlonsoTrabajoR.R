#PREGUNTA 1
setwd("C:/Users/marta/Downloads/TRABAJOR")#Con setwd nos dirigiremos al directorio que pasemos por el argumento.

data <- read.table("datos-trabajoR.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) #Con la función read.table leeremos datos tabulares desde un archivo de texto y cargarlos en un objeto del tipo data 

head(data)#Con esta función nos devuelve las primeras filas del objeto data que hemos creado.

summary(data)#La utilizamos para obtener un resumen estadístico del objeto, es útil para comprender la distribución de los datos y obtener información clave sobre su estructura.

dim(data)#Nos devuelve las dimensiones que tiene el array, el [1] nos dice que es de dos elementos, y los siguientes números son las filas y columnas respectivamente.

str(data)#Nos dice el número de filas y columnas mostrándonos el nombre de ellas y sus primeros valores.

#Existen 3 variables y 50 tratamientos.

#PREGUNTA 2
boxplot(data$Variable1,col="pink",main="boxplot de la Variable1")#Con esta función creamos un boxplot de la primera variable y lo pintamos de rosa
boxplot(data$Variable2,col="purple",main="boxplot de la Variable2")#Con esta función creamos el boxplot de la variable 2 y lo pintamos de morado

#PREGUNTA 3
plot(data$Variable1, data$Variable2, col = data$Tratamiento, pch = 19, xlab = "Variable1", ylab = "Variable2", main = "Gráfico de dispersión de la Variable1 vs Variable2")#Con esta función creamos un gráfico de dispersión , en el que estén enfrentados la variable 1 y la variable 2, cada una de distinto color

#PREGUNTA 4
legend("topright", legend = levels(data$Tratamiento), col = unique(data$Tratamiento), pch = 19, title = "Tratamiento")#Con esta función crearemos una leyenda a partir del plot que hemos creado arriba deberemos cargar el gráfico primero para que se nos añada la leyenda.

#PREGUNTA 5
hist(data$Variable1,col="pink",main="histograma de la Variable1")#Con esta función, creamos un histograma, usando los colores que hemos utilizado anteriormente para el boxplot de la variable 1
hist(data$Variable2,col="purple",main="histograma de la Variable2")#Con esta función, creamos un histograma, usando los colores que hemos utilizado anteriormente para el boxplot de la variable 2

#PREGUNTA 6
data$TratamientoFactor <- factor(data$Tratamiento)# Crea un factor a partir de la columna "Tratamiento" y guardarlo en una nueva variable llamada "TratamientoFactor"

#PREGUNTA 7
resultados_aggregate_Variable1 <- aggregate(Variable1 ~ TratamientoFactor, data = data, FUN = function(x) c(Media = mean(x), Desviacion = sd(x)))#Calculamos la media y la desviación estandar de la Variable1 para cada tratamiento
resultados_aggregate_Variable2 <- aggregate(Variable2 ~ TratamientoFactor, data = data, FUN = function(x) c(Media = mean(x), Desviacion = sd(x)))#Calculamos la media y la desviación estandar de la Variable2 para cada tratamiento

print("Variable1:")#Nos enseña el valor de la Variable1
print(resultados_aggregate_Variable1)#Nos enseña el contenido de la variable2 en este tratamiento
print("Variable2:")#Nos enseña el valor de la Variable2
print(resultados_aggregate_Variable2)#Nos enseña el contenido de la Variable2 en este tratamiento

resultados_media_Variable1 <- tapply(data$Variable1, data$TratamientoFactor, FUN = mean)#Con esta función obtenemos los resultados de la media de la Variable1 en "TratamientoFactor"
resultados_desviacion_Variable1 <- tapply(data$Variable1, data$TratamientoFactor, FUN = sd)#Con esta función obtenemos los resultados de la desviación estándar de la Variable1 en "TratamientoFactor"

resultados_media_Variable2 <- tapply(data$Variable2, data$TratamientoFactor, FUN = mean)#Con esta función obtenemos los resultados de la media de la Variable2 en "TratamientoFactor"
resultados_desviacion_Variable2 <- tapply(data$Variable2, data$TratamientoFactor, FUN = sd)#Con esta funcion obtenemos los resultados de la desviación estándar de la Variable2 en "TratamientoFactor"

print("Variable1 - Media:")#Con esta función nos muestra la media de la Variable1
print(resultados_media_Variable1)#Esta función nos da el resultado de las medias calculadas de la Variable1 para el "TratamientoFactor"
print("Variable1 - Desviación Estándar:")#Con esta función nos enseña la desviación estándar de la Variable1
print(resultados_desviacion_Variable1)#Esta función nos da el resultado de la desviación estándar de la Variable1 para el "TratamientoFactor"

print("Variable2 - Media:")#Esta función nos muestra la media de la Variable2
print(resultados_media_Variable2)#Esta función nos da el resultado de las medias calculadas de la Variable2 para "TratamientoFactor"
print("Variable2 - Desviación Estándar:")#La función nos muestra la desviación estándar para la Variable2
print(resultados_desviacion_Variable2)#Esta función nos muestra los resultados de la desviación estándar para la Variable2 para "TratamientoFactor"

#PREGUNTA 8
frecuencia_tratamiento <- table(data$TratamientoFactor)#Con esta función utilizada en el factor "TratamientoFactor" contamos los elementos para cada tratamiento

print(frecuencia_tratamiento)#Esta función nos permite ver la información relacionada con la frecuencia de tratamientos

#PREGUNTA 9 
tratamiento_1 <- data[data$TratamientoFactor == "Tratamiento 1", ]#Extraemos los datos para el tratamiento 1 y los guardamos en una variable
tratamiento_4 <- data[data$TratamientoFactor == "Tratamiento 4", ]#Extraemos los datos para el tratamiento 4 y los guardamos en una variable

#PREGUNTA 10
#Tenemos una hipótesis nula que dice que la media del tratamiento 1 es variable 1 es igual a la media del tratamiento 4 en la variable 1
T1V1<-data[1:10,2]#Esta función nos da los datos del tratamiento 1 para la variable 1
T4V1<-data[30:40,2]#Esta función nos da los datos del tratamiento 4 para la variable 1

#Estas dos funciones sirven para comprobar si nuestros datos siguen una distribución normal si el pvalue>0.005 significa que nuestros datos siguen una distribución normal
shapiro.test(T1V1)#> de 0.005, sigue una distribución normal
shapiro.test(T4V1)#> de 0.005, sigue una distribución normal

#Para saber si se cumple la hipótesis, debemos comparar el valor de las medias con t.test
mT1V1<-mean(T1V1)#Hallamos las medias, del tratamiento 1 para la variable 1
mT4V1<-mean(T4V1)#Hallamos las medias del tratamiento 4 para la variable 1

#Si el resultado del pvalue al hacer el t.test es <0.005, indicará que las medias son diferentes
t.test(T1V1,T4V1,mu=0)#El resultado es <0.005, por lo que se rechaza la hipótesis nula, es decir, las medias el tratamiento 1 y 4 son diferentes

#Ahora calculamos la misma comparación, pero esta vez con sus varianzas. Utilizamos el F-test
var.test(T1V1,T4V1)#El resultado da <0.005, por lo que también se rechaza la hipótesis nula, las varianzas son diferentes

