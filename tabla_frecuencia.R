################## EJEMPLO PARA DISTRIBUCIÓN UNIFORME #################################

# datos 
edades <- c(22, 38, 29, 20, 20, 24, 21, 19, 25, 25, 28, 19, 21, 22, 22, 25, 19, 21,
            30, 27, 26, 29, 28, 25, 25, 22, 21, 23, 22, 33, 28, 18, 36, 25, 25, 18,
            27, 28, 29, 29, 20, 27, 20, 20, 32, 28, 27, 26, 23, 22, 25, 25, 24, 24, 
            25, 26, 23, 25, 26, 19, 21, 26, 21, 21, 25, 27, 24, 28, 29, 20, 23, 23,
            27, 30, 26, 28, 26, 26, 29, 28, 30, 23, 32, 31, 30, 22, 25, 19, 25, 20,
            23, 29, 23, 24, 26, 36, 23, 27, 26, 28)

# cálculo de la cantidad de clases con la regla de Sturges
k1 <- nclass.Sturges(edades)

# creación de la tabla con la cantidad de intervalos
tabla1 <- cut(edades, breaks=k1)

# tabla de frecuencias absolutas 
FA1 <- table(tabla1)
FA1

# tabla de frecuencias relativas
FR1 <- table(tabla1)/length(edades)
FR1

barplot(FR1, ylab='Probabilidades')

#################### EJEMPLO PARA UNA DISTRIBUCION NORMAL #############################

# datos
datos1 <- rnorm(1000)

# calculo de clases
k2 <- nclass.Sturges(datos1)

# tabla de frecuencias
tabla2 <- cut(datos1, breaks=k2)

# frecuencias absolutas
FA2 <- table(tabla2)
FA2

barplot(FA2, ylab='Frecuencias')

# frecuencias relativas
FR2 <- table(tabla2)/length(datos1)
FR2

barplot(FR2, ylab='Probabilidades')

#################### EJEMPLO PARA UNA DISTRIBUCION EXPONENCIAL #############################

# simulación de 1000 exponenciales con media uno
datos2 <- rexp(1000, rate = 1)

# cantidad de clases
k3 <- nclass.Sturges(datos2)

# tabla de frecuencias
tabla3 <- cut(datos2, breaks=k3)

# frecuencias absolutas
FA3 <- table(tabla3)
FA3

barplot(FA3, ylab='Frecuencias')

# frecuencias relativas
FR3 <- table(tabla3)/length(datos2)
FR3

barplot(FR3, ylab='Probabilidades')

################### EJEMPLO PARA UNA DISTRIBUCION DE POISSON #########################
#                                                                                    #      
# Esta distribución se usa cuando los datos representan                              #
# el número de ocurriencias por unidad de tiempo.                                    #
# Por ejemplo, el número de llegadas de autos a un                                   #
# establecimiento en una hora.                                                       #
#                                                                                    #
######################################################################################

# datos
# lambda es el promedio de ocurrencias por unidad de tiempo (quizas 10 carros por minuto)
datos3 <- rpois(1000, lambda = 10)

# calculo de la cantidad de intervalos
k4 <- nclass.Sturges(datos3)

# frecuencias
tabla4 <- cut(datos3, breaks=k4)

# frecuencias absolutas
FA4 <- table(tabla4)
FA4

barplot(FA4, ylab='Frecuencias')

# frecuencias relativas
FR4 <- table(tabla4)/length(datos3)
FR4

