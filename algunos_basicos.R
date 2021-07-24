
################################ 1. Manipulación de datos #############################

# edades de los empleados de una empresa
edades <- c(22, 38, 29, 20, 20, 24, 21, 19, 25, 25, 28, 19, 21, 22, 22, 25, 19, 21,
            30, 27, 26, 29, 28, 25, 25, 22, 21, 23, 22, 33, 28, 18, 36, 25, 25, 18,
            27, 28, 29, 29, 20, 27, 20, 20, 32, 28, 27, 26, 23, 22, 25, 25, 24, 24, 
            25, 26, 23, 25, 26, 19, 21, 26, 21, 21, 25, 27, 24, 28, 29, 20, 23, 23,
            27, 30, 26, 28, 26, 26, 29, 28, 30, 23, 32, 31, 30, 22, 25, 19, 25, 20,
            23, 29, 23, 24, 26, 36, 23, 27, 26, 28)

summary(edades)

tab <- table(edades)
tab

boxplot(edades)

bar <- barplot(tab, xlab = "Edades", ylab = "Frecuencias", col = "blue",
               main = "Edades de los empleados de la empresa.")

text(bar, tab + 1, labels = tab)
tab

# tabla de frecuencias
# se genera el numero de clases con la regla de Sturges
k <- nclass.Sturges(edades)

# se separa los valores de edades en k intervalos de clase
intervalos <- cut(edades, breaks=k)

# ahora tabulamos cuantos valores cayeron en cada intervalo
FA <- table(intervalos)

FA

# grafica de las frecuencias
barplot(FA)

# frecuencia relativa
# calcular la probabilidad de caer en cada intervalo
FR <- table(intervalos)/length(edades)

FR

barplot(FR)

############################### 2. Ténicas de conteo #################################

library(gtools)

N <- 5  # Número de elementos
n <- 2 # grupos de 2 en 2

alumnos <- c(1:N) # Son los alumnos con id un número consecutivo

combinaciones <- combinations(N, n, alumnos)

bolas <- c("rojo", "amarillo", "azul")

permutations(3, 2, bolas, repeats.allowed = F) # sin reemplazamiento
permutations(3, 2, bolas, repeats.allowed=T) # con reemplazamiento

combinations(3, 2, bolas)
permutations(2, 2, bolas)

# numerico

nums <- c(12, 7, 9, 4, 18)
p1 <- permutations(5, 2, nums, repeats.allowed = F) # sin reemplazamiento
p2 <- permutations(5, 2, nums, repeats.allowed = T) # con reemplazamiento
p

suma <- (p1[1,1] + p1[1,2])/2

v <- numeric()

for(fila in 1:20)
{
  sumas <- (p1[fila, 1] + p1[fila, 2]) / 2
  v[fila] <- sumas
  print(sumas)
}


############################# 3. Trabajando con dataframe Iris #######################

flor <- iris
ancho_P <- flor$Petal.Width
ancho_S <- flor$Sepal.Width

largo_P <- flor$Petal.Length
largo_S <- flor$Sepal.Length

barplot(table(ancho_S), col = "blue")
table(ancho_S)
summary(ancho_S)

barplot(table(largo_S), col = "orange")

sepalosL <- matrix(table(largo_S))

ap <- table(ancho_P)

a_Petalos <- barplot(table(ancho_P), col = "pink")

text(a_Petalos, ap + 1, labels = ap)

############################# 4. Trabajo de banco #####################################

#library(readr) # no es necesario

# buscar la ruta del archivo
file.choose()

# cargar el archivo en la variable banco
banco <- read.csv2("C:\\Users\\Patricia\\Documents\\RParaEstadistica\\practicaLibro\\bank.csv")

# guardar las edades en una variable
edades <- banco$age

promedio <- mean(edades)
edadesF <- data.frame(table(edades))
barplot(table(edades), col = "blue")
summary(edades)

trabajos <- banco$job

colores <- c("blue", "green", "orange", "purple", "brown", "yellow", "black", "pink", "plum", 
             "salmon", "beige", "red")

barplot(table(trabajos), col = colores, xlab = "Trabajos", main = "Trabajos de clientes del banco.")
cantT <- data.frame(table(trabajos))


################################ 5. FUNCIONES #####################################
#                                                                                 #
#     mi_funcion <- function(arg1, arg2,..., argn)                                #
#           {                                                                     #
#               operaciones                                                       # 
#               resultado                                                         #
#           }                                                                     #
#                                                                                 #           
###################################################################################


sumar <- function(n1, n2)
{
   n1 + n2
}

sumar(n1=4, n2=2)


# calcular la media

media <- function(valores, n)
{
   sum(valores)/n
}

valores <- c(10, 10, 10, 20)

media(valores, n=4)




