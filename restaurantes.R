################## ANALISIS DE UN LISTADO DE RESTAURANTES DE UNA CIUDAD #####################

restaurantes <- read.csv('Top250.csv')

head(restaurantes)

summary(restaurantes)

categorias <- restaurantes$Segment_Category

barplot(table(categorias))

tabla_categorias <- table(restaurantes$Segment_Category)
barplot(tabla_categorias, main = 'Restaurantes en Australia',ylab='Frecuencias')        
plot(tabla_categorias)

# selecciona todos los restaurantes que son categoria 'Chicken'
restaurante_chicken <- table(restaurantes$Restaurant
                             [restaurantes$Segment_Category=='Varied Menu'])
restaurante_chicken

n_chicken <- length(restaurantes$Restaurant
                    [restaurantes$Segment_Category=='Varied Menu'])
n_chicken

# funcion que calcula moda
calcular_moda <- function(vec)
{
  uni <- unique(vec)
  uni[which.max(tabulate(match(vec, uni)))]
}

moda <- calcular_moda(restaurantes$Sales)

length(moda)

# ver los valores nulos
is.na(restaurantes)

valores_nulos <- sum(is.na(restaurantes))
valores_nulos
mean(is.na(restaurantes))

# asignar valor 0 en los NA
restaurantes[is.na(restaurantes)] = 00
head(restaurantes)
restaurantes$Rank

is.na(restaurantes)

# dimension de los elementos en el dataframe restaurantes donde los elemento sean 00
length(restaurantes[restaurantes!=0])
1835+415

250*9

var(restaurantes$Sales)

media <- mean(restaurantes$Sales)
mediana <- median(restaurantes$Sales)

barplot(table(restaurantes$Sales))


################## TABLA DE FRECUENCIA DE DATOS AGRUPADOS #########################

attach(restaurantes) # fijar el dataframe en el que trabajaremos
summary(Sales)

TFI <- as.data.frame(table(Sales = factor(cut(Sales, breaks=10))))

# agregando la frecuencia relativa
attach(TFI)
TFI[['FreqR']] <- with(TFI, Freq/250)

################# PROBANDO TABLA DE FRECUENCIA #####################

tipo_rest <- table(restaurantes$Segment_Category)
tipo_rest

barplot(tipo_rest)

sum(tipo_rest)

warnings()

data_tipo <- data.frame(tipo_rest)

max(data_tipo)
`colnames<-`('Tipo de restaurantes', 'Frecuencias')


