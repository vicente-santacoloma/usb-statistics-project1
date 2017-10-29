###################################################################################################
##                                          PROYECTO 1                                           ##
###################################################################################################
## Integrantes: Johanna Chan                                                                     ##
##              Fatima Querales                                                                  ##
##              Vicente Santacoloma                                                              ##
###################################################################################################

###################################################################################################
## 1. Realice un análisis descriptivo de cada variable contenida en el archivo de datos.         ##
###################################################################################################

Data = read.table("proyect1.dat", header=T, dec=".")

RedMeat = Data[,2]
WhiteMeat = Data[,3]
Eggs = Data[,4]
Milk = Data[,5]
Fish = Data[,6]
Cereals = Data[,7]
Starch = Data[,8]
Nuts = Data[,9]
FrVeg= Data[,10]

## Analisis completo de cada variable
AnalisisRedMeat = summary(RedMeat)
AnalisisWhiteMeat = summary(WhiteMeat)
AnalisisEggs = summary(Eggs)
AnalisisMilk = summary(Milk)
AnalisisFish = summary(Fish)
AnalisisCereals = summary(Cereals)
AnalisisStarch = summary(Starch)
AnalisisNuts = summary(Nuts)
AnalisisFrVeg = summary(FrVeg)

## Deviacion estandar de cada variable
DesviacionRedMeat = sd(RedMeat)
DesviacionWhiteMeat = sd(WhiteMeat)
DesviacionEggs = sd(Eggs)
DesviacionMilk = sd(Milk)
DesviacionFish = sd(Fish)
DesviacionCereals = sd(Cereals)
DesviacionStarch = sd(Starch)
DesviacionNuts = sd(Nuts)
DesviacionFrVeg = sd(FrVeg)

## Agregar estas desviaciones al proyecto.

## Histogramas
hist(RedMeat, main="Histograma de RedMeat", xlab="Porcentaje de RedMeat", ylab="Numero de Paises")
hist(WhiteMeat, main="Histograma de WhiteMeat", xlab="Porcentaje de WhiteMeat", ylab="Numero de Paises")
hist(Eggs, main="Histograma de Eggs", xlab="Porcentaje de Eggs", ylab="Numero de Paises")
hist(Milk, main="Histograma de Milk", xlab="Porcentaje de Milk", ylab="Numero de Paises")
hist(Fish, main="Histograma de Fish", xlab="Porcentaje de Fish", ylab="Numero de Paises")
hist(Cereals, main="Histograma de Cereals", xlab="Porcentaje de Cereals", ylab="Numero de Paises")
hist(Starch, main="Histograma de Starch", xlab="Porcentaje de Starch", ylab="Numero de Paises")
hist(Nuts, main="Histograma de Nuts", xlab="Porcentaje de Nuts", ylab="Numero de Paises")
hist(FrVeg, main="Histograma de FrVeg", xlab="Porcentaje de FrVeg", ylab="Numero de Paises")

## Diagrama de caja
boxplot(RedMeat, main="Boxplot de RedMeat", xlab="Porcentaje de RedMeat", ylab="Numero de Paises")
boxplot(WhiteMeat, main="Boxplot de WhiteMeat", xlab="Porcentaje de WhiteMeat", ylab="Numero de Paises")
boxplot(Eggs, main="Boxplot de Eggs", xlab="Porcentaje de Eggs", ylab="Numero de Paises")
boxplot(Milk, main="Boxplot de Milk", xlab="Porcentaje de Milk", ylab="Numero de Paises")
boxplot(Fish, main="Boxplot de Fish", xlab="Porcentaje de Fish", ylab="Numero de Paises")
boxplot(Cereals, main="Boxplot de Cereals", xlab="Porcentaje de Cereals", ylab="Numero de Paises")
boxplot(Starch, main="Boxplot de Starch", xlab="Porcentaje de Starch", ylab="Numero de Paises")
boxplot(Nuts, main="Boxplot de Nuts", xlab="Porcentaje de Nuts", ylab="Numero de Paises")
boxplot(FrVeg, main="Boxplot de FrVeg", xlab="Porcentaje de FrVeg", ylab="Numero de Paises")

## Diagrama de caja para todas las variables
# boxplot(split(RedMeat, WhiteMeat, Eggs, Milk, Fish, Cereals, Starch, Nuts, FrVeg), main="Boxplot para todas los tipos de proteinas",
# xlab="Porcentaje de consumo", ylab="Numero de Paises")

## Falta justificar el diagrama de caja anterior que involucra a todas las variables.

###################################################################################################
## 2. Agrupe a los países de acuerdo a su consumo de proteínas característico. Justifique (desde ##
## un punto de vista estadístico) el número de grupos formados y sus integrantes.                ##
###################################################################################################

## Agrupacion
# hc = hclust(dist(Data[,2:10]),"complete")
# plot(hc,main="Dendagrama de Consumo de Proteinas Similares")

## Respuesta:
##
## En este dendograma, se puede apreciar que tenemos dos grandes grupos, que a su vez se dividen en
## otros dependiendo de que tan similares sean sus consumos de proteinas. Por ejemplo tenemos a los paises
## 4(Bulgaria) y 25(Yugoslavia) en un solo grupo, lo cual es perfectamente razonable pues ambos paises
## (Yugoslavia ya no es un pais) son vecinos. Ademas a este se le agrega 18(Rumania) con una ligera diferencia,
## el cual es ademas vecino de los otros dos.
##
## Tambien vale la pena destacar que los paises nordicos como 6(Dinamarca), 20(Suecia), 8(Finlandia),
## y 15(Noruega) presenta un consumo de proteinas similar.


###################################################################################################
## 3. Agregue al archivo de datos una nueva variable llamada “zona” utilizando para ello la divi-##
## sión que se presenta en la figura 1. Cada país debe tener una zona asignada. Utilice la infor-##
## mación para responder los siguientes ítems:                                                   ##
###################################################################################################

Data[1,11] = 3
Data[2,11] = 2
Data[3,11] = 1
Data[4,11] = 3
Data[5,11] = 2
Data[6,11] = 1
Data[7,11] = 1
Data[8,11] = 2
Data[9,11] = 4
Data[10,11] = 3
Data[11,11] = 3
Data[12,11] = 1
Data[13,11] = 4
Data[14,11] = 1
Data[15,11] = 1
Data[16,11] = 2
Data[17,11] = 4
Data[18,11] = 3
Data[19,11] = 4
Data[20,11] = 1
Data[21,11] = 4
Data[22,11] = 1
Data[23,11] = 2
Data[24,11] = 1
Data[25,11] = 3
Data[26,11] = 1
Data[27,11] = 3
Data[28,11] = 4
Data[29,11] = 3
Data[30,11] = 2


###################################################################################################
## a. Determine el porcentaje del consumo total de proteínas para cada zona propuesta en el mapa.##
###################################################################################################

Zona = Data[,11]
PromedioZona = matrix(nrow=4, ncol=9)

for (i in 1:4) {

  PromedioZona[i,1] = mean (RedMeat[Zona == i])
  PromedioZona[i,2] = mean (WhiteMeat[Zona == i])
  PromedioZona[i,3] = mean (Eggs[Zona == i])
  PromedioZona[i,4] = mean (Milk[Zona == i])
  PromedioZona[i,5] = mean (Fish[Zona == i])
  PromedioZona[i,6] = mean (Cereals[Zona == i])
  PromedioZona[i,7] = mean (Starch[Zona == i])
  PromedioZona[i,8] = mean (Nuts[Zona == i])
  PromedioZona[i,9] = mean (FrVeg[Zona == i])

}

## Datos Obtenidos de PromedioZona:
##
## Zona  RedMeat    WhiteMeat  Eggs       Milk      Fish      Cereals   Starch  Nuts    FrVeg
## 1     10.930000  10.100000  3.510000   19.55000  6.170000  22.63000  4.9300  1.6700  2.940000
## 2     8.766667   8.500000   2.966667   19.35000  3.383333  34.75000  5.2000  1.6000  4.216667
## 3     7.112500   5.925000   1.687500   10.87500  2.050000  43.10000  2.5375  4.9125  3.375000
## 4     10.200000  7.033333   2.633333   14.26667  5.716667  30.13333  4.3000  3.4000  6.233333


###################################################################################################
## b. Presente un código de R que ejecute un intervalo de confianza que permita determinar si    ##
## dos proporciones son estadísticamente diferentes. ¿Se puede aplicar el código para responder  ##
## si la proporción de países de la zona IV que consumen más del 9% de carnes rojas es significa-##
## tivamente diferente a la proporción de países de la zona III que consumen más del 9% de carnes##
## rojas?.                                                                                       ##
###################################################################################################

# Metodo que genera el intervalo para la diferencia de dos proporciones p1-p2
#
# Recibe como parametro las dos proporciones, los dos tamanos muestrales,y el coeficiente de confianza.
# Retorna el intervalo para p1-p2.
#
# En caso de no cumplirse las condiciones para el intervalo de la diferencia de dos proporciones,
# imprime por la salida estandar las que no se cumplieron.
#
IntervaloProporcion <- function (p1,p2, n1, n2, p) {
  q1 = 1 - p1
  q2 = 1 - p2
  c1 = n1 >= 30
  c2 = n2 >= 30
  c3 = n1*p1 >= 5
; c4 = n1*q1 >= 5
  c5 = n2*p2 >= 5
  c6 = n2*q2 >= 5
  if (c1 && c2 && c3 && c4 && c5 && c6) {
    alfa = 1-p
    z = qnorm(alfa/2, mean = 0, sd = 1, lower.tail=FALSE)
    intervalo = c(0,0)
    intervalo[1] = (p1-p2) - z*sqrt(((p1*q1)/n1) + ((p2*q2)/n2))
    intervalo[2] = (p1-p2) + z*sqrt(((p1*q1)/n1) + ((p2*q2)/n2))
    intervalo
  } else {
      cat("No se cumplieron la(s) condicion(es):\n")
      while (!(c1 && c2 && c3 && c4 && c5 && c6)) {
	if (!c1) {
	  cat("n1 >= 30 \n")
	  c1 = TRUE
	}
	if (!c2) {
	  cat("n2 >= 30 \n")
	  c2 = TRUE
	}
	if (!c3) {
	  cat("n1*p1 >= 5 \n")
	  c3 = TRUE
	}
	if (!c4) {
	  cat("n1*q1 >= 5 \n")
	  c4 = TRUE
	}
	if (!c5) {
	  cat("n2*p2 >= 5 \n")
	  c5 = TRUE
	}
	if (!c6) {
	  cat("n2*q2 >= 5 \n")
	  c6 = TRUE
	}
      }
  }
}

## Respuesta:
## No se puede aplicar el codigo ya que para calcular el intervalo de diferencia entre dos proporciones,
## Se debe cumplir que:
##      n1,n2 >= 30
##      n1p1, n1q1 >= 5
##      n2p2, n2q2 >= 5
##
## Para este caso en particular se tiene que luego de haber realizado el filtro de los paises de la
## Zona IV y III que cumplen las condiciones especificadas en el enunciado mediante:
Zona4 = Data[Data[,11]==4 & Data[,2] >= 9 ,2]
Zona3 = Data[Data[,11]==3 & Data[,2] >= 9 ,2]
## y luego de calcular p1 y p2:
p1 = length(Zona4)/n1
p2 = length(Zona3)/n2
## llamamos a la funcion con:
n1 = 30
n2 = 30
coeficienteConfianza = 0.95
intervalo = IntervaloProporcion(p1, p2, n1, n2, coeficienteConfianza)
## Obteniendo la respuesta:
## No se cumplieron la(s) condicion(es):
## n1*p1 >= 5
## n2*p2 >= 5

###################################################################################################
## c. Proporcione un ordenamiento (ranking de mayor a menor) de las zonas para determinar aque-  ##
## llas que consumen mayor cantidad de buenas proteínas: pescados y nueces.                      ##
###################################################################################################

## Pescados

## Zona 1  6.17
## Zona 4  5.716667
## Zona 2  3.383333
## Zona 3  2.05

## Nueces

## Zona 3  4.9125
## Zona 4  3.4
## Zona 1  1.67
## Zona 2  1.6

###################################################################################################
## d. Indique cuál de los dos criterios de formación de grupos (el utilizado en la pregunta 2    ##
## versus el de la zona geográfica, pregunta 3) es mejor. Justifique.                            ##
###################################################################################################

## Las agrupaciones generadas en la pregunta 2, a traves de un dendograma proporcionan mas informacion,
## ya que dependiendo del metodo con que se realize, podremos ir teniendo diferentes enfoques de los datos.
## Por ejemplo con los metodos:
##
## Ward: Tendremos grupos compactos y esfericos.
## Enlace Completo: Tendremos grupos de datos similares.
## Enlace Simple: Emplea la estrategia de cluster "friends of friends"
## Media: Es objetivo para grupos con caracteristica que esten entre los metodos de Enlace Completo
## y Simple. Por lo general en mas utilizado que los de Enlace Completo y Simple ya que produce
## grupos mas robustos.
## Ademas se tienen otros como Mcquitty, Centroide y Mediana. Sin embargo los dos ultimos pueden ser
## dificiles de interpretar puesto que no dan una medida de distancia monotona.
##
## Por otra parte, agrupando los datos por zona geografica, podemos en algunos casos tener informacion
## de interes, dependiendo de como se haga esta agrupacion. Vale destacar que esta agrupacion no va
## a depender de los datos, sino de los paises que se desee agrupar por su cercania geografica, y a
## partir de estos se sacara el promedio de cada variable para tener asi el consumo promedio de proteinas
## para cada zona.
##
## Mediante esta agrupacion, se puede por ejemplo comparar los promedios del consumo de proteinas
## proveniente del pescado de zonas formadas por paises con acceso al mar con otras donde sus paises no
## lo tengan.
## Sin embargo, las conclusiones que se puedan obtener a partir de esta agrupacion como ya se dijo anteriormente,
## va a depender mucho de como se realize, ya que si para la comparacion ya mencionada de consumo de pescado,
## se agrega a una zona conformada por paises que tengan acceso al mar, uno que no lo tenga, pese a estar geograficamente cercano a los
## otros, podremos estar teniando una informacion que desde el punto de vista del analisis nos puedan
## resultar erronea.
##
## Empleando un dendograma con los diferentes metodos, siempre tendremos grupos mas especificas que incluso como
## se vio en la pregunta 2, pertenecian a una misma region geografica que diferia de la propuesta en la pregunta 3.
