#############zero2hero - 0101

#1.01 Lectura del dataset

setwd("/dmef")  #Establezco el Working Directory

#Ahora vamos a cargar el dataframe como en R Base, tradicional,
#pero vamos a medir el tiempo para conocer la hora actual
#utilizo la fucion de R Sys.time y para calcular la diferencia utilizo
#as.numeric( t1 - t0, units = "secs") convirtiéndolo a segundos

Sys.time() #devuelve fecha, hora y huso horario ( -03 para el caso de Argentina)

t0  <- Sys.time()
dataset <- read.csv("./datasets/competencia1_2022.csv")
t1  <- Sys.time()
delta  <- as.numeric(  t1 - t0, units = "secs")  #calculo la diferencia de tiempos
print( delta) #imprimo


#Se busca bibliografía sobre que alternativa hay al manejo de dataframes del R Base
#Aparecen varias alternativas, benchmarks muestran la superioridad de data.table
#Pasamos a probar la libreria data.table a ver si los becnmarks están en lo cierto, o es solo marketing
#Si no se tiene instalada la libreria, instalarla primero con install.packages( "data.table", dependencies=TRUE )

install.packages( "data.table", dependencies=TRUE )
library( "data.table")

# Correremos dos veces el siguiente scrip. Veremos como la segunda corrida demora menos que 
# la primer corrida por el hecho de estar ya cargado en memoria.
t0  <- Sys.time()
dataset <- fread("./datasets/competencia1_2022.csv")
t1  <- Sys.time()
delta  <- as.numeric(  t1 - t0, units = "secs")  #calculo la diferencia de tiempos
print(delta) #imprimo

# Corrida 1: 3.44871 Vs Corrida 2: 1.754983

#Esta diferencia, calculada en forma muy burda (sin hacer varios experimentos)
#es alrededor de 35 a 1. Ciertamente leer un dataset es una tarea que se hace unicamente
#al comienzo de los programas, pero se lee en la bibligrafía que data.table funciona
#más rápido para todas las operaciones sobre datasets, lo que se confirmará en los 
#siguientes capítulos.

#Es una verdadera pena que los fans de tidyverse no exploren data.table,
#así como en Python que Pandas sea tan servilmente aceptada.

#############zero2hero - 0102

#1.02 Carga del dataset desde la nube y operaciones básicas

#Se muestra una alternativa a cargar el dataset directamente desde la nube,
#que funciona desde Kaggle, Google Colab
#Si se está corriendo desde la PC local, siempre es preferible cargar el archivo del disco local.

library( "data.table")   #cargo la libreria  data.table

# "https://storage.googleapis.com/dmeyf2022/competencia1_2022.csv" es el dataset
# de la Primera Competencia de la asignatura

dataset <- fread( "https://storage.googleapis.com/dmeyf2022/competencia1_2022.csv")

#Ahora, algunas operaciones básicas con el dataset
nrow( dataset )
ncol( dataset )

#Otra forma de ver la cantidad de registros del dataset, al estilo data table
#El .N es la cantidad de registros y va en la segunda poscion dataset[ 1, 2, 3 ]

dataset[ , .N ]

#Ahora hacemos la apertura por el periodo, el campo  foto_mes
dataset[ , .N, foto_mes ]

#nombre de las columnas del dataset
colnames( dataset )

#Exploración de clase_ternaria.
dataset[  , .N, list( foto_mes, clase_ternaria) ]

#varias formas de contar los BAJA+2
nrow(  dataset[ clase_ternaria=="BAJA+2" ])

dataset[ clase_ternaria=="BAJA+2", .N ] #el autentico estilo data.table

dataset[  , sum(clase_ternaria=="BAJA+2")]

#Conteo de proporcion de BAJA+2 en el dataset
dataset[ foto_mes==202101  ,  sum(clase_ternaria=="BAJA+2")/.N]
    
    #Conteo de la proporcion de BAJA+2 en un predicado
dataset[ foto_mes==202101 & ctrx_quarter < 20  ,  sum(clase_ternaria=="BAJA+2")/.N]

#Lift del predicado ctrx_quarter vs el universo
  #forma brutal de calcularlo
dataset[ foto_mes==202101 & ctrx_quarter < 20  ,  sum(clase_ternaria=="BAJA+2")/.N]  /dataset[ foto_mes==202101  ,  sum(clase_ternaria=="BAJA+2")/.N]

### Ganancias del dataset
#Primero le asigno a TODOS los registros el valor de -2000
dataset[ foto_mes==202101, ganancia := -2000]

#y finalmente a los BAJA+2 les asigno 78000
dataset[ foto_mes==202101 & clase_ternaria=="BAJA+2", ganancia := 78000]

#Calculo la ganancia que tendria una campaña en donde envío estímulo a TODOS los clientes
dataset[ foto_mes==202101 , sum(ganancia)]
    #Si le enviara estímulo a todos, se pierden 254 millones de pesos

#Ganancias de predicados univariados
dataset[ foto_mes==202101 & ctrx_quarter < 20,  sum( ganancia )  ]
    # 15670000

dataset[ foto_mes==202101 & ctrx_quarter < 4,  sum( ganancia )  ]
    # 12588000

    #Ahora, en forma brutal e ineficiente, busco donde esta el mejor corte de ctrx_quarter
for(  transacciones  in   0:50)
{
  cat(  transacciones, dataset[  foto_mes==202101 & ctrx_quarter < transacciones,  sum( ganancia )  ] , "\n")    
}

    # Observo que el predicado ctrx_quarter < 14 tiene la máxima ganancia

                # 13 16286000 
                # 14 16720000 <===========
                # 15 16486000 
                # 16 16056000 
                # 17 15700000 
                # 18 15778000 
                # 19 15630000 
                # 20 15670000 
                # 21 15048000 

#Ganancias de predicado complejo
dataset[  foto_mes==202101 & ctrx_quarter < 20 & mpasivos_margen < 29.8 ,  sum( ganancia )  ]
  #17044000 - Enhorabuena ! hemos alcanzado los 17 M de ganancia

# gráficos de densidades

    # Visualización de la densidad de una variable versus clase_ternaria
library("ggplot2") #cargo la libreria ggplot2
campo <- "cliente_antiguedad" 
ggplot(dataset[ foto_mes==202101] , aes_string(x = campo))+ geom_density(trim=TRUE, na.rm=TRUE)+ facet_grid( "clase_ternaria~ .")

      # los gráficos salen muy pequeños, busco la documentacion
      # https://blog.revolutionanalytics.com/2015/09/resizing-plots-in-the-r-kernel-for-jupyter-notebooks.html 
      # y agrando los graficos
options(repr.plot.width=15, repr.plot.height=15)
campo <- "cliente_antiguedad" 
ggplot(dataset[ foto_mes==202101], aes_string(x = campo)) + geom_density(trim=TRUE, na.rm=TRUE) + facet_grid( "clase_ternaria~ .")


#############zero2hero - 0103

#1.03 Construyendo un arbol

#Se construye un arbol de decisión, se ven distintas formas de pasar los parámetros
#y distintas formas de dibujarlo.
#Se muestran funcionalidades básicas de la libreria data.table

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
options(repr.plot.width=25, repr.plot.height=25)  #para que los gráficos me salgan legibles
setwd("/dmef") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

    # Ahora entreno un arbol de decision
    # "clase_ternaria ~ ." significa predecir clase_ternaria utilizando todo el resto
    # de las variables del dataset

modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101] )

print( modelo)
  
          # n= 161342 
          # 
          # node), split, n, loss, yval, (yprob)
          # * denotes terminal node
          # 
          # 1) root 161342 1514 CONTINUA (0.004115481 0.005268312 0.990616207) *

          #Esta impresión no es gráfica. No me sirve.
          #a pesar que no me sirve, he encontrado una piedra en el camino,
          # me está generando un arbol con un solo nodo, con solo la raiz

library("rpart.plot")
rpart.plot::prp(modelo)

          #Me ha salido una impresión del arbol, que es un solo nodo, pero solo dice continua.
          #leo documentacion rpart.plot https://cran.r-project.org/web/packages/rpart.plot/rpart.plot.pdf

prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.3)
          
          # extra=101 ==> Display the number of observations that fall in the node
          # digits=5 ==> The number of significant digits in displayed numbers
          # branch=1 ==> Controls the shape of the branch lines. Specify a value between 0 (V shaped branches) and 1 (square shouldered branches).
          # type=4 ==> Type of plot. Draw separate split labels for the left and right directions. Label all nodes, not just leaves
          # varlen=0 ==> Length of variable names in text at the splits
          # faclen=0 ==> Length of factor level names in splits
          # tweak=1.3 ==> Adjust the (possibly automatically calculated) cex. Using tweak is often easier than specifying cex.
          # cex ==> calculate the text size automatically

    # Ha salido solo la raiz del arbol
    # Los tres numeros que muestra en el nodo son la cantidad de BAJA+1, BAJA+2 y CONTINUA,
    # en ese orden, alfabetico.
    # la cantidad de CONTINUA la está mostrando en notacion científica

# cambio hiperparámetros del arbol para salga algo mas que un solo nodo


    # El hiperparámetro cp complexity limita el split de los nodos.
    # El default es cp=0.05
    # Pruebo con cp=0.0 a ver si "se abre el arbol"

    # Leo la documentación de la libreria rpart https://cran.r-project.org/web/packages/rpart/rpart.pdf
    # veo que existe un hiperparámetro de la funcion rpart llamado xval que es para hacer
    # cross validation, que por default viene seteado en xval=10 .
    # No me interesa en este momento que haga cross validation ==> xval=0

modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= 0.0 )

#imprimo el modelo graficamente
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1.3)

      # sale un arbol de gran profundidad que ni se puede visualizar
      # el mensaje ""labs do not fit even at cex 0.15, there may be some overplotting"" me dice que no pudo dibujarlo correctamente
      # lamentablemente me doy cuenta la libreria rpart.plot es mala
      # establezco maxdepth=2 para poder apreciarlo
modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= 0.0,
                   maxdepth= 2 )

prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1, cex=0.6)

      #Esperaba ver un albol de profundidad 2 sin embargo, por alguna misteriosa razón, se ha generado un arbol con un solo nodo.
      #Corto por lo sano, y establezco cp=-1 para que siempre se abra el arbol

# APRENDO A BORRAR LA MEMORIA
  
  # *Listo* los objetos que estan en la memoria de R en este momento
ls()
a <-  1
ls() # confirmo que apareció una nueva variable en memoria

gc() # Me fijo cuanta memoria esta disponible

#Borro TODO
rm( list=ls())

gc() # Me fijo cuanta memoria esta disponible luego de limpiar


#############################################################
####1.04 Transformado (innecesariamente) las variables

    # El objetivo de esta sección es analizar el efecto que tiene sobre el arbol de decision
    
          # Variables Colineales
          # Normalizacion de Variables
          # Transformada logarítmica
          # Outliers

rm( list=ls())
gc()

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library( "rpart.plot")

options(repr.plot.width=20, repr.plot.height=10) 
setwd("/dmef")  #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset

#Modelo
modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1, cex=0.6)

      # La variable mas importante que aparece es ctrx_quarter

#VARIABLES COLINEALES (agrego al dataset tres variables colineales con ctrx_quarter)
dataset[ foto_mes==202101 , ctrx_quarter_dos    :=  2*ctrx_quarter ]
dataset[ foto_mes==202101 , ctrx_quarter_tres   :=  3*ctrx_quarter ]
dataset[ foto_mes==202101 , ctrx_quarter_cuatro :=  4*ctrx_quarter ]

colnames( dataset )

modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1, cex=0.6)

#!!!!!!!!!!!!!!!!!!!!!!!!!!SORPRENDENTE, el arbol es inmune a las colinealidades

#NORMALIZACIÓN DE VARIABLES (Analizo la variable ctrx_quarter)

boxplot(  dataset[ foto_mes==202101, ctrx_quarter])
hist( dataset[ foto_mes==202101, ctrx_quarter] )
plot( density( dataset[ foto_mes==202101, ctrx_quarter] ) )

    #Normalizado
dataset[ foto_mes==202101, ctrx_quarter_normalizado := scale(ctrx_quarter)]
    #chequeo
plot( density( dataset[foto_mes==202101, ctrx_quarter_normalizado] ) ) #Confirmado, la variable está normalizada, ahora corremos nuevamente el arbol de decision

modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[ foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1, cex=0.6)

#!!!!!!!!!!!!!!!!SORPRENDENTE , el arbol de decision es inmune a las normalizacion de variables

#TRANSFORMACIÓN LOGARITMICA (hay que cargar de nuevo el dataset y luego transformar)
dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset
dataset[ foto_mes==202101 , ctrx_quarter_log :=log(ctrx_quarter+1)]  #sumo el uno porque no quiero infinitos
plot( density( dataset[ foto_mes==202101, ctrx_quarter_log] ) )
dataset[ , ctrx_quarter := NULL ] #ELIMINO del dataset la variable ctrx_quarter , para que solo juegue ctrx_quarter_log
modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1, cex=0.6)

#!!!!!!!!!!!!!!!SORPRENDENTE , el arbol de decision es inmune a la transformada logaritmica
# Por supuesto, el arbol original cortaba en ctrx_quarter < 14 y ahora corta en
# ctrx_quarter < 2.673 porque obviamente alteré esa variable, pero en realidad está cortando
# en el mismo punto.


#OUTLIERS (Ahora fabrico outliers y veo como se comporta el arbol)
dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset
boxplot(  dataset[ foto_mes==202101 , ctrx_quarter])
dataset[ foto_mes==202101 & ctrx_quarter > 1500, .N]   #cuento registros con ctrx_quarter > 1500
dataset[ foto_mes==202101 & ctrx_quarter > 1500,  ctrx_quarter := ctrx_quarter * 1000] #los transformo en outliers extremos
boxplot(  dataset[ foto_mes==202101 , ctrx_quarter])   #compruebo que sean outliers extremos

modelo <-  rpart(  formula= "clase_ternaria ~ ." ,
                   data= dataset[foto_mes==202101],
                   xval= 0,
                   cp= -1,
                   maxdepth= 2 )
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=1, cex=0.6)

#!!!!!!!!!!!!!!!!!!SORPRENDENTE, el arbol de decision es inmune a los outliers



############################################################
###### 1.05 Creando un data.table a partir de las columnas

# Hasta ahora, para crear una data.table estamos leyendo un archivo del disco
# ( o bajándolo de internet). Ahora veremos como crearla a partir de dos vectores

rm( list=ls())
gc()

library( "data.table")   #cargo la libreria  data.table

options(repr.plot.width=20, repr.plot.height=10) 
setwd("/dmef")  #Aqui se debe poner la ruta de la PC local

  # Suponer dos vectores:
            # uno con numero_de_clente
            # otro con decision de si a ese registro le envio o no estímulo
            # Importante : ambos vectores tienen la misma longitud

vector_ids <-  c( 107, 228, 351, 468, 579)
vector_ids
vector_enviar  <-   c( 0, 1, 1, 0, 1 )
vector_enviar

      # Creo dataset a partir de las dos columnas:
              # "numero_de_cliente"
              # "Predicted"
        
        # En R, un dataframe o data.table es una lista de columnas
        # LISTA y NO UN VECTOR
        # Los vectores necesitan todos valores del mismo tipo de datos
        # Los dataset pueden tener columnas que sean vectores de numeros, string
        # En R, existe el tipo list, para meter en una bolsa de gatos objetos de distintos tipo

tabla_final  <-   as.data.table(  list(  "numero_de_cliente"= vector_ids,
                                         "Predicted"=         vector_enviar))

#Finalmente, grabo ese archivo con la instruccion fwrite (libreria **data.table)
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/ZH2015/", showWarnings = FALSE )

fwrite( tabla_final,
        file= "./exp/ZH2015/entrega_de_juguete.csv",
        sep= ",")


##################################################################
########1.06 Hackeando Kaggle

#Voy a hackear Kaggle para descubrir cuantos "BAJA+2" hay en el mes 202101,
#que es donde no tengo la clase

# La idea es subir a Kaggle una prediccion que tenga TODOS 1, que se le envíe estímulo a todos
# Es obvio que esa prediccion va a dar una pésima ganancia
# Pero me va a permitir tener un sistema de dos ecuaciones con dos incognitas,
# y despejando tendre la cantidad de BAJA+2 que hay en los datos del futuro

rm( list=ls())
gc()

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library( "rpart.plot")

options(repr.plot.width=20, repr.plot.height=10) 
setwd("/dmef")  #Aqui se debe poner la ruta de la PC local

dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset
dataset[  , .N, list( foto_mes, clase_ternaria) ]       # cuento la clase

dfuturo <-  dataset[ foto_mes==202103 ]   # Defino data.table dfuturo que va a tener solo los datos de 202103
nrow( dfuturo )                           #cuento la cantidad de lineas del dataset
vector_ids  <-   dfuturo[ , numero_de_cliente]  #Creo un vector que sean los ids

vector_enviar <-  rep( 1,  nrow( dfuturo)) # Creo un vector de todos unos, con la cantidad de registros que tiene dfuturo utilizando la instruccion rep() de R 

tabla_final  <-   as.data.table(  list(  "numero_de_cliente"= vector_ids,
                                         "Predicted"=         vector_enviar)) # tabla a enviar a Kaggle



#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/ZH2016/", showWarnings = FALSE )

#genero el archivo para Kaggle
fwrite(tabla_final, 
       file= "./exp/ZH2016/todos_unos.csv",
       sep=  "," )


#Subo el archivo a Kaggle y me fijo cuanta ganancia me da en el Public Leaderboard
# da una ganancia de -249001.57 ( menos 249 millones )

#EL HACKEO
  #Tengo dos incognitas:
          # POS , los positivos, los "BAJA+2"
          # NEG, los negativos, los "BAJA+1" y "CONTINUA"

  #Se que se cumplen estas dos cosas:
          # POS + NEG = 162900
          # 78000*POS - 2000*NEG =-249001570

  # POS = 960 , o sea las BAJA+2 de 202103 son 960

  # ¿Qué nos dicen estas 960 BAJA+2 de marzo del 2021? ¿Explican algo?
  #       Las BAJA+2 que hay en enero del 2021 son 850 ====> 960 > 850
  #       Con lo cual es de esperar mayores ganancias de los modelos predictivos en 202103





#########################################################################33
###########1.07 Aplicando el arbol

rm( list=ls())
gc()

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library( "rpart.plot")

options(repr.plot.width=20, repr.plot.height=10) 
setwd("/dmef")  #Aqui se debe poner la ruta de la PC local

dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset #cargo el dataset

dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]

#genero el modelo
modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                        data= dtrain,
                        xval= 0,
                        cp= -1,
                        maxdepth= 2 )

#Cargo el dataset a donde voy a aplicar el modelo
#Aplico el modelo a los datos dapply pidiendo que me devuelva probabildades

prediccion  <- predict( modelo, dapply, type = "prob")
      #Prediccion es una matriz; me interesa la columna "BAJA+2" que es la probabilidad
      #que el modelo asigna a cada registro de dapply

prob_baja2  <- prediccion[, "BAJA+2"]

#Ahora decido si envio el estimulo o no
#si prob( BAJA+2) > 0.025 envio el estímulo
Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )

#creo una tabla con lo que voy a enviar a Kaggle
entrega  <-  as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, "Predicted"=Predicted)  )
entrega[ , .N, Predicted] #Esto significa que se enviaran 8184 estímulos

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/ZH2017/", showWarnings = FALSE )

fwrite( entrega, 
        file= "./exp/ZH2017/para_Kaggle_0107.csv",
        sep=  "," )




#######################################################################3
##########1.08 Unos buenos hiperparametros

rm( list=ls())
gc()

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library( "rpart.plot")

options(repr.plot.width=20, repr.plot.height=10) 
setwd("/dmef")  #Aqui se debe poner la ruta de la PC local

dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset #cargo el dataset
dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]

        # defino unos parametros interesantes
        # los hiperparametros van en una lista
        # notar la forma en que esos parametros se pasan a la funcion rpart

param  <- list("cp"= -0.5,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 5)

#genero el modelo
modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                        data= dtrain,
                        xval= 0,
                        control= param)

prediccion  <- predict( modelo, dapply, type = "prob") #Aplico modelo a los datos dapply pidiendo que me devuelva probabildades

# Genero la salida
prob_baja2  <- prediccion[, "BAJA+2"]
Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )  # 0.025 es la probabilidad de corte

entrega  <-  as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, "Predicted"=Predicted)  )

#creo la carpeta donde va el experimento
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( "./exp/ZH2018/", showWarnings = FALSE )

#genero el archivo para Kaggle
fwrite( entrega, 
        file= "./exp/ZH2018/para_Kaggle_0108.csv",
        sep=  "," )


################################################################################
########1.09 Dividir en Training y Testing (version libreria caret)

#El objetivo es hacer una division del dataset en training/testing
#que sea estratificada en la clase

rm( list=ls())
gc()

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library( "rpart.plot")

options(repr.plot.width=20, repr.plot.height=10) 
setwd("/dmef")  #Aqui se debe poner la ruta de la PC local

dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset #cargo el dataset
denero <- dataset[ foto_mes==202101 ]

library("caret")

    # https://www.rdocumentation.org/packages/caret/versions/6.0-88/topics/createDataPartition
    # la funcion createDataPartition devolverá un vector de posiciones, las que cumplen con la
    # particion indicada. p= 0.5 significa que queremos el 50% de los registros
    # dataset$clase_ternaria es el vector con la clase ternaria, que es por donde se estratificará

train_rows <- createDataPartition(denero$clase_ternaria, p= 0.50, list= FALSE)

dtrain <- denero[ train_rows]
dtest <- denero[ -train_rows] #-train_rows  significa el complemento  (no confundir con numeros negativos)

    # Compruebo la division
nrow( dtrain)
nrow( dtest )
nrow( dtrain) + nrow(dtest)
nrow( denero)

    # Compruebo que la particion es estratificada
denero[  , .N, clase_ternaria]
dtrain[  , .N, clase_ternaria]
dtest[  , .N, clase_ternaria]


################################################################################
#########1.10 Experimentos con azar Replicables - set.seed()

rm( list=ls())
gc()

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library( "rpart.plot")
library( "caret")

options(repr.plot.width=20, repr.plot.height=10) 
setwd("/dmef")  #Aqui se debe poner la ruta de la PC local

dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset #cargo el dataset
denero <- dataset[ foto_mes==202101 ]
library("caret")
train_rows <- createDataPartition(denero$clase_ternaria, p= 0.50, list= FALSE)
head( train_rows )
    #Volvamos a dividir crear una particion a ver si sale lo mismo
train_rows <- createDataPartition(denero$clase_ternaria, p= 0.50, list= FALSE)
head( train_rows )

    #Cáspitas ! No ha salido la misma particion. Si yo quiero replicar el mismo experimento, no lo voy a poder hacer, 

    #La solucion es setear la semilla
set.seed(13)
train_rows <- createDataPartition(denero$clase_ternaria, p= 0.50, list= FALSE)
head( train_rows )
    #Y ahora nuevamente
set.seed(13)  #uso la misma semilla que antes
train_rows <- createDataPartition(denero$clase_ternaria, p= 0.50, list= FALSE)
head( train_rows )

    ##el set.seed() permitirá replicar experimentos con exactitud



################################################################################
#####1.11 Arbol training/testing

#El objetivo es calcular la ganancia en testing de un arbol de decision

rm( list=ls())
gc()

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library( "rpart.plot")
library( "caret")

options(repr.plot.width=20, repr.plot.height=10) 
setwd("/dmef")  #Aqui se debe poner la ruta de la PC local

dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset #cargo el dataset
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101

      #creo training con 70%  y testing con 30%
set.seed(13)
train_rows <- createDataPartition(dataset$clase_ternaria, p= 0.70,list= FALSE)
dtrain  <- dataset[ train_rows]
dtest  <-  dataset[ -train_rows]

      #Entreno el modelo en los datos de training
param  <- list("cp"= -0.5,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 5)
modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                        data= dtrain,
                        xval= 0,
                        control= param)

      #aplico el modelo a los datos de testing
prediccion  <- predict( modelo, dtest, type = "prob")

      #Calculo la ganancia del modelo en los datos de testing
prob_baja2  <- prediccion[, "BAJA+2"] #a partir de la prediccion, calculo la probabilidad de BAJA+2 de cada registro de testing dtest


      # Para calcular la ganancia del modelo aplicado a testing debo tener en cuenta:
          # Solo estimulo a registros que el modelo asigno prob > 0.025
          # Si no estimulo, no gano ni pierdo.
          # Si estimulo un BAJA+2 => ganancia +78000
          # Si estimulo un BAJA+1 => ganancia -2000
          # Si estimulo un CONTINUA => ganancia -2000

ganancia_testing <- dtest[ , sum(  (prob_baja2>0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
ganancia_testing


      # Es importante entender que la ganancia esta calculada sobre los datos de testing,
      # que en este caso son apenas el 30%
      # Si quiero extrapolar a todo el dataset, debo hacer el cociente de esa ganancia por 0.30


ganancia_testing_normalizada  <-  ganancia_testing/0.3
ganancia_testing_normalizada


##################################################################################
######1.12 Funcion de Ganancia de Arbol training/testing

# El objetivo es poner el notebook anterior dentro de una funcion, e invocarla con distintas
# semillas, para maravillarnos ante la dispersion de las ganancias

rm( list=ls())
gc()

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library( "rpart.plot")
library("caret")

options(repr.plot.width=20, repr.plot.height=10) 
setwd("/dmef") #Aqui se debe poner la ruta de la PC local


GananciaArbol  <-  function( semilla, data, x, train=0.70) {
  #establezco la semilla
  set.seed(semilla)
  train_rows <- createDataPartition(dataset$clase_ternaria, p= 0.70,list= FALSE)
  
  modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                          data= data[ train_rows],  #los datos de training
                          xval= 0,
                          control= x)
  
  #Aplico el modelo a los datos de testing  
  prediccion  <- predict( modelo, data[ -train_rows], type = "prob")
  
  prob_baja2  <- prediccion[, "BAJA+2"]
  ganancia_testing <- data[ -train_rows, sum(  (prob_baja2>0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
  
  return( ganancia_testing)
}

    #Vale la pena notar que dentro de la funcion no se han creado dtrain y dtest,
    # sino que directamente se utilizan:
  
            #data[ train_rows] como training
            #data[ -train_rows] como testing

#Aqui empieza el programa
dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset #cargo el dataset
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101
  #defino unos buenos hiperparametros
param  <- list("cp"= -0.5,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 5)
  #Ahora hago algunas llamadas a la funcion
GananciaArbol(  11, dataset, x=param)
GananciaArbol(  13, dataset, x=param)
GananciaArbol(  17, dataset, x=param)
GananciaArbol(  19, dataset, x=param)
GananciaArbol(  23, dataset, x=param)

      # Esta dispersión es NOTABLE ya que el algoritmo es el mismo, con los mismos parámetros.
      # Lo único que cambia es qué semillas se utilizan para entrenar y testear
      # Es más notable aún que se ha tenido cuidado que la partición sea estratificada segun el campo clase_ternaria, lo que apriori uno supondria que va a generar particiones muy homogéneas.


################################################################################
######1.13 Acumulando resultados en un vector

# Este elemental notebook muestra desde cero como acumular resultados en un vector,
# será un primer paso para luego escribir scripts en donde guardemos los resultados de nuestros
# procesos en vectores o en un dataset si es necesario.

    #Partimos primero de un vector, al que queremos agregarle al final un elemento nuevo
vector  <- c( 1, 2, 3, 4, 5)
vector <- c( vector,  6 )

    #Ahora definimos una funcion para ejemplificar
potencia_invertida  <-  function( x )
  {
  return(  1/2^x )
  }

resultados <-  c() #creo el vector vacio donde voy a acumular

for( x in  1:8)
  {
  y  <- potencia_invertida( x)
  resultados  <- c( resultados, y)
  
  cat(resultados,"\n")
}


#################################################################################
#####1.14 Montecarlo Estimation, Arbol

#El objetivo es implementar con un for loop la estimacion montecarlo,
#que promedia las ganancias

rm( list=ls())
gc()

library("data.table")   #cargo la libreria  data.table
library("rpart")  #cargo la libreria  rpart
library("rpart.plot")
library("caret")

options(repr.plot.width=20, repr.plot.height=10) 
setwd("/dmef") #Aqui se debe poner la ruta de la PC local

GananciaArbol  <-  function( semilla, data, x, train=0.70) {
  #establezco la semilla
  set.seed(semilla)
  train_rows <- createDataPartition(dataset$clase_ternaria, p= 0.70,list= FALSE)
  
  modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                          data= data[ train_rows],  #los datos de training
                          xval= 0,
                          control= x)
  
  #Aplico el modelo a los datos de testing  
  prediccion  <- predict( modelo, data[ -train_rows], type = "prob")
  
  prob_baja2  <- prediccion[, "BAJA+2"]
  ganancia_testing <- data[ -train_rows, sum(  (prob_baja2> 0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
  
  #normalizo la ganancia
  ganancia_testing_normalizada  <- ganancia_testing/0.3  
  
  return( ganancia_testing_normalizada )
}

dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset #cargo el dataset
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101

#defino unos buenos hiperparametros
param  <- list("cp"= -0.5,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 5 )

#defino el vector de semillas
ksemillas  <- c(102191, 200177, 410551, 552581, 892237) #reemplazar por las propias semillas

vector_ganancias  <- c()  #vector donde voy a ir acumulando las ganancias

for( semilla in ksemillas)
{
  ganancia  <- GananciaArbol( semilla, dataset, x=param, train=0.70 )
  vector_ganancias  <- c( vector_ganancias, ganancia)
}

vector_ganancias

mean( vector_ganancias )


################################################################################
####1.15 Montecarlo Estimation, Arbol

rm( list=ls())
gc()

library("data.table")   #cargo la libreria  data.table
library("rpart")  #cargo la libreria  rpart
library("rpart.plot")
library("caret")

options(repr.plot.width=20, repr.plot.height=10) 
setwd("/dmef") #Aqui se debe poner la ruta de la PC local


GananciaArbol  <-  function( semilla, data, x, train=0.70) {
  #establezco la semilla
  set.seed(semilla)
  train_rows <- createDataPartition(dataset$clase_ternaria, p= 0.70,list= FALSE)
  
  modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                          data= data[ train_rows],  #los datos de training
                          xval= 0,
                          control= x)
  
  #Aplico el modelo a los datos de testing  
  prediccion  <- predict( modelo, data[ -train_rows], type = "prob")
  
  prob_baja2  <- prediccion[, "BAJA+2"]
  ganancia_testing <- data[ -train_rows, sum(  (prob_baja2> 0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
  
  #normalizo la ganancia
  ganancia_testing_normalizada  <- ganancia_testing/0.3  
  
  return( ganancia_testing_normalizada )
  
}


ArbolMontecarlo <- function( semillas, data, x, train=0.70)
{
  vector_ganancias <- c()  #vector donde voy a ir acumulando las ganancias
  for( semilla in ksemillas)
  {
    ganancia  <- GananciaArbol( semilla, dataset, x=x, train=0.70 )
    vector_ganancias  <-  c( vector_ganancias, ganancia)
  }
  
  return( mean( vector_ganancias))
}


dataset <- fread("./datasets/competencia1_2022.csv")   #cargo el dataset #cargo el dataset
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101
ksemillas  <- c(102191, 200177, 410551, 552581, 892237) #reemplazar por las propias semillas



param1  <- list("cp"= -0.5,
                "minsplit"=  900,
                "minbucket"= 440,
                "maxdepth"= 5 )
ganancia_montecarlo1  <- ArbolMontecarlo( ksemillas, dataset, x= param1, train= 0.70 )


param2  <- list("cp"= -0.5,
                "minsplit"=  1340,
                "minbucket"=  600,
                "maxdepth"= 6 )
ganancia_montecarlo2  <- ArbolMontecarlo( ksemillas, dataset, x= param2, train= 0.70 )


ganancia_montecarlo1
ganancia_montecarlo2


################################################################################
######## 2.01 Optimizacion Bayesiana

#ADVERTENCIA

    # El uso del método de Optimización Bayesiana para la optimización de hiperparámetros
    # en modelos predictivos tiene menos de una década.
    # Entender los fundamentos de la O.B. requieren de una sólida formación matemática y 
    # no son el objetivo de esta asignatura.

    # Para quienes deseen aventurarse a los detalles técnicos:
    # El paper original de la libreria mlrMBO (año 2018 ) https://arxiv.org/pdf/1703.03373.pdf
    # El método de Kriging , tal cual se deriva del uso original, en este pequeño libro de 106 páginas "Basic Steps in Geostatistics: The Variogram and Kriging" https://www.pdfdrive.com/basic-steps-in-geostatistics-the-variogram-and-kriging-e187336318.html


#Ejemplo de optimizacion bayesiana, univariada

rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

options(repr.plot.width=20, repr.plot.height=10)


#Defino la funcion a optimizar, un polinomio de grado 4
func_univariada01  <- function( x )
{
  y  <- -2 * (x+13) * (x-3) * (x-7) * (x-19)
  
  return( y )
}


#Grafico la funcion
intervalo  <- seq(-15,21,0.1)
plot(intervalo, func_univariada01(intervalo)) #La funcion tiene dos máximos, uno de ellos es el global.


#OPTIMIZACION BAYESIANA
    #¿Qué tan rápido encontrará la Optmización Bayesiana el máximo global?
    
    # Defino las caracteristicas de la optimizacion

          # fn: nombre de la funcion
          # minimize: en este caso le asigno FALSE, ya que deseo maximizar el valor
          # par.set: indica cuales son los hiperparmetros de la funcion, en este caso hay una sola variable que llamé x
          # makeNumericParam: indica que ese hiperparámetro es un numero real, una variable continua ( no es ni un entero ni una categoria )

obj.fun  <- makeSingleObjectiveFunction(
  fn=       func_univariada01,
  minimize= FALSE,   #estoy Maximizando la ganancia
  par.set=  makeParamSet(  makeNumericParam( "x", lower= -100, upper=  100) ),
)

#ahora defino la funcion proxy, la que se construye internamente intentando emular la realidad

          # cl: clase de learner, "reg.km" indica método kriging "regression kriging methodd"
          # predict.type: tipo de prediccion que deseo devuelva, "se" significa que espero dos valores media y standard error
          # covtype: funcion de covarianza que se va a utilizar, cual es la covarianza de dos mediciones como fucion de la distancia entre los puntos donde fueron tomadas las mediciones, fue inventada por Bertil Matérn

          # (kriging ==> https://www.youtube.com/watch?v=ZB7GvJrNKq0 )

fproxy  <- makeLearner( cl= "regr.km",
                        predict.type= "se", 
                        covtype= "matern3_2" )


#ultima definicion, especificar la optimizacion bayesiana

          # crit: criterio con el que se completan los valores iniciales "no inteligentes"
          # iters: iteraciones inteligentes que hará la Optimizacion Bayesiana, las que son adicionales a las primeras cuatro de este caso.

ctrl  <- makeMBOControl()
ctrl  <- setMBOControlInfill( ctrl, crit= makeMBOInfillCritEI())
ctrl  <- setMBOControlTermination( ctrl, iters= 25 )


# finalmente , lanzo la Optimizacion Bayesiana

          # fun: especificacion de la funcion que deseo optimizar, si maximizo o minimizo, cuales son las variables de la misma
          # learner: especifica cual es la función proxy interna que va a utilizar la Optimziación Bayesiana
          # control: indica la la forma en que se harán las iteraciones

run  <- mbo( fun=      obj.fun, 
             learner= fproxy, 
             control= ctrl )

tb_resultados  <- as.data.table( run$opt.path )

tb_resultados

tb_resultados[ which.max( tb_resultados$y ) ]

#CONCLUSION: 
    # La gran pregunta es : la Optimización Bayesiana, ¿se focaliza luego de muchas
    # iteraciones solo en el entorno del máximo que está cerca de x= -8 ?

    # Rta: SI ==> x = -7.639012



###################################################################################
#### 2.02 Optimizacion Bayesiana DOS variables

#El código

  #( por favor, verificar antes que se tienen instaladas todas las librerias necesarias, en particular plotly )

#Ejemplo de optimizacion bayesiana, univariada

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("plotly")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

options(repr.plot.width=20, repr.plot.height=10)


#Defino la funcion a optimizar
func_volcano  <- function( x )
{
  z  <- volcano[ x$b, x$a ]
  
  return( z )
}


#Grafico el volcan
p <- plot_ly(z = volcano, type = "surface")
p 


#La funcion tiene varios maximos locales
configureMlr( show.learner.output = FALSE)

obj.fun  <- makeSingleObjectiveFunction(
  fn=       func_volcano,
  minimize= FALSE,   #estoy Maximizando la ganancia
  has.simple.signature = FALSE,  #porque tengo DOS dimensiones
  par.set=  makeParamSet(  makeIntegerParam( "a", lower= 1, upper=  61),
                           makeIntegerParam( "b", lower= 1, upper=  87)
  ),
)


#ahora defino la funcion proxy
fproxy  <- makeLearner( cl= "regr.km",
                        predict.type= "se", 
                        covtype= "matern3_2" )

#ultima definicion, especificar la optimizacion bayesiana
ctrl  <- makeMBOControl()
ctrl  <- setMBOControlInfill( ctrl, crit= makeMBOInfillCritEI())
ctrl  <- setMBOControlTermination( ctrl, iters= 20 )


run  <- mbo( fun=     obj.fun, 
             learner= fproxy, 
             control= ctrl )

tb_resultados  <- as.data.table( run$opt.path )
tb_resultados
tb_resultados[ which.max( tb_resultados$y ) ]