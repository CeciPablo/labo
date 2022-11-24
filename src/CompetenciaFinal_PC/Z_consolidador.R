################################################################
#LOCAL
################################################################

rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("writexl")

setwd("C:/Users/teapcx/Downloads") #Aqui se debe poner la ruta de la PC local


D <- fread("./exp_COMPfinal_ZZ9420_M17_US040_ST100_r3_pred_S100033_M01_IOB060.csv")
D<- D[, !"foto_mes"]
D<- D[, !"prob"]


i<-1
j<-1



semillas <- c(100033, 100057, 8888 )

IOB<-c(60,46,32)

for (q in semillas){
  j<-1
  for(t in IOB){    
    dataset <- fread(paste0("./exp_COMPfinal_ZZ9420_M17_US040_ST100_r3_pred_S",
                            q,"_M0",j,"_IOB0",t,".csv"
                            ))
    dataset$Rank<-rank(dataset$"prob")
    dataset<- dataset[, !"foto_mes"]
    dataset<- dataset[, !"prob"]
    colnames(dataset)[2] <- i
    D<-merge(D, dataset, by="numero_de_cliente")
    i<-i+1
    j<-j+1
  
    }
}


fwrite( D,
        file= "consolidado.csv",
        sep= "\t" )


write_xlsx(D, "C:/Users/teapcx/Downloads/people.xlsx")



##########################################################################################
#CONSOLIDADOR MODELOS DE SEMILLAS CLOUD para Experimento con US 40
##########################################################################################


rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
install.packages("writexl")
require("writexl")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "COMPfinal/ZZ9420_M17_US040_ST100_r3"
setwd(paste0("~/buckets/b1/exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

D <- fread("./pred_S102191_M01_IOB060.csv")
D<- D[, !"foto_mes"]
D<- D[, !"prob"]


semillas <- c(102191, 869587, 417227, 290923, 666617, 666637, 666647, 500009, 600011, 700001, 100057,
              300007, 562361, 562399, 562403, 562409, 562417, 807299, 962041, 705689, 909463, 637597,
              700423, 700429, 700433, 700459, 700471, 838609, 882019, 124987, 348431, 819503, 888809,
              888827, 888857, 888869, 888887, 668111, 945577, 433889, 914371, 676241, 732497, 681979,
              281887, 936659, 692089, 999979, 100019, 101377, 131071, 174763, 238001, 257687, 564227,
              785501, 956341, 335897, 679909, 320923, 629243, 659819, 864379, 300647, 125707, 962303,
              983363, 113111, 724111, 725111, 742111, 644131, 734131, 200177, 410551, 552581, 892237,
              100049, 100069, 100103, 191911, 690143, 976279, 663589, 210127, 469363, 502133, 621923,
              704017, 839369, 490577, 915697, 879181, 283117, 902389, 635837, 450473, 886007, 633337,
              268123)

i<-1
j<-1

IOB<-c(60,46,32)

for (q in semillas){
  j<-1
  for(t in IOB){    
    dataset <- fread(paste0("./pred_S",
                            q,"_M0",j,"_IOB0",t,".csv"
    ))
    dataset$Rank<-rank(dataset$"prob")
    dataset<- dataset[, !"foto_mes"]
    dataset<- dataset[, !"prob"]
    colnames(dataset)[2] <- i
    D<-merge(D, dataset, by="numero_de_cliente")
    i<-i+1
    j<-j+1
    
  }
}


#fwrite( D,
#        file= "consolidado.csv",
#        sep= "\t" )


write_xlsx(D, "~/buckets/b1/exp/COMPfinal/ZZ9420_M17_US040_ST100_r3/consolidadoEX_21NOV.xlsx")


##########################################################################################
##########################################################################################
##########################################################################################
##########################################################################################
#CONSOLIDADOR MODELOS DE SEMILLAS CLOUD para Experimento con US 60
##########################################################################################


rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
install.packages("writexl")
require("writexl")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "COMPfinal/ZZ9420_M17_US060_ST100_r4"
setwd(paste0("~/buckets/b1/exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

D <- fread("./pred_S268123_M01_IOB096.csv")
D<- D[, !"foto_mes"]
D<- D[, !"prob"]


semillas <- c(268123, 633337, 886007, 450473, 635837, 902389, 283117, 879181, 915697, 490577, 839369,
              704017, 621923, 502133, 469363, 210127, 663589, 976279, 690143, 191911, 100103, 100069,
              100049, 892237, 552581, 410551, 200177, 734131, 644131, 742111, 725111, 724111, 113111,
              983363, 962303, 125707, 300647, 864379, 659819, 629243, 320923, 679909, 335897, 956341,
              785501, 564227, 257687, 238001, 174763, 131071, 101377, 100019, 999979, 692089, 936659,
              281887, 681979, 732497, 676241, 914371, 433889, 945577, 668111, 888887, 888869, 888857,
              888827, 888809, 819503, 348431, 124987, 882019, 838609, 700471, 700459, 700433, 700429,
              700423, 637597, 909463, 705689, 962041, 807299, 562417, 562409, 562403, 562399, 562361,
              300007, 100057, 700001, 600011, 500009, 666647, 666637, 666617, 290923, 417227, 869587,
              102191)

i<-1
j<-1

IOB<-c(96,29,14)

for (q in semillas){
  j<-1
  for(t in IOB){    
    dataset <- fread(paste0("./pred_S",
                            q,"_M0",j,"_IOB0",t,".csv"
    ))
    dataset$Rank<-rank(dataset$"prob")
    dataset<- dataset[, !"foto_mes"]
    dataset<- dataset[, !"prob"]
    colnames(dataset)[2] <- i
    D<-merge(D, dataset, by="numero_de_cliente")
    i<-i+1
    j<-j+1
    
  }
}


#fwrite( D,
#        file= "consolidado.csv",
#        sep= "\t" )
#

write_xlsx(D, "~/buckets/b1/exp/COMPfinal/ZZ9420_M17_US060_ST100_r4/consolidadoEX_21Nov.xlsx")


##########################################################################################
##########################################################################################
##########################################################################################

##########################################################################################
#CONSOLIDADOR MODELOS DE SEMILLAS CLOUD para Experimento con US 100
##########################################################################################


rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
install.packages("writexl")
require("writexl")

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "COMPfinal/ZZ9420_M17_US100_ST100_r7"
setwd(paste0("~/buckets/b1/exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

D <- fread("./pred_S983363_M01_IOB043.csv")
D<- D[, !"foto_mes"]
D<- D[, !"prob"]


semillas <- c(983363, 962303, 125707, 300647, 864379, 659819, 629243, 320923, 679909, 335897, 956341,
              785501, 564227, 257687, 238001, 174763, 131071, 101377, 100019, 999979, 692089, 936659,
              281887, 681979, 732497, 676241, 914371, 433889, 945577, 668111, 888887, 888869, 888857,
              888827, 888809, 819503, 348431, 124987, 882019, 838609, 700471, 700459, 700433, 700429,
              268123, 633337, 886007, 450473, 635837, 902389, 283117, 879181, 915697, 490577, 839369,
              704017, 621923, 502133, 469363, 210127, 663589, 976279, 690143, 191911, 100103, 100069,
              100049, 892237, 552581, 410551, 200177, 734131, 644131, 742111, 725111, 724111, 113111,
              700423, 637597, 909463, 705689, 962041, 807299, 562417, 562409, 562403, 562399, 562361,
              300007, 100057, 700001, 600011, 500009, 666647, 666637, 666617, 290923, 417227, 869587,
              102191)

i<-1
j<-1

IOB<-c(43,24,19)

for (q in semillas){
  j<-1
  for(t in IOB){    
    dataset <- fread(paste0("./pred_S",
                            q,"_M0",j,"_IOB0",t,".csv"
    ))
    dataset$Rank<-rank(dataset$"prob")
    dataset<- dataset[, !"foto_mes"]
    dataset<- dataset[, !"prob"]
    colnames(dataset)[2] <- i
    D<-merge(D, dataset, by="numero_de_cliente")
    i<-i+1
    j<-j+1
    
  }
}


#fwrite( D,
#        file= "consolidado.csv",
#        sep= "\t" )
#

write_xlsx(D, "~/buckets/b1/exp/COMPfinal/ZZ9420_M17_US100_ST100_r7/consolidadoEX_21Nov.xlsx")


##########################################################################################
##########################################################################################
##########################################################################################

#Script para correr local y analizar Status de las Bajas en meses anteriores

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


setwd( "/dmef/" )
dataset  <- fread( "./datasets/competencia4_2022.csv.gz", stringsAsFactors= TRUE)

#me quedo SOLO con la foto_mes:
dataset  <- dataset[foto_mes==202001, ] 

dataset<- dataset[, c("numero_de_cliente","clase_ternaria")]

fwrite( dataset, 
        file= "dataset_ene.txt",
        sep= "\t" )

#rm(datasetF)




#########################################################################################
########################################################################################