rm( list=ls()) #si lo corro se borra la base que tomo
gc() #si lo corro se borra la base que tomo


require("data.table")
require("rlist")
require("yaml")
require("lightgbm")
#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")
# paquetes extra julio 
require("here")# install.packages("here") # para tema de paths en proyectos
require("purrr") #install.packages("purrr") # para aplicar funciones a listas. en modo tidyverse 
# require("skimr") # install.packages("skimr") # skimr::skim
# require("janitor") # install.packages("janitor")

require("parallel")
require("rpart")
install.packages("rpart.plot")
library( "rpart.plot")
library("caret")

library("ROCR")

library("xgboost")

#----Dataset------------------------------------------------------------------------

setwd("/Users/clara/Documents/00-Posgrado/4_DM_Eco_y_Finanzas/")
#cargo el dataset
#dataset  <- fread("./datasetsOri/paquete_premium_202009.csv")
dataset  <- fread( "./datasets/paquete_premium_202009_ext2.csv" )

#creo la clase_binaria donde en la misma bolsa estan los BAJA+1 y BAJA+2
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

#--------Funtion Ganancia-----------------------------------------------------------

GananciaArbol <- function( semilla, data, x, train=0.70) {
  
  #establezco la semilla
  set.seed(semilla)

  #divido en TRAIN/TEST
  train_casos <- caret::createDataPartition(dataset[,get("clase01")], p = 0.7, list = FALSE)
  data_train  <-  dataset[  train_casos, ]
  data_test   <-  dataset[ -train_casos, ]
  
  print(dim(data_train))
  print(dim(data_test))
  ##los campos que se van a utilizar
  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01","ccajas_transacciones") )
  
  
  # campos_buenos  <- setdiff( colnames(dataset),
  #                              c("clase_ternaria", "clase01","internet","ctarjeta_visa_descuentos" , "mtarjeta_visa_descuentos" ,"ccajas_transacciones", "ctarjeta_master_descuentos" , "mtarjeta_master_descuentos" , "cextraccion_autoservicio" , "ccajas_otras" , "catm_trx" , "matm_other" , "tmobile_app" , "cmobile_app_trx" , "Master_Finiciomora" , "Master_mconsumosdolares" , "Master_madelantodolares" , "Master_cconsumos" , "Master_cadelantosefectivo" , "Visa_Finiciomora" , "Visa_msaldodolares" , "Visa_mpagado" , "Visa_mpagominimo" ) )
  # 
  #dejo los datos en el formato que necesita LightGBM
  data_train <- lightgbm::lgb.Dataset(data= data.matrix(data_train[ , campos_buenos, with=FALSE]),
                                      label= data_train$clase01 )
  
  #genero el modelo con los parametros por default
  modelo <- lightgbm::lightgbm( data= data_train,
                                params= x,
                                verbose= -100)
  
  #Aplico el modelo a los datos de testing

  umbral_prob=0.04 #segun resultado de mejor lightGBM 682
  prediccion  <- predict( modelo, 
                          data.matrix( data_test[, campos_buenos, with=FALSE ])) #Lo cambio a un formato para predict  
  
  #prediccion_train <-predict( modelo, 
  #                            data.matrix( data_train[, campos_buenos, with=FALSE ]))
  #prediccion_train <-predict( modelo, 
   #                            data_train[, campos_buenos, with=FALSE ])
  
  #Lo cambio a un formato para predict  
  
  prob_baja  <- as.integer( prediccion > umbral_prob )
 
  ganancia_test <- data_test[ , sum(  (prob_baja>umbral_prob) * ifelse( clase01==1, 48750, -1250) )]
  return( ganancia_test)
  
}

#----Parametros------------------------------------------------------------------------

#defino unos buenos hiperparametros

param <- list( objective= "binary",
               metric= "custom",
               first_metric_only= TRUE,
               boost_from_average= TRUE,
               feature_pre_filter= FALSE,
               verbosity= -100,
               seed= 999983,
               max_depth= 20, # -1 significa no limitar, por ahora lo dejo fijo
               min_gain_to_split= 0.48, #por ahora, lo dejo fijo
               lambda_l1= 0.016934, #por ahora, lo dejo fijo
               lambda_l2= 0.001083, #por ahora, lo dejo fijo
               max_bin= 31, #por ahora, lo dejo fijo
               num_iterations= 86, #un numero muy grande, lo limita early_stopping_rounds
               force_row_wise= TRUE, #para que los alumnos no se atemoricen con tantos warning
               learning_rate= 0.069934,
               feature_fraction= 0.32801,
               min_data_in_leaf=3374,
               num_leaves=395,
               prob_corte=0.044891
)

#----Semillas------------------------------------------------------------------------
#defino el vector de semillas

ksemillas <- c(589481,101504, 310597, 100519,200920,208202,999979,899971,799949,699863,599803) #reemplazar por las propias semillas

vector_ganancias <- c() #vector donde voy a ir acumulando las ganancias

for( semilla in ksemillas)
{
  ganancia <- GananciaArbol( semilla, dataset, x=param, train=0.70 )
  #print(semilla, ganancia)
  vector_ganancias <- c( vector_ganancias, ganancia)
}

data.table(ksemillas,vector_ganancias)
vector_ganancias


mean(  vector_ganancias)





