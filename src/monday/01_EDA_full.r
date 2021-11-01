#Necesita para correr en Google Cloud
#128 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU

#clase_binaria2   1={BAJA+2,BAJA+1}    0={CONTINUA}
#Entrena en a union de VEINTE  meses de [201901, 202009] - { 202006 }  haciendo subsampling al 10% de los continua
#Testea en  { 202011 }
#estima automaticamente la cantidad de registros a enviar al medio de la meseta (en lugar de la prob de corte)

#Optimizacion Bayesiana de hiperparametros de  lightgbm
#funciona automaticamente con EXPERIMENTOS
#va generando incrementalmente salidas para kaggle

# WARNING  usted debe cambiar este script si lo corre en su propio Linux

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


#para poder usarlo en la PC y en la nube sin tener que cambiar la ruta
#cambiar aqui las rutas en su maquina
switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "M:\\" },   #Windows
         Darwin  = { directory.root  <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root  <-  "~/buckets/b1/" } #Google Cloud
)
#defino la carpeta donde trabajo
setwd( directory.root )

karch_dataset    <- "./datasets/dataset_epic_v951.csv.gz"

dataset  <- fread(karch_dataset)

install.packages("dataMaid")
library('dataMaid')  
# Produce un "lindo" reporte 


months<- c(201901,201902,201903,201904,201905,201906,201907,201908,201909,201910,201911,201912,202001,202002,202003,202004,202005,202006,202007,202008,202009,202010,202011)


for (month in months){
  
  dataset_month <- dataset[dataset$foto_mes==month] #aca voy selecionando el mes para hacerlo
  
  fname <- paste0(month, "_EDA_pdf")
  
  makeDataReport(dataset_month,output="pdf", render = TRUE, file = fname, replace = TRUE) 
  
}
  

