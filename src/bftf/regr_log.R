rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
setwd("/Users/clara/Documents/00-Posgrado/4_DM_Eco_y_Finanzas/")

karch_dataset    <- "./datasets/dataset_epic_simple_v009.csv.gz"   #este dataset se genero en el script 812_dataset_epic.r

#levanto el dataset
dataset <-fread(karch_dataset )

#creo clase01 
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

dataset_non_na <- dataset[complete.cases(dataset),]

f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}

ss=data.frame(apply(dataset,2,f))
df <- as.data.frame(ss)

library("tidyr")
data4 <- dataset[, -c("clase_ternaria")] %>% drop_na() 

data2 <- dataset[complete.cases(dataset), ]  
df <- na.omit(dataset[, -c("clase_ternaria","clase01")])

dataset_train  <- dataset[dataset$foto_mes==202009]
dataset_test  <- df[df$foto_mes==202011]




model <- glm(clase01~cr_consumo_tarjeta+mcaja_ahorro+cr_ah_pay+ctrx_quarter+mcuenta_corriente+cr_pasivos+cr_ing_total+ctarjeta_visa_transacciones+cr_totsaldo_payroll,family=binomial(link='logit'), options(na.action = "na.exclude") ,data=dataset_train)