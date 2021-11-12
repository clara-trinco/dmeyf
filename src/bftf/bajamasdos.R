rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
library(ggplot2)
library(tidyverse)

require("rlist")
require("yaml")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")


setwd("/Users/clara/Documents/00-Posgrado/4_DM_Eco_y_Finanzas/")


karch_dataset    <- "./datasets/datasets-dataset_baja.csv.gz" 

dataset <- fread(karch_dataset)
colnames(dataset[,1001:1500])

dataset_modelito <-"./datasets/modelitos-E1007_modelitos.csv.gz" 
dataset_modelito <- fread(dataset_modelito)
#analizo ls variables importantes y su relacion con BAJA+2

a <-dataset[dataset$clase_ternaria=="BAJA+2"]

#Edad
ggplot(a, aes(x = cliente_edad)) +
  geom_bar(position = "identity", alpha = 0.5, fill = "#1B9E77", color = "white")+
  theme(legend.position="top") + 
  #theme_classic()
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggplot(dataset[dataset$clase_ternaria=="BAJA+2"], aes(x = cliente_antiguedad)) +
  geom_bar(position = "identity", alpha = 0.5, fill = "#1B9E77", color = "white")+
  theme(legend.position="top") + 
  #theme_classic()
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggplot(a, aes(x = cliente_antiguedad, y = mv_status01)) + 
  geom_jitter(alpha = 0.75) +
  scale_color_brewer(palette = "Set1") +  
  theme(legend.position = "none") + 
  theme_minimal() + 
  labs(y = "Antiguedad", x = "Edad") +
  ggtitle("Scatterplot: Peso en funcion de la altura")

#mv_status01, cr_eg_total, ctrx_quarter, mcaja_ahorro, cr_consumo_tarjeta, Visa_delinquency, mdescubierto_preacordado_tend, cr_ah_pay

ggplot(dataset, aes(clase_ternaria, mv_status01))+
  geom_count()

table(dataset$clase_ternaria, dataset$mv_status01)


table(dataset$clase_ternaria, dataset$cr_eg_total)

tabla1 <- dataset %>% group_by(clase_ternaria) %>% summarise(avg=mean(cr_eg_total,na.rm=TRUE))


tabla2 <- dataset %>% group_by(clase_ternaria) %>% summarise(avg=mean(ctrx_quarter_tend,na.rm=TRUE))

tabla3 <- dataset %>% group_by(clase_ternaria) %>% summarise(avg=mean(mcaja_ahorro,na.rm=TRUE))

tabla3
