###################################################
##################################################
###########################################################
########## ANALISIS PERFILES CLIENTES #####################

### LA MAYORIA DEL ANALISIS PUEDE REALIZARSE EN LA PC LOCAL, SIN VM.

rm( list=ls() )
gc()

# Levatamos las librer√≠as (ESTAS LIBRERIAS SIRVEN TANTO EN LA VM COMO EN LA PC LOCAL)
library("data.table")
#library("lightgbm")
library(dplyr)

library(ggplot2) 
library(ggrepel)
library(devtools)
#library(ggbiplot) #install_github("vqv/ggbiplot")
library(nortest)
library(readxl)

#install_github("vqv/ggbiplot", force = TRUE)
library(ggbiplot) 

#library(mongolite)
library(tidyverse)
#library(readxl)
library(psych)
library(ggplot2)
library(dplyr)
library(lubridate)
library(knitr)  
library(scales)
library(modeest)
library(grid)

install.packages("lessR")
library(lessR)

############# TRABAJO PREVIO EN LA NUBE:

######### ESTA PARTE LA HICE EN LA NUBE (PREPARACION DEL/LOS DATASETS REDUCIDOS PARA TRABAJARLOS LOCALMENTE):
###NOTA: SE REALIZA UNA UNICA VEZ EN LA NUBE, PARA FABRICAR LOS DATASETS, Y LUEGO ESTOS SE BAJAN Y SE TRABAJA SIEMPRE LOCALMENTE EN LA PC.

# Levantamos los datos

setwd("~/buckets/b1/")  

####### EL DATASET SIGUIENTE CONTIENE TODOS LOS REGISTROS, PERO SOLO 60 VARIABLES MAS IMPORTANTES (SIN LAGS, NI NADA).
#### LO FABRIQUE EN LA VM Y LO GUARDE COMO .csv.gz.

dataset_60_Vars  <- fread("./datasets/dataset_Variables_Reducidas_60.csv.gz")
#View(dataset_60_Vars) #Showing 1 to 10 of 7,489,081 entries, 62 total columns

##### EL DATASET DE 60 VARIABLES "dataset_Variables_Reducidas_60.csv.gz" HABIA SIDO CREADO UTILIZANDO LO SIGUIENTE:
#ME VOY A QUEDAR CON LAS 60 VARIABLES MAS IMPORTANTES: 
Vars_Importantes_60 = c("numero_de_cliente", "foto_mes", "cliente_edad", "cliente_antiguedad", 
                       "cliente_vip", "mtransferencias_recibidas", "cplazo_fijo",
                       "mcaja_ahorro_dolares", # HASTA ACA VARIABLES DE PERFIL DEL CLIENTE (AYUDARAN EN LOS BIPLOTS)
                       #ACA EMPIEZAN LAS VARIABLES IMPORTANTES (14): PREDICTIVAS!
                       "ctrx_quarter", "mcaja_ahorro", "cpayroll_trx", "mcuentas_saldo",
                       "mtarjeta_visa_consumo", "mpasivos_margen", "ctarjeta_visa_transacciones", 
                       "Visa_status", "mcuenta_corriente", "mpayroll", "mprestamos_personales",
                       "mdescubierto_preacordado", "ctarjeta_visa", "Master_status", 
                       "Visa_mpagospesos", "mactivos_margen", "Visa_mpagominimo",
                       "Visa_msaldototal", "Visa_msaldopesos", "mrentabilidad_annual",
                       "cproductos", "mautoservicio", "mcomisiones_mantenimiento",
                       "Visa_fechaalta", "Visa_mconsumospesos", "ccaja_ahorro", 
                       "Master_fechaalta", "cprestamos_personales",
                       "mtransferencias_recibidas", "ctarjeta_master",
                       "Visa_cconsumos", "ctarjeta_debito_transacciones",
                       "mrentabilidad", "mtarjeta_master_consumo",
                       "ccaja_seguridad", "mpagomiscuentas", "chomebanking_transacciones",
                       "ccomisiones_mantenimiento", "thomebanking", "Master_Fvencimiento",
                       "Master_msaldopesos", "Visa_Fvencimiento", "Visa_Finiciomora",
                       "mcuenta_debitos_automaticos", "tcallcenter", "Master_mfinanciacion_limite",
                       "mcomisiones_otras", "Visa_mfinanciacion_limite", "ccallcenter_transacciones",
                       "Visa_fultimo_cierre", "Visa_mlimitecompra", "mcomisiones", 
                       "mtransferencias_emitidas", "Master_mlimitecompra",
                       "clase_ternaria" )


##QUITO A LOS MESES 202101 y 202012:
dataset_60_Vars =  dataset_60_Vars[foto_mes< 202012] 

#creo la clases_binarias2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset_60_Vars[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]
View(dataset_60_Vars) #Showing 1 to 12 of 7,008,153 entries, 63 total columns


################
#######

#### CREO DATASETS MAS CHICOS PARA PODER LABURARLOS LOCALMENTE: 

# UN DATASET CON SOLO CLIENTES QUE ALGUNA VEZ SE DIERON DE BAJA
# OTRO DATASET CON CLIENTES QUE NUNCA SE DIERON DE BAJA: CLIENTES SIEMPRE CONTINUAS.

### CREO UNA TABLA QUE ME AGRUPE CLIENTES QUE SE DAN DE BAJA DE CLIENTES QUE NO:
dataset_clasif_bajas_cont = aggregate(clase01 ~ numero_de_cliente, data = dataset_60_Vars, FUN = sum)
#View(dataset_clasif_bajas_cont) #Showing 1 to 13 of 269,817 entries, 2 total columns: 269,817 CLIENTES TOTALES
#head(dataset_clasif_bajas_cont)

# SEPARO EN UNA TABLA A LOS CLIENTES QUE ALGUNA VEZ SE DIERON DE BAJA:
clientes_alguna_vez_bajas = dataset_clasif_bajas_cont[(dataset_clasif_bajas_cont$clase01>0),]
#View(clientes_alguna_vez_bajas) #Showing 1 to 13 of 33,052 entries, 2 total columns
#### ANOTAR BIEN ESTE NUMERO: 33,052 = CANTIDAD TOTAL DE CLIENTES QUE ALGUNA VEZ SE DIERON DE BAJA!!! 

# SEPARO OTRA UNA TABLA A LOS CLIENTES QUE NUNCA SE DIERON DE BAJA (SIEMPRE CONTINUAN):
clientes_siempre_continuan = dataset_clasif_bajas_cont[(dataset_clasif_bajas_cont$clase01==0),]
#View(clientes_siempre_continuan)  #Showing 1 to 13 of 236,765 entries, 2 total columns


####
#HAGO MERGE CON LA BASE TOTAL DE 60 VARIABLES:

#PARA AISLAR LOS REGISTROS SOLO DE LOS CLIENTES QUE ALGUNA VEZ SE DIERON DE BAJA:
dataset_60_bajas = merge(dataset_60_Vars, clientes_alguna_vez_bajas, by.x = "numero_de_cliente", by.y = "numero_de_cliente")
View(dataset_60_bajas) #Showing 1 to 12 of 507,282 entries, 64 total columns

#PARA AISLAR LOS REGISTROS DE LOS CLIENTES QUE SIEMPRE CONTINUARON:
dataset_60_continuan = merge(dataset_60_Vars, clientes_siempre_continuan, by.x = "numero_de_cliente", by.y = "numero_de_cliente")
View(dataset_60_continuan) #Showing 1 to 6,500,871 of 6,500,871 entries, 64 total columns

###
#SUB-SAMPLEO EL DATASET DE CONTINUAS (PARA PODER TRABAJARLO LOCALMENTE):
ktrain_subsampling  <- 0.01 

vector_azar  <- runif( nrow(dataset_60_continuan) )

dataset_60_continuan_sub =  dataset_60_continuan[ ( vector_azar < ktrain_subsampling ) ] 
#View(dataset_60_continuan_sub) #Showing 1 to 12 of 64,679 entries, 64 total columns


####
######## CREO ARCHIVOS .csv PARA BAJARLOS A LA PC Y TRABAJAR TODO LOCAL!!!!:

fwrite(dataset_60_bajas, 
       file= "./datasets/dataset_60_BAJAS.csv.gz", 
       sep= "," )

fwrite(dataset_60_continuan_sub, 
       file= "./datasets/dataset_60_CONTINUAN_sub.csv.gz", 
       sep= "," )



################## TRABAJO LOCAL EN LA PC:
#########################################
#########################################

#### IMPORTO A LA PC LOCAL LOS DATASETS CHICOS CREADOS EN LA NUBE (CON 60 VARIABLES, TODAS LAS BAJAS EN UN DATASET Y EL 1% DE LOS CONTINUAS EN OTRO DATASET):

dataset_60_BAJAS <- fread("C:/DIEGO_/MASTER_DM/2_CUAT_2021/DM_FINANZAS/VIDEOS/dataset_60_BAJAS.csv.gz")
dataset_60_CONTINUAN_s <- fread("C:/DIEGO_/MASTER_DM/2_CUAT_2021/DM_FINANZAS/VIDEOS/dataset_60_CONTINUAN_sub.csv.gz")

#View(dataset_60_BAJAS) #Showing 1 to 11 of 507,282 entries, 64 total columns
#View(dataset_60_CONTINUAN_s) #Showing 1 to 11 of 64,679 entries, 64 total columns

#CAMBIO ORDEN DE COLUMNAS PARA VISUALIZAR MEJOR: #OJO: XXXX CANTIDAD COLUMNAS!!!!!!!
dataset_60_BAJAS = dataset_60_BAJAS[, c(1, 2, 64, 63, 62, 3:61)] #OJO: XXXX CANTIDAD COLUMNAS!!!!!!!
dataset_60_CONTINUAN_s = dataset_60_CONTINUAN_s[, c(1, 2, 64, 63, 62, 3:61)] #OJO: XXXX CANTIDAD COLUMNAS!!!!!!!
dataset_60_BAJAS$clase01.y = NULL
dataset_60_BAJAS$clase01.x = NULL
dataset_60_CONTINUAN_s$clase01.y = NULL
dataset_60_CONTINUAN_s$clase01.x = NULL
#View(dataset_60_BAJAS)
#View(dataset_60_CONTINUAN_s)


#######
############## ***ANALISIS DE LOS PERFILES DE CLIENTES***:

View(dataset_60_BAJAS)       #Showing 296 to 306 of 507,282 entries, 62 total columns
View(dataset_60_CONTINUAN_s) #Showing 1 to 64,679 of 64,679 entries, 62 total columns

#View(clientes_alguna_vez_bajas) #Showing 1 to 13 of 33,052 entries, 2 total columns
#### ANOTAR BIEN ESTE NUMERO: 33,052 = CANTIDAD TOTAL DE CLIENTES QUE ALGUNA VEZ SE DIERON DE BAJA!!! 


###
#### ****LOGICA GRAL DEL ANALISIS****:

# TENGO QUE IR RECORRIENDO CADA UNO DE ESTOS 33,052 CLIENTES Q SE DIERON DE BAJA:

# VER LAS COLUMNAS mcomisionesotras y ccomisionesmantenimiento, PARA VER SI SUBIERON DE 0 A UN VALOR.
# SI ES AFIRMATIVO: ESCRIBIR EN UNA COLUMNA "MOTIVOS BAJA": "COMISIONES"

# VER LAS COLUMNAS mpretamospersonales y cpretamospersonales, PARA VER SI SUBIO DE 0 A 1 Y LA OTRA FUE BAJANDO HASTA 0.
# SI ES AFIRMATIVO: ESCRIBIR EN UNA COLUMNA "MOTIVOS BAJA": "PRESTAMOS"

# VER LAS COLUMNAS npayroll y mpayroll, PARA VER SI BAJARON A 0.
# SI ES AFIRMATIVO: ESCRIBIR EN UNA COLUMNA "PAYROLL".

# VER LA COLUMNA edad, PARA VER SI ES MAYOR A 70.
# SI ES AFIRMATIVO: ESCRIBIR EN UNA COLUMNA "MUERTE".

# VER LAS COLUMNAS Visalimcompra y Masterlimcompra: VER SI NO SUBIERON EN EL ULTIMO A—O.
# SI ES AFIRMATIVO: ESCRIBIR EN UNA COLUMNA "LIM TARJETA".


#View(dataset_60_BAJAS)       #Showing 296 to 306 of 507,282 entries, 62 total columns
#View(dataset_60_CONTINUAN_s) #Showing 1 to 64,679 of 64,679 entries, 62 total columns


# 1) CAMBIO DE ORDEN LAS COLUMNAS, PARA TENER MAS CERCA LAS QUE NECESITO TRABAJAR:

dataset_60_BAJAS_r = dataset_60_BAJAS[, c(1, 2, 3, 5, 6, 30, 31, 9, 34, 35, 36, 12, 26, 27, 57, 51, 56, 61, 50, 48, 45, 7, 8,4, 10, 11, 13:25, 28, 29, 32, 33, 37:44, 46, 47, 49, 52:55, 58:60, 62)] 
#View(dataset_60_BAJAS_r) #Showing 1 to 11 of 507,282 entries, 62 total columns: OK


####### DEJO COMENTADAS SOLUCIONES QUE ME PASARON CLARA Y JOAQUIN (DESPUES YO ARME LA MIA PROPIA BASADO EN EL DE JOAQUIN Y EL SCRIP 951):
##CLA CLA:
#[11:02, 24/11/2021] Diego: 
#for (indiceFila in 1:nrow(df))
#{ if numero_de_cliente == df[indiceFila, numero_de_cliente]
#  { if df[indiceFila, columnax] == 0 { df[indiceFila,columna_nva] = "Pago Prestamo"
#  }
#    numero_de_cliente = df[indiceFila, numero_de_cliente }
#  [11:02, 24/11/2021] Diego: Algo asi???
#    [11:02, 24/11/2021] Diego: Puede ser?
#    [11:07, 24/11/2021] Cla Cla Maestria!!!: perfecto! pero la asigancion del numero de cliente nuevo irira en el esle dedl primer if xq sino lo estas cambiando siempre

##JOAQUIN:
#[11:07, 24/11/2021] Joaquin Aramendia Maestria: Eso es f·cil hacer con data.table
#[11:07, 24/11/2021] Diego: Como seria?
#  [11:10, 24/11/2021] Joaquin Aramendia Maestria: 

#data.table:
# dataset[, ever_zero := <condiciÛn sobre .SD>, by=numero_de_cliente, .SDcols=<columnas para subsets>]

# Deudas totales del cliente
#dataset[ , mdeudas := rowSums( cbind( mv_msaldototal, mprestamos_hipotecarios, mprestamos_prendarios, mprestamos_personales ), na.rm=TRUE ) ]
###########


######## ***MI SOLUCION:

#### 1) CREO LAGS 1 EN TODAS LAS VARIABLES, QUE ME AYUDARAN MAS ADELANTE EN EL ANALISIS:
####LAGS
######

### PRIMERO CREO LA FUNCION DE LAGS (OJO, CUANDO SE USEN VARIABLES CATEGORICAS SE DEBERA PONER EL deltas EN CERO)
Lags  <- function( dataset, cols, nlag, deltas )
{
  
  sufijo  <- paste0( "_lag", nlag )
  
  dataset[ , paste0( cols, sufijo) := shift(.SD, nlag, NA, "lag"), 
           by= numero_de_cliente, 
           .SDcols= cols]
  #agrego los deltas de los lags, con un "for" nada elegante
  if( deltas )
  {
    sufijodelta  <- paste0( "_delta", nlag )
    
    for( vcol in cols )
    {
      dataset[,  paste0(vcol, sufijodelta) := get( vcol)  - get(paste0( vcol, sufijo))]
    }
  }
}

### AHORA CREO LOS LAGS 1:
#(OJO, CUANDO SE USEN VARIABLES CATEGORICAS SE DEBERA PONER EL deltas EN CERO):

cols_analiticas  <- setdiff( colnames(dataset_60_BAJAS_r),  c("numero_de_cliente","foto_mes","clase_ternaria") )

Lags( dataset_60_BAJAS_r, cols_analiticas, 1, 1 )
#View(dataset_60_BAJAS_r) #Showing 1 to 11 of 507,282 entries, 180 total columns
####


#### 2) ANALISIS DE PAYROLL:
# OBJETIVO: VER LAS COLUMNAS npayroll y mpayroll, PARA VER SI BAJARON A 0.
# METODO: CREO UNA NUEVA COLUMNA "Payroll_STATUS" QUE ME MARCARA SI SE DIO O NO ESA CONDICION EN ALGUN MOMENTO (MES) DE CADA CLIENTE.

dataset_60_BAJAS_r11 = dataset_60_BAJAS_r

dataset_60_BAJAS_r11 = dataset_60_BAJAS_r11[ , Payroll_STATUS := ifelse(is.na(cpayroll_trx) | is.na(mpayroll), "", ifelse( cpayroll_trx == 0 & cpayroll_trx_lag1 != 0 & mpayroll == 0 & mpayroll_lag1 != 0, "PERDIO TRABAJO", "")), by = numero_de_cliente, .SDcols = Payroll_STATUS ]

# CAMBIO ORDEN DE COLUMNAS, PARA COMODIDAD DE VISUALIZACION:
dataset_60_BAJAS_r11 = dataset_60_BAJAS_r11[, c(1, 2, 3, 4, 5, 6, 65, 7, 66, 181, 8:64, 67:180  )] 

#View(dataset_60_BAJAS_r11)
#View(dataset_60_BAJAS_r11[dataset_60_BAJAS_r11$Payroll_STATUS=="PERDIO TRABAJO"]) #Showing 1 to 12 of 8,148 entries, 181 total columns = 8148/33052 = 24,7% !!!!!   (VER!!!!): PODRIA ADEMAS AGREGAR Q BAJARON CONSUMOS TARJETA!!! (PORQ TAL VEZ NO SON TODAS PERDIDAS DE TRABAJO!!!)
#View(dataset_60_BAJAS_r11[(dataset_60_BAJAS_r11$Payroll_STATUS=="PERDIO TRABAJO") & (dataset_60_BAJAS_r11$foto_mes > 202003) ]) #Showing 1 to 12 of 557 entries, 181 total columns = 557/33052 = 1,7% !!!!!
##

#View(dataset_60_BAJAS_r) #Showing 1 to 10 of 507,282 entries, 181 total columns
#View(dataset_60_BAJAS_r11) #Showing 1 to 10 of 507,282 entries, 181 total columns


#### 3) ANALISIS DE COMISIONES:
# OBJETIVO: VER LAS COLUMNAS mcomisionesotras y ccomisionesmantenimiento, PARA VER SI SUBIERON DE 0 A UN VALOR.
# METODO: CREO UNA NUEVA COLUMNA "Comisiones_STATUS" QUE ME MARCARA SI SE DIO O NO ESA CONDICION EN ALGUN MOMENTO (MES) DE CADA CLIENTE.

dataset_60_BAJAS_r2 = dataset_60_BAJAS_r11
#View(dataset_60_BAJAS_r2) #Showing 1 to 12 of 507,282 entries, 182 total columns

dataset_60_BAJAS_r2 = dataset_60_BAJAS_r2[ , Comisiones_STATUS := ifelse(is.na(mcomisiones) & is.na(ccomisiones_mantenimiento) & is.na(mcomisiones_mantenimiento) & is.na(mcomisiones_otras) & is.na(cproductos) , "", 
                                                                  ifelse( (mcomisiones != 0 & mcomisiones_lag1 ==0)
                                                                         | (ccomisiones_mantenimiento != 0 & ccomisiones_mantenimiento_lag1 == 0) 
                                                                         | (mcomisiones_mantenimiento  !=0 & mcomisiones_mantenimiento_lag1 ==0)
                                                                         | (mcomisiones_otras !=0 &  mcomisiones_otras_lag1 ==0)
                                                                          & (cproductos == cproductos_lag1),
                                                                         "AUMENTARON COMISIONES", "")), by = numero_de_cliente]

#View(dataset_60_BAJAS_r2[dataset_60_BAJAS_r2$Comisiones_STATUS=="AUMENTARON COMISIONES"]) #Showing 1 to 11 of 42,066 entries, 182 total columns
## ***IMPORTANTE: NOTESE QUE LA CANTIDAD DE AUMENTOS DE COMISIONES SUPERA A LA CANTIDAD DE CLIENTES:
# ES DECIR, Q A TODOS LOS CLIENTES LES SUBIERON LAS COMISIONES ALGUNA VEZ.
# POR LO TANTO: ***PARA DETECTAR SI ESTE FUE UN MOTIVO O NO DE Q SE DIERA DE BAJA, HABRA Q ANALIZAR SI:
# ***ESTE AUMENTO DE COMISION SE DIO *JUSTO UNOS MESES ANTES DE Q SE DIERA DE BAJA* (CUESTION Q SE ANALIZARA MAS ADELANTE CON LAGS MAYORES Y TAMBIEN PARA OTRAS VARIABLES)


#### 4) ANALISIS DE PRESTAMOS:
# OBJETIVO 1: VER SI SACO EL PRESTAMO Y AL POCO TIEMPO SE DIO DE BAJA: VER LAS COLUMNAS mpretamospersonales y cpretamospersonales, PARA VER SI SUBIO DE 0 A 1 Y LA OTRA FUE BAJANDO HASTA 0..
# OBJETIVO 2: VER SI CANCELO EL PRESTAMO Y AL POCO TIEMPO SE DIO DE BAJA: VER LAS COLUMNAS ANTERIORES PERO A LA INVERSA: SI CANCELO EL PRESTAMO Y DESPUES SE DIO DE BAJA.
# METODO: CREO UNA NUEVA COLUMNA "Prestamos_STATUS" QUE ME MARCARA SI SE DIO O NO ESA CONDICION EN ALGUN MOMENTO (MES) DE CADA CLIENTE.

dataset_60_BAJAS_r2 = dataset_60_BAJAS_r2[ , Prestamos_STATUS := ifelse(is.na(cprestamos_personales) | is.na(mprestamos_personales), "", 
                                                                        ifelse( (cprestamos_personales == 0) & (cprestamos_personales_lag1 != 0) 
                                                                                & (mprestamos_personales == 0) & (mprestamos_personales_lag1 != 0), 
                                                                                "CANCELO PRESTAMO", 
                                                                                ifelse( (cprestamos_personales != 0) & (cprestamos_personales_lag1 == 0) 
                                                                                        & (mprestamos_personales != 0) & (mprestamos_personales_lag1 == 0), 
                                                                                        "SACO PRESTAMO", "") ) ) ]

#View(dataset_60_BAJAS_r2) #Showing 1 to 12 of 507,282 entries, 183 total columns


#### 5) ANALISIS DE LIMITE DE TARJETA:
# OBJETIVO: VER SI *UN A—O SEGUIDO* NO LE AUMENTARON EL LIMITE DE TARJETA (SOLO LO REALICE PARA LA TARJETA VISA).
# METODO:
# PRIMERO DEBERE CREAR HASTA LAGS 10 DE LA VARIABLE "Visa_mlimitecompra" (PARA VER SI NO SE LA AUMENTARON UN A—O ENTERO Y SE PUDRIO Y SE DIO DE BAJA: BAJA+2 (2 MESES) + LAG 10 (10 MESES) = 12 MESES)
# LUEGO CREO UNA NUEVA COLUMNA "Lim_Tarjeta_STATUS_ABSOLUTO" QUE ME MARCARA SI SE DIO O NO ESA CONDICION.

#CREO LAGS 10 PARA LA VARIABLE "Visa_mlimitecompra":
cols_analiticas5  <- c("Visa_mlimitecompra")
#Lags( dataset_60_BAJAS_r2, cols_analiticas5, 1, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas5, 2, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas5, 3, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas5, 4, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas5, 5, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas5, 6, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas5, 7, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas5, 8, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas5, 9, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas5, 10, 0 )

#CREO UNA NUEVA COLUMNA "Lim_Tarjeta_STATUS_ABSOLUTO" QUE ME MARCARA SI SE DIO O NO ESA CONDICION: SI NO SE LA AUMENTARON UN A—O ENTERO.
dataset_60_BAJAS_r2 = 
  dataset_60_BAJAS_r2[ , Lim_Tarjeta_STATUS_ABSOLUTO := ifelse(is.na(Visa_mlimitecompra), "", 
                                                                                   ifelse( (Visa_mlimitecompra <= Visa_mlimitecompra_lag7)
                                                                                           | (Visa_mlimitecompra <= Visa_mlimitecompra_lag8) 
                                                                                           | (Visa_mlimitecompra <= Visa_mlimitecompra_lag9)
                                                                                           | (Visa_mlimitecompra <= Visa_mlimitecompra_lag10),
                                                                                           "BAJO LIMITE COMPRA", "")), by = numero_de_cliente]



######### ****AHORA QUE YA HE CREADO TODAS LAS COLUMNAS QUE DETECTAN SI SE DIERON LAS CONDICIONES DE:
# PAYROLL, COMISIONES, LIMITE DE TARJETA, PRESTAMOS, ETC, DEBERE ANALIZAR SI:
# *ESAS CONDICIONES FUERON O NO MOTIVO DE LA BAJA DEL CLIENTE ???
# *PARA ESO, HAY QUE VER SI UNA DE ESAS CONDICIONES SE DIO DURANTE LOS 6 ULTIMOS MESES PREVIOS A LA BAJA (BAJA+2 + LAG 4)!!!

#### PRIMERO: DEBO CREAR LAGS 4 DE TODAS LAS VARIABLES QUE CREE ANTERIORMENTE, PARA VER:
# *SI ALGUNA CONDICION SE DA DENTRO DE LOS 6 MESES PREVIOS (BAJA+2 + LAG 4) A UNA BAJA (ASI LA CLASIFICO COMO "MOTIVO DE BAJA")

####LAGS:
# *OJO: AL SER VARIABLES CATEGORICAS, SE DEBE PONER EL VALOR DE "deltas" EN CERO (O SE ROMPE):

cols_analiticas2  <- c("Comisiones_STATUS", "Payroll_STATUS" )
Lags( dataset_60_BAJAS_r2, cols_analiticas2, 1, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas2, 2, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas2, 3, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas2, 4, 0 )

cols_analiticas3  <- c("Prestamos_STATUS")
Lags( dataset_60_BAJAS_r2, cols_analiticas3, 1, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas3, 2, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas3, 3, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticas3, 4, 0 )

cols_analiticasB  <- c("Lim_Tarjeta_STATUS_ABSOLUTO")
Lags( dataset_60_BAJAS_r2, cols_analiticasB, 1, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticasB, 2, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticasB, 3, 0 )
Lags( dataset_60_BAJAS_r2, cols_analiticasB, 4, 0 )


####
#View(dataset_60_BAJAS_r2)  #Showing 1 to 10 of 507,282 entries, 209 total columns
#####

####### ***AHORA SI DETECTO EL MOTIVO DE BAJA:
# ***PARA ELLO, VEO CUAL DE LAS CONDICIONES ANTES VISTAS SE ENCUENTRAN PRESENTES DENTRO DE LOS 6 MESES PREVIOS A UNA BAJA (BAJA+2 + LAG 4):
# *CREO COLUMNA "MOTIVO_BAJA", QUE ME IRA MARCANDO LOS MOTIVOS DE BAJA, SEGUN LAS CONDICIONES DETECTADAS DENTRO DE LOS 6 MESES PREVIOS A LA BAJA:

dataset_60_BAJAS_r2[ , MOTIVO_BAJA := ifelse( clase_ternaria == "BAJA+2",  
                                        ifelse( cliente_edad>70, "MUERTE",
                                        ifelse(Payroll_STATUS == "PERDIO TRABAJO" 
                                                 | Payroll_STATUS_lag1 == "PERDIO TRABAJO"
                                                 | Payroll_STATUS_lag2 == "PERDIO TRABAJO"
                                                 | Payroll_STATUS_lag3 == "PERDIO TRABAJO"
                                                 | Payroll_STATUS_lag4 == "PERDIO TRABAJO",
                                                 "LABORAL", 
                                        
                                        ifelse(Prestamos_STATUS == "CANCELO PRESTAMO" 
                                                 | Prestamos_STATUS_lag1 == "CANCELO PRESTAMO"
                                                 | Prestamos_STATUS_lag2 == "CANCELO PRESTAMO"
                                                 | Prestamos_STATUS_lag3 == "CANCELO PRESTAMO"
                                                 | Prestamos_STATUS_lag4 == "CANCELO PRESTAMO",
                                                 "PRESTAMO: CANCELACION", 
                                               
                                        ifelse(Prestamos_STATUS == "SACO PRESTAMO" 
                                                 | Prestamos_STATUS_lag1 == "SACO PRESTAMO"
                                                 | Prestamos_STATUS_lag2 == "SACO PRESTAMO"
                                                 | Prestamos_STATUS_lag3 == "SACO PRESTAMO"
                                                 | Prestamos_STATUS_lag4 == "SACO PRESTAMO",
                                                  "PRESTAMO: OTORGAMIENTO",  
                                                   
                                        ifelse(Comisiones_STATUS == "AUMENTARON COMISIONES" 
                                               | Comisiones_STATUS_lag1 == "AUMENTARON COMISIONES"
                                               | Comisiones_STATUS_lag2 == "AUMENTARON COMISIONES"
                                               | Comisiones_STATUS_lag3 == "AUMENTARON COMISIONES"
                                               | Comisiones_STATUS_lag4 == "AUMENTARON COMISIONES",
                                        "COMISIONES",
                                        
                                        ifelse(Lim_Tarjeta_STATUS_ABSOLUTO == "BAJO LIMITE COMPRA" 
                                               | Lim_Tarjeta_STATUS_ABSOLUTO_lag1 == "BAJO LIMITE COMPRA"
                                               | Lim_Tarjeta_STATUS_ABSOLUTO_lag2 == "BAJO LIMITE COMPRA"
                                               | Lim_Tarjeta_STATUS_ABSOLUTO_lag3 == "BAJO LIMITE COMPRA"
                                               | Lim_Tarjeta_STATUS_ABSOLUTO_lag4 == "BAJO LIMITE COMPRA",
                                               "LIMITE TARJETA", 
                                               "OTROS")))))), ""), by = numero_de_cliente]
                                              
                                              
#dataset_60_BAJAS_r2$MOTIVO_BAJA[(dataset_60_BAJAS_r2$cliente_edad>70) & (dataset_60_BAJAS_r2$clase_ternaria=="BAJA+2")] = "VEJEZ"


View(dataset_60_BAJAS_r2[dataset_60_BAJAS_r2$MOTIVO_BAJA == "COMISIONES"]) #Showing 1 to 12 of 10,286 entries, 196 total columns = 10286/33052 = 31%   
View(dataset_60_BAJAS_r2[dataset_60_BAJAS_r2$MOTIVO_BAJA == "LABORAL"]) #Showing 1 to 12 of 2,763 entries, 193 total columns = 2763/33052 = 8.4%  
View(dataset_60_BAJAS_r2[dataset_60_BAJAS_r2$MOTIVO_BAJA == "MUERTE"]) #Showing 1 to 12 of 1,799 entries, 193 total columns = 1799/33052 = 5.4%
View(dataset_60_BAJAS_r2[dataset_60_BAJAS_r2$MOTIVO_BAJA == "PRESTAMO: CANCELACION"])  # Showing 1 to 12 of 1,970 entries, 196 total columns = 1970/33052 = 6%
View(dataset_60_BAJAS_r2[dataset_60_BAJAS_r2$MOTIVO_BAJA == "PRESTAMO: OTORGAMIENTO"])  # Showing 1 to 12 of 475 entries, 196 total columns = 457/33052 = 1.4%
View(dataset_60_BAJAS_r2[dataset_60_BAJAS_r2$MOTIVO_BAJA == "LIMITE TARJETA"])  #Showing 1 to 9 of 4,495 entries, 232 total columns = 4495/33052 = 13.6%
View(dataset_60_BAJAS_r2[dataset_60_BAJAS_r2$MOTIVO_BAJA == "OTROS"]) #Showing 1 to 12 of 1,648 entries, 210 total columns = 1648/33052 = 5% XXXX MAL!!!TIENE Q DAR 34% !!!!! ESO DEBE SER PORQ CUANDO HABIA N/A YA LE PUSE QUE NO PONGA NINGUNA CONDICION DE BAJA!!!!

View(dataset_60_BAJAS_r2)
####COMPLETO LOS NA DE MOTIVOS DE BAJA CON "OTROS":
dataset_60_BAJAS_r2$MOTIVO_BAJA[is.na(dataset_60_BAJAS_r2$MOTIVO_BAJA) & (dataset_60_BAJAS_r2$clase_ternaria=="BAJA+2")] = "OTROS"
dataset_60_BAJAS_r2$MOTIVO_BAJA[(dataset_60_BAJAS_r2$MOTIVO_BAJA =="") & (dataset_60_BAJAS_r2$clase_ternaria=="BAJA+2")] = "OTROS"

### COMPRUEBO Q HAYA QUEDADO LA CANTIDAD CORRECTA DE "OTROS" Y Q LA SUMA TOTAL DE 100%:
View(dataset_60_BAJAS_r2[dataset_60_BAJAS_r2$MOTIVO_BAJA == "OTROS"])  #Showing 1 to 12 of 10,431 entries, 210 total columns = 10431/33052 = 31.56% 
### SUMO TODAS LAS BAJAS EXACTO = 10286 POR COMISIONES + 2763 POR LABORAL + 1799 POR MUERTE + 1970 CANCEL PREST + 475 OTORG PREST + 4495 LIM TARJETA + 10431 OTROS = 32,219 => -33052 = ME FALTAN 833 (2.5%) DE CLIENTES CON BAJAS !!!!! XXXX EL MOTIVO PUEDE SER: QUE HAYA 833 CLIENTES Q SOLO TIENEN BAJA+1 Y NO TIENEN CARGADA BAJA+2.

## TOTAL EXPLICADO BAJAS: COMISIONES + LABORAL + VEJEZ + PRESTAMO: CANCELACION + PRESTAMO: OTORGAMIENTO + LIMITE TARJETA = 65.8%
## OTROS (INEXPLICADO): 34.2%  OK!!!


#########
######### ****GRAFICOS DE TORTAS****:

#### PARA REALIZAR EL GRAFICO DE TORTAS, DEBERE ANTES AGRUPAR TODO POR CLIENTE (PARA Q NO HAY REGISTROS DE MESES CON EL CAMPO "MOTIVO DE BAJA" NULOS, DADO Q EL MOTIVO DE BAJA SE REGISTRA SOLO 1 VEZ POR CLIENTE: EN EL REGISTRO DE BAJA+2)

#ELIMINO LOS REGISTROS Q NO SON BAJA+2, DEJANDO SOLO ESOS Q TIENEN EL MOTIVO DE BAJA:
CLIENTES_MOTIVOS_BAJA_0 = dataset_60_BAJAS_r2[dataset_60_BAJAS_r2$clase_ternaria == "BAJA+2"]
View(CLIENTES_MOTIVOS_BAJA_0) #Showing 1 to 12 of 32,219 entries, 210 total columns = 32,219 OK!!!!

######
#pie(table(CLIENTES_MOTIVOS_BAJA_0$MOTIVO_BAJA), main = "Motivos de baja de los clientes")
###

CLIENTES <- data.frame(cl = CLIENTES_MOTIVOS_BAJA_0$MOTIVO_BAJA)

#install.packages("lessR")
#library(lessR)

PieChart(cl, hole = 0, values = "%", data = CLIENTES,
         fill = 
        c("lightgreen", "red", "blue", "black", 
          "grey", "pink", "brown"), main = "Motivos de bajas de los clientes")
#legend("topleft", legend = c("Comisiones", "Laboral", "Lim. Tarj.", "Muerte",  "Cancelacion prest.", "Otorgam. prest.", "Otros"),
 #      fill =  c("lightgreen", "red", "blue", "black", 
  #                "pink", "brown","grey" ))


####### TO DO: FALTA:

## AGREGAR COLUMNA "ANIO": PARA LUEGO HACER GRAFICOS TORTAS Y BARRAS POR A—O.

## ANALIZAR LA EVOLUCION Y COMPOSICION ANUAL DE LOS MOTIVOS DE BAJAS (TORTAS POR A—O Y GRAFICO DE BARRAS POR A—O)

## HACER EL MISMO ANALISIS Y GRAFICO DE TORTAS PARA LAS CONTINUA (DATASET SUB-SAMPLEADO):
# PARA COMPARAR LOS % QUE DA EN EL CONTINUA VS. EN LAS BAJAS Y CALCULAR LOS LIFTS:
# POR EJEMPLO: SI EN BAJAS COMISIONES = 32% Y EN CONTINUA = 16% => TENDRIAMOS UN LIFT = 2 PARA EL MOTIVO DE BAJA POR SUBA DE COMISIONES

## CALCULAR CANTIDAD DE CLIENTES DE CADA BAJA Y SU GANANCIA PERDIDA EN $ (ANUAL).

## CALCULAR LA GANANCIA $ PERDIDA EN TODA LA VIDA UTIL REMANENTE DE LOS CLIENTES (70 A—OS - EDAD BAJA CLIENTE) X RENTABILIDAD ANUAL PROMEDIO

## HACER GRAFICOS DE TORTAS CON LAS RENTABILIDADES DE TODA LA VIDA UTIL REMANENTE POR MOTIVO DE BAJA.

## EN BASE A LOS GRAFICOS Y ANALISIS PREVIOS: DELINEAR PLANES DE ACCION.
#






