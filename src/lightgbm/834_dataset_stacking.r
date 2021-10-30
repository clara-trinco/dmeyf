#Necesita para correr en Google Cloud
#64 GB de memoria RAM
#256 GB de espacio en el disco local
#4 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

#setwd("~/buckets/b1/crudoB/" )
setwd("/Users/clara/Documents/00-Posgrado/4_DM_Eco_y_Finanzas/")

version  <- "v012"  #cambiar cada vez, asi se tiene versionado del dataset

dataset  <- fread( "./datasets/dataset_epic_simple_v009.csv.gz" )
dataset  <- copy(  dataset[  , c("numero_de_cliente","foto_mes","clase_ternaria","cr_eg_total","cr_consumo_tarjeta", "ctrx_quarter", "cr_ing_total", "cr_eg_total_cr_cant_prod", "ctrx_quarter_cr_consumo_tarjeta_1", "cr_eg_total_cr_consumo_tarjeta_1", "cr_consumo_tarjeta_cr_ing_total_1", "cr_eg_total_ctrx_quarter_1"),  with=FALSE] )
gc()


#leo TODOS los archivos que estan en la carpeta  modelitos
#y hago el join con  dataset  <numero_de_cliente, foto_mes, clase_ternaria>

archivos  <- list.files( pattern="modelitos.csv.gz", path="./modelitos/" )
for( archivo  in archivos )
{
  darchivo  <- fread( paste0("./modelitos/", archivo ) )
  dataset  <- merge( dataset, darchivo, by=c("numero_de_cliente","foto_mes") )
}

#"cr_eg_total","cr_consumo_tarjeta", "ctrx_quarter", "cr_ing_total", "cr_consumo_tarjeta", "cr_eg_total_cr_cant_prod"

gc()

fwrite( dataset,
        file=paste0( "./datasets/dataset_stacking_", version, ".csv.gz"),
        sep="," )

