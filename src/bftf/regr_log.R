rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
setwd("/Users/clara/Documents/00-Posgrado/4_DM_Eco_y_Finanzas/")

karch_dataset    <- "./datasets/dataset_epic_simple_v009.csv.gz"   #este dataset se genero en el script 812_dataset_epic.r

#levanto el dataset
dataset <-fread(karch_dataset )

#creo clase01 
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

##paso los NaN a 0 , decision polemica si las hay
nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
nans_qty  <- sum( unlist( nans) )

if( nans_qty > 0 )
{
  cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
  cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
  dataset[mapply(is.nan, dataset)] <- 0
}

a <- dataset[1:10,1:10]
a[1,5]<-NaN
print(a)


b <- c(1, Inf, NA, NaN)
b[which(!is.finite(b))] <- 0

a[2,5]<- NA
a[3,5]<- Inf

a
df<- do.call(data.frame,  
                   lapply(dataset,
                          function(x) replace(x, !is.finite(x), 0)))

data.frame(df)

df1 <- data.frame(matrix(unlist(df), nrow=length(df), byrow=TRUE))

dataset_train  <- df[df$foto_mes==202009]
dataset_test  <- df[df$foto_mes==202011]




model <- glm(clase01~cr_consumo_tarjeta+mcaja_ahorro+cr_ah_pay+ctrx_quarter+mcuenta_corriente+cr_pasivos+cr_ing_total+ctarjeta_visa_transacciones+cr_totsaldo_payroll,family=binomial(link='logit'), data=dataset_train)