#########################################################
##########FILTRADO DE DATOS DE LOS MICROARRAYS###########
#########################################################

# Se cargan las librerias necesarias
library(data.table)
library(writexl)

######################
#########EDTA#########
######################

# Se cargan de los datos de EDTA
edta_data= as.data.frame(fread("edta_data.txt", header= T, sep="\t", fill=T))

# Se definen los nombres de las columnas
colnames(edta_data)=c("logSignalA", "logRatioM","logControl", "StdErr.logControl","logExperiment", "StdErr.logExperiment", "pvalue","adj.pvalue", "ProbeID","Description","Group", "logControl","logControl","logControl","logExperiment", "logExperiment", "logExperiment")

# Se seleccionan las columnas de interés
edta_data= edta_data[,c("logRatioM","adj.pvalue","ProbeID","Description")]

# Se indican las columnas numéricas
edta_data$logRatioM=as.double(edta_data$logRatioM) 
edta_data$adj.pvalue=as.double(edta_data$adj.pvalue)

######################
#########FUR##########
######################

# Se cargan los datos de FUR
fur_data= as.data.frame(fread("fur_data.txt", header= T, sep="\t", fill=T))

# Se definen los nombres de las columnas
colnames(fur_data)=c("logSignalA", "logRatioM","logControl", "StdErr.logControl","logExperiment", "StdErr.logExperiment", "pvalue","adj.pvalue", "ProbeID","Description","Group", "logControl","logControl","logControl","logExperiment", "logExperiment", "logExperiment")

# Se seleccionan las columnas de interés
fur_data= fur_data[,c("logRatioM","adj.pvalue","ProbeID","Description")]

# Se indican las columnas numéricas
fur_data$logRatioM=as.double(fur_data$logRatioM) 
fur_data$adj.pvalue=as.double(fur_data$adj.pvalue) 


######################
#######ANÁLISIS#######
######################

# Se filtran los datos FUR con las condiciones:  (logRatioM<=-2 o logRatioM>=2) y adj.pvalue<0.05
genes_fur= subset(fur_data, ( logRatioM <= -2 | logRatioM >= 2) & adj.pvalue<0.05)

# Se filtran los datos que cumplen las condiciones: -2<=logRatioM<= 2 y adj.pvalue<0.05
genes_edta= subset(edta_data, (-2 <= logRatioM & logRatioM <= 2) & adj.pvalue<0.05)

# Se seleccionan los que convergen descartando los emptys
resultado = merge(x=genes_edta, y=genes_fur, by="ProbeID", incomparables = "EMPTY", all=F)

# Se establecen los nombres de las columnas del resultado
colnames(resultado)=c("ProbeID","logRatioM_EDTA","adj.pvalue_EDTA","Description.x","logRatioM_FUR","adj.pvalue_FUR","Description")

# Se eliminan la columna sobrante de Decription.x 
resultado = as.data.frame(within(resultado, rm(Description.x)))

# Se añade la columna de la expresión en Fur
resultado$Expresion_FUR <- cut(resultado$logRatioM_FUR, c(-Inf,0,Inf), c("infra", "sobre"))

# Se añade la columna del diferencial FUR-EDTA
resultado$Edta_fur = (resultado$logRatioM_FUR - resultado$logRatioM_EDTA)

# Ordenamos segun la expresión
resultado=resultado[order(resultado$Edta_fur),]

# Se guardan los resultados en excel
write_xlsx(resultado, "resultados.xlsx")

