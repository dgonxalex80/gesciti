# Analisis  envolvente de datos
# install.packages("deaR", dependencies = TRUE)
install.packages("viridis")  # Install
library("viridis")           # Load

file.choose()
base1<-read.csv("/home/deg/Documentos/PROYECTOS/Gesitic/base2.csv")
base1=as.data.frame(base1)
bse1=base1+matrix(10,14,11)

library(deaR) 
  # se genera la matrixz para el proceso
dataDEA=read_data(base1,dmus=1, inputs=2:5,outputs=6:12)
 # se genera el modelo
resultados <- model_basic (dataDEA,             # data 
                           orientation = "io",    # modelo orientado a input
                           rts = "crs")           # asume retornos variables a escala

eficiencia=efficiencies(resultados)
eficiencia=sort(eficiencia, decreasing = FALSE)
barplot(eficiencia,col=viridis(16),horiz = TRUE,las=1)
eficiencia

barplot(resultados1$lambdas.DMU) 
barplot(resultados1$lambdas.IPP9,horiz = TRUE, las=1)



plot(resultados)

resultados1=summary(resultados)
write_csv(resultados1,"/home/deg/Documentos/PROYECTOS/Gesitic/resultadosDEA.csv")

