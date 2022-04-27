#graficos
library(ggplot2)
base <- readRDS("~/Documentos/PROYECTOS/Gesitic/baseDEA.RDS")
#-----------------------------------------------------------

t=table(base$p2x,base$p2y)
pdf("fig-p2.pdf")
rownames(t)=c("Manufactureo","Servicios","Comercio")
colnames(t)=c("Micro","Pequeña","Mediana","Grande")
mosaicplot(t, las=1, main=" ",color = c(3,4,7,8))
dev.off()
#-----------------------------------------------------------
t7=sort(table(base$TIPOLO),decreasing =TRUE)
names(t7)=c("Amplia", "Estric", "Intenc", "No inno", "Potenc")
pdf("fig-tipolo.pdf")

barplot(t7, col=c(3,4,5,6,7), ylim=c(0,2500))
dev.off()
#-----------------------------------------------------------
t8=table(base$p8)
pdf("fig-p8.pdf")
pct=paste(round(100*t8/sum(t8), 1), "%")
pie(t8, labels = pct, main = " ",col = rainbow(length(t8)))
legend("topleft", c("No estratégico","No necesario", 
                     "No información", "Otras razones"), cex = 0.8,
       fill = rainbow(length(t8)))
dev.off()
# Pregunta con respuesta multiple a b c d e f  
# t12=
#-----------------------------------------------------------
t13a=table(base$p13a)
t13b=table(base$p13b)
t13c=table(base$p13c)

t13=sort(t13a[1:9]+t13b[1:9]+t13c[1:9], decreasing = FALSE)
pdf("fig-p13.pdf")
barplot(t13, col=rainbow(9), horiz = TRUE,  
        xlim = c(0,2000), las=1)
legend("bottomright",c("1-Incentivos fiscales", 
                   "2-Subvenciones directa", 
                   "3-Programas de garantica", 
                   "4-Financiamiento cond.preferenciales", 
                   "5-Fondos capital de riesgo",
                   "6-Fomenmto de la investigacion",
                   "7-Apoyo al incentivo entre universidades",
                   "8-Parques cientificos y tecnologicos",
                   "9-Incubadoras de empresas"))
dev.off()

#-----------------------------------------------------------
t16a=table(base$p16a)
t16b=table(base$p16b)
t16c=table(base$p16c)
t16=t16a+t16b+t16c
t16=sort(t16,decreasing = TRUE)
pdf("fig-p16.pdf")

barplot(t16, col=rainbow(8), horiz = TRUE,  
        las=1, xlim=c(0,3000))
legend("topright",c("1-Patentes", 
                     "2-Modernización tecnológica", 
                     "3-Nuevas competencias de recurso humano", 
                     "4-Innovación de producto", 
                     "5-Innovación comercia",
                     "6-Innovación de procesos",
                     "7-Innovación organizacional",
                     "8-Otro"))
dev.off()
#-----------------------------------------------------------
t42a=table(base$p42a)
t42b=table(base$p42b)
t42c=table(base$p42c)
t42=rbind(t42a,t42b,t42c)
rownames(t42)=c("c1","c2","c3")
colnames(t42)=c("si","no")  
t42=t(t42)
pdf("fig-p42.pdf")
barplot(t42, col=c("blue","white"), horiz = FALSE, ylim=c(0,4500))
legend("topright",c("c1-Ejecución y desarrollo de proyectos ",
                    "c2-Trabajos de creación ", 
                    "c3-Aplicación de nuevos conocimientos"))
dev.off()
#-----------------------------------------------------------


#-----------------------------------------------------------



