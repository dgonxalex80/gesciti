# Simulacion base de datos 
# base referente

# file.choose() # obtener direccion base
# base edit 2018
Edit.2017_2018=read.csv("/home/deg/Documentos/PROYECTOS/Gesitic/Edit 2017_2018.csv", sep=";")
#-------------------------------------------------------------------------------------
#  CATEGORIA 1 : INFORMACION BASICA DE LA ORGANIZACION
#
# id empresa
n=dim(Edit.2017_2018)
n=n[1]
p01=as.data.frame(1:7529)
base=p01
base$TIPOLO=Edit.2017_2018$TIPOLO
base$p2x=rbinom(n,2,0.2)
base$p2y=rbinom(n,3,0.2)
base$CIIU4=Edit.2017_2018$CIIU4
base$CIIU=floor(base$CIIU4/1000)
base$p4=rbinom(n,20,0.2)
base$p7=rbinom(n,1,0.4) #  0. no, 1. si
t7=table(base$p7)
base$p8=0
base$p8=rbinom(n,3,0.3)


#------------------------------------------------------------------------------
# CATEGORIA 2 : PERFIL COMPETITIVO 
#
# para los que esponden "no" en p7 termina la encuesta
base$p9=rbinom(n,1,0.4)  # no/si 
#------------------------------------------------------------------------------
# las variables a continuacion solo aplican para los que respondieron si en p7
#  la base solo sigue con base1
base1=subset(base,base$p7==0)
n2=length(base1$p7)

base1$p10=NA
base1$p10=rbinom(n2,4,0.4)+1 # ordinal 1,2,3,4,51

base1$p11=NA
base1$p11=rbinom(n2,4,0.4)+1 # ordinal 1,2,3,4,5

base1$p12a=NA
base1$p12a=rbinom(n2,4,0.4)+1 # ordinal 1,2,3,4,5

base1$p12b=NA
base1$p12b=rbinom(n2,4,0.4)+1 # ordinal 1,2,3,4,5

base$p12c=NA
base1$p12c=rbinom(n2,4,0.4)+1 # ordinal 1,2,3,4,5

base1$p12d=NA
base1$p12d=rbinom(n2,4,0.4)+1 # ordinal 1,2,3,4,5

base1$p12e=NA
base1$p12e=rbinom(n2,4,0.4)+1 # ordinal 1,2,3,4,5

base1$p12f=NA
base1$p12f=rbinom(n2,4,0.4)+1 # ordinal 1,2,3,4,5

base1$p12g=NA
base1$p12g=rbinom(n2,4,0.4)+1 # ordinal 1,2,3,4,5
#===============================================================================
 # queda solo las que responden si en p7
#===============================================================================
# preguntas de p13 - p17 ciclo que se repite varias veces 

base1$p13a=NA
base1$p13a=rbinom(n2,13,0.4)+1 # ordinal 1,2,3,4...14

base1$p14a=NA
base1$p14a=rbinom(n2,5,0.4)+1 # ordinal 1,2,3,4...6

base1$p15a=NA
base1$p15a=rbinom(n2,3,0.4)+1 # ordinal 1,2,3,4

base1$p16a=NA
base1$p16a=rbinom(n2,7,0.4)+1 # ordinal 1,2,3,5,6,..8

base1$p17a=NA
base1$p17a=rbinom(n2,1,0.3) # ordinal 0. no 1. si 
#------------------------------------------------------------
base1=base1[order(base1$p17a),]
ta=table(base$p17a)
base=base1
#------------------------------------------------------------
# los que responden si continuan 
base1=subset(base,base$p17a==0)
base2=subset(base,base$p17a==1)
n3=length(base1$p17a)


base1$p13b=NA
base1$p13b=rbinom(n3,13,0.4)+1 # ordinal 1,2,3,4...14
base1$p14b=rbinom(n3,5,0.4)+1 # ordinal 1,2,3,4,5,6
base1$p15b=NA
base1$p15b=rbinom(n3,3,0.4)+1 # ordinal 1,2,3,4
base1$p16b=NA
base1$p16b=rbinom(n3,7,0.4)+1 # ordinal 1,2,3,5,6,7,8
base1$p17b=NA
base1$p17b=rbinom(n3,1,0.3) # ordinal 0. no 1. si 

base2$p13b=NA
base2$p14b=NA
base2$p15b=NA
base2$p16b=NA
base2$p17b=NA
#==============================================================
base=rbind(base1,base2)
base=base[order(base1$p17b),]
ta=table(base$p17b)

base1=subset(base,base$p17b==0)
base2=subset(base,base$p17b==1)
n3=length(base1$p17b)
#--------------------------------------------------------------
# los que responden si continuan 
base1$p13c=NA
base1$p13c=rbinom(n3,13,0.4)+1 # ordinal 1,2,3,4,..14

base1$p14c=NA
base1$p14c=rbinom(n3,5,0.4)+1 # ordinal 1,2,3,4,5,6

base1$p15c=NA
base1$p15c=rbinom(n3,3,0.4)+1 # ordinal 1,2,3,4

base1$p16c=NA
base1$p16c=rbinom(n3,7,0.4)+1 # ordinal 1,2,3,5,6,7,8

base1$p17c=NA
base1$p17c=rbinom(n3,1,0.3)+1 # ordinal 1,2

base2$p13c=NA
base2$p14c=NA
base2$p15c=NA
base2$p16c=NA
base2$p17c=NA
#================================================================
base=rbind(base1,base2)
n4=length(base$p7)
n71=n4
#================================================================
base$p42a=NA
base$p42a=rbinom(n4,1,0.4)+1 # ordinal 1,2

base$p42b=NA
base$p42b=rbinom(n4,1,0.7)+1 # ordinal 1,2

base$p42c=NA
base$p42c=rbinom(n4,1,0.3)+1 # ordinal 1,2

base$p43a=NA
base$p43a=rbinom(n4,1,0.4)+1 # ordinal 1,2

base$p43b=NA
base$p43b=rbinom(n4,1,0.7)+1 # ordinal 1,2

#----------------------------------------------------------------

base$p44a=NA
base$p44a=rbinom(n4,1,0.4)+1 # ordinal 1,2

base$p44b=NA
base$p44b=rbinom(n4,1,0.4)+1 # ordinal 1,2

base$p44c=NA
base$p44c=rbinom(n4,1,0.2)+1 # ordinal 1,2

base$p44d=NA
base$p44d=rbinom(n4,1,0.5)+1 # ordinal 1,2

base$p44e=NA
base$p44e=rbinom(n4,1,0.7)+1 # ordinal 1,2

base$p44f=NA
base$p44f=rbinom(n4,1,0.5)+1 # ordinal 1,2

base$p44g=NA
base$p44g=rbinom(n4,1,0.4)+1 # ordinal 1,2

base$p44h=NA
base$p44h=rbinom(n4,1,0.7)+1 # ordinal 0,1

base$p44i=NA
base$p44i=rbinom(n4,1,0.3)+1 # ordinal 0,1

base$p44j=NA
base$p44j=rbinom(n4,1,0.8)+1 # ordinal 1,2

base$p44k=NA
base$p44k=rbinom(n4,1,0.3)+1 # ordinal 1,2

base$p44l=NA
base$p44l=rbinom(n4,1,0.4)+1 # ordinal 1,2

base$p44m=NA
base$p44m=rbinom(n4,1,0.3)+1 # ordinal 1,2

base$p44n=NA
base$p44n=rbinom(n4,1,0.5)+1 # ordinal 1,2

base$p44o=NA
base$p44o=rbinom(n4,1,0.6)+1 # ordinal 1,2

# ----------------------------------------------------------------------------

base$p45a=NA
base$p45a=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p45b=NA
base$p45b=rbinom(n71,1,0.6)+1 # ordinal 1,2

base$p45c=NA
base$p45c=rbinom(n71,1,0.3)+1 # ordinal 1,2

base$p45d=NA
base$p45d=rbinom(n71,1,0.8)+1 # ordinal 1,2

base$p45e=NA
base$p45e=rbinom(n71,1,0.6)+1 # ordinal 1,2

base$p45f=NA
base$p45f=rbinom(n71,1,0.3)+1 # ordinal 1,2

# -----------------------------------------------------------------------------
base$p46a=NA
base$p46a=rbinom(n71,1,0.4) # ordinal 0,1

base$p46b=NA
base$p46b=rbinom(n71,1,0.7) # ordinal 0,1
#------------------------------------------------------------------------------
# CATEGORIA 3 : RECURSOS UTILIZADOS
#------------------------------------------------------------------------------
#
base$p47=NA
base$p47=rbinom(n71,3,0.3)+1 # ordinal 1,2
#-------------------------------------------------------------------------------
base$p48a=NA
base$p48a=rbinom(n71,3,0.3)+1 # ordinal 1,2

base$p48b=NA
base$p48b=rbinom(n71,3,0.1)+1 # ordinal 1,2

base$p48c=NA
base$p48c=rbinom(n71,3,0.05)+1 # ordinal 1,2

#-------------------------------------------------------------------------------
base$p49=NA
base$p49=rbinom(n71,4,0.05)+1 # ordinal 1,2,3,4,5
#------------------------------------------------------------------------------
base$p50a=NA
base$p50a=rbinom(n71,1,0.7)+1 # ordinal 1,2,3,4,5

base$p50b=NA
base$p50b=rbinom(n71,1,0.5)+1 # ordinal 1,2,3,4,5

base$p50c=NA
base$p50c=rbinom(n71,1,0.2)+1 # ordinal 1,2,3,4,5

base$p50d=NA
base$p50d=rbinom(n71,1,0.1)+1 # ordinal 1,2,3,4,5

#------------------------------------------------------------------------------
base$p51=NA
base$p51=rbinom(n71,4,0.1)+1 # ordinal 1,2,3,4,5
#-------------------------------------------------------------------------------
# CATEGORIA 4 PATRONES DE INNOVACION
#
base$p52a=NA
base$p52a=rbinom(n71,1,0.3)+1 # ordinal 1,2

base$p52b=NA
base$p52b=rbinom(n71,1,0.3)+1 # ordinal 1,2

base$p52c=NA
base$p52c=rbinom(n71,1,0.2)+1 # ordinal 1,2

base$p52d=NA
base$p52d=rbinom(n71,1,0.1)+1 # ordinal 1,2
#-------------------------------------------------------------------------------

base$p53a=NA
base$p53a=rbinom(n71,1,0.3)+1 # ordinal 1,2

base$p53b=NA
base$p53b=rbinom(n71,1,0.3)+1 # ordinal 1,2

base$p53c=NA
base$p53c=rbinom(n71,1,0.2)+1 # ordinal 1,2

base$p53d=NA
base$p53d=rbinom(n71,1,0.1)+1 # ordinal 1,2

#------------------------------------------------------------------------------

base$p54a=NA
base$p54a=rbinom(n71,1,0.3)+1 # ordinal 1,2

base$p54b=NA
base$p54b=rbinom(n71,1,0.3)+1 # ordinal 1,2

base$p54c=NA
base$p54c=rbinom(n71,1,0.2)+1 # ordinal 1,2

base$p54d=NA
base$p54d=rbinom(n71,1,0.1)+1 # ordinal 1,2
#-------------------------------------------------------------------------------

base$p55a=NA
base$p55a=rbinom(n71,1,0.3)+1 # ordinal 1,2

base$p55b=NA
base$p55b=rbinom(n71,1,0.3)+1 # ordinal 1,2

base$p55c=NA
base$p55c=rbinom(n71,1,0.2)+1 # ordinal 1,2

base$p55d=NA
base$p55d=rbinom(n71,1,0.1)+1 # ordinal 1,2
#--------------------------------------------------------# Simulacion base de datos 
# base referente
#  CATEGORIA 5 : ARTICULACION EMPRESA - ACADEMIA - CENTRO DE TRANSFERENCIA TEC

base$p56a=NA
base$p56a=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p56b=NA
base$p56b=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p56c=NA
base$p564=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p56d=NA
base$p56d=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p56e=NA
base$p56e=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p56f=NA
base$p56f=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p56g=NA
base$p56g=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p56h=NA
base$p56h=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p56i=NA
base$p56i=rbinom(n71,1,0.4)+1 # ordinal 1,2
#-------------------------------------------------------------------------------

base$p57a=NA
base$p57a=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p57b=NA
base$p57b=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p57c=NA
base$p57c=rbinom(n71,1,0.4)+1 # ordinal 1,2
#-------------------------------------------------------------------------------
base$p58a=NA
base$p58a=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p58b=NA
base$p58b=rbinom(n71,1,0.4)+1 # ordinal 1,2
#------------------------------------------------------------------------------
base$p59a=NA
base$p59a=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p59b=NA
base$p59b=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p59c=NA
base$p59c=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p59d=NA
base$p59d=rbinom(n71,1,0.4)+1 # ordinal 1,2
#-------------------------------------------------------------------------------
base$p60a=NA
base$p60a=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p60b=NA
base$p60b=rbinom(n71,1,0.4)+1 # ordinal 1,2# Simulacion base de datos 
# base referente

base$p60c=NA
base$p60c=rbinom(n71,1,0.4)+1 # ordinal 1,2
#-------------------------------------------------------------------------------

base$p61a=NA
base$p61a=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p61b=NA
base$p61b=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p61c=NA
base$p61c=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p61d=NA
base$p61d=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p61e=NA
base$p61e=rbinom(n71,1,0.4)+1 # ordinal 1,2
#-------------------------------------------------------------------------------
base$p62a=NA
base$p62a=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p62b=NA
base$p62b=rbinom(n71,1,0.4)+1 # ordinal 1,2
#-------------------------------------------------------------------------------
base$p63a=NA
base$p63a=rbinom(n71,1,0.4)+1 # ordinal 1,2
# Simulacion base de datos 
# base referente
base$p63b=NA
base$p63b=rbinom(n71,1,0.4)+1 # ordinal 1,2

base$p63c=NA
base$p63c=rbinom(n71,1,0.4)+1 # ordinal 1,2
#-------------------------------------------------------------------------------
saveRDS(base, "/home/deg/Documentos/PROYECTOS/Gesitic/base.RDS")
# ------------------------------------------------------------------------------
# PREPARACION DE LA BASE PARA CORRER DEA
#
# DEFINIR LAS UNIDADES (IPP : P13 a, b,  c)
#
# DEFINIR ENTRADAS (input: p15 ) p47, p48,p49, p51)
#
# DEFINIR SALIDAS (output : p16 ) p42, p43,p44, p45, p46  )
#
# base1=base 
# base2=base[sample(nrow(base), 2000), ]
# base3=base[sample(nrow(base), 1500), ]
# base4=base[sample(nrow(base), 1000), ]
#
# DEFINIR LAS ENTRADAS INPUT
# input1 usaron Incentivos fiscales
# Ahora cual es la frecuencia de P15 para cada IPP1?
#------------------------------------------------------------------------------
i15a=table(base$p13a,base$p15a)
i15b=table(base$p13b,base$p15b)
i15c=table(base$p13c,base$p15c)
ia=i15a[1:10,]
ib=i15b[1:10,] 
ic=i15c[1:10,]
input1=ia+ib+ic
#------------------------------------------------------------------------------
o15a=table(base$p13a,base$p16a)
o15b=table(base$p13b,base$p16b)
o15c=table(base$p13c,base$p16c)
oa=o15a[1:10,]# Simulacion base de datos 
# base referente
ob=o15b[1:10,] 
oc=o15c[1:10,]
output1=oa+ob+oc
data1=cbind(input1,output1)
data1=as.data.frame(data1) # +matrix(15,10,12)
#-------------------------------------------------------------------------------
# base de DEA
write.csv(data1,file="data1.csv")
#-------------------------------------------------------------------------------

# Analisis de correspondencia

a42a=table(base$p13a,base$p42a); a42a=a42a[1:10,]
a42b=table(base$p13b,base$p42b); a42b=a42b[1:10,]
a42c=table(base$p13c,base$p42c); a42c=a42c[1:10,]

a43a=table(base$p13b,base$p43a); a43a=a43a[1:10,]
a43b=table(base$p13c,base$p43b); a43b=a43b[1:10,]

a44a=table(base$p13b,base$p44a); a44a=a44a[1:10,]
a44b=table(base$p13c,base$p44b); a44b=a44b[1:10,]
a44c=table(base$p13b,base$p44c); a44c=a44c[1:10,]
a44d=table(base$p13c,base$p44d); a44d=a44d[1:10,]
a44e=table(base$p13b,base$p44e); a44e=a44e[1:10,]
a44f=table(base$p13c,base$p44f); a44f=a44f[1:10,]
a44g=table(base$p13b,base$p44g); a44g=a44g[1:10,]
a44h=table(base$p13c,base$p44h); a44h=a44h[1:10,]
a44i=table(base$p13b,base$p44i); a44i=a44i[1:10,]
a44j=table(base$p13c,base$p44j); a44j=a44j[1:10,]
a44k=table(base$p13b,base$p44k); a44k=a44k[1:10,]
a44l=table(base$p13c,base$p44l); a44l=a44l[1:10,]
a44m=table(base$p13b,base$p44m); a44m=a44m[1:10,]
a44n=table(base$p13c,base$p44n); a44n=a44n[1:10,]
a44o=table(base$p13b,base$p44o); a44o=a44o[1:10,]

a45a=table(base$p13c,base$p45a); a45a=a45a[1:10,]
a45b=table(base$p13c,base$p45b); a45b=a45b[1:10,]
a45c=table(base$p13c,base$p45c); a45c=a45c[1:10,]
a45d=table(base$p13c,base$p45d); a45d=a45d[1:10,]
a45e=table(base$p13c,base$p45e); a45e=a45e[1:10,]
a45f=table(base$p13c,base$p45f); a45f=a45f[1:10,]

a46a=table(base$p13c,base$p46a); a46a=a46a[1:10,]
a46b=table(base$p13c,base$p46b); a46b=a46b[1:10,]

a=cbind(a42a,a42b,a42c,a43a,a43b,a44a,a44b,a44c,a44d,a44e,a44f,
        a44g,a44h,a44i,a44j,a44k,a44l,a44m,a44n,a44o,a45a,a45b,
        a45c,a45d,a45e,a45f,a46a,a46b)
act=data.frame(a[,1],a[,3],a[,5],a[,7],a[,9],a[,11],a[,13],a[,15],
               a[,17],a[,19],a[,21], a[,23],a[,25],a[,27],a[,29],
               a[,31],a[,33],a[,35],a[,37],a[,39],a[,41],a[,43],
               a[,45],a[,47],a[,49],a[51],a[,53],a[,55])+matrix(10,10,28)


write.csv(act,file="data2.csv")

# rm(list=ls()) # borra todo los objetos actuales

# remove(a42a,a42b,a42c,a43a,a43b,a44a,a44b,a44c,a44d,a44e,a44f,a44g,a44h,a44i,
#       a44j,a44k,a44l,a44m,a44n,a44o,a45a,a45b,a45c,a45d,a45e,a45f,a46a,
#       a46b)
# remove(i15a,i15b,i15c,ia,ib,ic,input1,n,n2,n3,n4,n71,o15a,o15b,o15c, oa,ob,oc,
#       output1,t7,ta)
#  remove(a,p01)
 
 #-----------------------------------------------------------------------------
 
 data_example <- read_data(Coll_Blasco_2006,
                           inputs = 2:3, 
                           outputs = 4:5, 
                           nd_inputs = 2, 
                           ud_outputs = 2)
 
 
 
 
 