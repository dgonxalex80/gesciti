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

a=cbind(a42a,a42b,a42c,
          a43a,a43b,
          a44a,a44b,a44c,a44d,a44e,a44f,a44g,a44h,a44i,a44j,a44k,a44l,a44m,a44n,a44o,
          a45a,a45b,a45c,a45d,a45e,a45f,
          a46a,a46b)
act=data.frame(a[,1],a[,3],a[,5],
      a[,7],a[,9],
      a[,11],a[,13],a[,15],a[,17],a[,19],a[,21], a[,23],a[,25],a[,27],a[,29],
      a[,31],a[,33],a[,35],a[,37],a[,39],
      a[,41],a[,43],a[,45],a[,47],a[,49],a[51],
      a[,53],a[,55])+matrix(10,10,28)

act=as.matrix(act)
colnames(act)=c("A2a","A2b","A2c",
                "A3a","A3b",
                "A4a","A4b","A4c","A4d","A4e","A4f",
                "A4g","A4h","A4i","A4j","A4k","A4l",
                "A4m","A4n","A4o",
                "A5a","A5b","A5c","A5d","A5e","A5f",
                "A6a","A6b")
rownames(act)=c("I1","I2","I3","I4","I5","I6","I7",
                "I8","I9","I10")

# act
#------------------------------------------------------------------------------
chisq.test(act)

library(RColorBrewer)
mosaicplot((act),las=2, col=brewer.pal(20,"Paired"), main="")
#------------------------------------------------------------------------------

x<-act
total <- sum(x)
f.marginal <- colSums(x)/total
c.marginal <- rowSums(x)/total
f.perfil <- rbind(prop.table(x, 1), marg = f.marginal)
c.perfil <- cbind(prop.table(x, 2), marg = c.marginal)
f.perfil

#------------------------------------------------------------------------------
# install.packages("factoextra", dependencies = TRUE)
# install.packages("FactoMineR", dependencies = TRUE)
library("FactoMineR")
library("factoextra")
mm<-x 
#  as.table(mm)
res.ca <- CA(mm, graph = FALSE)

eig.val <- get_eigenvalue(res.ca)
eig.val
png("fig-analisis-correspondencia1.png")
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 70))
dev.off()

png("fig-analisis-correspondencia2.png")
fviz_ca_biplot(res.ca, repel = TRUE)
dev.off()
#------------------------------------------------------------------------------
fviz_ca_biplot(res.ca, map ="colgreen", arrow = c(TRUE, FALSE),
               repel = TRUE)
#------------------------------------------------------------------------------
install.packages("corrplo", dependencies = TRUE)
library("corrplot")
row <- get_ca_row(res.ca)
corrplot(row$cos2, is.corr=FALSE)
#------------------------------------------------------------------------------
png("fig-analisis-correspondencia3.png")
fviz_ca_row(res.ca, col.row = "cos2",
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
            repel = TRUE)
dev.off()
png("fig-analisis-correspondencia4.png")
fviz_ca_col(res.ca, col.col = "cos2", 
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
            repel = TRUE)
dev.off()

