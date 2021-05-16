## ----class.source = 'fold-hide', setup, include=FALSE--------------------------------------------------------------------------------------------
library(knitr)
library(rmdformats)

## Global options
options(max.print="75")
opts_chunk$set(echo=FALSE,
	             cache=TRUE,
               prompt=FALSE,
               tidy=TRUE,
               comment=NA,
               message=FALSE,
               warning=FALSE)
opts_knit$set(width=75)


## ---- librerias, echo=TRUE-----------------------------------------------------------------------------------------------------------------------
installifnot <- function (pckgName){
if(!(require(pckgName, character.only=TRUE))){
  BiocManager::install(pckgName)
  }
}
installifnot("Biobase")
installifnot("hgu133plus2.db")
installifnot("affy")
installifnot("arrayQualityMetrics")
installifnot("genefilter")
installifnot("limma")
installifnot("annotate")
installifnot("hwriter")
installifnot("gplots")
installifnot("GOstats")


## ----class.source = 'fold-hide'------------------------------------------------------------------------------------------------------------------
listaArchivos <- list.files("datos")
nombres<-substr(listaArchivos,1,10)
targets0<- read.csv("targetsAll.csv", row.names =1 )
if (sum(nombres==rownames(targets0)))
  targets<- cbind(filenames=listaArchivos,targets0)


## ----class.source = 'fold-hide', echo=TRUE-------------------------------------------------------------------------------------------------------
selectSamples<- function (myID){
  set.seed(myID)
  selected <- c(sample(1:10, 6),11, sample(12:26, 5), sample(27:36,6))
  return(sort(selected))
}


## ------------------------------------------------------------------------------------------------------------------------------------------------
mySelected <- selectSamples(1234567)
selectedTargets <- targets[mySelected,]
table(selectedTargets$karyotype)


## ---- phenoData1---------------------------------------------------------------------------------------------------------------------------------
require(Biobase)
sampleInfo <-AnnotatedDataFrame(selectedTargets) 
show(pData(sampleInfo))


## ------------------------------------------------------------------------------------------------------------------------------------------------
library(affy)
fileNames <- paste0("datos/",pData(sampleInfo)$filenames)
rawData <- read.affybatch(filenames=fileNames,phenoData=sampleInfo)
show(rawData)


## ---- etiquetas----------------------------------------------------------------------------------------------------------------------------------
## ----preajustes
colores <- c(rep("yellow", 6), rep("blue", 6), rep("red", 6))
formas <-  c(rep(11, 6), rep(12, 6), rep(13, 6))
grupos <- as.factor(pData(rawData)$karyotype)
numSamples <- nrow(pData(rawData))
sampleNames <-pData(rawData)$title
colnames(exprs(rawData))<-sampleNames


## ---- explora1-----------------------------------------------------------------------------------------------------------------------------------
## ----plotHist
hist(rawData, main="Signal distribution", col=colores, lty=1:numSamples)
legend (x="topright", legend=sampleNames , col=colores, lty=1:numSamples, cex=0.6)

## ----boxplot
boxplot(rawData, cex.axis=0.6, col=colores, las=2, names=sampleNames, 
        main="Signal distribution for selected chips")


## ---- plotPCA------------------------------------------------------------------------------------------------------------------------------------
## ----plotPCA
library(ggplot2)
library(ggrepel)
plotPCA3 <- function (datos, labels, factor, title, scale,colores, size = 1.5, glineas = 0.25) {
  data <- prcomp(t(datos),scale=scale)
  # plot adjustments
  dataDf <- data.frame(data$x)
  Group <- factor
  loads <- round(data$sdev^2/sum(data$sdev^2)*100,1)
  # main plot
  p1 <- ggplot(dataDf,aes(x=PC1, y=PC2)) +
    theme_classic() +
    geom_hline(yintercept = 0, color = "gray70") +
    geom_vline(xintercept = 0, color = "gray70") +
    geom_point(aes(color = Group), alpha = 0.55, size = 3) +
    coord_cartesian(xlim = c(min(data$x[,1])-5,max(data$x[,1])+5)) +
    scale_fill_discrete(name = "Group")
  # avoiding labels superposition
  p1 + geom_text_repel(aes(y = PC2 + 0.25, label = labels),segment.size = 0.25, size = size) + 
    labs(x = c(paste("PC1",loads[1],"%")),y=c(paste("PC2",loads[2],"%"))) +  
    ggtitle(paste("Principal Component Analysis for: ",title,sep=" "))+ 
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_manual(values=colores)
  }


## ---- explora2-----------------------------------------------------------------------------------------------------------------------------------
plotPCA3(exprs(rawData), labels=sampleNames, size=2, factor =grupos, 
         olores = c("red", "blue", "green"), title="selected samples", scale=TRUE)


## ---- explora3-----------------------------------------------------------------------------------------------------------------------------------
clust.euclid.average <- hclust(dist(t(exprs(rawData))),method="average")
plot(clust.euclid.average, labels=sampleNames, main="Hierarchical clustering of samples",  hang=-1)


## ---- arrayQM, cache=TRUE, eval=FALSE------------------------------------------------------------------------------------------------------------
## library(arrayQualityMetrics)
## arrayQualityMetrics(rawData, outdir = "arrayQuality",intgroup= "karyotype", force=TRUE)


## ------------------------------------------------------------------------------------------------------------------------------------------------
library(affyio)
HybDates<- as.factor(get.celfile.dates(fileNames))
show(HybDates)
pData(rawData) <- cbind(pData(rawData), HybDates)
with(pData(rawData), table(HybDates, karyotype))


## ----class.source = 'fold-hide', explora2Bis-----------------------------------------------------------------------------------------------------
plotPCA3(exprs(rawData), labels=sampleNames, size=2, factor=HybDates, colores = c("red", "blue", "green", "grey", "black"), title="selected samples", scale=TRUE)


## ---- explora4-----------------------------------------------------------------------------------------------------------------------------------
plot(clust.euclid.average, labels=HybDates, main="Hierarchical clustering of samples",  hang=-1)


## ----class.source = 'fold-hide', normalizacion---------------------------------------------------------------------------------------------------
library(affy)
eset_rma <- rma(rawData)    
eset_rma


## ---- echo=TRUE----------------------------------------------------------------------------------------------------------------------------------
show(annotation(eset_rma))


## ----class.source = 'fold-hide', filtraje--------------------------------------------------------------------------------------------------------
library(genefilter)
filtered <- nsFilter(eset_rma, require.entrez=TRUE,
         remove.dupEntrez=TRUE, var.func=IQR,
         var.cutoff=0.5, var.filter=TRUE,
         filterByQuantile=TRUE, feature.exclude="^AFFX")


## ---- echo=TRUE----------------------------------------------------------------------------------------------------------------------------------
names(filtered)
class(filtered$eset)
print(filtered$filter.log)
eset_filtered <-filtered$eset


## ----class.source = 'fold-hide', saveData--------------------------------------------------------------------------------------------------------
save(eset_rma, eset_filtered, file=paste0("results/","datos.normalizados.Rda"))


## ----class.source = 'fold-hide', matDesign1, echo=TRUE-------------------------------------------------------------------------------------------
design1<-matrix(
            c(1,1,1,1,1,1, 0,0,0,0,0,0, 0,0,0,0,0,0,
              0,0,0,0,0,0, 1,1,1,1,1,1, 0,0,0,0,0,0,
              0,0,0,0,0,0, 0,0,0,0,0,0, 1,1,1,1,1,1),
            nrow=18,byrow=F)
colnames(design1)<-c("XX", "Xm", "Xp")
rownames(design1) <-  sampleNames 
print(design1)


## ----class.source = 'fold-hide', matDesign1b-----------------------------------------------------------------------------------------------------
design1b <- model.matrix(~ 0+grupos)
colnames(design1b)<-c("Xm", "Xp", "XX")
rownames(design1b) <-  sampleNames 
print(design1b)


## ----class.source = 'fold-hide', contrastes, echo=TRUE-------------------------------------------------------------------------------------------
library(limma)
cont.matrix <- makeContrasts (
      XvsM = Xm-XX,
      XvsP = Xp-XX,
      MvsP = Xp-Xm,
      levels=design1)
print(cont.matrix)


## ----class.source = 'fold-hide', linearmodelfit--------------------------------------------------------------------------------------------------
library(limma)
fit<-lmFit(eset_filtered, design1)
fit.main<-contrasts.fit(fit, cont.matrix)
fit.main<-eBayes(fit.main)


## ----class.source = 'fold-hide', topTabs1--------------------------------------------------------------------------------------------------------
topTab_XvsM <- topTable (fit.main, number=nrow(fit.main), coef="XvsM", adjust="fdr"); head(topTab_XvsM)
topTab_XvsP <- topTable (fit.main, number=nrow(fit.main), coef="XvsP", adjust="fdr"); head(topTab_XvsP)
topTab_MvsP  <- topTable (fit.main, number=nrow(fit.main) , coef="MvsP", adjust="fdr"); head(topTab_MvsP)


## ----class.source = 'fold-hide', volcano---------------------------------------------------------------------------------------------------------
library(annotate)
probeNames <-rownames(fit.main)
Symbols <- getSYMBOL(probeNames, annotation(eset_rma))
myNames <- paste(probeNames, Symbols, sep=".")
head(myNames)
volcanoplot(fit.main, coef="XvsM", highlight=10, names=Symbols)


## ----class.source = 'fold-hide', decideTests-----------------------------------------------------------------------------------------------------
res<-decideTests(fit.main, method="separate", adjust.method="fdr", p.value=0.05, lfc=0)


## ----class.source = 'fold-hide', resumeDecideTests, eval=TRUE------------------------------------------------------------------------------------
sum.res.rows<-apply(abs(res),1,sum)
res.selected<-res[sum.res.rows!=0,] 
print(summary(res))


## ----class.source = 'fold-hide',   fig.cap="Número de genes seleccionado en cada comparacion"----------------------------------------------------
vennDiagram (res.selected[,1:3], main="Genes in common #1", cex=0.9)


## ----class.source = 'fold-hide', prepareData-----------------------------------------------------------------------------------------------------
probeNames<-rownames(res)
probeNames.selected<-probeNames[sum.res.rows!=0]
exprs2cluster <-exprs(eset_filtered)[probeNames.selected,]
colnames(exprs2cluster)<-sampleNames
color.map <- function(grupo) { 
  if (grupo=="46XX"){
    c<- "yellow" 
  }else{ 
    if (grupo=="45Xm"){
      c<- "red"
    }else{
      c<- "blue"
   }
  }
return(c)}


## ----class.source = 'fold-hide', plotHeatMap1, fig.cap="Mapa de colores basado en los genes seleccionados por estar diferencialmente expresados. Como puede verse los tumores Apocrinos y Luminales tienen perfiles de expresión más parecidos entre ellos que cada uno con los de tipo Basal"----
grupColors <- unlist(lapply(pData(eset_filtered)$karyotype, color.map))
heatmap(exprs2cluster, col=rainbow(100), ColSideColors=grupColors, cexCol=0.9)


## ----class.source = 'fold-hide', ORA, eval=TRUE--------------------------------------------------------------------------------------------------
library(GOstats)
# Seleccionamos la "topTable"
topTab <- topTab_XvsM
# Definimos el universo de genes: todos los que se han incluido en el análisis
# EL programa trabaja con identificadores "entrez" y no admite duplicados
entrezUniverse = unique(getEG(rownames(topTab), "hgu133plus2.db"))
  
# Escogemos los grupos de sondas a incluir en el análisis
# Este análisis trabaja bien con varios centenares de genes 
# por lo que es habitual basarse en p-valores sin ajustar para incluirlos

whichGenes<-topTab["adj.P.Val"]<0.05
geneIds <-   unique(getEG(rownames(topTab)[whichGenes],"hgu133plus2.db"))
  
# Creamos los "hiperparámetros" en que se basa el análisis
GOparams = new("GOHyperGParams",
    geneIds=geneIds, universeGeneIds=entrezUniverse,
    annotation="org.Hs.eg.db", ontology="BP",
    pvalueCutoff=0.001, conditional=FALSE,
    testDirection="over")

# Ejecutamos los análisis

GOhyper = hyperGTest(GOparams)

# Creamos un informe html con los resultados
comparison <-"XXvsXM"
GOfilename <- paste0("GOResults.", comparison,".html")
htmlReport(GOhyper, file = GOfilename, summary.args=list("htmlLinks"=TRUE))


## ----insertaCodigo,  eval=FALSE, echo=TRUE-------------------------------------------------------------------------------------------------------
## 

