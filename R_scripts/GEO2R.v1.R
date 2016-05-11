#########################################################
##
## This script is intended to teach the basics of GEO2R
## It uses the GEOquery Package from Bioconductor to
## download some expression data and then heatmap.2 from
## the gplots package to plot a simple heatmap.
##
#########################################################

# Setup environment
rm(list=ls())
setwd("~/Dropbox/R course/WD/")
## install the dependencies (required packages)
### CRAN
required.packages <- c("gplots")
missing.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(missing.packages)) install.packages(missing.packages)
### biocLite
source("http://bioconductor.org/biocLite.R")
required.biocLite.packages <- c("GEOquery")
missing.biocLite.packages <- required.biocLite.packages[!(required.biocLite.packages %in% installed.packages()[,"Package"])]
if(length(missing.biocLite.packages)) biocLite(missing.biocLite.packages)
## load packages into the memory
lapply(required.packages, library, character.only = TRUE)
lapply(required.biocLite.packages, library, character.only = TRUE)

# Download the GEO file and load in R
GSE5327.file <- getGEOfile("GSE5327",destdir = getwd())
GSE5327 <- getGEO(filename = GSE5327.file,GSEMatrix=TRUE)
# alternatively you can do both steps using : getGEO("GSE5327",GSEMatrix=TRUE)

#sample names
names(GSMList(GSE5327))
#platforms used in this GSE
names(GPLList(GSE5327))
GSM.platforms <- lapply(GSMList(GSE5327),function(x) {Meta(x)$platform}) 
data.frame(GSM.platforms)

#example of an GSM experession vector 
Table(GSMList(GSE5327)[[1]])[1:100,]
#example of gene anotation data from GPL of GSM 1
Probe.anotaion.table <- Table(GPLList(GSE5327)[[1]])[,c(1,2,4,11,12,10)]
#Probeset extrated from GPL of GSM 1 
probesets <- as.character(Probe.anotaion.table$ID)
#creating the expression matrix ordered by the GPL order of probes
data.matrix <- do.call('cbind',lapply(GSMList(GSE5327),function(x) {
  tab <- Table(x)
  mymatch <- match(probesets,tab$ID_REF)
  return(tab$VALUE[mymatch])
  }))
data.matrix <- apply(data.matrix,2,function(x) {as.numeric(as.character(x))})
# log2 trnasform the data
data.matrix <- log2(data.matrix)
# Add the probe names as row names
rownames(data.matrix) <- probesets
data.matrix[1:5,]

#save to text file
write.csv (data.matrix,file="./GSE5327.epxr.matrix.txt")
write.csv (Probe.anotaion.table,file = "./GSE5327.probe.annot.table.txt")

#select some genes
gene.set <- c("MMP2","CTLA4","TP53")
#translte to probe ID
gene.set.probes <- Probe.anotaion.table[Probe.anotaion.table$`Gene Symbol` %in% gene.set,c("ID","Gene Symbol")]

#select the corresponding expression data
heatmap.data <- data.matrix[gene.set.probes$ID,]
#Prepare labels for the heatmap
heatmap.probe.labels <- paste0(gene.set.probes$ID," (",gene.set.probes$`Gene Symbol`,")")

#basic heatmap                               
dev.new()
heatmap.2(heatmap.data)

#nice heatmap
dev.new()
my.palette <- colorRampPalette(c("blue", "pink", "orange"))(n = 297)
heatmap.2(heatmap.data,
          main = "Heatmap of 3 random genes",
          col=my.palette,                   #set color sheme 
          key=TRUE,
          symm=FALSE,
          symkey=FALSE,
          symbreaks=TRUE,             
          scale="row",
          density.info="none",
          trace="none",
          labRow=heatmap.probe.labels,
          cexRow=0.75,cexCol=0.8,margins=c(7,7),
          Colv=TRUE,
          Rowv=FALSE,
          dendrogram = "col")

#Anotation data

#test R git commit



