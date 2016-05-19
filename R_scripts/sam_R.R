### Statistical analysis of microarray
## load package

## try http:// if https:// URLs are not supported
source("https://bioconductor.org/biocLite.R")
biocLite("impute")


install.packages("samr")
library(samr)

### create data matrix
dat<-read.csv('acuteObesity_cytof.csv', header = TRUE, row.names = 1)

### create variable y for paired and for unpaired
y = c(-1,1,-2,2,-3,3,-4,4,-5,5,-6,6,-7,7,-8,8)
y = c(rep(1,8), rep(2,8))

### create x
x = t(dat[1:16,])

### create list for SAM
d <- list(x=x, y=y, geneid=row.names(x), genenames=paste("g", as.character(1:nrow(x)),sep=""), logged2=TRUE)

### run SAM
samr.obj<-samr(d,  resp.type="Two class paired", nperms=1000)

### set delta, plot and get delta table 
delta=.4
samr.plot(samr.obj,delta)
delta.table <- samr.compute.delta.table(samr.obj)

### create significant genes table
siggenes.table<-samr.compute.siggenes.table(samr.obj,delta, d, delta.table)
siggenes.table

### plot FDR as a funcion of score d and add dashed line cutoff at 10% FDR
plot(siggenes.table$genes.lo[,8]~siggenes.table$genes.lo[,4], xlab = "Score(d)", ylab = "FDR")
abline(h=10,col=4,lty=2)

### write table
write.table(siggenes.table$genes.lo, "acuteCytofSAM.txt", sep = ',')

### save data
save.image('acuteCytof.RData')