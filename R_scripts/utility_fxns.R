#
plot_fdr<-function( qv, tag, cmd_line=TRUE ) {
	x<-1:length(qv)
	y<-sort(qv)
	xlab<-"# conditions"
	ylab<-"FDR"
	epsfile<-paste('fdr_plot_',tag,'.eps',sep='')
	if(cmd_line) {
		X11()
	}
	plot( x,y,type='l',las=1,xlab=xlab,ylab=ylab,main=tag,ylim=c(0,1))
	dev.copy2eps(file=epsfile)
	if(cmd_line) {
		dev.off()
	}
}
###	std_data	###
# standardize (mean centering and normalization by stdev) data 
# from a matrix or a vector
# d - input matrix or vector
# sc - scaling factor for how many stdev to normalize by
# way - row (1) or col (2) standardization for matrices
std_data<-function( d, sc=1, way=1 ) {
	if( is.array(d) ) {
		return( t(apply(d,way,function(x){ return((x-mean(x))/(sc*sd(x)))})) )
	} else if( is.vector(d) & length(d) > 1 ) {
		return( (d-mean(d))/(sc*sd(d)) )
	} else {
		print( "Unknown input" )
	}
}
