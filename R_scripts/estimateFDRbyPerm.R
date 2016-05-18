###	fdrByPerm	###
#Function computes q-values by analzying p-values estimated from permuted data.
# realPv is a matrix of the p-value of each factor's coefficient in the regression.
# LMcoef is a list of matrices (numMeasurents x numPermutations) where the number of
#	items in the list is numCoefficents. the list stores the actual coefficent
#	values from the regression analysis of a permuted dataset.
# thres is used to specify a fdr threshold vector. There is a default defined that
#	corresponds to the p-value.
# Note that for qvalues to be accurate the threshold vector needs to go from least 
#	specific (1 or bad pv) to very specific (good pv).
DefThes<-c(seq(1,0.2,-0.1),seq(0.2,0.051,-0.01),seq(0.05,0,-0.001))
fdrByPerm<-function( realPv,LMcoef,thres=DefThres ) {
	numPerms<-dim(LMcoef[[1]])[2]
	numMeas<-dim(LMcoef[[1]])[1]	# doesn't matter which factor we select for ref
	numCoef<-length(names(LMcoef))
	# calc pVals for rand data
	# Transform coef into a matrix of p-values by sequentially 
	# treating permuted datasets as real.
	rndPv<-LMcoef
	print(c("start rndPv loop",date()), quote=F)
	for( coef in names(LMcoef) ) {
		for( p in 1:numPerms ) {
			virtualReal<-LMcoef[[coef]][,p]
			rndCoef<-LMcoef[[coef]][,which(1:numPerms!=p)]
			rndPv[[coef]][,p]<-1-apply(abs(virtualReal) >= abs(rndCoef),1,sum)/(numPerms-1)
		}
	}
	print(c("finish rndPv loop",date()), quote=F)

#	#calc q-value by asking for a given threshold what's the probability of
#	#observing that many hits in a random dataset.
	qv<-array(1,dim=c(numMeas,numCoef))
	numCall<-array(0,dim=c(length(thres),numCoef))
	numFalse<-list()
	fdr<-array(0,dim=c(length(thres),numCoef))
	#
	rownames(numCall)<-paste('f',thres,sep="")
	colnames(numCall)<-names(LMcoef)
	rownames(fdr)<-paste('f',thres,sep="")
	colnames(fdr)<-names(LMcoef)
	rownames(qv)<-rownames(LMcoef[[1]])
	colnames(qv)<-names(LMcoef)
	print(c("start fdr loop",date()), quote=F)
	for( coef in names(LMcoef) ) {
		ic<-which(coef==names(LMcoef))
		numFalse[[coef]]<-array(0,dim=c(length(thres),numPerms))
		rownames(numFalse[[coef]])<-paste('f',thres,sep="")
		for( v in 1:length(thres) ) {
			atThres<-abs(realPv) <= thres[v]
			numCall[v,]<-apply(atThres,2,sum)
			numFalse[[coef]][v,]<-apply(abs(rndPv[[coef]]) <= array(thres[v],dim=c(numMeas,numPerms)),2,sum)
			if( numCall[v,coef] > 0 ) {
				if( mean(numFalse[[coef]][v,]) / numCall[v,coef] > 1 ) {
					fdr[v,coef]<-1
				} else {
					fdr[v,coef]<-mean(numFalse[[coef]][v,]) / numCall[v,coef]
				}
			} else {
				fdr[v,coef]<-NA
			}
			if( numCoef > 1 ) {
				qv[which(atThres[,ic]),ic]<-fdr[v,coef]
			} else {
				qv[which(atThres),ic]<-fdr[v,coef]
			}
		}
	}
	mfdr<-fdr
	for( j in 2:nrow(fdr) ) {
		for( coef in 1:numCoef ) {
			mfdr[j,coef]<-max(fdr[j-1,coef],fdr[j,coef])
		}
	}
	print(c("finish fdr loop",date()), quote=F)
	return( list( 'rndPv'=rndPv,'numCall'=numCall,'numFalse'=numFalse,'mfdr'=mfdr,'qv'=qv,'fdr'=fdr ) )
}	
