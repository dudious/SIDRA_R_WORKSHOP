###	regressNPermuteFast	###
# run linear regression with permutation on the outcomes
# inputs can be a vector or a matrix
#	predData - matrix of factors (rows = patients, cols = features/predictors)
#	y - response variable (rows = patients, cols = measurements)
#	numPerms - variable number of permutations to run
#	measName - useful for response vector (Yout is a dummy name)
#	chkEff - log timing information on how long it takes code to run
#	dbg - return debugging parameters
#	fullOut - print out information on where we are in code
# returns list of statistical parameters
regressNPermuteFast <- function( predData,y,numPerms=200,measName="Yout",chkEff=FALSE,dbg=FALSE,fullOut=FALSE,estPLM=FALSE,FAST=TRUE ) {
	if(chkEff) {
		beg<-date()	# for checking the efficiency of this code
	}
	# determine whether y is a vector or a matrix
	if( length(dim(y)) < 1 ) {
		numMeasures<-1
		numSamples<-length(y)
		namMeas<-measName
		namSamp<-names(y)
	} else {
		numMeasures<-dim(y)[2]
		numSamples<-dim(y)[1]
		namMeas<-dimnames(y)[[2]]
		namSamp<-dimnames(y)[[1]]
	}
	numCoef<-ncol(predData)+1	# each predictor in its own column + an intercept
	print(c(numMeasures,numSamples,numCoef))
	# initialize lists: 1st is the betas based on permutation
	rndCoef<-list()
	for( i in 1:numCoef ) {
		if( i == 1 ) {
			rndCoef[['intercept']]<-array(0,dim=c(numMeasures,numPerms))
			rownames(rndCoef[['intercept']])<-namMeas
		} else {
			rndCoef[[(dimnames(predData)[[2]])[i-1]]]<-array(0,dim=c(numMeasures,numPerms))
			rownames(rndCoef[[(dimnames(predData)[[2]])[i-1]]])<-namMeas
		}
	}
	# create list of stats from regression analysis
	r <- list( 	'beta' = array( 0, dim=c(numMeasures,numCoef) ), 
			'bConInt' = array( 0, dim=c(numMeasures,2) ), 
			'res' = array( 0, dim=c(numMeasures,numSamples) ),
			'rsq' = 0, 'fstat' = 0, 'pval' = 0, 
			'pv' = array( 0, dim=c(numMeasures,numCoef) )	)
	#
	# set random number generator (Mersenne-Twister)
	set.seed( 12345, kind = "Mersenne-Twister" )
	if( fullOut ) {
		print( "starting perms", quote=F )
	}
	rV<-array(0,dim=c(numPerms,numSamples))		# debugging
	if(estPLM) {
		pLM<-array(1,dim=c(numCoef,numPerms))		# get p-values of random models (mostly debugging -- slow)
	}
	for( p in 1:numPerms ) {
		rOrder <- sample( numSamples )
		rV[p,]<-rOrder				# debugging
		fits <- lm( y ~ predData[rOrder,] )	# shuffle data to create null dist
		#pLM[,p]<-lapply( summary( fits ), function( resp ) {
		#	fstat <- resp$fstatistic
		#	pval<-pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE)
		#	print(length(as.numeric(pval)))
		#	return( t(as.numeric(pval)) )
		#	} )
		# can this be sped up with apply?
		for( i in 1:numCoef ) {
			if(estPLM) {
				fstat<-summary(fits)[[i]]$fstatistic
				pLM[i,p]<-pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE)
			}
			if( numMeasures==1 ) {
				rndCoef[[i]][,p]   <- fits$coefficients[i]
			} else {
				rndCoef[[i]][,p]   <- fits$coefficients[i,]
			}
		}
		if( fullOut ) {
			if( p %% 10 == 0 ) { print( paste( "perms =", p ), quote=F ) }
		}
	}
	#return( list( "fits"=fits,"r"=r ) )
	if( fullOut ) {
		print( "starting meas", quote=F )
	}
	fits <- lm( y ~ predData )	# run regression
	# estimate probability that model is better than the null model
	for( coef in 1:numCoef ) {
		#r$bConInt[,] <- confint( fits )[coef,]	# still need to figure out confidence intervals
		if( numMeasures==1 ) {
			r$beta[,coef] <- fits$coefficients[coef]
			tmpA <- array( abs(fits$coefficients[coef]), dim=c(numMeasures,numPerms) )
			r$pv[,coef]   <- 1-(apply(tmpA >= abs( rndCoef[[coef]][,] ),1,sum) / numPerms)
		} else {
			r$beta[,coef] <- fits$coefficients[coef,]
			tmpA <- array( abs(fits$coefficients[coef,]), dim=c(numMeasures,numPerms) )
			r$pv[,coef]   <- 1-(apply(tmpA >= abs( rndCoef[[coef]][,] ),1,sum) / numPerms)
		}
	}
	r$res <- t( fits$residuals )
	if( numMeasures > 10000 ) {
		time_keeper <- 500
	} else if( numMeasures > 1000 ) {
		time_keeper <- 100
	} else { time_keeper <- 50 }
	if( fullOut ) {
		print( "big meas loop", quote=F )
	}
	if( numMeasures==1 ) {
		r$rsq <- summary( fits )$r.squared
		r$fstat <- summary( fits )$fstatistic[1]
		fstat <- summary( fits )$fstatistic
		r$pval <- pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE)
	} else {
		if(FAST) {
			stats   <- lapply( summary( fits ), function( resp ) {
					# report vector only
					rsq   <- resp$r.squared
					fstat <- resp$fstatistic
					pval  <- pf( fstat[1], fstat[2], fstat[3], lower.tail=FALSE )
					return( list( "rsq" = rsq, "fstat" = fstat[1], "pval" = pval ) )
				} )
			r$rsq   <- as.numeric( lapply( stats, function( resp ) { return( resp$rsq ) } ) )
			r$fstat <- as.numeric( lapply( stats, function( resp ) { return( resp$fstat ) } ) )
			r$pval  <- as.numeric( lapply( stats, function( resp ) { return( resp$pval ) } ) )
		} else {
			for( i in 1:numMeasures ) {
				r$rsq[i] <- summary( fits )[[i]]$r.squared
				r$fstat[i] <- summary( fits )[[i]]$fstatistic[1]
				fstat <- summary( fits )[[i]]$fstatistic
				r$pval[i] <- pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE)
				if( i %% time_keeper == 0 ) { print( paste( "meas =", i ) ) }
			}
		}
	}
	if( fullOut ) {
		print( "end of meas loop", quote=F )
	}
	# create summary stats, which includes information on regression model
	r$sumStats<-cbind(r$fstat,r$pval,r$rsq)
	# add labels to stats
	rownames(r$beta)<-namMeas;	colnames(r$beta)<-names(rndCoef)
	rownames(r$res)<-namMeas;	colnames(r$res)<-namSamp
	rownames(r$pv)<-namMeas;	colnames(r$pv)<-names(rndCoef)
	rownames(r$sumStats)<-namMeas;	colnames(r$sumStats)<-c("fstat","pval","rsq")
	#
	if(chkEff) {
		print( paste( "dt START:", beg, "STOP:", date() ), quote=F )
	}
	if(dbg) {
		return( list( "fits"=fits,"r"=r,"rndCoef"=rndCoef,"pLM"=pLM,"rV"=rV ) )
	} else if(estPLM) {
		return( list( "fits"=fits,"r"=r,"rndCoef"=rndCoef,"pLM" =pLM  ) )
	} else {
		return( list( "fits"=fits,"r"=r,"rndCoef"=rndCoef  ) )
	}
}
