# Copyright (C) 2012 Joel Granados <joel.granados@gmail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

source("common.R")
if ( !exists("binGetFuns") )
    source("colorTrans.R")

# Calc the Naive Bayesian element. In P(a|b) = (prod(P(b|a))*p(a))/p(b) we are
# calculating P(b|a).
# FIXME: We should implement pass by reference when possible
calcNaiveBayesElem <- function(colMat, bins)
{
    if ( dim(colMat)[2] != dim(bins)[2] )
        stop ("The 2nd dim of data must equal 2nd dim of bins")

    histlist = list()
    colMat = as.matrix(colMat)

    for (i in 1:dim(colMat)[2])
        histlist[[i]] = hist(colMat[,i], bins[,i], plot=FALSE)

    if ( length(histlist) == 0 )
        stop("Could not histlist")

    return (histlist)
}

# Create a Discrete Naive Bayesian Model
create.DiscNaiveBayesianModel <- function(env, bins)
{
    isParamInEnv(c("classes", "dataPoints"), env)
    if ( !is.matrix(env$dataPoints) )
        stop ( "The dataPoints argument must be a matrix" )
    if ( !is.vector(env$classes) )
        stop ( "The classes argument must be a boolean vector" )
    if ( length(env$classes) != dim(env$dataPoints)[1] )
        stop("Classes length must be equal to first dim dataPoints")
    if ( sum(env$classes) == 0 || sum(!env$classes) == 0 )
        stop ("Must include data for two classes")
    if ( class(env$classes) != "logical")
        stop ("Classes must be a logical vector")
    if ( dim(env$dataPoints)[2] != dim(bins)[2] )
        stop ("The 2nd dim of data must equal 2nd dim of bins")

    NBM = list() #Naive Bayesian Model (NBM)
    NBM$bins = bins
    NBM$cls1Hists = calcNaiveBayesElem(env$dataPoints[env$classes,],bins)
    NBM$cls0Hists = calcNaiveBayesElem(env$dataPoints[!env$classes,],bins)

    NBM$freq1 = sum(env$classes)/length(env$classes)
    NBM$freq0 = sum(!env$classes)/length(env$classes)

    NBM$dimension = dim(env$dataPoints)[2]

    # cross validation puts error here.
    NBM$error = NA

    # This allows a general calcMask function
    NBM$classifyFunc = classify.DiscNaiveBayesianModel

    return (NBM)
}

# Classify with discrete Naive Bayesian Model
classify.DiscNaiveBayesianModel <- function(NBM, env)
{
    isParamInEnv(c("img"), env)
    if ( !is.DiscNaiveBayesianModel(NBM) )
        stop("The NBM object is not a Naive Bayesian Model Object")
    if ( dim(env$img)[2] != NBM$dimension )
        stop("The dimensions of data and model should be thesame")

    # Fit the raw data into the bins.
    for (i in 1:dim(env$img)[2])
        env$img[,i] = findInterval(env$img[,i] , NBM$bins[,i], all.inside=TRUE)

    # OneZero[,1] -> One probabilities | OneZero[,2] -> Zero Probabilities.
    OneZero = matrix( rep(1,dim(env$img)[1]*2),
                      ncol=2, nrow=dim(env$img)[1] )

    # Calculate the One probabilities.
    for (i in 1:dim(env$img)[2])
        OneZero[,1] = OneZero[,1] * NBM$cls1Hists[[i]]$density[env$img[,i]]
    OneZero[,1] = OneZero[,1] * NBM$freq1

    # Calculate the Zero probabilities.
    for (i in 1:dim(env$img)[2])
        OneZero[,2] = OneZero[,2] * NBM$cls0Hists[[i]]$density[env$img[,i]]
    OneZero[,2] = OneZero[,2] * NBM$freq0

    # Return the classification.
    return(OneZero[,1] > OneZero[,2])
}

# This function is based on the method described in Pattern Recognition and
# Machine Learning by Christopher M. Bishop (page 33)
crossVal.DiscNaiveBayesianModel <- function(env, numBins, numFold, transform="-")
{
    if ( !is.matrix(env$dataPoints) )
        stop ( "The dataPoints argument must be a matrix" )
    if ( !is.vector(env$classes) )
        stop ( "The classes argument must be a boolean vector" )
    if ( length(env$classes) != dim(env$dataPoints)[1] )
        stop("Classes length must be equal to first dim dataPoints")
    if ( sum(env$classes) == 0 || sum(!env$classes) == 0 )
        stop ("Must include data for two classes")
    if ( class(env$classes) != "logical")
        stop ("Classes must be a logical vector")
    if ( (!transform %in% names(colorSpaceFuns))
         || (!transform %in% names(binGetFuns)) )
        stop ( "The transform string is not defined" )

    cls1 = env$dataPoints[env$classes,]
    cls0 = env$dataPoints[!env$classes,]

    cls1Ranges = floor( seq(0,dim(cls1)[1],dim(cls1)[1]/numFold) )
    cls0Ranges = floor( seq(0,dim(cls0)[1],dim(cls0)[1]/numFold) )

    finalError = c()

    for ( i in 1:(length(cls1Ranges)-1) ) # len(cls1Ranges) == len(cls0Ranges)
    {
        # Create Model
        data1 = cls1[ -((cls1Ranges[i]+1):cls1Ranges[i+1]), ]
        data0 = cls0[ -((cls0Ranges[i]+1):cls0Ranges[i+1]), ]

        dataTotal = new.env(parent=emptyenv())
        dataTotal$dataPoints = rbind(data1, data0)
        dataTotal$classes = c( rep(TRUE, dim(data1)[1]),
                               rep(FALSE, dim(data0)[1]) )
        rm(data1, data0); gc() # Keep memory usage down

        bins = binGetFuns[[transform]](numBins)
        nbm = create.DiscNaiveBayesianModel(dataTotal, bins)
        rm(dataPoints, classes, envir=as.environment(dataTotal))
        rm(dataTotal) ; gc() # Keep memory usage down.

        # Create test
        test1 = cls1[ ((cls1Ranges[i]+1):cls1Ranges[i+1]), ]
        test0 = cls0[ ((cls0Ranges[i]+1):cls0Ranges[i+1]), ]

        testTotal = new.env(parent=emptyenv())
        testTotal$img = rbind(test1, test0)
        testTotal$Cls = c( rep(TRUE, dim(test1)[1]), rep(FALSE, dim(test0)[1]) )
        rm (test1, test0); gc() # Keep memory usage down

        # Calc error.
        nbmResult = classify.DiscNaiveBayesianModel(nbm, testTotal)

        # There are two main error calcs:
        # 1. Root Mean Square error described in Patter Recognition and
        # Machine Learning by Bishop (page 7).
        #nbmError = sqrt( sum((nbmResult - testTotal$Cls)^2)/length(testTotal$Cls) )

        # 2. Accuracy rate. errors/total
        nbmError = sum(nbmResult != testTotal$Cls)/length(testTotal$Cls)

        finalError = append(finalError,nbmError)
        rm ( img, Cls, envir=as.environment(testTotal))
        rm ( testTotal, nbm, nbmResult, nbmError ); gc()# Keep memory usage down
    }

    rm (cls1, cls0, cls1Ranges, cls0Ranges); gc() #Keep memory usage down

    return (mean(finalError))
}

# Different from create.DiscNaiveBayesianModel
# because it creates the model from a directory.
generate.DiscNaiveBayesianModel <-
    function( directory, filenameOutput=FALSE,
              nbins=100, validate=FALSE, nfolds=4,
              labls=list(fg="foreground",bg="background"),
              transform="-", gparams=list())
{

    if ( !file.exists(directory) )
        stop ( paste("Directory ", directory, "not found.") )
    if ( sum(names(labls)==c("fg","bg")) != 2 )
        stop ( "The labels of the list must be 'fg', 'bg'" )
    if ( (!transform %in% names(colorSpaceFuns))
         || (!transform %in% names(binGetFuns)) )
        stop ( paste("The transform string ", transform, "is not defined") )

    # Gather all the pixels.
    env = new.env(parent=emptyenv())
    bgp = getPixels(directory, labls$bg, transform=transform, gparams=gparams)
    fgp = getPixels(directory, labls$fg, transform=transform, gparams=gparams)
    env$dataPoints = rbind(fgp, bgp)

    # Arbitrary decision: fg is 1 and bg is 0.
    env$classes = c(rep(TRUE,dim(fgp)[1]), rep(FALSE,dim(bgp)[1]))
    rm(fgp,bgp); gc() # keep memory usage down.

    err = NA
    if ( validate )
        err = crossVal.DiscNaiveBayesianModel( env, nbins, nfolds,
                                               transform=transform )

    bins = binGetFuns[[transform]](nbins)
    nbm = create.DiscNaiveBayesianModel(env, bins)
    nbm$error = err

    rm(dataPoints, classes, envir=as.environment(env)); gc()

    if ( filenameOutput != FALSE )
        save ( nbm, file=filenameOutput )

    return (nbm)
}

is.DiscNaiveBayesianModel <- function ( nbm )
{
    nbmNames = names(nbm)
    if ( is.null(nbmNames)
         || !"cls1Hists" %in% nbmNames || !"cls0Hists" %in% nbmNames
         || !"freq1" %in% nbmNames || !"freq0" %in% nbmNames
         || !"dimension" %in% nbmNames || !"bins" %in% nbmNames )
        return (FALSE)
    return (TRUE)
}
