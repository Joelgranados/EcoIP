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

# Calc the Naive Bayesian element. In P(a|b) = (prod(P(b|a))*p(a))/p(b) we are
# calculating P(b|a).
# FIXME: We are concerned about R's pass by value. The link found at
#   http://cran.r-project.org/doc/manuals/R-lang.html#Argument-evaluation
#   suggests that the colMat matrix will not be recopied.
calcNaiveBayesElem <- function(colMat, bins)
{
    histlist = list()
    colMat = as.matrix(colMat)

    for (i in 1:dim(colMat)[2])
        histlist[[i]] = hist(colMat[,i], bins, plot=FALSE)

    if ( length(histlist) == 0 )
        return (retError("Could not histlist"))

    return (histlist)
}

# Create a Discrete Naive Bayesian Model
create.DiscNaiveBayesianModel <- function(classes, dataPoints, numBins)
{
    if ( !is.matrix(dataPoints) )
        return (retError ( "The dataPoints argument must be a matrix" ))

    if ( !is.vector(classes) )
        return (retError ( "The classes argument must be a boolean vector" ))

    if ( length(classes) != dim(dataPoints)[1] )
        return (retError("Classes length must be equal to first dim dataPoints"))

    if ( sum(classes) == 0 || sum(!classes) == 0 )
        return (retError ("Must include data for two classes"))

    if ( class(classes) != "logical")
        return (retError ("Classes must be a logical vector"))

    NBM = list() #Naive Bayesian Model (NBM)
    NBM$bins = seq(0,1,1/numBins)
    NBM$cls1Hists = calcNaiveBayesElem(dataPoints[classes,],NBM$bins)
    NBM$cls0Hists = calcNaiveBayesElem(dataPoints[!classes,],NBM$bins)

    NBM$freq1 = sum(classes)/length(classes)
    NBM$freq0 = sum(!classes)/length(classes)

    NBM$dimension = dim(dataPoints)[2]

    return (NBM)
}

# Classify with discrete Naive Bayesian Model
classify.DiscNaiveBayesianModel <- function(NBM, dataInput)
{
    nbmNames = names(NBM)
    if ( is.null(nbmNames)
         || !"cls1Hists" %in% nbmNames || !"cls0Hists" %in% nbmNames
         || !"freq1" %in% nbmNames || !"freq0" %in% nbmNames
         || !"dimension" %in% nbmNames || !"bins" %in% nbmNames )
        return (retError("The NBM object is not a Naive Bayesian Model Object"))

    if ( dim(dataInput)[2] != NBM$dimension )
        return (retError("The dimensions of data and model should be thesame"))

    # Fit the raw data into the bins.
    for (i in 1:dim(dataInput)[2])
        dataInput[,i] = findInterval(dataInput[,i] , NBM$bins)

    # OneZero[,1] -> One probabilities | OneZero[,2] -> Zero Probabilities.
    OneZero = matrix( rep(1,dim(dataInput)[1]*2),
                      ncol=2, nrow=dim(dataInput)[1] )

    # Calculate the One probabilities.
    for (i in 1:dim(dataInput)[2])
        OneZero[,1] = OneZero[,1] * NBM$cls1Hists[[i]]$density[dataInput[,i]]
    OneZero[,1] = OneZero[,1] * NBM$freq1

    # Calculate the Zero probabilities.
    for (i in 1:dim(dataInput)[2])
        OneZero[,2] = OneZero[,2] * NBM$cls0Hists[[i]]$density[dataInput[,i]]
    OneZero[,2] = OneZero[,2] * NBM$freq0

    # Return the classification.
    return(OneZero[,1] > OneZero[,2])
}

# This function is based on the method described in Pattern Recognition and
# Machine Learning by Christopher M. Bishop (page 33)
crossVal.DiscNaiveBayesianModel <- function(classes, dataPoints, numBins, numFold)
{
    if ( !is.matrix(dataPoints) )
        return (retError ( "The dataPoints argument must be a matrix" ))

    if ( !is.vector(classes) )
        return (retError ( "The classes argument must be a boolean vector" ))

    if ( length(classes) != dim(dataPoints)[1] )
        return (retError("Classes length must be equal to first dim dataPoints"))

    if ( sum(classes) == 0 || sum(!classes) == 0 )
        return (retError ("Must include data for two classes"))

    if ( class(classes) != "logical")
        return (retError ("Classes must be a logical vector"))

    cls1 = dataPoints[classes,]
    cls0 = dataPoints[!classes,]

    cls1Ranges = floor( seq(0,dim(cls1)[1],dim(cls1)[1]/numFold) )
    cls0Ranges = floor( seq(0,dim(cls0)[1],dim(cls0)[1]/numFold) )

    finalError = c()

    for ( i in 1:(length(cls1Ranges)-1) ) # len(cls1Ranges) == len(cls0Ranges)
    {
        test1 = cls1[ ((cls1Ranges[i]+1):cls1Ranges[i+1]), ]
        data1 = cls1[ -((cls1Ranges[i]+1):cls1Ranges[i+1]), ]

        test0 = cls0[ ((cls0Ranges[i]+1):cls0Ranges[i+1]), ]
        data0 = cls0[ -((cls0Ranges[i]+1):cls0Ranges[i+1]), ]

        dataTotal = rbind(data1, data0)
        dataTotalCls = c( rep(TRUE, dim(data1)[1]), rep(FALSE, dim(data0)[1]) )

        testTotal = rbind(test1, test0)
        testTotalCls = c( rep(TRUE, dim(test1)[1]), rep(FALSE, dim(test0)[1]) )

        nbm = create.DiscNaiveBayesianModel(dataTotalCls, dataTotal, numBis)
        nbmResult = classify.DiscNaiveBayesianModel(nbm, testTotal)

        # We use the Root Mean Square error described in Patter Recognition and
        # Machine Learning by Bishop (page 7)
        nbmError = sqrt( ((nbmResult + testTotalCls)^2)/length(testTotalCls) )

        finalError = append(finalError,nbmError)
    }

    return (mean(finalError))
}
