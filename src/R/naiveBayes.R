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

# initialize a DiscNaiveBayesianModel instance
# Params: modelDir    = image/csv dir
#         testDir     = test image dir
#         nbins       = Num of bins for the discrete calc
#         nfolds      = It will validate with nfolds if >0.
#         labls       = Relevant labels in dirs.
#         transform   = Color transofrm to use. See colorTrans.R.
new.DiscNaiveBayesianModel <-
    function(   modelDir, testDir, nbins=100, nfolds=-1, transform="-",
                labls=list(fg="foreground",bg="background"), G=NULL )
{
    # Make sure we are not getting screwed by input vars.
    if ( !file.exists(modelDir) )
        stop ( paste("Directory", modelDir, "not found.") )
    if ( !file.exists(testDir) )
        stop ( paste("Directory", testDir, "not found.") )
    if ( sum(names(labls)==c("fg","bg")) != 2 )
        stop ( "The labels of the list must be 'fg', 'bg'" )
    if ( !exists("binGetFuns") )
        source("colorTrans.R")
    if ( !exists("fillPixels") )
        source("common.R")
    if ( (!transform %in% names(colorSpaceFuns))
         || (!transform %in% names(binGetFuns)) )
        stop ( paste("The transform string", transform, "is not defined") )
    source("common.R")

    # Calc outfile name and load model if we find one.
    outfile = getDigest(modelDir,
                        c(nbins, nfolds, as.character(labls),
                          transform, as.character(G)) )
    outfile = file.path( modelDir, paste(outfile, ".Rdata", sep="") )
    if ( file.exists(outfile) )
    {
        load(outfile)
        self$v.outfile = outfile
        return (self)
    }

    # Create the instance
    dnbm = new.env(parent=emptyenv())

    # Create the instance variables
    dnbm$v.type = "dnbm"
    dnbm$v.modelDir = modelDir
    dnbm$v.testDir = testDir
    dnbm$v.outfile = outfile
    dnbm$v.nbins = nbins
    dnbm$v.nfolds = nfolds
    dnbm$v.labels = labls
    dnbm$v.transform = transform
    dnbm$v.G = G
    dnbm$v.bins = binGetFuns[[transform]](nbins)
    dnbm$v.model = NULL

    # data
    dnbm$v.pixAccum = NULL

    # Create the instance methods
    dnbm$m.generate = generate.DiscNaiveBayesianModel
    dnbm$m.create = create.DiscNaiveBayesianModel
    dnbm$m.classify = classify.DiscNaiveBayesianModel
    dnbm$m.crosval = crossVal.DiscNaiveBayesianModel
    dnbm$m.calcelem = calcNaiveBayesElem
    dnbm$m.trans = colorSpaceFuns[[transform]]
    dnbm$m.fillPixels = fillPixels
    dnbm$m.calcMask = calcMask
    dnbm$m.calcMaskVideo = generate.MaskVideo
    dnbm$m.save = save.DiscNaiveBayesianModel

    return (dnbm)
}

load.DiscNaiveBayesianModel <- function ( filename )
{
    if ( !file.exists(filename) )
        stop ( "Cannot load from an unexisting file" )

    load ( filename )
    self$v.outfile = filename
    return (self)
}

save.DiscNaiveBayesianModel <- function (self, ow=F)
{
    if ( file.exists(self$v.outfile) && !ow )
        stop("Call with ow=T to overwrite")

    save(self, file=self$v.outfile)
}

# Calc Naive Bayesian elem. In P(a|b)=(prod(P(b|a))*p(a))/p(b) we calc P(b|a).
# leaveOut${from,to} | pp=str pointer for pixAccum
calcNaiveBayesElem <- function(self, leaveOut=NULL, pp)
{
    if ( !is.null(leaveOut) && !is.list(leaveOut) )
        stop ( "Error in leaveOut variable.")
    if ( is.list(leaveOut)
         && ( !"from" %in% names(leaveOut) || !"to" %in% names(leaveOut) ) )
        stop ( "Variable leavaOut must contain 'from' and 'to'." )
    if ( dim(self$v.pixAccum[[pp]])[2] != dim(self$v.bins)[2] )
        stop ("The 2nd dim of data must equal 2nd dim of bins")

    if ( is.null(leaveOut) )
        index = 1 : dim(self$v.pixAccum[[pp]])[1]
    else
        index = -(leaveOut$from:leaveOut$to)

    histlist = list()
    for ( i in 1:dim(self$v.pixAccum[[pp]])[2] )
        histlist[[i]] = hist ( self$v.pixAccum[[pp]][index,i],
                                self$v.bins[,i], plot=F)

    rm(index); gc()

    if ( length(histlist) == 0 )
        stop("Could not histlist")

    return (histlist)
}

# Create a Discrete Naive Bayesian Model
# fglo = foreground leave out. bflo = background leave out.
create.DiscNaiveBayesianModel <- function(self, fglo=NULL, bglo=NULL)
{
    self$v.model = list()
    self$v.model$bins = self$v.bins
    self$v.model$cls1Hists = self$m.calcelem(self, fglo, self$v.labels$fg)
    self$v.model$cls0Hists = self$m.calcelem(self, bglo, self$v.labels$bg)

    # Histograms have equal element totals
    cls1total = sum(self$v.model$cls1Hists[[1]]$counts)
    cls0total = sum(self$v.model$cls0Hists[[1]]$counts)

    self$v.model$freq1 = cls1total / (cls1total+cls0total)
    self$v.model$freq0 = cls0total / (cls1total+cls0total)

    # Same dims for fg and bg
    self$v.model$dimension = dim(self$v.pixAccum[[self$v.labels$fg]])[2]

    # cross validation puts error here.
    self$v.model$error = NA
}

# Classify with discrete Naive Bayesian Model
classify.DiscNaiveBayesianModel <- function(self, env)
{
    isParamInEnv(c("data"), env)
    if ( is.null ( self$v.model ) )
        stop("The model object is null. Please run generate.")
    if ( dim(env$data)[2] != self$v.model$dimension )
        stop("The dimensions of data and model should be thesame")

    # Fit the raw data into the bins.
    for (i in 1:dim(env$data)[2])
        env$data[,i] = findInterval(env$data[,i],
                                    self$v.model$bins[,i], all.inside=TRUE)

    # OneZero[,1] -> One probabilities | OneZero[,2] -> Zero Probabilities.
    OneZero = matrix( rep(1,dim(env$data)[1]*2),
                      ncol=2, nrow=dim(env$data)[1] )

    # Calculate the One probabilities.
    for (i in 1:dim(env$data)[2])
        OneZero[,1] = OneZero[,1] * self$v.model$cls1Hists[[i]]$density[env$data[,i]]
    OneZero[,1] = OneZero[,1] * self$v.model$freq1

    # Calculate the Zero probabilities.
    for (i in 1:dim(env$data)[2])
        OneZero[,2] = OneZero[,2] * self$v.model$cls0Hists[[i]]$density[env$data[,i]]
    OneZero[,2] = OneZero[,2] * self$v.model$freq0

    # Return the classification.
    return(OneZero[,1] > OneZero[,2])
}

# This function is based on the method described in Pattern Recognition and
# Machine Learning by Christopher M. Bishop (page 33)
crossVal.DiscNaiveBayesianModel <- function(self)
{
    if ( !is.list(self$v.pixAccum) )
        stop ( "The variable self$v.pixAccum needts to be a list" )

    # Temp label vars. Increase readability
    Lfg = self$v.labels$fg
    Lbg = self$v.labels$bg
    dcls1 = dim(self$v.pixAccum[[Lfg]])[1]
    dcls0 = dim(self$v.pixAccum[[Lbg]])[1]
    cls1Ranges = floor( seq(0, dcls1, dcls1/self$v.nfolds) )
    cls0Ranges = floor( seq(0, dcls0, dcls0/self$v.nfolds) )

    finalError = c()

    for ( i in 1:(length(cls1Ranges)-1) ) # len(cls1Ranges) == len(cls0Ranges)
    {
        # Create Model
        fglo=list(); fglo$from=cls1Ranges[i]+1; fglo$to=cls1Ranges[i+1]
        bglo=list(); bglo$from=cls0Ranges[i]+1; bglo$to=cls0Ranges[i+1]
        self$m.create(self,fglo=fglo, bglo=bglo)

        # Create test
        test1 = self$v.pixAccum[[Lfg]][ ((cls1Ranges[i]+1):cls1Ranges[i+1]), ]
        test0 = self$v.pixAccum[[Lbg]][ ((cls0Ranges[i]+1):cls0Ranges[i+1]), ]

        testTotal = new.env(parent=emptyenv())
        testTotal$data = rbind(test1, test0)
        testTotal$Cls = c( rep(TRUE, dim(test1)[1]), rep(FALSE, dim(test0)[1]) )
        rm (test1, test0); gc() # Keep memory usage down

        # Calc error.
        nbmResult = self$m.classify(self, testTotal)

        # There are two main error calcs:
        # 1. Root Mean Square error described in Patter Recognition and
        # Machine Learning by Bishop (page 7).
        #nbmError = sqrt( sum((nbmResult - testTotal$Cls)^2)/length(testTotal$Cls) )

        # 2. Accuracy rate. errors/total
        nbmError = sum(nbmResult != testTotal$Cls)/length(testTotal$Cls)

        finalError = append(finalError,nbmError)

        # Keep memory usage down
        rm ( data, Cls, envir=as.environment(testTotal))
        rm ( fglo, bglo, testTotal, nbmResult, nbmError ); gc()
    }

    #Keep memory usage down
    rm (cls1Ranges, cls0Ranges, Lfg, Lbg, dcls1, dcls0); gc()

    return (mean(finalError))
}

# Different from create.DiscNaiveBayesianModel
# because it creates the model from a directory.
generate.DiscNaiveBayesianModel <- function (self, fr=F)
{
    # Don't regenerate :)
    if ( ! is.null(self$v.model) && !fr)
    {
        print ( "To force regenerate call with fr=T" ); flush.console()
        return ()
    }

    # Gather all the pixels.
    self$m.fillPixels(self)

    err = NA
    if ( self$v.nfolds > 1 )
        err = self$m.crosval( self )

    self$m.create(self)
    self$v.model$error = err

    rm(v.pixAccum, envir=as.environment(self)); gc()
}

is.DiscNaiveBayesianModel <- function ( nbm )
{
    nbmNames = names(nbm)
    if ( is.null(nbmNames)
         || !"cls1Hists" %in% nbmNames || !"cls0Hists" %in% nbmNames
         || !"freq1" %in% nbmNames || !"freq0" %in% nbmNames
         || !"dimension" %in% nbmNames )
        return (FALSE)
    return (TRUE)
}
