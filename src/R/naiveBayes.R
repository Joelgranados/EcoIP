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
                labls=list(fg="foreground",bg="background"),
                priors=list(fg=NULL, bg=NULL), G=NULL )
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
    if ( !exists("common.fillPixels") )
        source("common.R")
    if ( (!transform %in% names(colorSpaceFuns))
         || (!transform %in% names(binGetFuns)) )
        stop ( paste("The transform string", transform, "is not defined") )
    if ( !exists ("common.InEnv") )
        source("common.R")

    # FIXME: Allow to define an outfile
    # Calc outfile name and load model if we find one.
    outfile = common.getDigest( modelDir,
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
    dnbm$v.priors = priors
    dnbm$v.transform = transform
    dnbm$v.G = G
    dnbm$v.bins = binGetFuns[[transform]](nbins)
    dnbm$v.model = NULL

    # Sizes represent number of blobs per training image.
    dnbm$v.numBlobs = list()
    dnbm$v.numBlobs$fg = list()
    dnbm$v.numBlobs$fg$values = c()
    dnbm$v.numBlobs$bg = list()
    dnbm$v.numBlobs$bg$values = c()

    # Sizes represent max and min sides of all containing squares.
    dnbm$v.polySize = list()
    dnbm$v.polySize[[ dnbm$v.labels$fg ]] = list()
    dnbm$v.polySize[[ dnbm$v.labels$fg ]][["values"]] = c()
    dnbm$v.polySize[[ dnbm$v.labels$bg ]] = list()
    dnbm$v.polySize[[ dnbm$v.labels$bg ]][["values"]] = c()

    # data
    dnbm$v.pixAccum = NULL

    # Create the instance methods
    dnbm$m.generate = generate.DiscNaiveBayesianModel
    dnbm$m.create = create.DiscNaiveBayesianModel
    dnbm$m.classify = classify.DiscNaiveBayesianModel
    dnbm$m.crosval = crossVal.DiscNaiveBayesianModel
    dnbm$m.calcelem = calcNaiveBayesElem
    dnbm$m.trans = colorSpaceFuns[[transform]]
    dnbm$m.fillPixels = common.fillPixels
    dnbm$m.calcMask = common.calcMask
    dnbm$m.save = save.DiscNaiveBayesianModel
    dnbm$m.print = print.DiscNaiveBayesianModel
    dnbm$m.addPS = dnbm.addPolySize
    dnbm$m.getMinPS = dnbm.getMinPolySize
    dnbm$m.getMaxPS = dnbm.getMaxPolySize
    dnbm$m.getMeanPS = dnbm.getMeanPolySize
    dnbm$m.getSDPS = dnbm.getStandardDeviationPolySize
    dnbm$m.addNB = dnbm.addNumBlobs
    dnbm$m.getMeanNB = dnbm.getMeanNumBlobs
    dnbm$m.getSDNB = dnbm.getStandardDeviationNumBlobs

    return (dnbm)
}

dnbm.addNumBlobs <- function( self, csv )
{
    # FIXME: check for validity of self & csv?
    fgbc = 0
    bgbc = 0
    for (i in 1:length(csv))
    {
        if ( csv[[i]]$label == self$v.labels$fg ) {
            fgbc = fgbc + 1
        } else if ( csv[[i]]$label == self$v.labels$bg ) {
            bgbc = bgbc + 1
        } else {
            stop("Unknown error")
        }
    }

    # We are interested in the distribution of the blob counts per image of the
    # images that contain blob counts. This is the reason we ingore zero values
    if ( fgbc > 0 )
        self$v.numBlobs$fg$values = append(self$v.numBlobs$fg$values, fgbc)

    if ( bgbc > 0 )
        self$v.numBlobs$bg$values = append(self$v.numBlobs$bg$values, bgbc)
}

# This label is the internal name bg fg
dnbm.getMeanNumBlobs <- function ( self, labl )
{
    if ( ! labl %in% names(self$v.labels) )
        stop ( "Incorrect label" )
    if ( length(self$v.numBlobs[[labl]]$values) == 0 )
        stop ( "The values structure in numBlobs" )

    return(mean(self$v.numBlobs[[labl]]$values))
}

# This label is the internal name bg fg
dnbm.getStandardDeviationNumBlobs <- function ( self, labl )
{
    if ( ! labl %in% names(self$v.labels) )
        stop ( "Incorrect label" )
    if ( length(self$v.numBlobs[[labl]]$values) == 0 )
        stop ( "The values structure is not available" )

    return ( sd(self$v.numBlobs[[labl]]$values) )
}

dnbm.addPolySize <- function( self, csv )
{
    # Fixme check consistency of csv and model
    width = abs(min(as.numeric(csv$polygon[,1]))
                - max(as.numeric(csv$polygon[,1]))) # width
    height = abs(min(as.numeric(csv$polygon[,2]))
                 - max(as.numeric(csv$polygon[,2]))) # height

    # FIXME: Increase accuracy by using min and max separetly
    val = round((width+height)/2)
    self$v.polySize[[csv$label]]$values =
        append(self$v.polySize[[csv$label]]$values, val)
}

dnbm.getMaxPolySize <- function( self, labl )
{
    if ( ! labl %in% self$v.labels )
        stop ( "Incorrect label" )
    if ( length(self$v.polySize[[labl]][["values"]]) == 0 )
        stop ( "The values structure is not available" )
    else
        return ( max(self$v.polySize[[labl]][["values"]]) )

}

dnbm.getMinPolySize <- function( self, labl )
{
    if ( ! labl %in% self$v.labels )
        stop ( "Incorrect label" )
    if ( length(self$v.polySize[[labl]][["values"]]) == 0 )
        stop ( "The values structure is not available" )
    else
        return ( min(self$v.polySize[[labl]][["values"]]) )
}

dnbm.getMeanPolySize <- function( self, labl )
{
    if ( ! labl %in% self$v.labels )
        stop ( "Incorrect label" )
    if ( length(self$v.polySize[[labl]][["values"]]) == 0 )
        stop ( "The values structure is not available" )
    else
        return ( mean(self$v.polySize[[labl]][["values"]]) )
}

dnbm.getStandardDeviationPolySize <- function( self, labl )
{
    if ( ! labl %in% self$v.labels )
        stop ( "Incorrect label" )
    if ( length(self$v.polySize[[labl]][["values"]]) == 0 )
        stop ( "The values structure is not available" )

    return ( sd(self$v.polySize[[labl]][["values"]]) )
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

print.DiscNaiveBayesianModel <- function (self)
{
    cat ( rep("=",72),"\n", sep="" )
    cat ( "Description of model", self$v.type, "\n" )
    cat ( "\tmodelDir: ", self$v.modelDir, "\n" )
    cat ( "\ttestDir: ", self$v.testDir, "\n" )
    cat ( "\toutfile: ", self$v.outfile, "\n" )
    cat ( "\tnbins: ", self$v.nbins, "\n" )
    cat ( "\tnfolds: ", self$v.nfolds, "\n" )
    #cat ( "\tlables: ", self$v.labels, "\n" )
    cat ( "\ttransform: ", self$v.transform, "\n" )
    cat ( "\tsize of G: ", as.character(dim(self$v.G)), "\n" )
    #cat ( "\tG: ", self$v.G, "\n" )
    #cat ( "\tbins: ", self$v.bins, "\n" )
    cat ( "\terror: ", self$v.model$error, "\n" )
    cat ( "\tfalse positives: ", self$v.model$fperror, "\n" )
    cat ( "\tfalse negatives: ", self$v.model$fnerror, "\n" )
    cat ( "\tfreq0: ", self$v.model$freq0, "\n" )
    cat ( "\tfreq1: ", self$v.model$freq1, "\n" )
    cat ( "\tpolySizeFGmean: ", self$m.getMeanPS(self, self$v.labels$fg), "\n" )
    cat ( "\tpolySizeFGSD: ", self$m.getSDPS(self, self$v.labels$bg), "\n" )
    cat ( "\tnumBlobsFGmean: ", self$m.getMeanNB(self, "fg"), "\n" )
    cat ( "\tnumBlobsFGSD: ", self$m.getSDNB(self, "fg"), "\n" )
    cat ( rep("=",72),"\n", sep="" )
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

    if ( is.null(self$v.priors$fg) || is.null(self$v.priors$bg) )
    {
        self$v.model$freq1 = cls1total / (cls1total+cls0total)
        self$v.model$freq0 = cls0total / (cls1total+cls0total)
    } else {
        self$v.model$freq1 = self$v.priors$fg
        self$v.model$freq0 = self$v.priors$bg
    }


    # Same dims for fg and bg
    self$v.model$dimension = dim(self$v.pixAccum[[self$v.labels$fg]])[2]

    # cross validation puts error here.
    self$v.model$error = NA
    self$v.model$fperror = NA
    self$v.model$fnerror = NA
}

# Classify with discrete Naive Bayesian Model
classify.DiscNaiveBayesianModel <- function(self, env)
{
    common.InEnv(c("data"), env)
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
    {
        sum1Hists = sum(self$v.model$cls1Hists[[i]]$counts)
        OneZero[,1] = ( OneZero[,1]
                        * ( self$v.model$cls1Hists[[i]]$counts[env$data[,i]]
                            / sum1Hists ) )
    }
    OneZero[,1] = OneZero[,1] * self$v.model$freq1

    # Calculate the Zero probabilities.
    for (i in 1:dim(env$data)[2])
    {
        sum0Hists = sum(self$v.model$cls0Hists[[i]]$counts)
        OneZero[,2] = ( OneZero[,2]
                        * ( self$v.model$cls0Hists[[i]]$counts[env$data[,i]]
                            / sum0Hists ) )
    }
    OneZero[,2] = OneZero[,2] * self$v.model$freq0

    # Return the classification.
    return(OneZero[,1] > OneZero[,2])
}

# This function is based on the method described in Pattern Recognition and
# Machine Learning by Christopher M. Bishop (page 33)
crossVal.DiscNaiveBayesianModel <- function(self)
{
    if ( !is.list(self$v.pixAccum) )
        stop ( "The variable self$v.pixAccum needs to be a list" )

    # Temp label vars. Increase readability
    Lfg = self$v.labels$fg
    Lbg = self$v.labels$bg
    dcls1 = dim(self$v.pixAccum[[Lfg]])[1]
    dcls0 = dim(self$v.pixAccum[[Lbg]])[1]
    cls1Ranges = floor( seq(0, dcls1, dcls1/self$v.nfolds) )
    cls0Ranges = floor( seq(0, dcls0, dcls0/self$v.nfolds) )

    finalFP = c() # Final False Positives
    finalFN = c() # Final False Negatives

    # if LIST == NULL, for loop does not exec.
    LIST = if(length(cls1Ranges)-1 <= 0){NULL}else{1:(length(cls1Ranges)-1)}
    for ( i in LIST )
    {
        # Create Model
        fglo=list(); fglo$from=cls1Ranges[i]+1; fglo$to=cls1Ranges[i+1]
        bglo=list(); bglo$from=cls0Ranges[i]+1; bglo$to=cls0Ranges[i+1]
        self$m.create(self,fglo=fglo, bglo=bglo)

        # There are two main error calcs:
        # 1. Root Mean Square error described in Patter Recognition and
        # Machine Learning by Bishop (page 7).
        #nbmError = sqrt( sum((nbmResult - testTotal$Cls)^2)/length(testTotal$Cls) )
        # 2. Accuracy rate. errors/total
        #nbmError = sum(nbmResult != testTotal$Cls)/length(testTotal$Cls)

        # Calc False Negatives
        T1 = new.env(parent=emptyenv())
        T1$data = self$v.pixAccum[[Lfg]][ ((cls1Ranges[i]+1):cls1Ranges[i+1]), ]
        T1$data = as.matrix(T1$data)
        T1$Cls = c( rep(TRUE, dim(T1$data)[1]) )
        T1res = self$m.classify(self, T1)
        T1error = sum(T1res != T1$Cls)/length(T1$Cls)
        finalFN = append(finalFN, T1error)
        rm(T1, T1res, T1error); gc()

        # Calc False Positives
        T0 = new.env(parent=emptyenv())
        T0$data = self$v.pixAccum[[Lbg]][ ((cls0Ranges[i]+1):cls0Ranges[i+1]), ]
        T0$data = as.matrix(T0$data)
        T0$Cls = c( rep(FALSE, dim(T0$data)[1]) )
        T0res =  self$m.classify(self, T0)
        T0error = sum(T0res != T0$Cls)/length(T0$Cls)
        finalFP = append(finalFP, T0error)
        rm(T0, T0res, T0error); gc()
    }

    #Keep memory usage down
    rm (cls1Ranges, cls0Ranges, Lfg, Lbg, dcls1, dcls0); gc()

    return (list("fp"=mean(finalFP), "fn"=mean(finalFN)))
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

    err = list("fp"=NA, "fn"=NA)
    if ( self$v.nfolds > 1 )
        err = self$m.crosval( self )

    self$m.create(self)
    self$v.model$error = NA
    self$v.model$fperror = err[["fp"]]
    self$v.model$fnerror = err[["fn"]]

    rm(v.pixAccum, envir=as.environment(self)); gc()
}

# We are to be called from common
update.DiscNaiveBayesianModel <- function (self)
{
    self$m.generate = generate.DiscNaiveBayesianModel
    self$m.create = create.DiscNaiveBayesianModel
    self$m.classify = classify.DiscNaiveBayesianModel
    self$m.crosval = crossVal.DiscNaiveBayesianModel
    self$m.calcelem = calcNaiveBayesElem
    #self$m.trans = colorSpaceFuns[[transform]]
    self$m.fillPixels = common.fillPixels
    self$m.calcMask = common.calcMask
    self$m.save = save.DiscNaiveBayesianModel
    self$m.print = print.DiscNaiveBayesianModel
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
