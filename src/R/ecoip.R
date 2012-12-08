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

#Parameters:
# trdir String
#           Path to training images and csv files. Required with DNBM.
# tedir String
#           Path to data images. Required with DNBM.
# bins Integer
#           Number of bins to use for the color signal. Default 100.
# folds Integer
#           Number of folds for S-fold error calculation, Default -1.
# color_space [rgb|hsv|CIEXYZ|CIELAB|CIELUV|yCbCr|ExG]
#           Color space in which the calculations are to take place
#           Default CIELAB. Has effect only with DNBM
# msgf_sigma Double
#           Standard deviation used to create gaussiand smoothing filter. It
#           is only used in the model calculation. Default is 4.
# msgf_size Integer
#           Size of gauss smoothing filter (in pixels). Used in model
#           calculation. Default is 5 pix 0 means no gaussian smoothing.
# priors foregroundPrior,backgroudPrior
#           Specifies the value of the prior in the Naive Bayesian Model
#           creation. Should idealy add 1. Default is autocalculated. Used in
#           naive bayesian model creation
# fglabl String : Name of foreground
# bglabl String : Name of background
eip.nbm <- function( trdir, tedir, bins=100, folds=-1, color_space="CIELAB",
                     msgf_size=5, msgf_sigma=4, priors=NULL, nbmsave=FALSE,
                     fglabl="foreground", bglabl="background" )
{
    G = NULL
    if ( msgf_size > 0 )
        G = makeBrush( size=msgf_size, sigma=msgf_sigma, shape="gaussian" )

    # Create the lable list
    lablList = list(fg=fglabl, bg=bglabl)

    # Get priors
    if ( is.null(priors) )
        priors = list(fg=NULL, bg=NULL)
    else
    {
        ptmp = strsplit(priors, ",")[[1]]
        if ( length(ptmp) != 2 )
            stop ("You must define 2 prior values for the --priors argument")

        pfg = as.numeric(ptmp[1])
        pbg = as.numeric(ptmp[2])

        if ( is.na(pfg) || is.na(pbg) )
            stop ( "Both the foreground and background values must be ints" )

        priors = list(fg=pfg, bg=pbg )
    }

    # Create the model
    dnbm = new.DiscNaiveBayesianModel( trdir, tedir, nbins=bins, nfolds=folds,
                                       transform=color_space, labls=lablList,
                                       G=G, priors=priors )
    dnbm$m.generate(dnbm)

    if ( nbmsave && !file.exists(dnbm$v.outfile) )
    {
        dnbm$m.save(dnbm)
        cat ( "\nThe new model was saved at", dnbm$v.outfile, "\n" )
    }

    return(dnbm)
}

# Parameters:
# encoding String: [signal|video]
# process String: [mask|blobs]
# model [String|model]
#           Path were required model is stored or the model class as returned
#           by eip.nbm.
# tedir String
#           Path to data images. Required with DNBM.
# morphs [shape,size,action[;shape,size,action]...]
#           Specify morphological actions. Relevant only in video.
#           shape = [box|disc|diamond]
#           action = [dilate|erode|open|close]
#           size = Size of the structuring element.
# vid_sbys Boolean
#           This option controls the type of video generated. When present a
#           video of the mask side by side with the original is created.
#           Default is to create only masked videos.
# remove_too_many Boolean
#           Remove images that contain 'too many' blobs. Decision is based on
#           standard deviation and mean from trained blobs. Default is FALSE.
# remove_too_big Boolean
#           Remove images that are have 'too big' blobs. Decision is based on
#           standard deviation and mean of trained blob size. Default FALSE.
# output String
#           Stuff gets output to this file path. Default depends on generate
eip.genOutput <- function( encoding, process, model, tedir, morphs="",
                          vid_sbys=FALSE, remove_too_many=FALSE,
                          remove_too_big=FALSE, output=NULL )
{
    # Sanity check the arguments
    if ( class(model) == "character" && file.exists(model) )
    {
        load(model)
        model = self
        rm (self)
    } else if ( is.null(model$v.type) || model$v.type != "dnbm" )
        stop ("You need to pass a model as returned by eip.nbm")

    if ( !is.null(encoding) && encoding != "signal" && encoding != "video" )
        stop ("The encoding needs to be either 'signal' or 'video'.")
    if ( !is.null(process) && process != "mask" && process != "blobs" )
        stop ("The process needs to be either 'mask' or 'blobs'.")
    if ( !file.exists(tedir) )
        stop("=== THE ", tedir, " DIRECTORY DOES NOT EXIST ===\n")
    else
        model$v.testDir = tedir

    # Create the smoothing gaussian filter.
    G = NULL
    if ( ! is.null(model$v.G) )
        G = model$v.G

    # Create the output
    if (is.null(output))
    {
        if (encoding == "signal") {
            output=file.path(getwd(), "signal.txt")
        } else if (encoding == "video") {
            output=file.path(getwd(), "video.mp4")
        } else
            output="output.txt"
    }
    #FIXME: add the possibility of returning the signal (not the video)
    if ( file.exists(output) )
        stop("=== THE ", output, " FILE EXISTS. ERASE IT ===\n")

    # Construct the morphs option.
    morphsList = list()
    if ( nchar(morphs) > 0 )
    {
        mstmp = strsplit(morphs, ";")[[1]]
        for ( i in 1:length(mstmp) )
        {
            # Order is shape, size, action
            mtmp = strsplit(mstmp[i], ",")[[1]]
            if ( length(mtmp) != 3 )
                stop ("=== morphs MUST HAVE shape, size AND action ===\n")
            if ( ! mtmp[1] %in% morphShapes )
                stop ("=== ", mtmp[1], " INVALID SHAPE IN morphs ===\n")

            ss = as.integer(mtmp[2])
            if ( is.na(ss) )
                stop ("=== ", mtmp[2], " IS NOT AN INTEGER in morphs ===\n")
            if ( ! mtmp[3] %in% names(morphFuncs) )
                stop ("=== ", mtmp[3], " INVALID ACTION IN morphs ===\n")

            # action, structuring element
            morphsList[[i]]=common.getStructElem(ss,act=mtmp[3],type=mtmp[1])
        }
    }


    # PER IMAGE PIPELINE.
    it = new.ImageTransformer(model$v.testDir, model)
    it$m.append ( it, list("transfunc"=it$m.calcMask,
                           "transargs"=list("G"=G)) )

    # Make sure we have a morphsList for blobs.
    if ( length(morphsList) <= 0 && process == "blobs")
    {
        warning("Adding a morphological filter", immediate.=T)
        mlsize = model$m.getMeanPS(model,model$v.labels$fg)
        morphsList[[1]] = common.getStructElem(mlsize)
    }
    # include the calcMorphs only when we have a morphsList.
    if ( length(morphsList) > 0 )
        it$m.append( it, list("transfunc"=it$m.calcMorph,
                              "transargs"= list("morphs"=morphsList)) )


    # These are the different outcomes of process, encoding combinations.
    if ( process == "mask" ){
        if ( encoding == "signal" ) {
            it$m.append ( it, list("transfunc"=it$m.accumMean,
                                   "transargs"=list()) )
        } else if ( encoding == "video" ) {
            if ( vid_sbys )
                it$m.append ( it, list("transfunc"=it$m.combine,
                                       "transargs"=list()) )
            it$m.append ( it, list("transfunc"=it$m.saveMask,
                                   "transargs"=list()) )
        } else
            stop ("Undefined error")

    } else if ( process == "blobs" ){
        if ( remove_too_many )
            it$m.append ( it, list("transfunc"=it$m.remTooManyBlob,
                                   "transargs"=list()) )
        if ( remove_too_big )
            it$m.append ( it, list("transfunc"=it$m.remTooBigBlob,
                                   "transargs"=list()) )

        if ( encoding == "signal" ) {
            it$m.append ( it, list("transfunc"=it$m.accumBlobCount,
                                   "transargs"=list()) )
        } else if ( encoding == "video" ) {
            it$m.append ( it, list("transfunc"=it$m.paintImgBlobs,
                                   "transargs"=list()) )
            it$m.append ( it, list("transfunc"=it$m.saveMask,
                                   "transargs"=list()) )
        } else
            stop ("Undefined error")
    } else
    {   stop ("Undefined error")}

    # IMAGE GROUP PIPELINE
    if ( encoding == "signal" )
        it$m.append ( it,
                      list("transfunc"=it$m.saveTable,
                           "transargs"=list("tablename"=output)),
                      indTrans=F )
    else if ( encoding == "video" )
        it$m.append ( it,
                      list("transfunc"=it$m.genVid,
                           "transargs"=list("videoname"=output)),
                      indTrans=F )

    # Exec the it structure
    # FIXME: CHECK THE RESULT
    res = it$m.trans( it )
    cat ( "\nThe new output was created at", output, "\n" )
}

# Parameters:
# model Is a model class.
# mfile String, Path to where the model class is kept.
eip.showModel <- function ( model=NULL, mfile=NULL )
{
    if ( !is.null(model) ) {
        model$m.print(model)
    } else if ( !is.null(mfile) ) {
        if ( !file.exists(mfile) )
            stop ( "mfile does not exist" )
        load ( mfile )
        self$m.print(self)
    } else
        stop ("Please define either a EcoIP object or a path to it.")
}

# histcmp -> histogram comparison
# Parameters :
# trdir String : Path to training directory
# bins Integer: Number of bins per channel
# pct Double
#           This is the percent of the total collected data that is used to
#           create the histogram comparison. Default is 0.05.
# output String : Path to created histogram
# fglabl String : Name of foreground
# bglabl String : Name of background
eip.histcmp <- function ( trdir, bins=100, pct=0.05, output=NULL,
                          fglabl="foreground", bglabl="background" )
{
    lablList = list(fg=fglabl, bg=bglabl)
    if ( !file.exists(trdir) )
        stop("Training dir does not exist")
    if ( is.null(output) )
        output = file.path(trdir, "histcmp.svg")
    if ( file.exists(output) )
        stop ( "File ", output, "exists, Remove it.")

    dnbm = new.DiscNaiveBayesianModel( trdir, getwd(), nbins=bins, nfolds=-1,
                                       transform="rgb", labls=lablList )
    CH = common.getColorHists(dnbm,pct)
    common.plotColorHists(CH, plotName=output)
    cat ( "\nThe new histogram analysis was created at", output, "\n" )
}

# Parameters:
# signal String or matrix.
#       If string file where the table is kept. If matrix, it represents the
#       signal. defaults to NULL.
# smoothed matrix.
#       2 column matrix. 1st col are dates, 2nd are values of the smoothed
#       signal
# sigmoid matrix
#       2 column matrix. 1st col are dates, 2nd are values of the sigmoid
#       signal
# tp list
#       tp$peaks are the dates where the smoothed signal peaks.
#       tp$valleys are the dates where the smoothed signal valleys.
# ip vector
#       The dates where the sigmoid signal changes concavity.
# xlabl String
#       X axis label, Defaults to Time.
# ylabl String
#       Y axis label, Defaults to Value.
# xlim vector
#       Two element vector c(from, to) that controls the x axis range.
# ylim vector
#       Two element vector c(from, to) that controls the y axix range.
# width Ingeger
#       Width of the resulting figure. Defaults to 10.
# height Ingeger
#       Height of the resulting figure. Defaults to 5.
# ptitle String
#       Title of the plot, Defaults to Title.
# minimum_show Double
#       All values over this will be given a label tick.
# output String
#       Name of the output file. Defaults to NULL.
# lwidth Double
#       Like width. Default is 0.25.
# type String
#       The type of plot point. Defaults to l.
# CEX Double
#       Proportion of text size.
# miss Boolean Vector
#       Vector that is TRUE when the offset is NA and FALSE otherwise. Dfault
#       NULL
# missing_color String
#       The color given to the missing dates. Defaults to azure with
#       transparency (F0FFFFAA).
# mark_training String
#       Date range where the training set is. FROMDATE,TODATE. Default NULL.
# color_training String
#       Default color of the training rectangle, Default "#FFF0FFAA" redish.
# si_col String
#       Color of the signal signal.
# sm_col String
#       Color of the smoothed signal
# sig_col
#       Color of the sigmoid signal
# si_lty
#       Type of line for the signal
# sm_lty
#       Type of line for the smoothed signal
# sig_lty
#       Type of line for the sigmoid signal
# tick_space
#       The minimum calculated space will be multiplied by this number.
eip.plot <-
function ( signal=NULL, smoothed=NULL, sigmoid=NULL, tp=NULL, ip=NULL,
           xlabl="Time", ylabl="Value", xlim=NULL, ylim=NULL,
           width=10, height=5, ptitle="Title", minimum_show=-1,
           output=NULL, lwidth=0.25, type="l", CEX=0.5,
           miss=NULL, missing_color="#F0FFFFAA",
           mark_training=NULL, color_training="#FFF0FFAA",
           si_col="red", sm_col="blue", sig_col="black",
           si_lty="dotted", sm_lty="dashed", sig_lty="solid",
           tick_space=1)
{
    eip.calc_missing_rect_pos <- function ( miss )
    {
        rectPos = matrix(0, ncol=2, nrow=0)
        i=1
        while ( i>0 && i<length(miss) )
        {
            if ( ! miss[i] ){
                i = i + 1
                next
            }

            ofset = i
            while ( miss[i] && i<length(miss) )
                i = i + 1

            rectPos = rbind(rectPos, c(ofset-1, i-ofset+1))
        }

        return (rectPos)
    }

    eip.calc_training_rect_pos <- function ( plotTable, dateStr )
    {
        # Recover dates from string.
        dtmp = strsplit ( dateStr, ",")[[1]]
        if ( length(dtmp) != 2 )
            stop ( "From, to dates must be separated by a coma (,)" )

        dFrom = as.Date(dtmp[1])
        dTo = as.Date(dtmp[2])

        # Sanity check dates
        if ( as.Date(plotTable[1,1]) > dFrom )
            stop ( "From date is less than data minimum" )
        if ( as.Date(plotTable[dim(plotTable)[1],1]) < dTo )
        {
            stop ( "To date is greater than data maximum" )
        }

        rectPos = c(0,0)
        rectPos[1] = which(as.Date(as.vector(plotTable[,1])) > dFrom)[1]
        rectPos[2] = rev(which(as.Date(as.vector(plotTable[,1])) < dTo))[1]

        return (rectPos)
    }

    # Calculates at and labels for the axis function.
    eip.calc_xaxis <- function ( tab, xlim, minimum_show, CEX, tick_space )
    {
        retVal = list()

        # Calc Indices
        if ( minimum_show < 0 )
            minimum_show = ( min(tab[,2], na.rm=TRUE)
                             + abs(min(tab[,2], na.rm=TRUE)
                                   + max(tab[,2], na.rm=TRUE)*0.25) )

        tmpInd = tab[,2]>minimum_show
        tmpInd[is.na(tmpInd)] = FALSE
        tmpInd[1:xlim[1]] = FALSE
        tmpInd[xlim[2]:length(tmpInd)] = FALSE

        # Calc the tick strings and points where to draw a ticks.
        if ( sum(tmpInd) > 1 )
        {
            retVal$labls = tab[tmpInd,1]
            retVal$AT = which(tmpInd)
        } else {
            retVal$labls = tab[,1]
            retVal$AT = seq(1, length(tab[,1]))
        }

        # This is painful. To avoid label overlap only include one tick per
        # "clumped label group". Remove overlapping labels from min distance.
        # This min distance is a function of the plot size and the font size.
        MD= tick_space * ( (par("cin")[1]*CEX)
                           / ( par("fin")[1]/abs(abs(par("usr")[1])
                                                 -abs(par("usr")[2])) ) )

        ATtmp = c(retVal$AT[1])
        Ltmp  = c(retVal$labls[1])
        for ( i in 1:(length(retVal$AT)-1) )
        {
            DIST = abs(ATtmp[length(ATtmp)] - retVal$AT[i+1])
            if ( DIST > MD )
            {
                ATtmp = c(ATtmp, retVal$AT[i+1])
                Ltmp = c(Ltmp, retVal$labls[i+1])
            }
        }
        retVal$AT = ATtmp
        retVal$labls = Ltmp

        return (retVal)
    }

    if ( is.null(signal) && is.null(smoothed) && is.null(sigmoid) )
        return()

    # Output to PDF.
    if ( ! is.null(output) )
        pdf(file=output, width=width, height=height)

    # Init plot
    sample_sig = NULL
    if ( ! is.null(smoothed) )
    {
        if ( is.null(sample_sig) )
            sample_sig = smoothed

        if ( is.null(xlim) ) {xlim = c( 0, dim(smoothed)[1] )}
        if ( is.null(ylim) ) {ylim = c( 0, max(smoothed[,2], na.rm=T) )}

        plot ( smoothed[,2], xlab=xlabl, ylab=ylabl, xlim=xlim, ylim=ylim,
               type=type, col=sm_col, lty=sm_lty, lwd=lwidth,
               main=ptitle, axes=F )
        par(new=T)
    }

    if ( ! is.null(sigmoid) )
    {
        if ( is.null(sample_sig) )
            sample_sig = sigmoid

        if (is.null(xlim)){xlim = c(0, dim(sigmoid)[1])}
        if (is.null(ylim)){ylim = c(0, max(as.numeric(sigmoid[,2]), na.rm=T))}

        plot ( sigmoid[,2], xlab=xlabl, ylab=ylabl, xlim=xlim, ylim=ylim,
               type=type, col=sig_col, lty=sig_lty, lwd=lwidth,
               main=ptitle, axes=F );
        par(new=T)
    }

    if ( ! is.null(signal) )
    {
        if ( class(signal) == "character" )
            signal = eip.get_table( signal )

        if ( is.null(sample_sig) )
            sample_sig = signal

        if ( is.null(xlim) ) {xlim = c( 0, dim(signal)[1] )}
        if ( is.null(ylim) ) {ylim = c(0, max(signal[,2], na.rm=T))}

        plot( signal[,2], xlab=xlabl, ylab=ylabl, xlim=xlim, ylim=ylim,
              type=type, col=si_col, lty=si_lty, pch=21, lwd=lwidth,
              main=ptitle, axes=F )
        par(new=T)
    }

    if ( ! is.null(tp) )
    {
        # Calc tp offset.
        tpOSPeaks = eip.getOffsetFromDate( sample_sig[,1], tp$peaks )
        tpOSValleys = eip.getOffsetFromDate( sample_sig[,1], tp$valleys )

        abline(v=tpOSPeaks, col="lightgreen")
        text ( tpOSPeaks, 0, as.character(tpOSPeaks),
               col="lightgreen", cex=CEX, pos=4, srt=90 )
        par(new=T)
        abline(v=tpOSValleys, col="pink")
        text ( tpOSValleys, 0, as.character(tpOSValleys),
               col="pink", cex=CEX, pos=4, srt=90 )
        par(new=T)
    }

    if ( ! is.null(ip) )
    {
        ipP = eip.getOffsetFromDate( sample_sig[,1], ip )
        plot ( ipP, as.numeric(sample_sig[,2][ipP]),
               xlab=xlabl, ylab=ylabl, xlim=xlim, ylim=ylim,
               main=ptitle, axes=F, col="red", lwd=lwidth )
        text ( ipP, as.numeric(sample_sig[,2][ipP]),
               as.character(sample_sig[,1][ipP]),
               col="red", cex=CEX, pos=4 )
    }

    # Check the dimensions of the signals
    eq_dims = c()
    if ( ! is.null(signal) )
        eq_dims = rbind(eq_dims,dim(signal))
    if ( ! is.null(smoothed) )
        eq_dims = rbind(eq_dims,dim(smoothed))
    if ( ! is.null(sigmoid) )
        eq_dims = rbind(eq_dims,dim(sigmoid))
    if(length(unique(eq_dims[,1])) != 1 || length(unique(eq_dims[,2])) != 1)
        stop ( "All argument signals must have the same dimensions")

    PLOT_LOWER = par("usr")[3]
    PLOT_UPPER = par("usr")[4]

    # create draw missing rectangles
    if ( ! is.null(miss) )
    {
        rectPos = eip.calc_missing_rect_pos ( miss )
        if ( dim(rectPos)[1] > 0 )
            for ( i in 1:dim(rectPos)[1] )
                rect ( rectPos[i,1], PLOT_LOWER,
                       rectPos[i,1]+rectPos[i,2], PLOT_UPPER,
                       col=missing_color, border=NA )
    }

    if ( ! is.null(mark_training) )
    {
        rectPos = eip.calc_training_rect_pos ( sample_sig, mark_training )
        rect ( rectPos[1], PLOT_LOWER, rectPos[2], PLOT_UPPER,
               col=color_training, border=NA )
    }

    # Calc AT : horizontal pos for the labels
    #      labls : The lable strings
    #      RD : Relative down. vertical pos.
    xaxis = eip.calc_xaxis ( sample_sig, xlim, minimum_show, CEX, tick_space )
    RD = par("usr")[3]-(abs(par("usr")[3]-par("usr")[4])*0.05)

    # draw axis
    axis(2)
    axis(1, xaxis$AT, labels=FALSE, lwd=lwidth, lwd.ticks=lwidth)
    text(xaxis$AT, RD, srt = 90, adj = 1, labels = xaxis$labls,
         xpd = TRUE, cex=CEX)
    box()

    if(!is.null(output)){dev.off()}
}

# Parameters:
# signal String, Vector.
#       If it is a string, it should point to a table in the filesystem.
#       When generating from file, we assume data is on the 2nd col.
#       When a vector it is what eip.get_table returns.
# stype String [MA|MA2|LO|GC]
#       MA -> Moving Average
#       MA2 -> Moving Average 2
#       LO -> Lowess
#       GC -> Gauss Convolve
#       MS -> Markov Smothing
#       Type of smoothing process. Default is MA2
# iter Numeric
#       Number of iterations when doing Moving average.Default is 3.
#       It is valid for MA, MA2 and GC options.
# ma_coeffs Numeric
#       Coefficients for MA. if =# then coeffs of that size is created.
#       if =c(#...#) then coeffs is used directly (sum(ma_coeffs)=1).
#       Default is 7.
# lo_span Numeric
#       Proportion of points that influences the smoothing.
# lo_iter Numeric
#       Number of iterations. More iterations means more robustness.
# gc_sigma Nueric
#       Value of sigma for Gauss filter in GC. Default 1.
# gc_size Numeric
#       Size of the 1-D Gauss filter. Default 5. Is forced to uneven.
# ma2_k Numeric
#       The range of the moving averate is 2*ma2_k+1. Default is 3.
# ms_w Numeric
#       Markov smoothing window. Defaults to 3.
eip.smooth <- function ( signal, output=NULL, stype="MA2", iter=3,
                         ma_coeffs=7, lo_span=2/3, lo_iter=3,
                         gc_sigma=1, gc_size=5, ma2_k=3, ms_w=3 )
{
    eip.moving_average <- function ( signal, coeffs, iter )
    {
        if ( class(coeffs) != "numeric" )
            stop ( "Moving average coefficients must be of type numeric" )

        if ( length(coeffs) == 1 )
            coeffs = c(rep(1,coeffs)/coeffs)

        if ( iter < 1 )
            stop ( "The iterator in Moving average must be greater than 0." )

        for ( i in 1:iter )
            signal = filter ( signal, coeffs, method="convolution" )

        return ( signal )
    }

    eip.moving_average2 <- function ( signal, k, iter )
    {
        if ( class(k) != "numeric" )
            stop ( "Moving average2 k must be of type numeric" )

        if ( k < 1 )
            stop ( "Moving average2 k must be greater than 0" )

        if ( iter < 1 )
            stop ( "The iterator in Moving average2 must be greater than 0." )

        for ( i in 1:iter )
        {
            stmp = c( rep(NA,k), signal, rep(NA,k) )
            signal = sapply( (k+1):(k+length(signal)),
                         function(i){mean(stmp[(i-k):(i+k)], na.rm=TRUE)})
        }
        return ( signal )
    }

    eip.lowess <- function ( signal, span=2/3, iter=3 )
    {
        if ( class(span) != "numeric" || class(iter) != "numeric" )
            stop ( "Lowess arguments need to be numeric" )

        return ( lowess ( signal, f=span, iter=iter )$y )
    }

    eip.gauss_convolve <- function ( signal, sigma, size, iter )
    {
        if ( class(sigma) != "numeric" || class(size) != "numeric" )
            stop ( "Gauss_convolve arguments need to be numeric" )

        if ( size < 3 )
            stop ( "Gauss_convolve size needs to be at least 3." )

        if ( sigma < 0 )
            stop ( "Gauss_convolve needs to be greater than one" )

        if ( iter < 1 )
            stop ( "The iterator in gauss convolve must be greater than 0." )

        gsize = floor(size/2)
        gfilter = exp(-(seq(-gsize,gsize,1)^2/(2*sigma^2)))/sqrt(2*pi*sigma^2)

        for ( i in 1:iter )
        {
            signal = c ( rep ( NA, gsize ),
                         convolve ( signal, gfilter, type="filter" ),
                         rep ( NA, gsize ) )
            signal[is.na(signal)] = 0
        }
        return ( signal )
    }

    eip.markov <- function ( signal, window=3 )
    {
        if ( class(window) != "numeric" )
            stop ( "Window in markov smoothing needs to be numeric" )

        if ( window < 1 )
            stop ( "Window in markov smoothing needs to be at least 1" )

        suppressMessages(library("Peaks", character.only=TRUE))

        return ( SpectrumSmoothMarkov ( as.numeric(signal), window=window ) )
    }

    # It returns a list with $peaks and $valleys.
    eip.turning_point <- function ( signal, s=1 )
    {
        retVal = list()
        dd=diff(sign(diff(signal[,2], lag=s, na.pad=FALSE)),na.pad=FALSE)

        non_na = which(!is.na(dd))
        non_zero = which(dd!=0)
        dd[non_na[1]] = (-1)*dd[non_zero[1]]
        dd[non_na[length(non_na)]] = (-1)*dd[non_zero[length(non_zero)]]

        retVal$peaks = signal[,1][ which ( dd < 0  ) + 1 ]
        retVal$valleys = signal[,1][ which ( dd > 0 ) + 1 ]

        return (retVal)
    }

    # Get or Check the signal
    if ( class(signal) == "character" )
        signal = eip.get_table( signal )

    # Check for sanity
    if ( dim(signal)[2] < 2 )
        stop ( "The signal should have at least 2 dimensions" )

    # Warn when we receive NAs
    if ( sum(is.na(signal[,2])) > 0 )
        warning ( "Signal contains NAs. Might lead to undefined behaviour." )

    s = signal[,2]
    s = switch( stype,
            MA = eip.moving_average ( s, ma_coeffs, iter ),
            MA2 = eip.moving_average2 ( s, ma2_k, iter ),
            GC = eip.gauss_convolve ( s, gc_sigma, gc_size, iter ),
            MS = eip.markov ( s, ms_w ),
            LO = eip.lowess ( s, lo_span, lo_iter ) )

    retVal = list()
    retVal$ss = signal
    retVal$ss[,2] = as.numeric(s)

    if ( !is.null(output) )
        write.table ( retVal$ss, file=output, quote=F,
                      row.names=F, col.names=F, sep=" " )

    retVal$tp = eip.turning_point(retVal$ss)
    return (retVal)
}

# Function calculates the sigmoid values and inflection points
# sm_obj list
#       Whatever eip.smooth returns.
# sig_obj list
#       Whatever eip.genMiss returns.
# maxSmoothSize Integer
#       The maximum number of smoothing sizes that eip.sigmoid should try
#       before giving up fitting the signal to a sigmoid.
# silent boolean
#       When false we output the error messages that caused the warnings. When
#       true we ignore the error messages that caused the warnings. Default is
#       True.
eip.sigmoid <- function ( sm_obj, sig_obj, maxSmoothSize=30, silent=T)
{
    getSigmoid <- function ( sig, sig_type )
    {
        if ( sig_type >= 0 ) {
            ini_a = min(sig)
            ini_b = max(sig) - ini_a
            b_mul = 1 # b multiplier

        } else if ( sig_type < 0 ) {
            ini_a = max(sig)
            ini_b = ini_a - min(sig)
            b_mul = -1 # b multiplier

        } else
            stop ( "Undefined error" )

        ini_d = .5 # FIXME: is there a better guess?
        ini_e = round(length(sig)/4)

        x = seq ( 1, length(sig) )

        fit = nls ( sig ~ a+((b_mul*b)/(1+exp(e-d*x))),
                    start = list(a=ini_a,b=ini_b,e=ini_e, d=ini_d),
                    control=list(maxiter=100))

        a = coef(fit)['a']
        b = coef(fit)['b']
        e = coef(fit)['e']
        d = coef(fit)['d']

        # Generate the sigmoid signal
        retVal = list()
        retVal$sigmoid = a + ( (b_mul*b) / (1+exp(e-d*x)) )

        # Generate the point of inflection
        der2 = D(D(expression(a+((b_mul*b)/(1+exp(e-d*x)))),'x'),'x')
        dtmp = diff(eval(der2),na.pad=F)
        retVal$ip = which ( diff (sign (dtmp)) == -2 )

        # This is painful: The second derivative can potentially change sign
        # within a very small range (10^-77 and lower). This means that we
        # will have a 'ghost' inflection point. We ignore the 'ghost' by
        # choosing the inflection point that has the greater dtmp value.
        if ( length(retVal$ip) > 1 )
            retVal$ip = iptmp[which.max(dtmp[retVal$ip])]

        return (retVal)
    }

    # Returns a 2 column matrix. Vectors from and to have values that
    # interlace: (from[1], to[1], from[2]....). Each row is an ordered pair
    # representing the from-to ranges. Col1<Col2.
    find_ranges <- function ( from, to )
    {
        first = which(from>to[1])[1]
        tmp = from[first:length(from)]
        ran_len = min ( length(to), length(tmp) )
        # Suppress the 'dims don't agree' message
        retVal = suppressWarnings( cbind(to,tmp,deparse.level=0)[1:ran_len,] )
        retVal = matrix(retVal, ncol=2)
        if ( sum(retVal[,1]<retVal[,2]) < dim(retVal)[1] )
            stop ( "The turning points are not interlaced.",
                   "This happens when you have a horizontal section.",
                   " Try to remove these." )
        return ( retVal )
    }

    # FIXME: repeated code
    eip.moving_average2 <- function ( signal, k, iter=3 )
    {
        for ( i in 1:iter )
        {
            stmp = c( rep(NA,k), signal, rep(NA,k) )
            signal = sapply( (k+1):(k+length(signal)),
                         function(i){mean(stmp[(i-k):(i+k)], na.rm=TRUE)})
        }
        return ( signal )
    }

    # It tries to fit a sigmoid function to signal. Every time it fails to
    # fit, it will smooth with a growing moving average filter. It will do
    # this 1,3,5,7...(2*max size+1).
    # PT is the positional tries: When fitting to a sigmoid we try to graba as
    # much of the leading and preceding sections of the range. We start off
    # with the range [ prevRange/2 + range + postRange/2] and decrease in PT
    # steps until [range].
    doSigTrials <- function ( from, to, signal,
                              maxSmoothSize, sig_type,
                              ranges, PT )
    {
        # Calc prevRange and postRange
        prevRange = ranges[ (ranges[,2] == from-1), ]
        if ( length(prevRange) == 0 )
            prevRange = rep ( 0, PT )
        else {
            middle = round( abs( (prevRange[2]-prevRange[1])/2 ) )
            prevRange = floor( seq(middle, 0, length.out=PT) )
        }

        postRange = ranges[ (ranges[,1] == to+1), ]
        if ( length(postRange) == 0 )
            postRange = rep ( 0, PT )
        else {
            middle = round( abs( (postRange[2]-postRange[1])/2 ) )
            postRange = floor( seq(middle, 0, length.out=PT) )
        }

        # We increase smoothing if we have not found a sigmoid fit for all the
        # PT ranges.
        for ( i in 0:maxSmoothSize )
        {
            for ( j in 1:PT )
            {
                tmpsig = eip.moving_average2 ( sig_obj$signal[,2], i )
                ff = from - prevRange[j]
                tt = to + postRange[j]
                res = try ( getSigmoid( tmpsig[ff:tt], sig_type ),
                            silent=silent )

                # If there are no inflection points length(res$ip) == 0 we
                # move on.
                if ( class (res) != "try-error" && length(res$ip) != 0)
                {
                    res$sigmoid = res$sigmoid[ (1+prevRange[j]):
                                               (length(res$sigmoid)-postRange[j]) ]

                    # If the inflection point is contained in either of the
                    # 'tails' default ot a sound value.
                    if ( res$ip - prevRange[j] < 0 )
                        res$ip = 1
                    else if ( res$ip - prevRange[j] > length(res$sigmoid) )
                        res$ip = length(res$sigmoid) - 1
                    else
                        res$ip = res$ip - prevRange[j]

                    return ( res )
                }
            }
        }
        warning( "Could not find sigmoid in range: (",
                 "[", from, "] ", signal[,1][from], " <-> ",
                 "[", to, "] ", signal[,1][to], ")", immediate.=T)
        return ( NULL )
    }

    tp = list()
    tp$peaks = eip.getOffsetFromDate(sm_obj$ss[,1], sm_obj$tp$peaks)
    tp$valleys = eip.getOffsetFromDate(sm_obj$ss[,1], sm_obj$tp$valleys)

    # Create the ranges matrix
    dosid = find_ranges ( tp$peaks, tp$valleys )
    upsid = find_ranges ( tp$valleys, tp$peaks )
    upsid[,1] = upsid[,1]+1 # Don't repeat coordinate
    upsid[,2] = upsid[,2]-1
    dosid = cbind(dosid,rep(-1,dim(dosid)[1])) # 1 is up, -1 is down
    upsid = cbind(upsid,rep(1,dim(upsid)[1]))
    ranges = rbind(upsid, dosid)

    # Create the sigmoid signal from all the subsignals.
    sigmoid_sig = rep ( 0, dim(sm_obj$ss)[1] )
    inflection_points = c()
    for ( i in 1:dim(ranges)[1] )
    {
        # Output progress
        cat(rep(" ",10),"\r",signif(i*100/dim(ranges)[1],4),"\t%\r",sep="")
        flush.console()

        sup = doSigTrials ( ranges[i,1], ranges[i,2],
                            sig_obj$signal, maxSmoothSize, ranges[i,3],
                            ranges, 5 )
        if ( is.null(sup) )
            next;

        sigmoid_sig[ ranges[i,1]: ranges[i,2] ] = sup$sigmoid
        inflection_points = append(inflection_points, sup$ip+ranges[i,1])
    }

    cat ( rep(" ", 10), "\r" ) # leave the console clean
    flush.console()

    # Create & return list
    retVal = list()
    retVal$sigmoid = sm_obj$ss
    retVal$sigmoid[,2] = as.numeric(sigmoid_sig)
    retVal$ip = sm_obj$ss[,1][sort(inflection_points)]

    return(retVal)
}

eip.version <- function ()
{
    return ( "@EIP_VER_NUM@" )
}

# Creates missing dates and does a linear interpolation to fill the missing
# data.
# signal matrix or String. Whatever eip.get_table returns. or path to the
#           signal file
eip.genMiss <- function ( signal )
{
    eip.generate_missing_dates <- function ( plotTable )
    {
        ncols = dim(plotTable)[2]

        # Init return matrix
        allDates = matrix(0, nrow=0, ncol=ncols)

        # Init Date
        dateCount = as.Date(plotTable[1,1])

        for ( i in 1:dim(plotTable)[1] )
        {
            if ( dateCount > as.Date(plotTable[i,1]) ) {
                stop("Lost count of dates when including missing dates...")
            } else if ( dateCount < as.Date(plotTable[i,1]) ) {
                while ( dateCount < as.Date(plotTable[i,1]) )
                {
                    allDates = rbind ( allDates,
                                       c(as.character(dateCount),
                                         rep(NA,ncols-1)) )
                    dateCount = dateCount + 1
                }
            }

            allDates = rbind ( allDates, c(plotTable[i,1], plotTable[i,2]) )
            dateCount = dateCount + 1
        }

        allDates = data.frame(allDates, stringsAsFactors=FALSE)
        allDates[,2] = as.double(allDates[,2])
        colnames (allDates) <- names(plotTable)
        return (allDates)
    }

    eip.interpolate_missing_dates <- function ( signal )
    {
        # Offsets where numbers are.
        non_na = which ( !is.na(signal[,2]) )

        # non_na offset where NA run starts
        na_run_start = non_na[which(diff(non_na)>1)]

        # length of na runs
        na_run_len =  diff(non_na)[which(diff(non_na)>1)] - 1

        if ( length(non_na) >= dim(signal)[1] )
            return () #There are no NAs.

        for ( i in 1:length(na_run_start) )
        {
            from = na_run_start[i]
            to = na_run_start[i]+na_run_len[i]+1
            x = c(from, to)
            y = c(signal[from,2],signal[to,2])
            func = approxfun ( x , y )

            for ( j in (x[1]+1):(x[2]-1))
                signal[j,2] = func(j)
        }

        return ( signal )
    }
    # init retVal
    retVal = list()

    # Get or Check the signal
    if ( class(signal) == "character" )
        signal = eip.get_table( signal )

    # Missing dates filled with NAs.
    retVal$signal = eip.generate_missing_dates ( signal )

    # Mark missing dates
    retVal$missing = is.na(retVal$signal[,2])

    # Do a linear interpolation for each NA run.
    retVal$signal = eip.interpolate_missing_dates ( retVal$signal )

    return ( retVal )
}

# Helper function for eip.plot and eip.smooth.signal
eip.get_table <- function ( tfile )
{
    # Get the data. result in a data.frame
    tabl = try(read.table(tfile), silent=TRUE)
    if ( class(tabl) == "try-error" )
    {
        if ( class(try(load(tfile), silent=TRUE)) == "try-error" )
            stop ( "The ", tabl, " file does not contain data" )
    }

    # Make sure our data is ordered
    tabl = tabl[order(tabl$V1),]

    # Extract the dates
    SUBFROM = 1
    SUBTO = 10
    tabl[,1] = substr(basename(as.character(tabl[,1])),SUBFROM, SUBTO)

    # Make sure there are no repeated dates.
    if ( sum(duplicated(tabl[,1])) > 0 )
    {
        print ( "The duplicated elemements:" )
        print (tabl[duplicated(tabl[,1]),])
        stop ( paste("There are duplicated dates in", tfile) )
    }

    return (tabl)
}

eip.getOffsetFromDate <- function ( signal, tpDate )
{
    return ( as.vector( sapply(tpDate, function(x){which(signal==x)}) ) )
}

# Check to see if R environment has everything.
if ( as.integer(R.version[["svn rev"]]) < 57956 )
    stop("=== R REVISION GREATER THAN 57956, INSTALL R 1.15.x ===\n")
