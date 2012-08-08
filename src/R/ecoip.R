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
            stop ( "You must define 2 prior values for the --priors argument" )

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
#           Path were required model is stored or the model class as returned by
#           eip.nbm.
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
#           standard deviation and mean of trained blob size. Default is FALSE.
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
            morphsList[[i]] = common.getStructElem(ss,act=mtmp[3],type=mtmp[1])
        }
    }


    # PER IMAGE PIPELINE.
    it = new.ImageTransformer(model$v.testDir, model)
    it$m.append ( it, list("transfunc"=it$m.calcMask,
                           "transargs"=list("G"=G)) )

    #FIXME: We disable adj_mod for now. This is only for signal.
#    if ( !is.null(adj_mod) )
#    {
#        stmp = model
#        load(adj_mod)
#        it$m.append( it, list("transfunc"=it$m.remNonBG ,
#                              "transargs"= list("adjModel"=model)) )
#        model=stmp
#    }

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

# FIXME: DISABLED adj_mod for now
#    if ( !is.null(opts$adj_mod) )
#    {
#        tname = paste(opts$output,"adj",sep="")
#        it$m.append ( it, list("transfunc"=it$m.saveAdjTable,
#                               "transargs"=list("tablename"=tname,
#                                                "genRdata"=opts$sig_rdata)),
#                  indTrans=F )
#    }
#

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
# tfile String.
#       File where the table is kept. Has no default.
# ignore_missing Boolean
#       Don't plot the missing dates. Default is FALSE.
# output String
#       Name of the output file. Defaults to plot.svg. Default is plot.eps.
# width Ingeger
#       Width of the resulting figure. Defaults to 10.
# height Ingeger
#       Height of the resulting figure. Defaults to 5.
# lwidth Double
#       Like width. Default is 0.25.
# xlabl String
#       X axis label, Defaults to Time.
# ylabl String
#       Y axis label, Defaults to Value.
# type String
#       The type of plot point. Defaults to l.
# lcolor String
#       Line color. Defaults to red.
# ptitle String
#       Title of the plot, Defaults to Title.
# minimum_show Double
#       All values over this will be given a label tick.
# missing_color String
#       The color given to the missing dates. Defaults to azure with
#       transparency (F0FFFFAA).
eip.plot <- function ( tfile, ignore_missing=FALSE, output="plot.pdf",
                      width=10, height=5, lwidth=0.25,
                      xlabl="Time", ylabl="Value", type="l", lcolor="red",
                      ptitle="Title", minimum_show=-1, missing_color="#F0FFFFAA")
{
    #Helper function for eip.plot
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
                                       c(as.character(dateCount),rep(NA,ncols-1)) )
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

    eip.calc_rect_positions <- function ( plotTable )
    {
        rectPos = matrix(0, ncol=2, nrow=0)
        i=1
        while ( i>0 && i<dim(plotTable)[1] )
        {
            if ( ! is.na(plotTable[i,2]) ){
                i = i + 1
                next
            }

            ofset = i
            while ( is.na(plotTable[i,2]) )
                i = i + 1

            rectPos = rbind(rectPos, c(ofset-1, i-ofset+1))
        }

        return (rectPos)
    }

    eip.get_table <- function ( tfile )
    {
        # Get the data
        table = try(read.table(tfile), silent=TRUE)
        if ( class(table) == "try-error" )
        {
            if ( class(try(load(tfile), silent=TRUE)) == "try-error" )
                stop ( "The ", table, " file does not contain data" )
        }

        # Make sure our data is ordered
        table = table[order(table$V1),]

        # Extract the dates
        SUBFROM = 1
        SUBTO = 10
        table[,1] = substr(basename(as.character(table[,1])),SUBFROM, SUBTO)

        # Make sure there are no repeated dates.
        if ( sum(duplicated(table[,1])) > 0 )
        {
            print ( "The duplicated elemements:" )
            print (table[duplicated(table[,1]),])
            stop ( paste("There are duplicated dates in", tfile) )
        }

        return (table)
    }

    # Calculates at and labels for the axis function.
    eip.calc_xaxis <- function ( table, minimum_show, CEX )
    {
        retVal = list()

        # Calc Indices
        if ( minimum_show < 0 )
            minimum_show = ( min(table[,2], na.rm=TRUE)
                             + abs(min(table[,2], na.rm=TRUE)
                                   + max(table[,2], na.rm=TRUE)*0.25) )

        tmpInd = table[,2]>minimum_show
        tmpInd[is.na(tmpInd)] = FALSE

        # Calc the tick strings and points where to draw a ticks.
        if ( sum(tmpInd) > 1 )
        {
            retVal$labls = table[tmpInd,1]
            retVal$AT = which(tmpInd)
        } else {
            retVal$labls = table[,1]
            retVal$AT = seq(1, length(table[,1]))
        }

        # This is painful. To avoid label overlap only include one tick per
        # "clumped label group". Remove overlapping labels from min distance.
        # This min distance is a function of the plot size and the font size.
        MD = ( (par("cin")[1]*CEX)
               / ( par("fin")[1]/abs(abs(par("usr")[1])-abs(par("usr")[2])) ) )

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

    table = eip.get_table ( tfile )

    if ( ! ignore_missing )
    {
        table = eip.generate_missing_dates ( table )
        rectPos = eip.calc_rect_positions ( table )
    }

    # Output to PDF.
    pdf(file=output, width=width, height=height)

    # FIXME: Give control to the user
    LWD = lwidth # Linewidth
    CEX = .5 # Fontsize

    # Init plot
    plot(table[,2], pch=21, xlab=xlabl, ylab=ylabl,
         type=type, col=lcolor, main=ptitle, axes=F, lwd=LWD)

    # create draw rectangles
    if ( ! ignore_missing )
    {
        YFrom = par("usr")[3]
        YTo = par("usr")[4]
        for ( i in 1:dim(rectPos)[1] )
            rect ( rectPos[i,1], YFrom, rectPos[i,1]+rectPos[i,2], YTo,
                   col="#F0FFFFAA", border=NA )
    }

    # Calc AT : horizontal pos for the labels
    #      labls : The lable strings
    #      RD : Relative down. vertical pos.
    xaxis = eip.calc_xaxis ( table, minimum_show, CEX )
    RD = par("usr")[3]-(abs(par("usr")[3]-par("usr")[4])*0.05)

    # draw axis
    axis(2)
    axis(1, xaxis$AT, labels=FALSE, lwd=LWD, lwd.ticks=LWD)
    text(xaxis$AT, RD, srt = 90, adj = 1, labels = xaxis$labls, xpd = TRUE, cex=CEX)
    box()

    dev.off()
}


# Check to see if R environment has everything.
if ( as.integer(R.version[["svn rev"]]) < 57956 )
    stop("=== R REVISION GREATER THAN 57956, INSTALL R 1.15.x ===\n")
