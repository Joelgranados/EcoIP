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

printopts <- function (opts)
{
    optnames = names(opts)
    cat ( rep("=",72),"\n", sep="")
    cat ( "Option structure: \n" )
    for ( i in 1:length(optnames) )
        if ( class(opts[[optnames[i]]]) != "list" )
            cat ( "\topts$", optnames[i],": ",opts[[optnames[i]]],"\n",sep="" )
    cat ( rep("=",72),"\n", sep="")
    flush.console()
}

generate.DNBM <- function(opts)
{
    # Create the smoothing gaussian filter.
    G = NULL
    if ( opts$msgf_size > 0 )
        G = makeBrush(  size=opts$msgf_size, sigma=opts$msgf_sigma,
                        shape="gaussian" )

    # Create the lable list
    lablList = list(fg=opts$fglabl, bg=opts$bglabl)

    dnbm = new.DiscNaiveBayesianModel( opts$trdir, opts$tedir,
            nbins=opts$bins, nfolds=opts$folds, transform=opts$color_space,
            labls=lablList, G=G, priors=opts$priors )

    dnbm$m.generate(dnbm)

    if ( !file.exists(dnbm$v.outfile) )
        dnbm$m.save(dnbm)

    cat ( "\nThe new model was created at", dnbm$v.outfile, "\n" )
}

generate.signal <- function(opts)
{
    # This will load self into the current env.
    load(opts$mfile)

    # Create the smoothing gaussian filter.
    G = NULL
    if ( ! is.null(self$v.G) )
        G = self$v.G

    if ( !is.null(opts$tedir) )
        self$v.testDir = opts$tedir

    # Per image pipeline.
    it = new.ImageTransformer(self$v.testDir, self)
    it$m.append ( it, list("transfunc"=it$m.calcMask,
                           "transargs"=list("G"=G)) )

    if ( !is.null(opts$adj_mod) )
    {
        stmp = self
        load(opts$adj_mod)
        it$m.append( it, list("transfunc"=it$m.remNonBG ,
                              "transargs"= list("adjModel"=self)) )
        self=stmp
    }

    # Always use user-defined morphList. If not defined and counting blobs, we
    if ( length(opts$morphsList) > 0 )
        it$m.append( it, list("transfunc"=it$m.calcMorph,
                              "transargs"= list("morphs"=opts$morphsList)) )
    else if ( opts$generate == "bc_sig" )
    {
        mlsize = self$m.getMeanPS(self,self$v.labels$fg)
        opts$morphsList[[1]] = common.getStructElem(mlsize)

        it$m.append( it, list("transfunc"=it$m.calcMorph,
                              "transargs"= list("morphs"=opts$morphsList)) )
        warning("Adding a morphological filter", immediate.=T)
    }

    if ( opts$generate == "ma_sig" ) {
        it$m.append ( it, list("transfunc"=it$m.accumMean,"transargs"=list()) )
    } else if ( opts$generate == "bc_sig" ) {
        if ( opts$remove_too_many )
            it$m.append ( it, list("transfunc"=it$m.remTooManyBlob,
                                   "transargs"=list()) )

        if ( opts$remove_too_big )
            it$m.append ( it, list("transfunc"=it$m.remTooBigBlob,
                                   "transargs"=list()) )

        it$m.append ( it, list("transfunc"=it$m.accumBlobCount,
                               "transargs"=list()) )
    } else
        stop( "Undefined Error" ) # should not reach this.


    # Image Group pipeline
    it$m.append ( it, list("transfunc"=it$m.saveTable,
                           "transargs"=list("tablename"=opts$output,
                                            "genRdata"=opts$sig_rdata)),
                  indTrans=F )

    if ( !is.null(opts$adj_mod) )
    {
        tname = paste(opts$output,"adj",sep="")
        it$m.append ( it, list("transfunc"=it$m.saveAdjTable,
                               "transargs"=list("tablename"=tname,
                                                "genRdata"=opts$sig_rdata)),
                  indTrans=F )
    }

    res = it$m.trans( it )

    if ( res != 0)
        return (1)
    cat ( "\nThe new signal was created at", opts$output, "\n" )
    return (0)
}

generate.video <- function(opts)
{
    # This will load self into the current env.
    load(opts$mfile)

    # Create the smoothing gaussian filter.
    G = NULL
    if ( ! is.null(self$v.G) )
        G = self$v.G

    if ( !is.null(opts$tedir) )
        self$v.testDir = opts$tedir

    # Per image pipeline.
    it = new.ImageTransformer(self$v.testDir, self)
    it$m.append ( it, list("transfunc"=it$m.calcMask,
                           "transargs"=list("G"=G)) )

    # Always use user-defined morphList. If not defined and counting blobs, we
    if ( length(opts$morphsList) > 0 )
        it$m.append( it, list("transfunc"=it$m.calcMorph,
                              "transargs"= list("morphs"=opts$morphsList)) )
    else if ( opts$generate == "bc_vid" )
    {
        mlsize = self$m.getMeanPS(self,self$v.labels$fg)
        opts$morphsList[[1]] = common.getStructElem(mlsize)

        it$m.append( it, list("transfunc"=it$m.calcMorph,
                              "transargs"= list("morphs"=opts$morphsList)) )
        warning("Adding a morphological filter", immediate.=T)
    }

    if ( opts$generate == "ma_vid" )
    {
        if ( opts$vid_sbys )
            it$m.append ( it, list("transfunc"=it$m.combine,
                                   "transargs"=list()) )
    } else if ( opts$generate == "bc_vid" ) {
        if ( opts$remove_too_many )
            it$m.append ( it, list("transfunc"=it$m.remTooManyBlob,
                                   "transargs"=list()) )

        if ( opts$remove_too_big )
            it$m.append ( it, list("transfunc"=it$m.remTooBigBlob,
                                   "transargs"=list()) )

        it$m.append ( it, list("transfunc"=it$m.paintImgBlobs,
                               "transargs"=list()) )
    } else
        stop ( "Undefined Error" ) # should not get here

    it$m.append ( it, list("transfunc"=it$m.saveMask,
                           "transargs"=list()) )

    # Image Group pipeline
    it$m.append ( it, list("transfunc"=it$m.genVid,
                           "transargs"=list("videoname"=opts$output)),
                  indTrans=F )

    # Exec the it structure
    res = it$m.trans( it )

    if ( res != 0)
        stop ( "There was an error generating video " )
    cat ( "\nThe new video was created at", opts$output, "\n" )
    return (0)
}

generate.modelInformation <- function(opts)
{
    # This will load self into the current env.
    load(opts$mfile)

    self$m.print(self)
}

generate.histcmp <- function(opts)
{
    lablList = list(fg=opts$fglabl, bg=opts$bglabl)
    dnbm = new.DiscNaiveBayesianModel( opts$trdir, getwd(), nbins=opts$bins,
                                       nfolds=-1, transform="rgb",
                                       labls=lablList )

    CH = common.getColorHists(dnbm,opts$hc_pct)
    common.plotColorHists(CH, plotName=opts$output)
    cat ( "\nThe new histogram analysis was created at", opts$output, "\n" )
    return(0)
}

# Parameters:
# eipMode [DNBM|modInfo|ma_vid|bc_vid|ma_sig|bc_sig]
#           DNBM -> Discreate Naive Bayesian Model.
#           modInfo -> Prints the models info.
#           ma_vid -> A video of the masks. Depends on ffmpeg
#           bc_vid -> A video that counts blobs. Depends on ffmpeg
#           ma_sig -> A signal of masks means.
#           bc_sig -> A signal of blob counts.
#           histcmp -> Histogram comparison.
#           update -> Update model with current code.
#           This argument is necessary
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
# morphs [shape,size,action[;shape,size,action]...]
#           Specify morphological actions. Relevant only in video.
#           shape = [box|disc|diamond]
#           action = [dilate|erode|open|close]
#           size = Size of the structuring element.
# fglabl String
#           String used in csv files for foreground. Default 'foreground'
# bglabl String
#           String used in csv files for background. Default 'background'
# mfile String
#           Path were required model is stored. Required with modInfo,
#           video & signal
# sig_rdata Boolean
#           When used the format of the signal output file is R binary
# output String
#           Stuff gets output to this file path. Default depends on generate
# sig_overwrite Boolean
#           Weather to overwrite the signal file or not. Default is NO.
# vid_sbys Boolean
#           This option controls the type of video generated. When present a
#           video of the mask side by side with the original is created.
#           Default is to create only masked videos.
# vid_overwrite Boolean
#           Weather to overwrite the video file or not. Default is NO.
# msgf_sigma Double
#           Standard deviation used to create gaussiand smoothing filter. It
#           is only used in the model calculation. Default is 4.
# msgf_size Integer
#           Size of gauss smoothing filter (in pixels). Used in model
#           calculation. Default is 5 pix 0 means no gaussian smoothing.
# hc_pct Double
#           This is the percent of the total collected data that is used to
#           create the histogram comparison. Valid only with histcmp option.
#           Default is 0.05
# priors foregroundPrior,backgroudPrior
#           Specifies the value of the prior in the Naive Bayesian Model
#           creation. Should idealy add 1. Default is autocalculated. Used in
#           naive bayesian model creation
# remove_too_many Boolean
#           Remove images that contain 'too many' blobs. Decision is based on
#           standard deviation and mean from trained blobs. Default is FALSE.
# remove_too_big Boolean
#           Remove images that are have 'too big' blobs. Decision is based on
#           standard deviation and mean of trained blob size. Default is FALSE.
# adj_mod String
#           Path to the Adjacent model.
# debug Boolean
#           Prints debug information

eip.ecoip <- function ( eipMode, trdir=NULL, tedir=NULL, bins=100, folds=-1,
                       color_space="CIELAB", morphs="", mfile=NULL,
                       fglabl="foreground", bglable="background",
                       sig_rdata=FALSE, output=NULL, sig_overwrite=FALSE,
                       vid_sbys=FALSE, vid_overwrite=FALSE, msgf_sigma=4,
                       msgf_size=5, hc_pct=0.05, priors=NULL,
                       remove_too_many=FALSE, remove_too_big=FALSE,
                       adj_mod=NULL, debug=FALSE )
{
    if (is.null(eipMode))
        stop("=== PLEASE DEFINE THE eipMode ARGUMENT ===\n")
    else
        opts$eipMode = eipMode
    opts$trdir = trdir
    opts$tedir = tedir
    opts$bins = bins
    opts$folds = folds
    opts$color_space = color_space
    opts$morphs = morphs
    opts$morphsList = list()
    opts$fglabl = fglabl
    opts$bglabl = bglabl
    opts$mfile = mfile
    opts$sig_rdata = sig_rdata
    opts$output = output
    opts$sig_overwrite = sig_overwrite
    opts$vid_overwrite = vid_overwrite
    opts$hc_pct = hc_pct
    opts$priors = priors
    opts$remove_too_much = remove_too_much
    opts$remove_too_big = remove_too_big
    opts$adj_mod = adj_mode
    opts$debug = debug

    if (is.null(opts$output) && !is.null(opts$eipMode))
    {
        if ( opts$eipMode == "histcmp" ){
            opts$output = file.path(getwd(), "histcmp.svg")
        } else if (opts$eipMode == "bc_sig" || opts$eipMode == "ma_sig"){
            if (opts$sig_rdata == FALSE)
                opts$output=file.path(getwd(), "signal.txt")
            else
                opts$output=file.path(getwd(), "signal.Rdata")
        } else if (opts$eipMode == "ma_vid" || opts$eipMode == "bc_vid"){
            opts$output=file.path(getwd(), "video.mp4")
        } else
            opts$output="output.txt"
    }

    if ( is.null(opts$priors) )
        opts$priors = list(fg=NULL, bg=NULL)
    else
    {
        ptmp = strsplit(opts$priors, ",")[[1]]
        if ( length(ptmp) != 2 )
            stop ( "You must define 2 prior values for the --priors argument" )

        pfg = as.numeric(ptmp[1])
        pbg = as.numeric(ptmp[2])

        if ( is.na(pfg) || is.na(pbg) )
            stop ( "Both the foreground and background values must be ints" )

        opts$priors = list(fg=pfg, bg=pbg )
    }

    # Check the dependancies in the options.
    if ( opts$eipMode == "DNBM"
        && (is.null(opts$trdir) || is.null(opts$tedir)) )
        stop("=== tedir AND trdir MUST BE DEFINED ===\n")
    if ( ( opts$eipMode == "modInfo" || opts$eipMode == "update"
          || opts$eipMode == "ma_vid" || opts$eipMode == "bc_vid"
          || opts$eipMode == "ma_sig" || opts$eipMode == "bc_sig" )
    && is.null(opts$mfile) )
        stop("=== MUST DEFINE mfile_WHEN USING",opts$eipMode,"  ===\n")
    if ( opts$eipMode == "histcmp" && is.null(opts$trdir) )
        stop("=== trdir MUST BE DEFINED WITH histcmp OPTION ===\n")

    # Check to see if ffmpeg is installed.
    if ( opts$eipMode == "ma_vid" || opts$eipMode == "bc_vid" )
    {
        res = system("ffmpeg -version", ignore.stderr=T, ignore.stdout=T)
        if ( res != 0 )
            stop("=== THE ffmpeg COMMAND MUST BE INSTALLED ===\n")
    }

    # Check file system stuff
    if ( !is.null(opts$trdir) && !file.exists(opts$trdir) )
        stop("=== THE ", opts$trdir, " DIRECTORY DOES NOT EXIST ===\n")
    if ( !is.null(opts$tedir) && !file.exists(opts$tedir) )
        stop("=== THE ", opts$tedir, " DIRECTORY DOES NOT EXIST ===\n")
    if ( file.exists(opts$output) )
        stop("=== THE ", opts$output, " FILE EXISTS. ERASE IT ===\n")
    if ( !is.null(opts$mfile) && !file.exists(opts$mfile) )
        stop("=== THE ", opts$mfile, " FILE DOES NOT EXIST ===\n")
    if ( !is.null(opts$adj_mod) && !file.exists(opts$adj_mod) )
        stop("=== THE ", opts$adj_mod, " FILE DOES NOT EXIST ===\n")

    # Construct the morphs option.
    if ( nchar(opts$morphs) > 0 )
    {
        mstmp = strsplit(opts$morphs, ";")[[1]]
        opts$morphsList = list()
        for ( i in 1:length(mstmp) )
        {
            # Order is shape, size, action
            mtmp = strsplit(mstmp[i], ",")[[1]]
            if ( length(mtmp) != 3 )
                stop ("=== --morphs MUST HAVE shape, size AND action ===\n")
            if ( ! mtmp[1] %in% morphShapes )
                stop ("=== ", mtmp[1], " INVALID SHAPE IN --morphs ===\n")

            ss = as.integer(mtmp[2])
            if ( is.na(ss) )
                stop ("=== ", mtmp[2], " IS NOT AN INTEGER in --morphs ===\n")
            if ( ! mtmp[3] %in% names(morphFuncs) )
                stop ("=== ", mtmp[3], " INVALID ACTION IN --morphs ===\n")

            # action, structuring element
            opts$morphsList[[i]] = common.getStructElem(ss,
                                                        act=mtmp[3],
                                                        type=mtmp[1])
        }
    }

    # FIXME: Should check to see if all the params are in range.
    if ( !is.null(opts$debug) )
        printopts(opts) #print for debugging

    # Execute function
    if ( opts$eipMode == "DNBM" ){
        generate.DNBM(opts)
    } else if ( opts$eipMode == "ma_sig" || opts$eipMode == "bc_sig" ) {
        generate.signal(opts)
    } else if ( opts$eipMode == "bc_vid" || opts$eipMode == "ma_vid") {
        generate.video(opts)
    } else if ( opts$eipMode == "histcmp") {
        generate.histcmp(opts)
    } else if ( opts$eipMode == "modInfo" ){
        generate.modelInformation(opts)
    } else if ( opts$eipMode == "update" ){
        common.update(opts$mfile)
    } else {
        stop("=== THE ", opts$eipMode, " OPTION IS NOT DEFINED ===\n")
    }

    return (0)
}

# Check to see if R environment has everything.
if ( as.integer(R.version[["svn rev"]]) < 57956 )
    stop("=== R REVISION GREATER THAN 57956, INSTALL R 1.15.x ===\n")
