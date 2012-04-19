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

ecoip_packages = c("getopt", "fields", "digest", "EBImage", "RSVGTipsDevice")
if ( ! exists("cmdCmd") )
    cmdCmd = "ecoip_exec"

usage <- function( optMat, st=0, long=FALSE )
{
    cat ( "Usage:\n" )
    cat ( cmdCmd, " --generate=",
                    "[DNBM|modInfo|ma_vid|bc_vid|ma_sig|bc_sig]",
                    " OPTIONS\n", sep="" )
    cat ( "\nOPTIONS\n" )

    # If long=TRUE, prints all; else prints numshort
    numshort = 11
    for ( i in 1: (numshort+(long*(dim(optMat)[1]-numshort))) )
        cat ( "  [--",optMat[i,1],"|-",optMat[i,2],"]\n",optMat[i,5], sep="")
    cat ("\n")

    flush.console()
    return (st)
}

version <- function()
{
    cat ( "\tName: @EIP_NAME@\n" )
    cat ( "\tVersion: @EIP_VER_MAJOR@.@EIP_VER_MINOR@.@EIP_VER_DATE@\n" )
}

examples <- function()
{
    treetr = file.path("samples","images","treetr")
    treete = file.path("samples","images","treete")
    flowertr = file.path("samples","images","flowertr")
    flowerte = file.path("samples","images","flowerte")
    treepath = file.path("samples","images","treetr",
                         "8a177586c94f027fb88051702348de24.Rdata" )
    flowerpath = file.path("samples","images","flowertr",
                           "ab0281a9d63d24b33a15de822e790a9b.Rdata" )

    cat ( "\nThese examples work with the images located in samples.\n" )
    cat ( "For more information on each argument: `ecopi --help`\n" )
    cat ( "\n\tCREATING A MODEL:\n" )
    cat ( "\t",cmdCmd," --generate=DNBM\n",
          "\t\t--train_dir=",treetr,"\n",
          "\t\t--data_dir=",treete,"\n",
          "\t\t--color_space=CIELAB --folds=4 --bins=200\n", sep="" )

    cat ( "\n\tCREATING A MODEL FOR BLOB COUNT:\n" )
    cat ( "\t",cmdCmd," --generate=DNBM\n",
          "\t\t--train_dir=",flowertr,"\n",
          "\t\t--data_dir=",flowerte,"\n",
          "\t\t--color_space=CIELAB --folds=4 --bins=200\n", sep="" )

    cat ( "\n\tVISUALIZING THE MODEL:\n" )
    cat ( "\t",cmdCmd," --generate=modInfo\n\t\t--model_file=",treepath,"\n",
          sep="" )

    cat ( "\n\tCREATING A VIDEO:\n" )
    cat ( "\t",cmdCmd," --generate=ma_vid --vid_sbys\n",
          "\t\t--data_dir=",treete,"\n",
          "\t\t--model_file=",treepath,"\n", sep="" )

    cat ( "\n\tCREATING A BLOB COUNT VIDEO:\n" )
    cat ( "\t",cmdCmd," --generate=bc_vid\n",
          "\t\t--morphs=\"disc,5,close;disc,5,open\"\n",
          "\t\t--model_file=",flowerpath,"\n", sep="" )


    cat ( "\n\tCREATING A SIGNAL:\n" )
    cat ( "\t",cmdCmd," --generate=ma_sig\n",
          "\t\t--data_dir=",treete,"\n",
          "\t\t--model_file=",treepath,"\n", sep="" )

    cat ( "\n\tCREATING A BLOB COUNT SIGNAL:\n" )
    cat ( "\t",cmdCmd," --generate=bc_sig\n",
          "\t\t--morphs=\"disc,5,close;disc,5,open\"\n",
          "\t\t--model_file=",flowerpath,"\n", sep="" )

    return (0)
}

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

    dnbm = new.DiscNaiveBayesianModel( opts$train_dir, opts$data_dir,
            nbins=opts$bins, nfolds=opts$folds, transform=opts$color_space,
            labls=lablList, G=G)

    dnbm$m.generate(dnbm)

    if ( !file.exists(dnbm$v.outfile) )
        dnbm$m.save(dnbm)

    cat ( "\nThe new model was created at", dnbm$v.outfile, "\n" )
}

generate.signal <- function(opts)
{
    # Create the smoothing gaussian filter.
    G = NULL
    if ( opts$gf_size > 0 )
        G = makeBrush(  size=opts$gf_size, sigma=opts$gf_sigma,
                        shape="gaussian" )
    # This will load self into the current env.
    load(opts$model_file)

    # Per image pipeline.
    it = new.ImageTransformer(self$v.testDir, self)
    it$m.append ( it, list("transfunc"=it$m.calcMask,
                           "transargs"=list("G"=G)) )

    if ( length(opts$morphsList) > 0 )
        it$m.append( it, list("transfunc"=it$m.calcMorph,
                              "transargs"= list("morphs"=opts$morphsList)) )

    if ( opts$generate == "ma_sig" ) {
        it$m.append ( it, list("transfunc"=it$m.accumMean,
                               "transargs"=list()) )
    } else if ( opts$generate == "bc_sig" ) {
        if ( length(opts$morphsList) < 1 )
        {
            # If blobs and no morphsList, we generate a def morphsList.
            # Default is disc of size mean of fg sizes. Close.
            # Fixme: What if maxPolySize does not contain valid vals.
            opts$morphsList[[1]] = list("dilate",
                    makeBrush(self$v.maxPolySize[[self$v.labels$fg]], "disc"))
            opts$morphsList[[2]] = list("erode",
                    makeBrush(self$v.minPolySize[[self$v.labels$fg]], "disc"))

            it$m.append( it, list("transfunc"=it$m.calcMorph,
                                  "transargs"= list("morphs"=opts$morphsList)) )
            warning("Adding a morphological filter", immediate.=T)
        }
        it$m.append ( it, list("transfunc"=it$m.accumBlobCount,
                               "transargs"=list()) )
    } else
        stop( "Undefined Error" ) # should not reach this.


    # Image Group pipeline
    it$m.append ( it, list("transfunc"=it$m.saveTable,
                           "transargs"=list("tablename"=opts$sig_output,
                                            "genRdata"=opts$sig_rdata)),
                  indTrans=F )

    res = it$m.trans( it )

    if ( res != 0)
        return (1)
    cat ( "\nThe new signal was created at", opts$sig_output, "\n" )
    return (0)
}

generate.video <- function(opts)
{
    # Create the smoothing gaussian filter.
    G = NULL
    if ( opts$gf_size > 0 )
        G = makeBrush(  size=opts$gf_size, sigma=opts$gf_sigma,
                        shape="gaussian" )

    # This will load self into the current env.
    load(opts$model_file)

    if ( !is.null(opts$data_dir) )
        self$v.testDir = opts$data_dir

    # Per image pipeline.
    it = new.ImageTransformer(self$v.testDir, self)
    it$m.append ( it, list("transfunc"=it$m.calcMask,
                            "transargs"=list("G"=G)) )

    if ( length(opts$morphsList) > 0 )
        it$m.append ( it, list("transfunc"=it$m.calcMorph,
                               "transargs"= list("morphs"=opts$morphsList)) )

    if ( opts$generate == "ma_vid" )
    {
        if ( opts$vid_sbys )
            it$m.append ( it, list("transfunc"=it$m.combine,
                                   "transargs"=list()) )
    } else if ( opts$generate == "bc_vid" ) {
        if ( length(opts$morphsList) < 1 )
        {
            # If blobs and no morphsList, we generate a def morphsList.
            # Default is disc of size mean of fg sizes. Close.
            # Fixme: What if maxPolySize does not contain valid vals.
            opts$morphsList[[1]] = list("dilate",
                    makeBrush(self$v.maxPolySize[[self$v.labels$fg]], "disc"))
            opts$morphsList[[2]] = list("erode",
                    makeBrush(self$v.minPolySize[[self$v.labels$fg]], "disc"))

            it$m.append( it, list("transfunc"=it$m.calcMorph,
                                  "transargs"= list("morphs"=opts$morphsList)) )
            warning("Adding a morphological filter", immediate.=T)
        }
        it$m.append ( it, list("transfunc"=it$m.paintImgBlobs,
                               "transargs"=list()) )
    } else
        stop ( "Undefined Error" ) # should not get here

    it$m.append ( it, list("transfunc"=it$m.saveMask,
                           "transargs"=list()) )

    # Image Group pipeline
    it$m.append ( it, list("transfunc"=it$m.genVid,
                           "transargs"=list("videoname"=opts$vid_output)),
                  indTrans=F )

    # Exec the it structure
    res = it$m.trans( it )

    if ( res != 0)
        stop ( "There was an error generating video " )
    cat ( "\nThe new video was created at", opts$vid_output, "\n" )
    return (0)
}

generate.modelInformation <- function(opts)
{
    # This will load self into the current env.
    load(opts$model_file)

    self$m.print(self)
}

ecoip_exec <- function ( arguments = "" )
{
    optMat = matrix ( data=c(
    "help",     "h",    0, "logical",
        "\tPrints help information\n",

    "aid",      "H",    0, "logical",
        "\tPrints the totality of the help information\n",

    "examples",  "e",   0, "logical",
        "\tPrints example commands to get you started\n",

    "version",  "v",    0, "logical",
        "\tPrints version information\n",

    "generate", "G",    1, "character",
        paste ( "\t[DNBM|modInfo|ma_vid|bc_vid|ma_sig|bc_sig]\n",
                "\tDNBM -> Discreate Naive Bayesian Model.\n",
                "\tmodInfo -> Prints the models info.\n",
                "\tma_vid -> A video of the masks. Depends on ffmpeg\n",
                "\tbc_vid -> A video that counts blobs. Depends on ffmpeg\n",
                "\tma_sig -> A signal of masks means.\n",
                "\tbc_sig -> A signal of blob counts.\n",
                "\tThis argument is necessary\n" ),

    "train_dir", "T",    2, "character",
        "\tPath to training images and csv files. Required with DNBM\n",

    "data_dir", "d",    2, "character",
        paste ( "\tPath to data images. Required with DNBM\n",
                "\tIf undefined for signal or video, the saved dir is used\n" ),

    "bins",      "b",   2, "integer",
        "\tNumber of bins to use for the color signal. Default 100.\n",

    "folds",     "f",   2, "integer",
        "\tNumber of folds for S-fold error calculation, Default -1.\n",

    "color_space","c",  2, "character",
        paste ( "\tColor space in which the calculations are to take place\n",
                "\t[rgb|hsv|CIEXYZ|CIELAB|CIELUV|yCbCr|ExG]. Default rgb.\n",
                "\tHas effect only with DNBM\n" ),

    "morphs",   "M",    2, "character",
        paste ( "\t[shape,size,action[;shape,size,action]...]\n",
                "\tSpecify morphological actions. Relevant only in video.\n",
                "\tshape = [box|disc|diamond]\n",
                "\taction = [dilate|erode|open|close]\n",
                "\tsize = Size of the structuring element.\n" ),

    "rinstall", "I",    0, "logical",
        paste ( "\tInstalls needed packages. It will not install imageMagick\n",
                "\tnor GTK+. Need to have admin rights.\n" ),

    "fglabl",    "F",   2, "character", # Foreground label
        "\tString used in csv files for foreground. Default 'foreground'\n",

    "bglabl",    "B",   2, "character", # Background label
        "\tString used in csv files for background. Default 'background'\n",

    "model_file","m",   2, "character", # Path were a model is kept.
        paste ( "\tPath were required model is stored.",
                "\tRequired with modInfo, video & signal\n" ),

    "sig_rdata", "s",   0, "logical", # Format of signal output.
        "\tWhen used the format of the signal output file is R binary\n",

    "sig_output","S",   2, "character", # Output path for signal
        "\tPath of the ouput signal file. Default is signal.{txt,Rdata}\n",

    "sig_overwrite", "1", 0, "logical", #Weather to overwrite the created signal.
        "\tWeather to overwrite the signal file or not. Default is NO.\n",

    "vid_output","V",   2, "character", # Output path for video.
        paste ( "\tPath of the ouput video file. Extension will define format\n",
                "\tCommand uses ffmpeg to convert video. Default is video.mp4\n" ),

    "vid_sbys", "i",    0,  "logical", # Generate a maks and color side by side vid?
        paste ( "\tThis option controls the type of video generated\n",
                "\tWhen present a video of the mask side by side with the original\n",
                "\tis created. Default is to create only masked videos\n"),

    "vid_overwrite", "o", 0, "logical", #Weather to overwrite the created video.
        "\tWeather to overwrite the video file or not. Default is NO.\n",

    "msgf_sigma","W",   2, "double",    # Sigma for model gauss filter
        paste ( "\tStandard deviation used to create gaussiand smoothing filter.\n",
                "\tIt is only used in the model calculation. Default is 4\n" ),

    "msgf_size", "w",   2, "integer",   # Size for model gauss filter
        paste ( "\tSize of gauss smoothing filter (in pixels).\n",
                "\tIt is only used in the model calculation. Default is 5 pix\n",
                "\t0 means no gaussian smoothing.\n" ),

    "gf_sigma", "Z",   2, "double",    # Sigma for video gauss filter
        paste ( "\tStandard deviation used to create gaussiand smoothing filter.\n",
                "\tIt is used in the video or signal generation. Default is 4\n" ),

    "gf_size",  "z",   2, "integer", # Size for video gauss filter
        paste ( "\tSize of gauss smoothing filter (in pixels).\n",
                "\tIt is used in the video or signal generation. Default is 5 pix\n",
                "\t0 means no gaussian smoothing.\n" ),

    "debug",    "D",    0,  "logical", "\tPrints debug information\n" ),

    ncol=5, byrow=T )
    cmdArgs = strsplit(arguments, " ")[[1]]
    opts = getopt ( optMat, opt=cmdArgs )

    # Set the defaults
    if (is.null(opts$bins)) {opts$bins=100}
    if (is.null(opts$folds)) {opts$folds=-1}
    if (is.null(opts$color_space)) {opts$color_space="rgb"}
    if (is.null(opts$fglabl)) {opts$fglabl="foreground"}
    if (is.null(opts$bglabl)) {opts$bglabl="background"}
    if (is.null(opts$msgf_sigma)) {opts$msgf_sigma=4}
    if (is.null(opts$msgf_size)) {opts$msgf_size=5}
    if (is.null(opts$gf_sigma)) {opts$gf_sigma=4}
    if (is.null(opts$gf_size)) {opts$gf_size=5}
    if (is.null(opts$vid_output))
        {opts$vid_output=file.path(getwd(), "video.mp4")}
    if (is.null(opts$vid_sbys)) {opts$vid_sbys=FALSE}
    if (is.null(opts$vid_overwrite)) {opts$vid_overwrite=FALSE}
    if (is.null(opts$sig_rdata)) {opts$sig_rdata=FALSE}
    if (is.null(opts$sig_overwrite)) {opts$sig_overwrite=FALSE}
    if (is.null(opts$sig_output) && opts$sig_rdata == FALSE)
        {opts$sig_output=file.path(getwd(), "signal.txt")}
    if (is.null(opts$sig_output) && opts$sig_rdata == TRUE)
        {opts$sig_output=file.path(getwd(), "signal.Rdata")}
    if (is.null(opts$morphs)) {opts$morphs=""}
    opts$morphsList = list()

    # Take care of simple user commands.
    if ( !is.null(opts$help) )
        return (usage(optMat))
    if ( !is.null(opts$aid) )
        return (usage(optMat, long=TRUE))
    if ( !is.null(opts$rinstall) )
    {
        if ( ecoip_install("fields") == 1
             || ecoip_install("digest") == 1
             || ecoip_install("EBImage") == 1 )
            stop ( "Automatic package install failed" )
    }
    if ( !is.null(opts$version) )
    {
        version()
        return (0)
    }
    if ( !is.null(opts$examples) )
        return (examples())

    # Check the dependancies in the options.
    if ( length(cmdArgs) == 0 )
    {
        return (usage(optMat))
        return (1)
    }
    if ( is.null(opts$generate) )
        stop("=== PLEASE DEFINE THE --generate OPTION ===\n")
    if ( opts$generate == "DNBM"
         && (is.null(opts$train_dir) || is.null(opts$data_dir)) )
        stop("=== DATA_DIR AND TRAIN_DIR MUST BE DEFINED ===\n")
    if ( ( opts$generate == "modInfo"
           || opts$generate == "ma_vid" || opts$generate == "bc_vid"
           || opts$generate == "ma_sig" || opts$generate == "bc_sig" )
         && is.null(opts$model_file) )
        stop("=== MUST DEFINE --model_file_WHEN USING signal OR video  ===\n")

    # Check to see if ffmpeg is installed.
    if ( opts$generate == "ma_vid" || opts$generate == "bc_vid" )
    {
        res = system("ffmpeg -version", ignore.stderr=T, ignore.stdout=T)
        if ( res != 0 )
            stop("=== THE ffmpeg COMMAND MUST BE INSTALLED ===\n")
    }

    # Check file system stuff
    if ( !is.null(opts$train_dir) && !file.exists(opts$train_dir) )
        stop("=== THE ", opts$train_dir, " DIRECTORY DOES NOT EXIST ===\n")
    if ( !is.null(opts$data_dir) && !file.exists(opts$data_dir) )
        stop("=== THE ", opts$data_dir, " DIRECTORY DOES NOT EXIST ===\n")
    if ( ( opts$generate == "ma_vid" || opts$generate == "bc_vid" )
         && file.exists(opts$vid_output) && !opts$vid_overwrite )
        stop("=== THE ", opts$vid_output, " FILE EXISTS. ERASE IT ===\n")
    if ( ( opts$generate == "ma_sig" || opts$generate == "bc_sig" )
         && file.exists(opts$sig_output) && !opts$sig_overwrite )
        stop("=== THE ", opts$sig_output, " FILE EXISTS. ERASE IT ===\n")
    if ( !is.null(opts$model_file) && !file.exists(opts$model_file) )
        stop("=== THE ", opts$model_file, " FILE DOES NOT EXIST ===\n")

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
            opts$morphsList[[i]] = list( mtmp[3], makeBrush(ss, mtmp[1]) )
        }
    }

    # FIXME: Should check to see if all the params are in range.
    if ( !is.null(opts$debug) )
        printopts(opts) #print for debugging

    # Bring in all needed packages and sources
    for ( i in 1:length(ecoip_packages) )
        suppressMessages(library(ecoip_packages[i], character.only=TRUE))
    if ( class(try(source(file.path(ecoip_wd,"common.R")))) == "try-error"
         || class(try(source(file.path(ecoip_wd,"naiveBayes.R"))))=="try-error"
         || class(try(source(file.path(ecoip_wd,"colorTrans.R"))))=="try-error"
         || class(try(source(file.path(ecoip_wd,"imageTrans.R"))))=="try-error")
        stop ( "Make sure you call source with chdir=TURE\n" )

    # Execute function
    if ( opts$generate == "DNBM" ){
        generate.DNBM(opts)
    } else if ( opts$generate == "ma_sig" || opts$generate == "bc_sig" ) {
        generate.signal(opts)
    } else if ( opts$generate == "bc_vid" || opts$generate == "ma_vid") {
        generate.video(opts)
    } else if ( opts$generate == "modInfo" ){
        generate.modelInformation(opts)
    } else {
        stop("=== THE ", opts$generate, " OPTION IS NOT DEFINED ===\n")
    }

    return (0)
}

ecoip_install <- function (package_str)
{
    if ( suppressMessages(require(package_str, character.only=TRUE)) == FALSE )
    {
        cat ("=== You don't have ",package_str, "  installed ===\n",
             "The ",package_str, " package is necessary. Trying to install.\n",
             sep="")

        if ( as.integer(file.access(.libPaths()[1], 2)) == -1 )
            stop ( "\t=== You do not have admin permissions ===\n",
                   "\tYou need to execute R with the admin user.\n",
                   "\tPlease repeat process as administrator.\n" )

        if ( package_str != "EBImage" )
        {
            install.packages(package_str,repos=c("http://cran.us.r-project.org"))
        } else {
            source("http://bioconductor.org/biocLite.R")
            biocLite(package_str)
        }
        suppressMessages(require(package_str, character.only=TRUE))
    }
    return(0)
}

# Check to see if R environment has everything.
if ( as.integer(R.version[["svn rev"]]) < 57956 )
    stop("=== R REVISION GREATER THAN 57956, INSTALL R 1.15.x ===\n")

for ( i in 1:length(ecoip_packages) )
    if ( ! ecoip_packages[i] %in% installed.packages()[,1] )
        if ( ecoip_install(ecoip_packages[i]) == 1 )
            stop ( "Installation of ",ecoip_packages[i]," failed." )
rm(i); gc() # keep it clean

ecoip_wd = getwd()

library(getopt) # So the argument parsing can occur.
