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

library(getopt)
cmdCmd = "ecoip_exec"

usage <- function( optMat, st=0 )
{
    cat ( "Usage:\n" )
    cat ( cmdCmd, "--generate=[DNBM|video|signal] OPTIONS\n" )
    cat ( "\nOPTIONS\n" )

    for ( i in 1:dim(optMat)[1] )
        cat ( "  [--",optMat[i,1],"|-",optMat[i,2],"]\n",optMat[i,5], sep="")

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
    trpath = file.path("samples","images","training")
    tepath = file.path("samples","images","testing")
    mdpath = file.path("samples","images","training",
                        "8a177586c94f027fb88051702348de24.Rdata" )
    # FIXME: calculate the executable path.

    cat ( "\nThese examples work with the images located in samples.\n" )
    cat ( "For more information on each argument: `ecopi --help`\n" )
    cat ( "\n\tCREATING A MODEL:\n" )
    cat ( "\t",cmdCmd," --generate=DNBM\n",
          "\t\t--train_dir=",trpath,"\n",
          "\t\t--data_dir=",tepath,"\n",
          "\t\t--color_space=CIELAB --folds=4 --bins=200\n", sep="" )

    cat ( "\n\tVISUALIZING THE MODEL:\n" )
    cat ( "\t",cmdCmd," --generate=modInfo\n\t\t--model_file=",mdpath,"\n",
          sep="" )

    cat ( "\n\tCREATING A VIDEO:\n" )
    cat ( "\t",cmdCmd," --generate=video --vid_sbys\n",
          "\t\t--data_dir=",tepath,"\n",
          "\t\t--model_file=",mdpath,"\n", sep="" )

    cat ( "\n\tCREATING A SIGNAL:\n" )
    cat ( "\t",cmdCmd," --generate=signal\n",
          "\t\t--data_dir=",tepath,"\n",
          "\t\t--model_file=",mdpath,"\n", sep="" )

    return (0)
}

printopts <- function (opts)
{
    optnames = names(opts)
    cat ( rep("=",72),"\n", sep="")
    cat ( "Option structure: \n" )
    for ( i in 1:length(optnames) )
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

    if ( !is.null(opts$data_dir) )
        self$v.testDir = opts$data_dir
    self$m.calcMaskSignal ( self, signalname=opts$sig_output, G=G,
                            genRdata=opts$sig_rdata )

    cat ( "\nThe new signal was created at", opts$sig_output, "\n" )
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

    self$m.calcMaskVideo( self, videoname=opts$vid_output,
            together=opts$vid_sbys, G=G ) # FIXME: missing morph.

    cat ( "\nThe new video was created at", opts$vid_output, "\n" )
}

generate.modelInformation <- function(opts)
{
    # This will load self into the current env.
    load(opts$model_file)

    self$m.print(self)
}

ecoip_exec <- function ( arguments = "" )
{
    # FIXME: introduce the morphological stuff somehow.
    optMat = matrix ( data=c(
    "help",     "h",    0, "logical",
        "\tPrints help information\n",

    "examples",  "e",    0, "logical",
        "\tPrints example commands to get you started\n",

    "version",  "v",    0, "logical",
        "\tPrints version information\n",

    "generate", "G",    1, "character",
        paste ( "\t[DNBM|video|signal]. This argument is needed.\n",
                "\tDNBM -> Discreate Naive Bayesian Model.\n",
                "\tmodInfo -> Prints the models info.\n",
                "\tvideo -> A video of the test images. Depends on ffmpeg\n",
                "\tsignal -> Two dim signal of the mean of test masks.\n" ),

    "train_dir", "T",    2, "character",
        "\tPath to training images and csv files. Required with DNBM\n",

    "data_dir", "D",    2, "character",
        paste ( "\tPath to data images. Required with DNBM\n",
                "\tIf undefined for signal or video, the saved dir is used\n" ),

    "bins",      "b",   2, "integer",
        "\tNumber of bins to use for the color signal. Default 100.\n",

    "folds",     "f",   2, "integer",
        "\tNumber of folds for S-fold error calculation, Default -1.\n",

    "color_space","c",  2, "character",
        paste ( "\tColor space in which the calculations are to take place\n",
                "\t[rgb|hsv|CIEXYZ|CIELAB|CIELUV|yCbCr]. Default rgb.\n",
                "\tHas effect only with DNBM"),

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
                "\t0 means no gaussian smoothing.\n" ) ),
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

    # Take care of simple user commands.
    if ( !is.null(opts$help) )
        return (usage(optMat))
    if ( !is.null(opts$version) )
    {
        version()
        return (0)
    }
    if ( !is.null(opts$examples) )
        return (examples())

    # Check the dependancies in the options.
    if ( is.null(opts$generate) )
    {
        cat("=== PLEASE DEFINE THE --generate OPTION ===\n")
        return (usage(optMat, st=1))
    }
    if ( opts$generate == "DNBM"
         && (is.null(opts$train_dir) || is.null(opts$data_dir)) )
    {
        cat("=== DATA_DIR AND TRAIN_DIR MUST BE DEFINED ===\n")
        return (usage(optMat, st=1))
    }
    if ( ( opts$generate == "signal" || opts$generate == "video"
           || opts$generate == "modInfo"  )
         && is.null(opts$model_file) )
    {
        cat("=== MUST DEFINE --model_file_WHEN USING signal OR video  ===\n")
        return (usage(optMat, st=1))
    }

    # Check to see if ffmpeg is installed.
    if ( opts$generate == "video" )
    {
        res = system("ffmpeg -version", ignore.stderr=T, ignore.stdout=T)
        if ( res != 0 )
        {
            cat("=== THE ffmpeg COMMAND MUST BE INSTALLED ===\n")
            return (usage(optMat, st=1))
        }
    }

    # Check file system stuff
    if ( !is.null(opts$train_dir) && !file.exists(opts$train_dir) )
    {
        cat("=== THE", opts$train_dir, "DIRECTORY DOES NOT EXIST ===\n")
        return (usage(optMat, st=1))
    }
    if ( !is.null(opts$data_dir) && !file.exists(opts$data_dir) )
    {
        cat("=== THE", opts$data_dir, "DIRECTORY DOES NOT EXIST ===\n")
        return (usage(optMat, st=1))
    }
    if ( opts$generate == "video" && file.exists(opts$vid_output)
         && !opts$vid_overwrite)
    {
        cat("=== THE", opts$vid_output, "FILE EXISTS. ERASE IT ===\n")
        return (usage(optMat, st=1))
    }
    if ( opts$generate == "signal" && file.exists(opts$sig_output)
         && !opts$sig_overwrite )
    {
        cat("=== THE", opts$sig_output, "FILE EXISTS. ERASE IT ===\n")
        return (usage(optMat, st=1))
    }
    if ( !is.null(opts$model_file) && !file.exists(opts$model_file) )
    {
        cat("=== THE", opts$model_file, "FILE DOES NOT EXIST ===\n")
        return (usage(optMat, st=1))
    }

    # FIXME: Should check to see if all the params are in range.
    printopts(opts) #print for debugging

    # Execute function
    if ( opts$generate == "DNBM" ){
        generate.DNBM(opts)
    } else if ( opts$generate == "signal" ) {
        generate.signal(opts)
    } else if ( opts$generate == "video" ) {
        generate.video(opts)
    } else if ( opts$generate == "modInfo" ){
        generate.modelInformation(opts)
    } else {
        cat("=== THE", opts$generate, "OPTION IS NOT DEFINED ===\n")
        return (usage(optMat, st=1))
    }

    return (0)
}

# Check to see if R environment has everything.
if ( as.integer(R.version[["svn rev"]]) < 57956 )
    cat("=== THE R SVN REVISION MUST BE GREATER THAN 57956 ===\n")
if ( class(try(source("common.R"))) == "try-error"
     || class(try(source("naiveBayes.R"))) == "try-error"
     || class(try(source("colorTrans.R"))) == "try-error" )
{
    cat ( "Make sure you call source with chdir=TURE\n" )
    return (1)
}
if ( require(fields) == FALSE || require(digest) == FALSE )
    cat("=== R MUST HAVE fields and digest INSTALLED ===\n")
if ( require(EBImage) == FALSE )
    cat("=== Install EBImage. Consider instructions at ",
        "http://www.bioconductor.org/packages",
        "/release/bioc/html/EBImage.html ===\n", sep="")

