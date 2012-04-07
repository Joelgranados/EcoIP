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

library(EBImage)
library(digest)
library(fields)

# Globals required for some methods.
morphFuncs = list( "dilate"=dilate, "erode"=erode,
                   "open"=opening, "close"=closing )
morphShapes = c("box", "disc", "diamond")
validImgRegex = ".jpg$|.tiff$|.png$"

# This is painful :(.
# Matlab's coordinate system: (0,0) is in the top left corner. The
#   coordinates are (columns, rows). It doesn't rotate the input image.
# R's coordinate system: (0,0) is in the botton left corner. The coordinates
#   are (rows, columns). For some reason EBImage rotates the image 90
#   degrees on input.
# So, after EBImage's 90 deg rotation and the fact that R's coordinate are
# (rows, columns), Matlab's coordinate work seemlessly. Therefore the input
# csv files should be formated like in Matlab.
getCSV <- function(filename)
{
    if ( !file.exists(filename) )
        stop ( paste("File", filename, "not found.") )

    retL = list()

    # Valid for files created by annotation
    input = try( read.table(filename, skip=4, header=FALSE, sep=",", fill=TRUE),
                 silent=TRUE )

    if ( class(input) == "try-error" )
        return (retL)

    for ( i in 1:length(input[,1]) )
    {
        #col16...-> polygon
        ptemp = input[i,16:length(input[i,])]
        # Remove the NAs from the list.
        ptemp = ptemp[sapply(ptemp, function(x) !any(is.na(x)))]
        numCoor = length(ptemp)/2 # Number of coordinates

        # col1->filename, col3->label
        retL[[i]] = list(
            name=as.character(input[i,1]),
            label=as.character(input[i,3]),
            polygon= matrix( data=ptemp, ncol=2, nrow=numCoor, byrow=TRUE) )
    }

    return (retL)
}

# Construct a list of (csvFile, imgFile) pairs.
getImgCsv <- function(directory)
{
    filePairs = list()

    # Valid image extensions: .jpg, .tiff, .png
    imageFiles = list.files(path=directory, pattern=validImgRegex,
                            full.names=TRUE, ignore.case=TRUE)

    for ( ifOffset in 1:length(imageFiles) )
    {
        imgFile = imageFiles[ifOffset]
        csvFile = paste(imageFiles[ifOffset], ".csv", sep="")

        if ( !file.exists(csvFile) )
            next

        # If csvFile and imgFile exist append to filePairs
        appToCount = length(filePairs)+1
        filePairs[[appToCount]] = list(csv=csvFile, img=imgFile)
    }

    return (filePairs)
}

getDigest <- function(directory, arguments)
{
    if ( !file.exists(directory) )
        stop ( paste("Directory", directory, "not found.") )
    if ( !is.vector(arguments) )
        strop ( "Argument 'arguments' is not a vector" )

    # Create string
    filePairs = getImgCsv(directory)
    if ( length(filePairs) < 1 )
        stop ( paste("Did not get anything out of", directory) )

    dirstr = ""
    for ( i in 1:length(filePairs) )
        dirstr = paste( dirstr,
                        basename(filePairs[[i]]$img),
                        file.info(filePairs[[i]]$img)$size,
                        basename(filePairs[[i]]$csv),
                        file.info(filePairs[[i]]$csv)$size,
                        sep="")

    for ( i in 1:length(arguments) )
        dirstr = paste( dirstr, arguments[i], sep="" )

    return ( digest(dirstr, serialize=F) )
}

# When gwidth is > 0 we filter with gaussian
getRGBMat <- function(filename, retEBimg=F)
{
    if ( !file.exists(filename) )
        stop ( paste("File", filename, "not found.") )

    retImg = readImage(filename)
    if ( !retEBimg )
        retImg = imageData(retImg)

    return (retImg)
}

displayMat <- function (mat)
{
    animate(mat)
}

# Appends csv polygon pixels of img to self$v.pixAccum
appendCSVPixels <- function(self, csv)
{
    isParamInEnv(c("t.img"), self)
    if ( length(dim(self$t.img)) != 3 ) # Dims are: row, cols, and Colorspace
        stop ("The image must have three dimensions.")

    # Color trans is pass-by-ref. Create environment.
    ctEnv = new.env(parent=emptyenv())

    # Get numcolumns and numrows
    nRows = dim(self$t.img)[1]; nCols = dim(self$t.img)[2]

    # Mat has all coordinates. dim(Mat)=[nRows*nCols,2]. Resulting in trans of:
    # ab = [ 1,2,...nRow, 1,2,...nRow,..., 1    ,2    ,...nRow
    #        1,1,...1   , 2,2,...2,   ..., nCols,nCols,...nCol]
    ab=cbind(rep(c(1:nRows),nCols),
             c(matrix(rep(c(1:nCols),nRows), nrow=nRows, ncol=nCols, byrow=T)))

    # Change dimensions so we can call in.poly
    dim(self$t.img) <- c(nRows*nCols,3)
    for (i in 1:length(csv))
    {
        # Append all labels to v.pixAccum. Initialize if nonexistent
        if ( !csv[[i]]$label %in% names(self$v.pixAccum) )
            self$v.pixAccum[[ csv[[i]]$label ]] = NULL

        ctEnv$data = self$t.img[ (in.poly(ab, csv[[i]]$polygon)), ]

        # Transform and asign to v.pixAccum
        self$m.trans( ctEnv )
        self$v.pixAccum[[ csv[[i]]$label ]] =
            rbind(self$v.pixAccum[[ csv[[i]]$label ]], ctEnv$data)
    }
    # Change dimensions back before return
    dim(self$t.img) <- c(nRows, nCols, 3)
}

# Similar to class method for navieBayes instances.
fillPixels <- function (self)
{
    # Check all csv files
    filePairs = getImgCsv(self$v.modelDir)
    for ( i in 1:length(filePairs) )
    {
        cat ( "...", signif(i*100/length(filePairs), 4), "%", sep="", file="")
        flush.console()

        self$t.img = getRGBMat(filePairs[[i]]$img)
        csv = getCSV(filePairs[[i]]$csv)

        # No rows in csv file
        if ( length(csv) == 0 )
            next

        # Remove unwanted labels
        csvtmp = list()
        for ( j in 1:length(csv) )
            if ( csv[[j]]$label %in% self$v.labels )
                csvtmp[[length(csvtmp)+1]] = csv[[j]]
        rm(csvtmp); gc()

        if ( !is.null(self$v.G) && is.matrix(self$v.G) )
            self$t.img = filter2(self$t.img, self$v.G)

        appendCSVPixels(self, csv)
    }

    rm("t.img", envir=as.environment(self))
    rm(csv); gc() # Keep memory clean.

    if ( length(names(self$v.pixAccum)) == 0 )
        stop ("Failed to accumulate any pixels.")
}

# Here, G can be different from self$v.G
calcMask <-function ( self, filename, G=NULL )
{
    if ( !file.exists(filename) )
        stop ( paste("File", filename, "not found.") )
    if ( is.null(self$v.model) )
        stop("You must calculate a model, run generate.")

    env = new.env(parent=emptyenv())
    env$data = getRGBMat(filename)

    if ( !is.null(G) )
        env$data = filter2(env$data, G)

    row_img = dim(env$data)[1]
    col_img = dim(env$data)[2]
    depth_img = dim(env$data)[3]
    # Organize pixels in a vertical vector.
    dim(env$data) <- c(row_img*col_img, depth_img)

    # Transform the image before classifying.
    self$m.trans( env )

    # After this call, data changes.
    imgMask = self$m.classify(self, env)
    dim(imgMask) <- c(row_img, col_img)

    rm(data, envir=as.environment(env)) #Try to keep it clean
    rm(env, row_img, col_img, depth_img);gc()

    return (imgMask)
}

# FIXME: should we pass-by-ref?
# actions = list of action elements. Each consists of 2 sub-elements:
# 1. Morphological operation, 2. Kernel (created with makeBrush)
# Ex: actions = list( list("dilate",G), list("erode",G))
calcMorph <- function ( mask, actions )
{
    if ( sum(mask>1) != 0 || sum(mask<0) != 0 )
        stop ( "The mask must be a binary array" )
    if ( !is.list(actions) )
        stop ( "The actions parameter must be a list" )

    for ( i in 1:length(actions) )
    {
        mfunc = morphFuncs[[ actions[[i]][[1]] ]]
        mask = mfunc ( mask, actions[[i]][[2]] )
    }

    return (mask)
}

# Will use the ffmpeg command in this method. For Windows users go to
# http://code.google.com/p/winff/downloads/list for the ffmpeg app.
generate.MaskVideo <- function( self, videoname=NULL, G=NULL, together=F,
                                morphs = list() )
{
    tmpdir = create.tmpdir()
    FILES = list.files( self$v.testDir, full.names=T,
                        pattern=validImgRegex, ignore.case=TRUE )

    for (i in 1:length(FILES))
    {
        cat ( "...", signif(i*100/length(FILES), 4), "%" , sep="", file="")
        flush.console()
        mask = self$m.calcMask(self, FILES[i], G=G)

        if ( length(morphs) > 0 )
            mask = calcMorph(maks, morphs)

        if ( together ) # combine img with a 3d mask.
        {
            img = readImage(FILES[i])
            mask = combine( img, combine(mask,mask,mask), along=1 )
            colorMode(mask) <- Color
        }

        imgname = file.path ( tmpdir, paste(i,".jpg",sep="") )
        writeImage(mask, imgname)
    }

    # FIXME: check if we created any images....
    # Create video
    if ( is.null(videoname) ) # FIXME: change this arbitrary name...
        videoname = file.path(self$v.testDir, "video.mp4")

    if ( .Platform$OS.type == "windows" )
        cmd = paste("ffmpeg -y -r 2 -b 1800 -i ",
                    "\"", gsub("/", "\\\\", tmpdir), "%d.jpg\" ",
                    "\"", gsub("/", "\\\\", videoname), "\"",
                    sep="")
    else
        cmd = paste("ffmpeg -y -r 2 -b 1800 -i ",
                    tmpdir, "%d.jpg ", videoname, sep="")

    result = system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE)

    # Remove temp dir.
    unlink ( tmpdir, recursive=T, force=T )
    return (result)
}

generate.SignalFromMask <- function( self, signalname=NULL, G=NULL,
                                     morphs=list(), genRdata=F )
{
    FILES = list.files( self$v.testDir, full.names=T,
                        pattern=validImgRegex, ignore.case=TRUE )

    signal = NULL
    for (i in 1:length(FILES))
    {
        cat ( "...", signif(i*100/length(FILES), 4), "%", sep="", file="")
        flush.console()
        mask = self$m.calcMask(self, FILES[i], G=G)

        if ( length(morphs) > 0 )
            mask = calcMorph(maks, morphs)

        signal = rbind ( signal, c ( FILES[i], mean(mask) ) )
    }
    rm(mask); gc()

    if ( length (signal) < 1 )
        stop ( "Signal could not be created." )

    # save the signal file
    if ( genRdata )
        save( signal, file=signalname )
    else
        write.table ( signal, file=signalname, quote=F,
                      row.names=F, col.names=F, sep="\t" )
}

# This is annoying: tempdir() will give current session tempdir. This is used
# by the session and cannot be erased. Don't know how to tmpdir in R :(.
# Try to create a unique tempdir within R's session tempdir.
create.tmpdir <- function ()
{
    randnum = floor(abs(rnorm(1)*10^7))
    tmpdir = file.path(tempdir(), paste(Sys.getpid(),randnum,sep="_"))

    # This is painful: R works with "/" and "\". When using system all path
    # separators should be the same. tmpdir() return an OS specific path. There
    # is a chance of using mixed separators in Win. We try to avoid that here.
    tmpdir = paste( gsub("[\\]", "/", tmpdir), "/", sep="" )
    dir.create(tmpdir, recursive=T)
    return(tmpdir)
}

isParamInEnv <- function( params, env )
{
    if ( length(params) < 1 )
        stop ( "Pass a vector to the checkPassByReferenc method" )
    if ( !is.environment(env) )
        stop ("The env parameter needs to be an environment.")

    objsInEnv = ls(envir=as.environment(env))
    for ( i in 1:length(params) )
        if ( ! params[i] %in% objsInEnv )
            stop (passByRefMessage(
                paste("The", params[i], "var needs to be in env")))
}

passByRefMessage <- function(mess)
{
    return (
        paste("To control memory usage we have implemented pass by reference\n",
              "by using R's environments. When calling one of these functions,\n",
              "first put all of the expected arguments in a new environment;\n",
              "then pass the newly created environment. ?new.env, ?assign, \n",
              "?rm, ?get for more information.\n",
              "Error: ", mess)
        )
}

