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

# Globals required for some methods.
if ( require(EBImage) == FALSE )
    stop ("Package EBImage not found. Please install.")
morphFuncs = list( "dilate"=dilate, "erode"=erode,
                   "open"=opening, "close"=closing )
morphShapes = c("box", "disc", "diamond")

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
        stop ( paste("File ", filename, "not found.") )

    # Valid for files created by annotation
    input = read.csv(filename, skip=4, header=FALSE)

    retL = list()

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

# When gwidth is > 0 we filter with gaussian
getRGBMat <- function(filename, retEBimg=F)
{
    if ( !file.exists(filename) )
        stop ( paste("File ", filename, "not found.") )

    retImg = readImage(filename)
    if ( !retEBimg )
        retImg = imageData(retImg)

    return (retImg)
}

displayMat <- function (mat)
{
    animate(mat)
}

# imgEnv is environment with parent "empty" and contains img
# Must return pixels to call multiple times for same image.
getInPolyPixels <- function(imgEnv, poligono)
{
    isParamInEnv(c("img"), imgEnv)
    if ( require(fields) == FALSE )
        stop ("Package fields not found. Please install it.")
    if ( length(dim(imgEnv$img)) != 3 ) # Dims are: row, cols, and Colorspace
        stop ("The image must have three dimensions.")

    # Get numcolumns and numrows
    nRows = dim(imgEnv$img)[1]
    nCols = dim(imgEnv$img)[2]

    # Mat has all coordinates. dim(Mat)=[nRows*nCols,2]. Resulting in trans of:
    # ab = [ 1,2,...nRow, 1,2,...nRow,..., 1    ,2    ,...nRow
    #        1,1,...1   , 2,2,...2,   ..., nCols,nCols,...nCol]
    ab=cbind(rep(c(1:nRows),nCols),
             c(matrix(rep(c(1:nCols),nRows), nrow=nRows, ncol=nCols, byrow=T)))

    # in.poly -> True if{coordinate in polygon}, False otherwize.
    dim(imgEnv$img) <- c(nRows*nCols,3)
    pixRet = imgEnv$img[ (in.poly(ab, poligono)), ]
    dim(imgEnv$img) <- c(nRows, nCols, 3)

    rm (ab);gc() # Save memory...

    return (pixRet)
}

# Appends csv polygon pixels of img to pixAccum
# env is environment. env$img and env$pixAccum
appendCSVPixels <- function(csv, env, transform="-")
#FIXME: transform should not be here
{
    isParamInEnv(c("img", "pixAccum"), env)
    if ( require(fields) == FALSE )
        stop ("Package fields not found. Please install it.")
    if ( length(dim(env$img)) != 3 ) # Dims are: row, cols, and Colorspace
        stop ("The image must have three dimensions.")

    # Color trans is pass-by-ref. Create environment.
    ctEnv = new.env(parent=emptyenv()) #FIXME remove with transform

    # Get numcolumns and numrows
    nRows = dim(env$img)[1]; nCols = dim(env$img)[2]

    # Mat has all coordinates. dim(Mat)=[nRows*nCols,2]. Resulting in trans of:
    # ab = [ 1,2,...nRow, 1,2,...nRow,..., 1    ,2    ,...nRow
    #        1,1,...1   , 2,2,...2,   ..., nCols,nCols,...nCol]
    ab=cbind(rep(c(1:nRows),nCols),
             c(matrix(rep(c(1:nCols),nRows), nrow=nRows, ncol=nCols, byrow=T)))

    # Change dimensions so we can call in.poly
    dim(env$img) <- c(nRows*nCols,3)
    for (i in 1:length(csv))
    {
        # Append all labels to pixAccum. Initialize if nonexistent
        if ( !csv[[i]]$label %in% names(env$pixAccum) )
            env$pixAccum[[ csv[[i]]$label ]] = NULL

        ctEnv$img = env$img[ (in.poly(ab, csv[[i]]$polygon)), ]

        # Transform and asign to pixAccum
        colorSpaceFuns[[transform]]( ctEnv )
        env$pixAccum[[ csv[[i]]$label ]] =
            rbind(env$pixAccum[[ csv[[i]]$label ]], ctEnv$img)
    }
    # Change dimensions back before return
    dim(env$img) <- c(nRows, nCols, 3)
}

# Construct a list of (csvFile, imgFile) pairs.
getImgCsv <- function(directory)
{
    filePairs = list()

    # Valid image extensions: .jpg, .tiff, .png
    imageFiles = list.files(path=directory, pattern=".jpg|.tiff|.png",
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

# Get all pixels of all images inside dir. Every image has a csv file.
getPixels <- function(directory, transform="-", gparams=list())
{
    if ( !exists("colorSpaceFuns" ) )
        source("colorTrans.R")
    if ( !file.exists(directory) )
        stop ( paste("Directory ", directory, "not found.") )
    if ( ! transform %in% names(colorSpaceFuns) )
        stop ( "The transform paramter must be valid" )

    # appendCSVPixels is pass-by-ref. Create environment.
    env = new.env(parent=emptyenv())

    # Accumulator of pixel values
    env$pixAccum = list()

    # Check all csv files
    filePairs = getImgCsv(directory)
    for ( i in 1:length(filePairs) )
    {
        print( paste(i, " of ", length(filePairs))); flush.console()

        csv = getCSV(filePairs[[i]]$csv)
        env$img = getRGBMat(filePairs[[i]]$img)

        # FIXME: gblur doc suggests filter2
        if ( length(gparams) == 2 )
            env$img = gblur(env$img, r=gparams$r, s=gparams$s)

        appendCSVPixels(csv, env, transform)
    }

    rm("img", envir=as.environment(env))
    rm(csv); gc() # Keep memory clean.

    if ( length(names(env$pixAccum)) == 0 )
        stop ("Failed to accumulate any pixels.")

    return (env$pixAccum)
}

# The model parameter lets us assume that the needed code is sourced.
calcMask <-function ( filename, model, transform="-", gparams=list() )
{
    if ( !file.exists(filename) )
        stop ( paste("File ", filename, "not found.") )

    modelNames = names(model)
    if ( is.null(modelNames) || !"classifyFunc" %in% modelNames )
        stop("The model parameter must be a model.")

    if ( !exists("colorSpaceFuns" ) )
        source("colorTrans.R")

    env = new.env(parent=emptyenv())
    env$img = getRGBMat(filename)
    # FIXME: gblur doc suggests filter2
    if ( length(gparams) == 2 )
        env$img = gblur(env$img, r=gparams$r, s=gparams$s)
    row_img = dim(env$img)[1]
    col_img = dim(env$img)[2]
    depth_img = dim(env$img)[3]
    # Organize pixels in a vertical vector.
    dim(env$img) <- c(row_img*col_img, depth_img)

    # Transform the image before classifying.
    colorSpaceFuns[[transform]]( env )

    # After this call img is changed.
    imgMask = model$classifyFunc(model, env)
    dim(imgMask) <- c(row_img, col_img)

    rm(img, envir=as.environment(env)) #Try to keep it clean
    rm(env);gc()

    return (imgMask)
}

# The actions parameter is a list of action elements. Each action element
# consists in 3 sub-elements:
# 1. Morphological operation, 2. Kernel shape and 3.  Kernel size.
# Ex: actions = list( list("dilate","box",3), list("erode","diamond",6))
morphologyMask <- function ( mask, actions )
{
    if ( class(actions) != "list" )
        stop ( "The actions parameter must be a list" )

    # Check to see if mask is binary
    if ( sum(mask>1) != 0 || sum(mask<0) != 0 )
        stop ( "The mask must be a binary array" )

    for ( i in 1:length(actions) )
    {
        tmpFunc = morphFuncs[[ actions[[i]][[1]] ]]
        tmpKern = makeBrush( actions[[i]][[3]], shape=actions[[i]][[2]] )
        mask = tmpFunc ( mask, tmpKern )
    }

    return (mask)
}

generateImgSequence <- function ( directory, model, actions=list(),
                                  transform="-", sideByside=F, gparams=list() )
{
    if ( !exists("colorSpaceFuns" ) )
        source("colorTrans.R")
    if ( !file.exists(directory) )
        stop ( paste("Directory ", directory, "not found.") )
    if ( ! transform %in% names(colorSpaceFuns) )
        stop ( "The transform paramter must be valid" )
    if ( class(actions) != "list" )
        stop ( "The actions parameter must be a list" )

    FILES = list.files(directory, full.names=T)

    for (i in 1:length(FILES))
    {
        img = readImage(FILES[i])
        mask = calcMask(model, transform=transform, filename=FILES[i],
                        gparams=gparams)

        if ( length(actions) > 0 )
            mask = morphologyMask(mask, actions)

        if ( sideByside )
        {
            dimMask = dim(mask)
            dim(mask) <- c(dimMask[1]*dimMask[2],1)
            mask = cbind(mask, mask, mask)
            dim(img) <- c(dimMask[1]*dimMask[2],3)
            mask = rbind(mask, img)
            dim(mask) = c(dimMask[1], dimMask[2]*2, 3)
            mask = Image(mask)
            colorMode(mask) <- Color
        }

        writeImage(mask, file=paste(i,".jpg",sep=""))
    }
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
                paste("The ", params[i], "var needs to be in env")))
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

