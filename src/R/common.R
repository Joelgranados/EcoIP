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

getRGBMat <- function(filename)
{
    if ( !file.exists(filename) )
        stop ( paste("File ", filename, "not found.") )

    retImg = readImage(filename)
    retImg = imageData(retImg)

    return (retImg)
}

displayMat <- function (mat)
{
    animate(mat)
}

getInPolyPixels <- function(img, poligono)
{
    if ( require(fields) == FALSE )
        stop ("Package fields not found. Please install it.")

    # Dimensions are: rows, columns and ColorSpace.
    if ( length(dim(img)) != 3 )
        stop ("The image must have three dimensions.")

    # Get numcolumns and numrows
    nRows = dim(img)[1]
    nCols = dim(img)[2]

    # Create [1,2,...nRows,1,2,...nRows...1,2...nRows]. repeated nCols times
    a = rep(c(1:nRows),nCols)
    # Create [1,...1,2,....2,...nCols,nCols,...nCols]. #s are repeated nRows
    b = c(matrix(rep(c(1:nCols),nRows), nrow=nRows, ncol=nCols, byrow=TRUE))

    # Mat has all coordinates. dim(Mat) = [nRows*nCols,2]
    ab = cbind(a,b)

    # Create binary mat. True when pixel is in poly, false otherwise.
    inMat = matrix(in.poly(ab, poligono),ncol=nCols,nrow=nRows)

    # To visualize the masked image:
    # > img[,,{1,2,3}] = img[,,{1,2,3}]*inMat
    # > displayMat(img)
    pixels = img[,,1][inMat]
    if (dim(img)[3] > 1) # Avoid for(i in 2:1)
        for (i in 2:dim(img)[3])
            pixels = cbind(pixels, img[,,i][inMat])

    rm (a,b,ab, inMat) # Save memory...
    gc()

    return (pixels)
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

# List of all pixels of all images inside directory.
# For every image there is a csv file.
getPixels <- function(directory, label, transform="-")
{
    if ( !exists("colorSpaceFuns" ) )
        source("colorTrans.R")
    if ( !file.exists(directory) )
        stop ( paste("Directory ", directory, "not found.") )
    if ( ! transform %in% names(colorSpaceFuns) )
        stop ( "The transform paramter must be valid" )

    # Accumulator of pixel values
    pixAccum = NULL

    filePairs = getImgCsv(directory)

    # Check all csv files
    for ( i in 1:length(filePairs) )
    {
        csv = getCSV(filePairs[[i]]$csv)
        img = getRGBMat(filePairs[[i]]$img)

        # Check all annotations in csv file
        for (j in 1:length(csv))
        {
            if (csv[[j]]$label!=label)
                next

            # Assign selected pix to RGB
            assign("RGB",
                   getInPolyPixels(img,csv[[j]]$polygon),
                   envir=globalenv() )

            # Transform and asign to pixAccum
            pixAccum = rbind(pixAccum, colorSpaceFuns[[transform]]())
        }
    }
    rm(img, csv) # Keep memory clean.
    gc()

    if (is.null(pixAccum))
        stop ("Failed to accumulate any pixels.")

    return (pixAccum)
}

# The model parameter lets us assume that the needed code is sourced.
calcMask <-function ( filename, model, transform="-" )
{
    if ( !file.exists(filename) )
        stop ( paste("File ", filename, "not found.") )

    modelNames = names(model)
    if ( is.null(modelNames) || !"classifyFunc" %in% modelNames )
        stop("The model parameter must be a model.")

    img = getRGBMat(filename)
    row_img = dim(img)[1]
    col_img = dim(img)[2]
    depth_img = dim(img)[3]
    # Organize pixels in a vertical vector.
    dim(img) <- c(row_img*col_img, depth_img)

    #FIXME: going to ignore transfomm for now. This is where it goes.

    imgMask = model$classifyFunc(nbm, img)
    dim(imgMask) <- c(row_img, col_img)

    rm(img) #Try to keep it clean
    gc()

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

# Checks if params in globalenv
in.globalEnv <- function ( params )
{
    if ( length(params) < 1 )
        stop ( "Pass a vector to the checkPassByReferenc method" )

    objsInEnv = ls(envir=globalenv())
    for ( i in 1:length(params) )
        if ( ! params[i] %in% objsInEnv )
            stop (passByRefMessage(
                paste("The ", params[i], "var needs to be in globalenv")))
}

passByRefMessage <- function(mess)
{
    return (
        paste("To control memory usage we have implemented pass by reference\n",
              "by using R's environments. When calling one of these functions,\n",
              "first put all of the expected arguments in the globalenv().\n",
              "Note that at the end those args will be removed. If they are\n",
              "refed in other environments, they wont be garbage collected.\n",
              "?new.env, ?assign, ?rm, ?get for more information.\n",
              "Error: ", mess)
        )
}

