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

new.ImageTransformer <- function( imgDir, model )
{
    if ( !file.exists(imgDir) )
        stop ( paste("Directory", imgDir, "not found.") )

    # Create the instance
    it = new.env(parent=emptyenv())

    # Create instance vars
    it$type = "Image Transformer"
    it$v.model = model
    it$v.imgDir = imgDir
    it$v.imgList = list.files( imgDir, full.names=T, pattern=validImgRegex,
                               ignore.case=TRUE )
    if (length(it$v.imgList) < 1)
        stop( paste("Directory",imgDir, "contained no images.") )

    # Create transformation list
    it$v.indTrans = list() # Individual transformations. Per image.
    it$v.grpTrans = list() # Group tranfromations. Per image group.

    # Class methods
    it$m.trans = imgTfm.transform
    it$m.append = imgTfm.append
    it$m.calcMask = imgTfm.calcMask
    it$m.calcMorph = imgTfm.calcMorph
    it$m.combine = imgTfm.combine
    it$m.saveMask = imgTfm.saveMask
    it$m.accumMean = imgTfm.accumMean
    it$m.accumBlobCount = imgTfm.accumBlobCount
    it$m.remTooBigBlob = imgTfm.remTooBigBlob
    it$m.remTooManyBlob = imgTfm.remTooManyBlob
    it$m.remRangeBlob = imgTfm.remRangeBlob
    it$m.paintImgBlobs = imgTfm.paintImgBlobs
    it$m.genVid = imgTfm.genVid
    it$m.saveTable = imgTfm.saveTable


    return (it)
}

imgTfm.transform <- function( self )
{
    # tmpenv used as global list
    tmpenv = new.env(parent=emptyenv())
    tmpenv$tmpdir = common.tmpdir()

    iLIST = if(length(self$v.imgList)<=0)
            {NULL}else{1:length(self$v.imgList)}
    for ( i in iLIST )
    {
        cat ( rep(" ",10),"\r",
              signif(i*100/length(self$v.imgList),4),"\t%\r", sep="" )
        flush.console()

        # Assume self$v.indTrans[[1]] will be imgTfm.calcMask
        # Result of for loop will be handled in tmpenv$mask
        jLIST = if(length(self$v.indTrans)<=0)
                {NULL}else{1:length(self$v.indTrans)}
        for ( j in jLIST )
        {
            # function is defined in self$v.indTrans
            transfunc = self$v.indTrans[[j]]$transfunc
            res = transfunc ( self, tmpenv, self$v.imgList[i], i,
                              self$v.indTrans[[j]]$transargs )

            if ( res != 0) # propagate the error
                return (res)
        }
    }
    cat("\n"); flush.console()

    kLIST = if(length(self$v.grpTrans)<=0)
            {NULL}else{1:length(self$v.grpTrans)}
    for ( k in kLIST )
    {
        transfunc = self$v.grpTrans[[k]]$transfunc
        res = transfunc ( self, tmpenv, k, self$v.grpTrans[[k]]$transargs )

        if ( res != 0 )
            return (res)
    }
    return(0)
}

imgTfm.append <- function ( self, elem, indTrans=TRUE )
{
    if ( indTrans )
        self$v.indTrans[[length(self$v.indTrans)+1]] = elem
    else
        self$v.grpTrans[[length(self$v.grpTrans)+1]] = elem
}

# tmpenv = contains the mask (except for calcMask where it is created)
# imgpath = path to the image
# offset = the counter in the main loop. 'i'.
# transargs = specific arguments for the method.
imgTfm.calcMask <- function ( self, tmpenv, imgpath, offset, transargs )
{
    common.InList(c("G"), transargs)

    # Note *1 to change to numerical matrix
    tmpenv$mask = common.calcMask(self$v.model, imgpath, G=transargs$G) * 1
    return (0)
}

imgTfm.calcMorph <- function ( self, tmpenv, imgpath, offset, transargs )
{
    common.InEnv(c("mask"), tmpenv)
    common.InList(c("morphs"), transargs)

    tmpenv$mask = common.calcMorph(tmpenv$mask, transargs$morphs)
    return (0)
}

imgTfm.combine <- function ( self, tmpenv, imgpath, offset, transargs )
{
    common.InEnv(c("mask"), tmpenv)
    # Only accept 2D masks
    if ( length(dim(tmpenv$mask)) != 2 )
        stop ( "Must call imgTfm.combine with a 2D mask only\n" )

    img = readImage(imgpath)
    tmpmask = combine(tmpenv$mask,tmpenv$mask,tmpenv$mask)
    tmpenv$mask = combine( img, tmpmask , along=1 )
    colorMode(tmpenv$mask) <- Color
    rm(img, tmpmask); gc()
    return (0)
}

imgTfm.paintImgBlobs <- function ( self, tmpenv, imgpath, offset, transargs )
{
    common.InEnv(c("mask"), tmpenv)

    tmpenv$mask = bwlabel ( tmpenv$mask )
    xy = computeFeatures.moment(tmpenv$mask)[, c("m.cx", "m.cy")]

    if ( is.null(xy) )  {
        tmpenv$mask = readImage(imgpath)
    } else {
        # This is painful: When computeFeatures.moment returns a 1 row numeric
        # elem, nrow returns NULL (which is bad). To avoid this we use matrix.
        xy = matrix(xy, ncol=2)
        img = readImage(imgpath)
        tmpenv$mask = paintObjects(tmpenv$mask, img)
        font = drawfont(weight=600, size=16)
        tmpenv$mask = drawtext( tmpenv$mask, xy=xy,
                                labels=as.character(1:nrow(xy)), font=font,
                                col="yellow" )
        rm ( img, xy, font); gc()
    }
    return (0)
}

imgTfm.saveMask <- function ( self, tmpenv, imgpath, offset, transargs )
{
    common.InEnv(c("mask", "tmpdir"), tmpenv)
    imgname = file.path ( tmpenv$tmpdir, paste(offset,".jpg",sep="") )
    writeImage(tmpenv$mask, imgname)
    return (0)
}

imgTfm.accumMean <- function ( self, tmpenv, imgpath, offset, transargs )
{
    common.InEnv(c("mask"), tmpenv)

    tmpenv$table = rbind ( tmpenv$table, c(imgpath, mean(tmpenv$mask)) )
    return (0)
}

# Zeros an image if blob "too big". Training polygon size + 3 standard
# deviations of training polygon size is a "too big" blob.
imgTfm.remTooBigBlob <- function ( self, tmpenv, imgpath, offset, transargs )
{
    common.InEnv(c("mask"), tmpenv)
    fgMeanPS = self$v.model$m.getMeanPS(self$v.model, self$v.model$v.labels$fg)
    fgSDPS = self$v.model$m.getSDPS(self$v.model, self$v.model$v.labels$fg)

    seSize = fgMeanPS+(3*fgSDPS)
    if ( seSize > 20 )
        warning( paste("The morphological size is larger than 20.",
                       "This probably means that execution times will be long.",
                       "Try withou the --remove* options or reduce the size",
                       "of the training polygons"), immediate.=T )
    acts = list()
    acts[[1]]=list("open", makeBrush(seSize, "disc"))
    numBlobs = max(bwlabel(common.calcMorph(tmpenv$mask, actions=acts)))

    # FIXME: put num elem calculation in a tmp variable????
    if ( numBlobs > 0 )
    {
        warning("Ignoring number of blobs in ", imgpath, immediate.=T)
        tmpenv$mask[] = 0
    }
    return(0)
}

imgTfm.remTooManyBlob <- function ( self, tmpenv, imgpath, offset, transargs )
{
    common.InEnv(c("mask"), tmpenv)
    fgMeanNB = self$v.model$m.getMeanNB(self$v.model, "fg")
    fgSDNB = self$v.model$m.getSDNB(self$v.model, "bg")
    numBlobs = max(bwlabel(tmpenv$mask))
    if ( numBlobs > fgMeanNB+(3*fgSDNB) )
    {
        warning("Ignoring number of blobs in ", imgpath, immediate.=T)
        tmpenv$mask[] = 0
    }
    return(0)
}

# Granulometries. We treat the morphological structuring elem as a sift.
# Computer Vision and Applications. Bernd Jahne and Horst HauBecker. p496.
# Remove all elements smaller than min and larger than max.
# Elements could be wrongly eliminated if they clumped together to form a big
# blob. Also if elements are smaller than min.
imgTfm.remRangeBlob <- function ( self, tmpenv, imgpath, offset, transargs )
{
    common.InEnv(c("mask"), tmpenv)

    fglabl = self$v.model$v.labels$fg
    fgMeanPS = self$v.model$m.getMeanPS(self$v.model, fglabl)
    fgSDPS = self$v.model$m.getSDPS(self$v.model, fglabl)

    # eliminate less than min elems
    acts = list()
    acts[[1]] = common.getStructElem(fgMeanPS-(3*fgSDPS), act="open")
    siftMin = common.calcMorph(tmpenv$mask, actions=acts)

    # eliminate more than max elems
    acts[[1]] = common.getStructElem(fgMeanPS+(3*fgSDPS), act="open")
    siftMax = common.calcMorph(tmpenv$mask, actions=acts)

    tmpenv$mask = siftMin - siftMax
    tmpenv$mask[tmpenv$mask<0] = 0
    rm(siftMin, siftMax); gc()
    return(0)
}

# FIXME: make transargs and transfunc shorter.
# FIXME: accumBlobCount and accumMean conflict. they use table.
imgTfm.accumBlobCount <- function ( self, tmpenv, imgpath, offset, transargs )
{
    common.InEnv(c("mask"), tmpenv)

    # FIXME: look at precalculated option to speed up
    numblobs = max(bwlabel(tmpenv$mask))
    tmpenv$table = rbind ( tmpenv$table, c(imgpath, numblobs) )
    return(0)
}

# http://www.imagemagick.org/script/binary-releases.php#windows
imgTfm.genVid <- function ( self, tmpenv, offset, transargs )
{
    common.InEnv(c("tmpdir"), tmpenv)
    ARGS = c("videoname", "framerate", "bitrate")
    DEFS = c(file.path(self$v.model$v.testDir, "video.mp4"), 2, 1800)
    transargs = common.InList(ARGS, transargs, defVals=DEFS)

    if ( .Platform$OS.type == "windows" )
        cmd = paste("ffmpeg -y -r ", transargs$framerate,
                    " -b ", transargs$bitrate, " -i ",
                    "\"", gsub("/", "\\\\", tmpenv$tmpdir), "%d.jpg\" ",
                    "\"", gsub("/", "\\\\", transargs$videoname), "\"",
                    sep="")
    else
        cmd = paste("ffmpeg -y -r ", transargs$framerate,
                    " -b ", transargs$bitrate, " -i ",
                    tmpenv$tmpdir, "%d.jpg ", transargs$videoname, sep="")

    result = system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE)

    return (result)
}

imgTfm.saveTable <- function ( self, tmpenv, offset, transargs )
{
    common.InEnv(c("table"), tmpenv)
    ARGS = c("tablename", "genRdata")
    DEFS = c(file.path(self$v.model$v.testDir, "table"), FALSE)
    transargs = common.InList(ARGS, transargs, defVals=DEFS)

    # FIXME: check tables class.
    if ( length (tmpenv$table) < 1 )
        stop ( "table has no elements in imgTfm.saveTable\n" )

    if ( transargs$genRdata )
        save( tmpenv$table, file=transargs$tablename )
    else
        write.table ( tmpenv$table, file=transargs$tablename, quote=F,
                      row.names=F, col.names=F, sep="\t" )

    return (0)
}

#FIXME: put title, ylab, xlab, linetype, filetype in transargs
imgTfm.genPlot <- function ( self, tmpenv, offset, transargs )
{
    common.InEnv(c("table"), tmpenv)
    ARGS = c("type", "color", "xlab", "ylab", "width", "height", "name",
             "title")
    DEFS = c("l", "red", "Time", "Value", 1024, 768, "plot.svg",
             "Title")
    transargs = common.InList( ARGS, transargs, defVals=DEFS )

    devSVGTips(file=transargs$name, width=transargs$width,
               height=transargs$height)
    plot(tmpenv$table[,2], pch=21, xlab=transargs$xlab, ylab=transargs$ylab,
         type=transargs$type, col=transargs$color, axes=FALSE,
         main=transargs$title)
    axis(1, at=NULL, labels=TRUE)
    axis(2)
    dev.off()
}

# Helper function: Returns number of elements in tmpenv$mask given a
# Structuring element size seSize
imgTfm.numMorphElem <- function( self, tmpenv, seSize )
{
    if ( seSize > 20 )
        warning( paste("The morphological size is larger than 20.",
                       "This probably means that execution times will be long.",
                       "Try withou the --remove* options or reduce the size",
                       "of the training polygons"), immediate.=T )
    acts = list()
    acts[[1]]=list("open", makeBrush(seSize, "disc"))
    return ( max(bwlabel(common.calcMorph(tmpenv$mask, actions=acts))) )
}
