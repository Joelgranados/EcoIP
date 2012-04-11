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
    it$v.model = model
    it$v.imgDir = imgDir
    it$v.imgList = list.files( imgDir, full.names=T, pattern=validImgRegex,
                               ignore.case=TRUE )

    # Create transformation list
    it$v.indTrans = list() # Individual transformations. Per image.
    it$v.grpTrans = list() # Group tranfromations. Per image group.

    # Class methods
    it$m.trans = imgTfm.transform
    it$m.append = imgTfm.append
    it$m.calcMask = imgTfm.calcMask
    it$m.calcMorph = imgTfm.calcMorphs
    it$m.combine = imgTfm.combine
    it$m.saveMask = imgTfm.saveMask
    it$m.accumMean = imgTfm.accumMean
    it$m.genVid = imgTfm.genVid
    it$m.saveTable = imgTfm.saveTable

    return (it)
}

imgTfm.transform <- function( self )
{
    # tmpenv used as global list
    tmpenv = new.env(parent=emptyenv())
    tmpenv$tmpdir = common.tmpdir()

    # FIXME: the for will execute wtih length 0
    for ( i in 1:length(self$v.imgList) )
    {
        cat ( "...", signif(i*100/length(self$v.imgList), 4), "%" , sep="")
        flush.console()

        # Assume self$v.indTrans[[1]] will be imgTfm.calcMask
        # Result of for loop will be handled in tmpenv$mask
        for ( j in 1:length(self$v.indTrans) )
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

    # FIXME: the for will execute wtih length 0
    for ( k in 1:length(self$v.grpTrans) )
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
    # check transargs
    if ( ! "G" %in% names(transargs) )
    {
        cat ( "G was undefined in imgTfm.calcMask\n" )
        return (1)
    }

    tmpenv$mask = common.calcMask(self$v.model, imgpath, G=transargs$G)
    return (0)
}

imgTfm.calcMorphs <- function ( self, tmpenv, imgpath, offset, transargs )
{
    common.InEnv(c("mask"), tmpenv)
    # check transargs
    if ( ! "morphs" %in% names(transargs) )
    {
        cat ( "morphs was undefined in imgTfm.calcMorphs\n" )
        return (1)
    }

    tmpenv$mask = common.calcMorph(tmpenv$mask, transargs$morphs)
    return (0)
}

imgTfm.combine <- function ( self, tmpenv, imgpath, offset, transargs )
{
    common.InEnv(c("mask"), tmpenv)
    # Only accept 2D masks
    if ( length(dim(tmpenv$mask)) != 2 )
    {
        cat ( "Must call imgTfm.combine with a 2D mask only\n" )
        return (1)
    }

    img = readImage(imgpath)
    tmpmask = combine(tmpenv$mask,tmpenv$mask,tmpenv$mask)
    tmpenv$mask = combine( img, tmpmask , along=1 )
    colorMode(tmpenv$mask) <- Color
    rm(img, tmpmask); gc()
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
    common.InEnv(c("mask","table"), tmpenv)
    if ( ! "table" %in% ls(envir=as.environment(tmpenv)) )
        tmpenv$table = NULL
    tmpenv$table = rbind ( tmpenv$table, c(imgpath, mean(tmpenv$mask)) )
    flush.console()
    return (0)
}

# http://www.imagemagick.org/script/binary-releases.php#windows
imgTfm.genVid <- function ( self, tmpenv, offset, transargs )
{
    common.InEnv(c("tmpdir"), tmpenv)
    # FIXME: change this arbitrary name...
    if ( ! "videoname" %in% names(transargs) )
        transargs$videoname = file.path(self$v.model$v.testDir, "video.mp4")

    if ( .Platform$OS.type == "windows" )
        cmd = paste("ffmpeg -y -r 2 -b 1800 -i ",
                    "\"", gsub("/", "\\\\", tmpenv$tmpdir), "%d.jpg\" ",
                    "\"", gsub("/", "\\\\", transargs$videoname), "\"",
                    sep="")
    else
        cmd = paste("ffmpeg -y -r 2 -b 1800 -i ",
                    tmpenv$tmpdir, "%d.jpg ", transargs$videoname, sep="")

    result = system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE)

    return (result)
}

imgTfm.saveTable <- function ( self, tmpenv, offset, transargs )
{
    common.InEnv(c("table"), tmpenv)
    if ( ! "table" %in% ls(envir=as.environment(tmpenv)) )
    {
        cat ( "table was undefined in imgTfm.saveTable\n" )
        return (1)
    }
    if ( length (tmpenv$table) < 1 )
    {
        cat ( "table has no elements in imgTfm.saveTable\n" )
        return (1)
    }

    if ( ! "tablename" %in% names(transargs) )
        transargs$tablename = file.path(self$v.model$v.testDir, "table.txt")

    # FIXME: check tables class.
    if ( ! "genRdata" %in% names(transargs) )
        transargs$genRdata = FALSE

    if ( transargs$genRdata )
        save( tmpenv$table, file=transargs$tablename )
    else
        write.table ( tmpenv$table, file=transargs$tablename, quote=F,
                      row.names=F, col.names=F, sep="\t" )

    return (0)
}
