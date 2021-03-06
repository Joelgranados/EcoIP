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

test.createMask <- function()
{
    library(EBImage); library(digest); library(fields)
    source("common.R"); source("colorTrans.R"); source("naiveBayes.R");
    nbmI = new.DiscNaiveBayesianModel( "images", "images",
            nbins=200, transform="CIELAB" )
    nbmI$m.generate(nbmI)
    mask = nbmI$m.calcMask(nbmI, "images/2009-01-01-img.jpg")
    print(abs(sum(mask)-137790))
    checkTrue( abs(sum(mask)-137790) < 1000 )
}

test.createMaskVideo <- function()
{
    library(EBImage); library(digest); library(fields)
    source("common.R");source("colorTrans.R");
    source("naiveBayes.R"); source("imageTrans.R");
    nbmI = new.DiscNaiveBayesianModel( "images", "images",
            nbins=200, transform="CIELAB" )
    nbmI$m.generate(nbmI)

    it = new.ImageTransformer(nbmI$v.testDir, nbmI)
    it$m.append ( it, list("transfunc"=it$m.calcMask,
                           "transargs"=list("G"=NULL)) )
    it$m.append ( it, list("transfunc"=it$m.saveMask,
                            "transargs"=list()) )

    it$m.append ( it, list("transfunc"=it$m.genVid,
                           "transargs"=list("videoname"="images/video.mp4")),
                      indTrans=F )

    res = it$m.trans( it )

    checkEquals(res,0)
    checkTrue( file.exists("images/video.mp4") )
    unlink ( "images/video.mp4" ) # cleanup.
}
