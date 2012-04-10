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
    source("naiveBayes.R")
    nbmI = new.DiscNaiveBayesianModel( "images", "images",
            nbins=200, transform="CIELAB" )
    nbmI$m.generate(nbmI)
    mask = nbmI$m.calcMask(nbmI, "images/img1.jpg")
    checkEquals( sum(mask), 137790 )
}

test.createMaskVideo <- function()
{
    source("naiveBayes.R"); source("imageTrans.R")
    nbmI = new.DiscNaiveBayesianModel( "images", "images",
            nbins=200, transform="CIELAB" )
    nbmI$m.generate(nbmI)

    it = new.ImageTransformer(nbmI$v.testDir, nbmI)
    imgTfm.add2Pipe ( it, list("transfunc"=imgTfm.calcMask,
                                "transargs"=list("G"=NULL)) )
    imgTfm.add2Pipe ( it, list("transfunc"=imgTfm.saveMask,
                               "transargs"=list()) )

    imgTfm.add2Pipe ( it, list("transfunc"=imgTfm.genVid,
                               "transargs"=list("videoname"="images/video.mp4")),
                      indTrans=F )

    res = imgTfm.transform( it )

    checkEquals(res,0)
    checkTrue( file.exists("images/video.mp4") )
    unlink ( "images/video.mp4" ) # cleanup.
}
