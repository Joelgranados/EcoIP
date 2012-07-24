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

test.DiscNaiveBayesianModel_Normal <- function ()
{
    checkEquals( require(digest), TRUE)
    checkEquals( require(fields), TRUE)
    source("common.R")
    source("colorTrans.R")
    source("naiveBayes.R")
    nbmI = new.DiscNaiveBayesianModel( "images", "images", nbins=200,
                nfolds=4, transform="CIELAB" )
    nbmI$m.generate(nbmI)
    checkTrue( abs(0.1730723-nbmI$v.model$fperror) < 0.001 )
    checkTrue( abs(0.2691616-nbmI$v.model$fnerror) < 0.001 )
}

test.DiscNaiveBayesianModel_Filter <- function()
{
    checkEquals( require(EBImage), TRUE )
    checkEquals( require(digest), TRUE)
    checkEquals( require(fields), TRUE)
    source("common.R")
    source("colorTrans.R")
    source("naiveBayes.R")
    G = makeBrush(size=5, shape="gaussian", sigma = 4)
    nbmI = new.DiscNaiveBayesianModel( "images", "images", nbins=200,
                nfolds=4, transform="CIELAB", G=G )
    nbmI$m.generate(nbmI)
    checkTrue( abs(0.09555706-nbmI$v.model$fperror) < 0.001 )
    checkTrue( abs(0.2439016-nbmI$v.model$fnerror) < 0.001 )
}
