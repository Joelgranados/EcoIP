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

require(RUnit)

test.DiscNaiveBayesianModel <- function ()
{
    source("naiveBayes.R")
    nbm = generate.DiscNaiveBayesianModel("images", nbins=200, validate=T,
                transform="CIELAB" )
    print(nbm$error)
    checkEquals( signif(nbm$error,7), 0.2020977 )
}
