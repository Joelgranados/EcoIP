#!/usr/bin/Rscript
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

# Check arguments
arguments = commandArgs(trailingOnly=T)
if (length(arguments) < 1)
{
    print ( paste ( "Did not receive any args." ) )
    q(sa='no', st=1)
}

# Check the test file
test_name = paste(arguments[1],".R", sep="")
if ( !file.exists(test_name) )
{
    print ( paste( "File",test_name,"does not exists" ) )
    q(sa='no', st=1)
}

# Run the test
if ( require(RUnit) == FALSE )
    q(sa='no', st=1)

res = runTestFile(test_name)
if (res[[1]]$nFail != 0 || res[[1]]$nErr != 0)
    q(sa='no',st=1)
q(sa='no',st=0)
