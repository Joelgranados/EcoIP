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

eip.install <- function ()
{
    # Define some vars
    eip.trz = "EcoIP_@EIP_VER_NUM@.tar.gz"
    eip.from = paste("http://sourceforge.net/projects/ecoip/files/@EIP_VER_DATE@/",
                     eip.trz, "/download", sep="")
    eip.to = paste ( "~/", eip.trz, sep="" )

    print(eip.from)
    print(eip.to)
    # Download source.
    download.file ( eip.from, eip.to, method="wget" )

    # Install source.
    install.packages( eip.to )

    # Clean up
    unlink ( eip.to )
}
