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

source("ecoip", chdir=T)

test.defaultModelVars <- function()
{
    unlink("images/263c0d32.Rdata")
    ecoip_exec("--generate=DNBM --trdir=images --tedir=images --folds=3")
    load("images/263c0d32.Rdata")

    checkEquals( dim(self$v.G)[1], 5 )
    checkEquals( dim(self$v.G)[2], 5 )

    checkEquals( self$v.labels$fg, "foreground" )
    checkEquals( self$v.labels$bg, "background" )

    checkEquals( dim(self$v.bins)[1], 101 )
    checkEquals( dim(self$v.bins)[2], 3 )
    checkEquals( self$v.nbins, 100 )

    checkEquals( self$v.nfolds, 3 )

    checkEquals( self$v.outfile, "images/263c0d32.Rdata" )

    checkEquals( self$v.testDir, "images" )

    checkEquals( self$v.transform, "CIELAB" )

    checkTrue( is.null(self$v.priors$fg) )
    checkTrue( is.null(self$v.priors$bg) )

    checkEquals( self$v.type, "dnbm" )

    checkTrue( abs(0.1553446-self$v.model$fperror) < 0.1 )
    checkTrue( abs(0.2333156-self$v.model$fnerror) < 0.1 )

    checkEquals( length(ls(self)), 32 )

    checkEquals( sum(self$v.numBlobs$fg$values==c(1,1)), 2 )
    checkEquals( sum(self$v.numBlobs$bg$values==c(5,3)), 2 )

    checkEquals(sum(self$v.polySize[[self$v.labels$fg]]$values==c(280,280)), 2)
    checkEquals( sum(self$v.polySize[[self$v.labels$bg]]$values ==
                     c(360, 76, 184, 176, 183, 360, 302, 244)), 8 )

    # Clean up
    unlink("images/263c0d32.Rdata")
}

test.msgf <- function()
{
    unlink("images/612deb9d.Rdata")
    ecoip_exec( paste("--generate=DNBM",
                      "--trdir=images --tedir=images",
                      "--msgf_size=11 --msgf_sigma=10") )
    load ( "images/612deb9d.Rdata" )
    G=makeBrush(11, shape="gaussian", sigma=10)

    checkEquals ( sum(self$v.G == G), 121 )

    # Clean up
    unlink("images/612deb9d.Rdata")
}

test.histcmp <- function()
{
    unlink ( "images/histcmp.svg" )
    ecoip_exec( "--generate=histcmp --trdir=images --output=images/histcmp.svg" )
    checkTrue ( file.exists("images/histcmp.svg") )

    # Clean up
    unlink ( "images/histcmp.svg" )
}

test.MaSig <- function()
{
    unlink("images/MaSig.txt")
    unlink("images/b011c4db.Rdata")
    ecoip_exec("--generate=DNBM --trdir=images --tedir=images")
    ecoip_exec( paste("--generate=ma_sig --mfile=images/b011c4db.Rdata",
                      "--tedir=images --output=images/MaSig.txt") )

    checkTrue ( file.exists("images/MaSig.txt") )

    TABLE = read.table("images/MaSig.txt")
    TABLE[,1] = basename(as.character(TABLE[,1]))
    checkEquals( sum(TABLE[,1]==c("2009-01-01-img.jpg", "2009-01-02-img.jpg")), 2 )
    checkEquals(sum(abs(TABLE[,2]- c(0.4015983, 0.2734440)) < c(0.1, 0.1)), 2)

    # Clean up
    unlink("images/MaSig.txt")
    unlink("images/b011c4db.Rdata")
}

test.BcSig <- function()
{
    unlink("images/BcSig.txt")
    unlink("images/b011c4db.Rdata")
    ecoip_exec("--generate=DNBM --trdir=images --tedir=images")
    ecoip_exec( paste("--generate=bc_sig --mfile=images/b011c4db.Rdata",
                      "--tedir=images --output=images/BcSig.txt") )

    checkTrue ( file.exists("images/BcSig.txt") )

    TABLE = read.table("images/BcSig.txt")
    checkEquals( sum(abs(TABLE[,2]- c(3, 4)) < c(2, 2)), 2 )

    TABLE[,1] = basename(as.character(TABLE[,1]))
    checkEquals( sum(TABLE[,1]==c("2009-01-01-img.jpg", "2009-01-02-img.jpg")), 2 )

    # Clean up
    unlink("images/BcSig.txt")
    unlink("images/b011c4db.Rdata")
}

test.BcSigRemove <- function()
{
    unlink("images/BcSig.txt")
    unlink("images/b011c4db.Rdata")
    ecoip_exec("--generate=DNBM --trdir=images --tedir=images")
#    Its commented out because it takes tooooo loooong.
#    ecoip_exec( paste("--generate=bc_sig --mfile=images/b011c4db.Rdata",
#                      "--tedir=images --output=images/BcSig.txt",
#                      "--remove_too_big") )
#
#    checkTrue ( file.exists("images/BcSig.txt") )
#
#    TABLE = read.table("images/BcSig.txt")
#    checkEquals( sum(abs(TABLE[,2]- c(0, 6)) < c(2, 2)), 2 )
#
#    TABLE[,1] = basename(as.character(TABLE[,1]))
#    checkEquals( sum(TABLE[,1]==c("2009-01-01-img.jpg", "2009-01-02-img.jpg")), 2 )
#
#    unlink("images/BcSig.txt")
    ecoip_exec( paste("--generate=bc_sig --mfile=images/b011c4db.Rdata",
                      "--tedir=images --output=images/BcSig.txt",
                      "--remove_too_many") )

    checkTrue ( file.exists("images/BcSig.txt") )

    TABLE = read.table("images/BcSig.txt")
    checkEquals( sum(abs(TABLE[,2]- c(5, 3)) < c(2, 2)), 2 )

    TABLE[,1] = basename(as.character(TABLE[,1]))
    checkEquals( sum(TABLE[,1]==c("2009-01-01-img.jpg", "2009-01-02-img.jpg")), 2 )

    # Clean up
    unlink("images/BcSig.txt")
    unlink("images/b011c4db.Rdata")
}

test.histcmp <- function()
{
    unlink("images/BcSig.txt")
    ecoip_exec ( paste("--generate=histcmp --trdir=images",
                       "--output=images/histcmp.svg") )

    checkTrue ( file.exists("images/histcmp.svg") )

    unlink ("images/histcmp.svg")
}

test.plot <- function ()
{
    source("ecoip_plot", chdir=T)
    unlink("images/MaSig.txt")
    unlink("images/b011c4db.Rdata")
    ecoip_exec("--generate=DNBM --trdir=images --tedir=images")
    ecoip_exec( paste("--generate=ma_sig --mfile=images/b011c4db.Rdata",
                      "--tedir=images --output=images/MaSig.txt") )

    checkTrue ( file.exists("images/MaSig.txt") )

    ecoip_plot ( paste ( "--tfile=images/MaSig.txt --output=images/MaSig.eps",
                "--title=MaSig" ) )

    # Clean up
    unlink("images/MaSig.txt")
    unlink("images/b011c4db.Rdata")
    unlink("images/MaSig.eps")
}
