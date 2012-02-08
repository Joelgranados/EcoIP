# Data Generator Scripts.
# Copyright (C) 2012 Eric Graham <egraham@cens.ucla.edu>
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

import os
import sys
import csv
import Image
import ImageChops
from numpy import *
from scipy import *
import math
import ColorConverter
from browse_files import *

def main():

    # This program will compare two masks for one image.  Masks are
    # mutually exclusive but may not complete, in that the foreground is
    # a subset of all possible foregrounds.  Thus, a foreground mask
    # of leaves may not have all leaves selected.

    # *****************************************************************
    # Open dialog to find the photo subdirectories.
    # Each subfolder holds mask files (.bmp)that have the same name as
    # each photo (.jpg).
    # Also, each subfolder should have a number of .csv files that hold
    # the data look-up table frequencies for processing images.
    # *****************************************************************

    photoPath = getMainDir(message='Select the photo directory: ')
    if photoPath == '':
        print("\nNo directory selected, program aborted.\n")
        return

    fgMaskDir = getMainDir(message='Select the foreground mask directory: ')
    if fgMaskDir == '':
        print("\nNo directory selected, program aborted.\n")
        return

    bgMaskDir = getMainDir(message='Select the background mask directory: ')
    if bgMaskDir == '':
        print("\nNo directory selected, program aborted.\n")
        return

    write256 = True

    # *****************************************************************
    # Load the 1-D, 2-D, and 3-D color spaces probabilities into a
    # dictionary with the color space tuples (XX), (XX,YY), or (XX,YY,ZZ)
    # as keys
    # *****************************************************************

    print 'Working on', fgMaskDir

    # get CSV files
    fgCsvFiles = [name for name in os.listdir(fgMaskDir) if name.endswith(".csv")]

    # get the mask file
    fgMaskFiles = [name for name in os.listdir(fgMaskDir) if name.endswith(".bmp")]

    for fgCsvFile in fgCsvFiles:  #  for each color space file...
        print("Data array name: %s"%fgCsvFile)
        allPixelMasterArray = []  # for keeping track of pixels in mask
        allPixelMasterArray.append(
                ['Path and filename',
                 'number pixels examined',
                 'number of black pixels',
                 'foremask pixels',
                 'correct foreground',
                 'incorrect foreground',
                 'correct background',
                 'segments before clean',
                 'segments after clean',
                 'foreBigNans',
                 'QsegFore',
                 'QsegBack'])
        # the dictonary that will hold the probabilities
        fgProbDict = {}
        tmpFd = open(os.path.join(fgMaskDir, fgCsvFile), 'rb')
        # open the data file
        fgCsvReader = csv.reader(tmpFd, delimiter = ',')
        # File format is: Xcol, [Ycol], frequency, probability, count
        fgCsvReader.next() #Skip header row
        for fgCsvRow in fgCsvReader:
            if len(fgCsvRow) == 4:
                index = (int(float(fgCsvRow[0])),)
            elif len(fgCsvRow) == 5:
                index = (int(float(fgCsvRow[0])), int(float(fgCsvRow[1])))
            elif len(fgCsvRow) == 6:
                index = (int(float(fgCsvRow[0])), int(float(fgCsvRow[1])),
                        int(float(fgCsvRow[2])))
            probOffset = len(fgCsvRow) - 2 # its always the penultimate.
            fgProbDict[index] = float(fgCsvRow[probOffset])

        tmpFd.close()

# *****************************************************************
# Start processing individual image files that are associated with
# masks
# *****************************************************************

        # loop through the files and get the mask files
        for fgMaksFile in fgMaskFiles:
            foreMaskPathFilename = os.path.join(fgMaskDir, fgMaksFile)
            backMaskPathFilename = os.path.join(bgMaskDir, fgMaksFile)

            #  make the path and file name for opening a file in a subdir
            imagePathFilename = \
                    os.path.splitext(os.path.join(photoPath,fgMaksFile))[0] + '.jpg'

            print("\nProcessing mask:%s and image %s" \
                    %(fgMaksFile,imagePathFilename))

            # open the image and mask files
            foregroundImage = Image.open(imagePathFilename)
            foregroundImage.load()

            # get image dimensions
            imSize = foregroundImage.size

            # split image into RGB
            foregroundSource = foregroundImage.split()

            # list for creating a mask for segmenting (0 or 255)
            varMaskList = list(foregroundSource[0].getdata())

            # list for generating a probability image (0 to 255)
            varProbList = []

            # get sequential pixel values for every pixel in image
            foreR = list(foregroundSource[0].getdata())
            foreG = list(foregroundSource[1].getdata())
            foreB = list(foregroundSource[2].getdata())

            foregroundImage = ''
            foregroundSource = ''

# *****************************************************************
#  convert each pixel RGBs to different color spaces
# *****************************************************************

            print("Converting colors and matching with array in %s"%fgCsvFile)

            # if a color is not encountered, keep a record of "NaN's"
            foreBigNans = 0
            blackPixelCount = 0
            count = 0
            # send pixel data to subroutines to calculate colors
            for i in range(0, len(foreR)):
                r = foreR[i]/255.0
                g = foreG[i]/255.0
                b = foreB[i]/255.0
                # process only if not black (mask is black)
                if not (r == 0 and g == 0 and b == 0):
                    count = count + 1

                    # set the appropriate color component equal
                    # to the variables XX, YY, or ZZ for tallying
                    (tupleFlag, XX, YY, ZZ) = \
                            getColorComponent(fgCsvFile=fgCsvFile):


                    # *******************************************************
                    # find pixel values in the probability array that was
                    # imported and add to a list
                    # *******************************************************
                    if tupleFlag == 0:
                        index = (int(float(XX)),)
                    elif tupleFlag == 1:
                        index = (int(float(XX)),int(float(YY)))
                    elif tupleFlag == 2:
                        index = (int(float(XX)),int(float(YY)),int(float(ZZ)))

                    if index in fgProbDict:
                        foreProbability = fgProbDict[index]
                    else:
                        foreBigNans = foreBigNans + 1
                        foreProbability = 0

                # r=0, g=0, b=0: too dark, count as background,
                # but keep track of the number
                else:
                    foreProbability = 0
                    blackPixelCount = blackPixelCount + 1

                # put the probability value into a list for all pixels
                varProbList.append(foreProbability)

                # create the mask image from the probabilities, > 50% indicates foreground
                if foreProbability > 0.5:
                    varMaskList[i] = 1
                else:
                    varMaskList[i] = 0

            print 'Cleaning and segmenting...'

# *****************************************************************
#  write the pixels back to image files  --  Comment out to not write to disk
# *****************************************************************

##                        print 'Saving image files...'
##                        # put the mask list into the range 0 - 255
##                        varMaskList = array(varMaskList) * 255
##                        foregroundImage = Image.new('1', imSize, 'white')
##                        #  var list should be 255 for preserve and 0 for black
##                        foregroundImage.putdata(varMaskList)
##
##                        foregroundImage.save(imagePathFilename + fgCsvFile + '_pixel-masked.bmp')
##                        foregroundImage = ImageChops.invert(foregroundImage)
##
##                        #  put the probability list into the range 0 - 255
##                        varProbList = array(varProbList) * 255
##                        newProbImage = Image.new('L', imSize, 'white')
##                        #  probability list is greyscale
##                        newProbImage.putdata(varProbList)
##                        newProbImage.save(foreMaskPathFilename + fgCsvFile + '_probability.bmp')
##
##                        varProbList = []

# *****************************************************************
#  Segment Image to count blobs, first before then after removing some "noise"
# *****************************************************************

            #  structuring element used as a mask for determining blobs
            structElement = array([[1,1,1], [1,1,1], [1,1,1]])

            #  structuring element used for binary dialation
            structDialationElement = array([[0,1,0], [1,1,1], [0,1,0]])

            varMaskList = array(varMaskList)  #  turn to a numpy array

            # make it the image shape -- note width x height, not rows x cols!
            varMaskList.resize((imSize[1],imSize[0]))

            #  Count blobs before cleanup
            segmentBeforeCount = ndimage.label(varMaskList, structDialationElement)[1]

            #  ********************************
            #  ***  Begin removing "noise"  ***
            #  ********************************

            #  remove stray small groups of foreground pixels

            varMaskList = ndimage.binary_dilation(
                    varMaskList,
                    structure = structDialationElement,
                    iterations = 1)
            # remove pixel noise by eroding one-pass
            varMaskList = ndimage.binary_erosion(
                    varMaskList,
                    structure = structElement,
                    iterations = 1)
            varMaskList = ndimage.binary_erosion(
                    varMaskList,
                    structure = structElement,
                    iterations = 1)
            # add back border pixels in blobs that survived
            varMaskList = ndimage.binary_dilation(
                    varMaskList,
                    structure = structDialationElement,
                    iterations = 1)

            varMaskList = varMaskList.ravel()
            # invert the list for the next step
            for i in range(0, len(varMaskList)):
                if varMaskList[i] > 0:
                    varMaskList[i] = 0
                else:
                    varMaskList[i] = 1

            varMaskList = array(varMaskList)
            varMaskList.resize((imSize[1],imSize[0]))

            varMaskList = ndimage.binary_dilation(
                    varMaskList,
                    structure = structDialationElement,
                    border_value = 1)
            varMaskList = ndimage.binary_erosion(
                    varMaskList,
                    structure = structElement,
                    border_value = 1)
            varMaskList = ndimage.binary_erosion(
                    varMaskList,
                    structure = structElement,
                    border_value = 1)
            varMaskList = ndimage.binary_dilation(
                    varMaskList,
                    structure = structDialationElement,
                    border_value = 1)

            varMaskList = varMaskList.ravel()
            # invert the list for the next step
            for i in range(0, len(varMaskList)):
                if varMaskList[i] > 0:
                    varMaskList[i] = 0
                else:
                    varMaskList[i] = 1

            #  ******************************
            #  ***  End removing "noise"  ***
            #  ******************************

            varMaskList = array(varMaskList)
            varMaskList.resize((imSize[1],imSize[0]))
            # segment the array into continuous regions of
            # increasing integer values, skip the array and
            # return the number of blobs found
            segmentCount = \
                    ndimage.label(varMaskList, structDialationElement)[1]

# *****************************************************************
# write the pixels back to image files after removing noise
# --  Comment out to not write to disk
# *****************************************************************

##                        print 'Saving image files...'
##                        #  put the mask list into the range 0 - 255
##                        varMaskListImage = varMaskList * 255
##                        foregroundImage = Image.new('1', imSize, 'white')
##                        #  var list should be 255 for preserve and 0 for black
##                        foregroundImage.putdata(varMaskListImage)
##
##                        foregroundImage.save(os.path.join(
##                                imagePathFilename,fgCsvFile+'_cleaned_pixel-masked.bmp'))
##                        foregroundImage = ImageChops.invert(foregroundImage)
##
##                        #  put the probability list into the range 0 - 255
##                        varProbList = array(varProbList) * 255
##                        newProbImage = Image.new('L', imSize, 'white')
##                        #  probability list is greyscale
##                        newProbImage.putdata(varProbList)
##                        newProbImage.save(os.path.join(
##                                  foreMaskPathFilename,fgCsvFile+'_cleaned_probability.bmp'))
##
##                        varProbList = []

# *****************************************************************
# compare to the masks to get the correct and incorrect pixel
# counts of both 'just pixels' and segmented images
# *****************************************************************

            print 'Comparing to masks...'
            foreCount = 0
            backCount = 0
            badForeCount = 0
            badBackCount = 0
            foreMaskCount = 0
            backMaskCount = 0
            QsegFore = 0.0
            QsegBack = 0.0

            # turn varMaskList into a linear list of pixels,
            # 0 and 1 of the estimated foreground
            varMaskList = varMaskList.ravel()

            # open the mask image with foreground areas in white,
            # background in black
            foreMaskImage = Image.open(foreMaskPathFilename).convert("1")

            # turn the foreground mask into a list
            foreMaskList = list(foreMaskImage.getdata())
            foreMaskImage = ''

            # open the mask image with foreground areas in white,
            # background in black
            backMaskImage = Image.open(backMaskPathFilename).convert("1")
            #  turn the foreground mask into a list
            backMaskList = list(backMaskImage.getdata())
            backMaskImage = ''

            for i in range(0, len(foreR)):
                if foreMaskList[i] == 255:  #  mask says it's foreground
                    foreMaskCount = foreMaskCount + 1

                    #  segment says it's foreground.  No else statement
                    # because there may be correct
                    if varMaskList[i] == 1:
                        # "foreground" pixles in the background of
                        # this mask
                        foreCount = foreCount + 1
                # masks are mutually exclusive, but not complete
                else:
                    # background mask says it's not foreground.  No else
                    # statement because nothing defined outside of this
                    if backMaskList[i] == 255:
                        backMaskCount = backMaskCount + 1

                        #  segment says it's foreground, a mistake
                        if varMaskList[i] == 1:
                            # back foreground count
                            badForeCount = badForeCount + 1

                        #  segment correctly says it's not foreground
                        else:
                            #  good background count
                            backCount = backCount + 1

            QsegFore = round(foreCount/(foreMaskCount + 0.0001)*100.0, 1)
            QsegBack = round(backCount/(backMaskCount + 0.0001)*100.0, 1)

# *****************************************************************
#  Output stuff
# *****************************************************************

            print '\nColor space:', fgCsvFile
            print 'pixels looked at:', count, \
                    ', black pixels:', blackPixelCount, \
                    ', sum:', count + blackPixelCount
            print 'pixels not in big probability array:', foreBigNans
            print 'pixels in foreground mask:', foreMaskCount
            print 'correct foreground pixels:', \
                    foreCount , '( =', QsegFore, '%)'
            print 'incorrect foreground pixels:', badForeCount
            print 'correct background pixels:', \
                    backCount, '( =', QsegBack, '%)'
            print 'blobs before cleanup:', segmentBeforeCount, \
                    ', blobs after cleanup:', segmentCount
            allPixelMasterArray.append(
                    [imagePathFilename,
                     count,
                     blackPixelCount,
                     foreMaskCount,
                     foreCount,
                     badForeCount,
                     backCount,
                     segmentBeforeCount,
                     segmentCount,
                     foreBigNans,
                     QsegFore,
                     QsegBack])

        print '\nWriting output file...'

        # file name to write to
        textPathFilename = \
                os.path.join(fgMaskDir, \
                fgCsvFile+'_probability_segmented.csv')

        writerFile = open(textPathFilename, 'wb')
        writer = csv.writer(writerFile)

        for i in range(0,len(allPixelMasterArray)):
            writer.writerow(allPixelMasterArray[i])
        writerFile.close()

# convert RGBs to different color spaces with
# range 0 - 255 for each color component (see subroutines)
def getColorComponent(fgCsvFile=""):
    tupleFlag = 0
    XX, YY, ZZ = 0, 0, 0

    if fgCsvFile == ""
        return (-1, XX, YY, ZZ)

    if fgCsvFile[0:3] == 'RG_':
        XX = foreR[i]
        YY = foreG[i]
        tupleFlag = 1
    elif fgCsvFile[0:3] == 'RB_':
        XX = foreR[i]
        YY = foreB[i]
        tupleFlag = 1
    elif fgCsvFile[0:3] == 'GB_':
        XX = foreG[i]
        YY = foreB[i]
        tupleFlag = 1

    elif fgCsvFile[0:2] == 'R_':
        XX = foreR[i]
    elif fgCsvFile[0:2] == 'G_':
        XX = foreG[i]
    elif fgCsvFile[0:2] == 'B_':
        XX = foreB[i]

    elif fgCsvFile[0:3] == 'BR_':
        if foreR[i] <> 0:
            XX = float(foreB[i])/foreR[i]
    elif fgCsvFile[0:3] == 'GR_':
        if foreR[i] <> 0:
            XX = float(foreG[i])/foreR[i]
    elif fgCsvFile[0:5] == 'GRBR_':
        if foreR[i] <> 0:
            XX = float(foreG[i])/foreR[i]
            YY = float(foreB[i])/foreR[i]
            tupleFlag = 1

    elif fgCsvFile[0:6] == 'RGB3D_':
        XX = foreR[i]
        YY = foreG[i]
        ZZ = foreB[i]
        tupleFlag = 2

    elif fgCsvFile[0:6] == 'HSL_H_':
        XX, S, L = ColorConverter.rgb_to_HSL(r, g, b, write256)
    elif fgCsvFile[0:6] == 'HSL_S_':
        H, XX, L = ColorConverter.rgb_to_HSL(r, g, b, write256)
    elif fgCsvFile[0:6] == 'HSL_L_':
        H, S, XX = ColorConverter.rgb_to_HSL(r, g, b, write256)
    elif fgCsvFile[0:7] == 'HSL_HS_':
        XX, YY, L = ColorConverter.rgb_to_HSL(r, g, b, write256)
        tupleFlag = 1

    elif fgCsvFile[0:6] == 'Yxy_Y_':
        XX, x, y = ColorConverter.rgb_to_Yxy(r, g, b, write256)
    elif fgCsvFile[0:6] == 'Yxy_x_':
        Y, XX, y = ColorConverter.rgb_to_Yxy(r, g, b, write256)
    elif fgCsvFile[0:7] == 'Yxy_y2_':
        Y, x, XX = ColorConverter.rgb_to_Yxy(r, g, b, write256)
    elif fgCsvFile[0:7] == 'Yxy_xy_':
        Y, XX, YY = ColorConverter.rgb_to_Yxy(r, g, b, write256)
        tupleFlag = 1

    elif fgCsvFile[0:8] == 'NRGB_NR_':
        XX, NG, NB = ColorConverter.rgb_to_NRGB(r, g, b, write256)
    elif fgCsvFile[0:8] == 'NRGB_NG_':
        NR, XX, NB = ColorConverter.rgb_to_NRGB(r, g, b, write256)
    elif fgCsvFile[0:8] == 'NRGB_NB_':
        NR, NG, XX = ColorConverter.rgb_to_NRGB(r, g, b, write256)

    elif fgCsvFile[0:6] == 'NRGB1_':
        XX, NRGB_2 = ColorConverter.rbg_to_NRGB_2D(r, g, b, write256)
    elif fgCsvFile[0:6] == 'NRGB2_':
        NRGB_1, XX = ColorConverter.rbg_to_NRGB_2D(r, g, b, write256)
    elif fgCsvFile[0:7] == 'NRGB12_':
        XX, YY = ColorConverter.rbg_to_NRGB_2D(r, g, b, write256)
        tupleFlag = 1

    elif fgCsvFile[0:6] == 'Lab_L_':
        XX, a, b2 = ColorConverter.rgb_to_Lab(r, g, b, write256)
    elif fgCsvFile[0:6] == 'Lab_a_':
        L2, XX, b2 = ColorConverter.rgb_to_Lab(r, g, b, write256)
    elif fgCsvFile[0:6] == 'Lab_b_':
        L2, a, XX = ColorConverter.rgb_to_Lab(r, g, b, write256)
    elif fgCsvFile[0:7] == 'Lab_ab_':
        L2, XX, YY = ColorConverter.rgb_to_Lab(r, g, b, write256)
        tupleFlag = 1

    elif fgCsvFile[0:12] == 'Ingling_r_g_':
        XX, b_y, V3 = ColorConverter.rgb_to_Ingling(r, g, b, write256)
    elif fgCsvFile[0:12] == 'Ingling_b_y_':
        r_g, XX, V3 = ColorConverter.rgb_to_Ingling(r, g, b, write256)
    elif fgCsvFile[0:10] == 'Ingling_V_':
        r_g, b_y, XX = ColorConverter.rgb_to_Ingling(r, g, b, write256)
    elif fgCsvFile[0:13] == 'Ingling_rgby_':
        XX, YY, V3 = ColorConverter.rgb_to_Ingling(r, g, b, write256)
        tupleFlag = 1

    elif fgCsvFile[0:10] == 'ExRGB_R14_':
        XX, ExR20, ExG, ExB = ColorConverter.rgb_to_ExRGB(r, g, b, write256)
    elif fgCsvFile[0:10] == 'ExRGB_R20_':
        ExR14, XX, ExG, ExB = ColorConverter.rgb_to_ExRGB(r, g, b, write256)
    elif fgCsvFile[0:8] == 'ExRGB_G_':
        ExR14, ExR20, XX, ExB = ColorConverter.rgb_to_ExRGB(r, g, b, write256)
    elif fgCsvFile[0:8] == 'ExRGB_B_':
        ExR14, ExR20, ExG, XX = ColorConverter.rgb_to_ExRGB(r, g, b, write256)
    elif fgCsvFile[0:7] == 'ExRGB1_':

        XX, ExRGB_2 = ColorConverter.rgb_to_ExRGB_2D(r, g, b, write256)
    elif fgCsvFile[0:7] == 'ExRGB2_':
        ExRGB_1, XX = ColorConverter.rgb_to_ExRGB_2D(r, g, b, write256)
    elif fgCsvFile[0:8] == 'ExRGB12_':
        XX, YY = ColorConverter.rgb_to_ExRGB_2D(r, g, b, write256)
        tupleFlag = 1

    elif fgCsvFile[0:7] == 'ATD_A1_':
        XX, T1, D1, t1, d1, A2, T2, D2 = \
                ColorConverter.rgb_to_ATD(r, g, b, write256)
    elif fgCsvFile[0:7] == 'ATD_T1_':
        A1, XX, D1, t1, d1, A2, T2, D2 = \
                ColorConverter.rgb_to_ATD(r, g, b, write256)
    elif fgCsvFile[0:7] == 'ATD_D1_':
        A1, T1, XX, t1, d1, A2, T2, D2 = \
                ColorConverter.rgb_to_ATD(r, g, b, write256)
    elif fgCsvFile[0:6] == 'ATD_t_':
        A1, T1, D1, XX, d1, A2, T2, D2 = \
                ColorConverter.rgb_to_ATD(r, g, b, write256)
    elif fgCsvFile[0:6] == 'ATD_d_':
        A1, T1, D1, t1, XX, A2, T2, D2 = \
                ColorConverter.rgb_to_ATD(r, g, b, write256)
    elif fgCsvFile[0:7] == 'ATD_A2_':
        A1, T1, D1, t1, d1, XX, T2, D2 = \
                ColorConverter.rgb_to_ATD(r, g, b, write256)
    elif fgCsvFile[0:7] == 'ATD_T2_':
        A1, T1, D1, t1, d1, A2, XX, D2 = \
                ColorConverter.rgb_to_ATD(r, g, b, write256)
    elif fgCsvFile[0:7] == 'ATD_D2_':
        A1, T1, D1, t1, d1, A2, T2, XX = \
                ColorConverter.rgb_to_ATD(r, g, b, write256)
    elif fgCsvFile[0:8] == 'ATD_TD1_':
        A1, XX, YY, t1, d1, A2, T2, D2 = \
                ColorConverter.rgb_to_ATD(r, g, b, write256)
        tupleFlag = 1
    elif fgCsvFile[0:8] == 'ATD_TD2_':
        A1, T1, D1, t1, d1, A2, XX, YY = \
                ColorConverter.rgb_to_ATD(r, g, b, write256)
        tupleFlag = 1
    elif fgCsvFile[0:7] == 'ATD_td_':
        A1, T1, D1, XX, YY, A2, T2, D2 = \
                ColorConverter.rgb_to_ATD(r, g, b, write256)
        tupleFlag = 1

    elif fgCsvFile[0:9] == 'NDI123_1_':
        XX, NDI2, NDI3 = ColorConverter.rgb_to_NDI123(r, g, b, write256)
    elif fgCsvFile[0:9] == 'NDI123_2_':
        NDI1, XX, NDI3 = ColorConverter.rgb_to_NDI123(r, g, b, write256)
    elif fgCsvFile[0:9] == 'NDI123_3_':
        NDI1, NDI2, XX = ColorConverter.rgb_to_NDI123(r, g, b, write256)
    elif fgCsvFile[0:13] == 'NDI123_NDI12_':
        XX, YY, NDI3 = ColorConverter.rgb_to_NDI123(r, g, b, write256)
        tupleFlag = 1
    elif fgCsvFile[0:13] == 'NDI123_NDI23_':
        NDI1, XX, YY = ColorConverter.rgb_to_NDI123(r, g, b, write256)
        tupleFlag = 1
    elif fgCsvFile[0:13] == 'NDI123_NDI13_':
        XX, NDI2, YY = ColorConverter.rgb_to_NDI123(r, g, b, write256)
        tupleFlag = 1

    elif fgCsvFile[0:7] == 'i123_1_':
        XX, i2, i3 = ColorConverter.rgb_to_i1i2i3(r, g, b, write256)
    elif fgCsvFile[0:7] == 'i123_2_':
        i1, XX, i3 = ColorConverter.rgb_to_i1i2i3(r, g, b, write256)
    elif fgCsvFile[0:7] == 'i123_3_':
        i1, i2, XX = ColorConverter.rgb_to_i1i2i3(r, g, b, write256)
    elif fgCsvFile[0:9] == 'i123_i12_':
        XX, YY, i3 = ColorConverter.rgb_to_i1i2i3(r, g, b, write256)
        tupleFlag = 1
    elif fgCsvFile[0:9] == 'i123_i23_':
        i1, XX, YY = ColorConverter.rgb_to_i1i2i3(r, g, b, write256)
        tupleFlag = 1
    elif fgCsvFile[0:9] == 'i123_i13_':
        XX, i2, YY = ColorConverter.rgb_to_i1i2i3(r, g, b, write256)
        tupleFlag = 1

    elif fgCsvFile[0:5] == 'CIVE_':
        XX = ColorConverter.rgb_to_CIVE(r, g, b, write256)

    elif fgCsvFile[0:7] == 'shadow_':
        XX = ColorConverter.rgb_to_shadow(r, g, b, write256)

    return (tupleFlag, XX, YY, ZZ)


if __name__ == "__main__":
    main()
    x = raw_input('Done!  Enter any key to quit.')
