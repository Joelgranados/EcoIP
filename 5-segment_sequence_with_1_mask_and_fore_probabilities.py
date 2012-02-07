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
from scipy import ndimage
import math
import ColorConverter
from browse_files import *

def main():

    # *****************************************************************
    # takes an image sequence, uses the calculated color frequencies,
    # and segments the images
    # *****************************************************************

    photoPath = getMainDir(message='Select the photo directory: ')
    if photoPath == '':
        print("\nNo directory selected, program aborted.\n")
        return

    photoPath = photoPath + '/'

    probPath = getMainDir(message='Select the probabilities directory: ')
    if probPath == '':
        print("\nNo directory selected, program aborted.\n")
        return

    probPath = probPath + '/'

##    probImagesPath = getMainDir(message='Select the probabilities images output directory: ')
##    if probImagesPath == '':
##        print("\nNo directory selected, program aborted.\n")
##        return
##
##    probImagesPath = probImagesPath + '/'

    foreMaskPathFilename = \
            getMainDir(message='Select the mask file, cancel for no mask: ')
    if foreMaskPathFilename == '':
        print("\nNo file selected.  No mask file used."
                "  Image output into same subdirectory.\n")
        maskFlag = 0
        foreMaskPathFilename = photoPath
    else:
        maskFlag = 1

    fileNamePrepend = raw_input('Enter file output name:')
    print("Output file will be in mask directory or if"
            " no mask, then in photo directory")

    write256 = True

    # *****************************************************************
    # Load the 1_d and 2-D color spaces probabilities into a dictionary
    # with the color space tuples (XX, YY) as keys
    # *****************************************************************


    # get the probability file names in each subdirectory
    fileList = os.listdir(probPath)
    photoList = os.listdir(photoPath)
    # for each color space file...
    for csvFile in fileList:
        # could be other types of files in the subdirectory
        if os.path.splitext(probPath + csvFile)[1] == '.csv':
            allPixelMasterArray = []  # for keeping track of pixels in mask
            allPixelMasterArray.append(['Path and filename',
                                        'number pixels examined',
                                        'number of black pixels',
                                        'number of foremask pixels',
                                        'foreground pixels',
                                        'ForeCount in mask',
                                        'backCount',
                                        'segments before clean',
                                        'segments after clean',
                                        'foreBigNans'])
            box = (0,0)
            #  the dictonary that will hold the probabilities
            bigForeDataDict = {}
            foreDataName = (probPath + csvFile)
            print 'Data array name:', csvFile
            readerFile = open(foreDataName, 'rb')
            #  open the data file
            dataReader = csv.reader(readerFile, delimiter = ',')
            headerFlag = 1
            #  File format is: Xcol, [Ycol], frequency, probability, count
            for dataItem in dataReader:
                if headerFlag == 1:  #  skip the first row, has only header text
                    headerFlag = 0
                else:
                    if len(dataItem) == 4:
                        bigForeDataDict[int(float(dataItem[0]))] = \
                                float(dataItem[2])
                    elif len(dataItem) == 5:
                        bigForeDataDict[(int(float(dataItem[0])),
                                         int(float(dataItem[1])))] = \
                                                 float(dataItem[3])
                    elif len(dataItem) == 6:
                        bigForeDataDict[(int(float(dataItem[0])),
                                         int(float(dataItem[1])),
                                         int(float(dataItem[2])))] = \
                                                 float(dataItem[4])
            readerFile.close()

            if len(dataItem) == 3: tupleFlag = 0
            elif len(dataItem) == 4: tupleFlag = 1

# *****************************************************************
# Start processing individual image files that are associated with masks
# *****************************************************************
            photoFileIter = iter(photoList)
            loopFiles = True
            while loopFiles:
                openFile = True
                while openFile:
                    try:
                        photoFile = photoFileIter.next()
                        # could be other types of files in the subdirectory
                        if os.path.splitext(photoPath + photoFile)[1] == '.jpg':
                            # make the path and file name for opening a file
                            # in a subdirectory
                            imagePathFilename = photoPath + photoFile
                            print '\nProcessing image:', imagePathFilename
                            # open the image and mask files
                            foregroundImage = Image.open(imagePathFilename)
                            foregroundImage.load()
                            openFile = False

                    except StopIteration:
                        loopFiles = False
                        break

                    except:
                        print("\nSkkpped file!: %s\n" % imagePathFilename)

                # end of file encountered, break out of top while loop
                if not loopFiles: break

                imSize = foregroundImage.size  #  get image dimensions

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

                # if a color is not encountered, keep a record of "NaN's"
                foreBigNans = 0
                blackPixelCount = 0

# *****************************************************************
# convert each pixel RGBs to different color spaces with range 0 - 255
# *****************************************************************

                print 'Converting colors and matching with array...'
                count = 0
                tupleFlag = 0

                print csvFile
                # convert RGBs to different color spaces with range
                # 0 - 255 for each color component (see subroutines)

                # send pixel data to subroutines to calculate colors
                for i in range(0, len(foreR)):
                    r = foreR[i]/255.0
                    g = foreG[i]/255.0
                    b = foreB[i]/255.0
                    # process only if not black (mask is black)
                    if not (r == 0 and g == 0 and b == 0):

                        # set the appropriate color component equal
                        # to the variables XX, YY, or ZZ for tallying
                        count = count + 1

                        if csvFile[0:3] == 'RG_':
                            XX = foreR[i]
                            YY = foreG[i]
                            tupleFlag = 1
                        elif csvFile[0:3] == 'RB_':
                            XX = foreR[i]
                            YY = foreB[i]
                            tupleFlag = 1
                        elif csvFile[0:3] == 'GB_':
                            XX = foreG[i]
                            YY = foreB[i]
                            tupleFlag = 1

                        elif csvFile[0:2] == 'R_':
                            XX = foreR[i]
                        elif csvFile[0:2] == 'G_':
                            XX = foreG[i]
                        elif csvFile[0:2] == 'B_':
                            XX = foreB[i]

                        elif csvFile[0:3] == 'BR_':
                            if foreR[i] <> 0:
                                XX = float(foreB[i])/foreR[i]
                        elif csvFile[0:3] == 'GR_':
                            if foreR[i] <> 0:
                                XX = float(foreG[i])/foreR[i]
                        elif csvFile[0:5] == 'GRBR_':
                            if foreR[i] <> 0:
                                XX = float(foreG[i])/foreR[i]
                                YY = float(foreB[i])/foreR[i]
                                tupleFlag = 1

                        elif csvFile[0:6] == 'RGB3D_':
                            XX = foreR[i]
                            YY = foreG[i]
                            ZZ = foreB[i]
                            tupleFlag = 2

                        elif csvFile[0:6] == 'HSL_H_':
                            XX, S, L = ColorConverter.rgb_to_HSL(r, g, b, write256)
                        elif csvFile[0:6] == 'HSL_S_':
                            H, XX, L = ColorConverter.rgb_to_HSL(r, g, b, write256)
                        elif csvFile[0:6] == 'HSL_L_':
                            H, S, XX = ColorConverter.rgb_to_HSL(r, g, b, write256)
                        elif csvFile[0:7] == 'HSL_HS_':
                            XX, YY, L = ColorConverter.rgb_to_HSL(r, g, b, write256)
                            tupleFlag = 1

                        elif csvFile[0:6] == 'Yxy_Y_':
                            XX, x, y = ColorConverter.rgb_to_Yxy(r, g, b, write256)
                        elif csvFile[0:6] == 'Yxy_x_':
                            Y, XX, y = ColorConverter.rgb_to_Yxy(r, g, b, write256)
                        elif csvFile[0:7] == 'Yxy_y2_':
                            Y, x, XX = ColorConverter.rgb_to_Yxy(r, g, b, write256)
                        elif csvFile[0:7] == 'Yxy_xy_':
                            Y, XX, YY = ColorConverter.rgb_to_Yxy(r, g, b, write256)
                            tupleFlag = 1

                        elif csvFile[0:8] == 'NRGB_NR_':
                            XX, NG, NB = ColorConverter.rgb_to_NRGB(r, g, b, write256)
                        elif csvFile[0:8] == 'NRGB_NG_':
                            NR, XX, NB = ColorConverter.rgb_to_NRGB(r, g, b, write256)
                        elif csvFile[0:8] == 'NRGB_NB_':
                            NR, NG, XX = ColorConverter.rgb_to_NRGB(r, g, b, write256)

                        elif csvFile[0:6] == 'NRGB1_':
                            XX, NRGB_2 = ColorConverter.rbg_to_NRGB_2D(r, g, b, write256)
                        elif csvFile[0:6] == 'NRGB2_':
                            NRGB_1, XX = ColorConverter.rbg_to_NRGB_2D(r, g, b, write256)
                        elif csvFile[0:7] == 'NRGB12_':
                            XX, YY = ColorConverter.rbg_to_NRGB_2D(r, g, b, write256)
                            tupleFlag = 1

                        elif csvFile[0:6] == 'Lab_L_':
                            XX, a, b2 = ColorConverter.rgb_to_Lab(r, g, b, write256)
                        elif csvFile[0:6] == 'Lab_a_':
                            L2, XX, b2 = ColorConverter.rgb_to_Lab(r, g, b, write256)
                        elif csvFile[0:6] == 'Lab_b_':
                            L2, a, XX = ColorConverter.rgb_to_Lab(r, g, b, write256)
                        elif csvFile[0:7] == 'Lab_ab_':
                            L2, XX, YY = ColorConverter.rgb_to_Lab(r, g, b, write256)
                            tupleFlag = 1

                        elif csvFile[0:12] == 'Ingling_r_g_':
                            XX, b_y, V3 = ColorConverter.rgb_to_Ingling(r, g, b, write256)
                        elif csvFile[0:12] == 'Ingling_b_y_':
                            r_g, XX, V3 = ColorConverter.rgb_to_Ingling(r, g, b, write256)
                        elif csvFile[0:10] == 'Ingling_V_':
                            r_g, b_y, XX = ColorConverter.rgb_to_Ingling(r, g, b, write256)
                        elif csvFile[0:13] == 'Ingling_rgby_':
                            XX, YY, V3 = ColorConverter.rgb_to_Ingling(r, g, b, write256)
                            tupleFlag = 1

                        elif csvFile[0:10] == 'ExRGB_R14_':
                            XX, ExR20, ExG, ExB = ColorConverter.rgb_to_ExRGB(r, g, b, write256)
                        elif csvFile[0:10] == 'ExRGB_R20_':
                            ExR14, XX, ExG, ExB = ColorConverter.rgb_to_ExRGB(r, g, b, write256)
                        elif csvFile[0:8] == 'ExRGB_G_':
                            ExR14, ExR20, XX, ExB = ColorConverter.rgb_to_ExRGB(r, g, b, write256)
                        elif csvFile[0:8] == 'ExRGB_B_':
                            ExR14, ExR20, ExG, XX = ColorConverter.rgb_to_ExRGB(r, g, b, write256)

                        elif csvFile[0:7] == 'ExRGB1_':
                            XX, ExRGB_2 = ColorConverter.rgb_to_ExRGB_2D(r, g, b, write256)
                        elif csvFile[0:7] == 'ExRGB2_':
                            ExRGB_1, XX = ColorConverter.rgb_to_ExRGB_2D(r, g, b, write256)
                        elif csvFile[0:8] == 'ExRGB12_':
                            XX, YY = ColorConverter.rgb_to_ExRGB_2D(r, g, b, write256)
                            tupleFlag = 1

                        elif csvFile[0:7] == 'ATD_A1_':
                            XX, T1, D1, t1, d1, A2, T2, D2 = \
                                    ColorConverter.rgb_to_ATD(r, g, b, write256)
                        elif csvFile[0:7] == 'ATD_T1_':
                            A1, XX, D1, t1, d1, A2, T2, D2 = \
                                    ColorConverter.rgb_to_ATD(r, g, b, write256)
                        elif csvFile[0:7] == 'ATD_D1_':
                            A1, T1, XX, t1, d1, A2, T2, D2 = \
                                    ColorConverter.rgb_to_ATD(r, g, b, write256)
                        elif csvFile[0:6] == 'ATD_t_':
                            A1, T1, D1, XX, d1, A2, T2, D2 = \
                                    ColorConverter.rgb_to_ATD(r, g, b, write256)
                        elif csvFile[0:6] == 'ATD_d_':
                            A1, T1, D1, t1, XX, A2, T2, D2 = \
                                    ColorConverter.rgb_to_ATD(r, g, b, write256)
                        elif csvFile[0:7] == 'ATD_A2_':
                            A1, T1, D1, t1, d1, XX, T2, D2 = \
                                    ColorConverter.rgb_to_ATD(r, g, b, write256)
                        elif csvFile[0:7] == 'ATD_T2_':
                            A1, T1, D1, t1, d1, A2, XX, D2 = \
                                    ColorConverter.rgb_to_ATD(r, g, b, write256)
                        elif csvFile[0:7] == 'ATD_D2_':
                            A1, T1, D1, t1, d1, A2, T2, XX = \
                                    ColorConverter.rgb_to_ATD(r, g, b, write256)
                        elif csvFile[0:8] == 'ATD_TD1_':
                            A1, XX, YY, t1, d1, A2, T2, D2 = \
                                    ColorConverter.rgb_to_ATD(r, g, b, write256)
                            tupleFlag = 1
                        elif csvFile[0:8] == 'ATD_TD2_':
                            A1, T1, D1, t1, d1, A2, XX, YY = \
                                    ColorConverter.rgb_to_ATD(r, g, b, write256)
                            tupleFlag = 1
                        elif csvFile[0:7] == 'ATD_td_':
                            A1, T1, D1, XX, YY, A2, T2, D2 = \
                                    ColorConverter.rgb_to_ATD(r, g, b, write256)
                            tupleFlag = 1

                        elif csvFile[0:9] == 'NDI123_1_':
                            XX, NDI2, NDI3 = ColorConverter.rgb_to_NDI123(r, g, b, write256)
                        elif csvFile[0:9] == 'NDI123_2_':
                            NDI1, XX, NDI3 = ColorConverter.rgb_to_NDI123(r, g, b, write256)
                        elif csvFile[0:9] == 'NDI123_3_':
                            NDI1, NDI2, XX = ColorConverter.rgb_to_NDI123(r, g, b, write256)
                        elif csvFile[0:13] == 'NDI123_NDI12_':
                            XX, YY, NDI3 = ColorConverter.rgb_to_NDI123(r, g, b, write256)
                            tupleFlag = 1
                        elif csvFile[0:13] == 'NDI123_NDI23_':
                            NDI1, XX, YY = ColorConverter.rgb_to_NDI123(r, g, b, write256)
                            tupleFlag = 1
                        elif csvFile[0:13] == 'NDI123_NDI13_':
                            XX, NDI2, YY = ColorConverter.rgb_to_NDI123(r, g, b, write256)
                            tupleFlag = 1

                        elif csvFile[0:7] == 'i123_1_':
                            XX, i2, i3 = ColorConverter.rgb_to_i1i2i3(r, g, b, write256)
                        elif csvFile[0:7] == 'i123_2_':
                            i1, XX, i3 = ColorConverter.rgb_to_i1i2i3(r, g, b, write256)
                        elif csvFile[0:7] == 'i123_3_':
                            i1, i2, XX = ColorConverter.rgb_to_i1i2i3(r, g, b, write256)
                        elif csvFile[0:9] == 'i123_i12_':
                            XX, YY, i3 = ColorConverter.rgb_to_i1i2i3(r, g, b, write256)
                            tupleFlag = 1
                        elif csvFile[0:9] == 'i123_i23_':
                            i1, XX, YY = ColorConverter.rgb_to_i1i2i3(r, g, b, write256)
                            tupleFlag = 1
                        elif csvFile[0:9] == 'i123_i13_':
                            XX, i2, YY = ColorConverter.rgb_to_i1i2i3(r, g, b, write256)
                            tupleFlag = 1

                        elif csvFile[0:5] == 'CIVE_':
                            XX = ColorConverter.rgb_to_CIVE(r, g, b, write256)

                        elif csvFile[0:7] == 'shadow_':
                            XX = ColorConverter.rgb_to_shadow(r, g, b, write256)

# *****************************************************************
# find pixel values in the probability array that was imported and
# add to a list
# *****************************************************************

                        # if the color space is a 2D one, with a tuple
                        # describing the data
                        if tupleFlag == 1:
                            if (int(float(XX)),int(float(YY))) in bigForeDataDict:
                                # grab the probabilities for any pixel from
                                # the Big arrays
                                foreProbability = \
                                        bigForeDataDict[(int(float(XX)),
                                                         int(float(YY)))]
                            else:
                                foreBigNans = foreBigNans + 1
                                foreProbability = 0
                        # if the color space is a 1D one, then one value defines
                        # a dictionary entry
                        elif tupleFlag == 0:
                            if int(float(XX)) in bigForeDataDict:
                                # grab the probabilities for any pixel from the
                                # Big arrays
                                foreProbability = bigForeDataDict[int(float(XX))]
                            else:
                                foreBigNans = foreBigNans + 1
                                foreProbability = 0
                        elif tupleFlag == 2:  #  3-d color space
                            if (int(float(XX)),int(float(YY)),int(float(ZZ))) in bigForeDataDict:
                                foreProbability = \
                                        bigForeDataDict[(int(float(XX)),
                                                         int(float(YY)),
                                                         int(float(ZZ)))]
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

                    # create the mask image from the probabilities, > 50%
                    # indicates foreground
                    if foreProbability > 0.5:
                        varMaskList[i] = 1
                    else:
                        varMaskList[i] = 0

                print 'Cleaning and segmenting...'

# *****************************************************************
# write the pixels back to image files  --  Comment out to not
# write to disk
# *****************************************************************

##                    print 'Saving image files...'
##                    # put the mask list into the range 0 - 255
##                    varMaskList = array(varMaskList) * 255
##                    foregroundImage = Image.new('1', imSize, 'white')
##                    # var list should be 255 for preserve and 0 for black
##                    foregroundImage.putdata(varMaskList)
##
##                    foregroundImage.save(imagePathFilename + csvFile + '_pixel-masked.bmp')
##                    foregroundImage = ImageChops.invert(foregroundImage)
##
##                    # put the probability list into the range 0 - 255
##                    varProbList = array(varProbList) * 255
##                    newProbImage = Image.new('L', imSize, 'white')
##                    # probability list is greyscale
##                    newProbImage.putdata(varProbList)
##                    newProbImage.save(foreMaskPathFilename + csvFile + '_probability.bmp')
##
##                    varProbList = []

# *****************************************************************
# Segment Image to count blobs, first before then after removing noise
# *****************************************************************

                # structuring element used as a mask for determining blobs
                structElement = array([[1,1,1], [1,1,1], [1,1,1]])
                #  structuring element used for binary dialation
                structDialationElement = array([[0,1,0], [1,1,1], [0,1,0]])

                varMaskList = array(varMaskList)  #  turn to a numpy array
                # make it the image shape -- note width x height, not rows x cols!
                varMaskList.resize((imSize[1],imSize[0]))

                #  Count blobs before cleanup
                segmentBeforeCount = \
                        ndimage.label(varMaskList, structDialationElement)[1]

                # ********************************
                # ***  Begin removing "noise"  ***
                # ********************************

                # remove stray small groups of foreground pixels
                varMaskList = \
                        ndimage.binary_dilation(varMaskList,
                                                structure = structDialationElement,
                                                iterations = 1)
                #  remove pixel noise by eroding one-pass
                varMaskList = \
                        ndimage.binary_erosion(varMaskList,
                                               structure = structElement,
                                               iterations = 1)

                varMaskList = \
                        ndimage.binary_erosion(varMaskList,
                                               structure = structElement,
                                               iterations = 1)
                # add back border pixels in blobs that survived
                varMaskList = \
                        ndimage.binary_dilation(varMaskList,
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

                varMaskList = \
                        ndimage.binary_dilation(varMaskList,
                                                structure = structDialationElement,
                                                border_value = 1)
                varMaskList = \
                        ndimage.binary_erosion(varMaskList,
                                               structure = structElement,
                                               border_value = 1)
                varMaskList = \
                        ndimage.binary_erosion(varMaskList,
                                               structure = structElement,
                                               border_value = 1)
                varMaskList = \
                        ndimage.binary_dilation(varMaskList,
                                                structure = structDialationElement,
                                                border_value = 1)

                varMaskList = varMaskList.ravel()
                #  invert the list for the next step
                for i in range(0, len(varMaskList)):
                    if varMaskList[i] > 0:
                        varMaskList[i] = 0
                    else:
                        varMaskList[i] = 1

                # ******************************
                # ***  End removing "noise"  ***
                # ******************************

                varMaskList = array(varMaskList)
                varMaskList.resize((imSize[1],imSize[0]))
                # segment the array into continuous regions of increasing
                # integer values, skip the array and return the number of
                # blobs found
                segmentCount = ndimage.label(varMaskList, structDialationElement)[1]

# *****************************************************************
#  write the pixels back to image files after removing noise
# --  Comment out to not write to disk
# *****************************************************************

##                print 'Saving image files...'
##                #  put the mask list into the range 0 - 255
##                varMaskListImage = varMaskList * 255
##                foregroundImage = Image.new('1', imSize, 'white')
##                #  var list should be 255 for preserve and 0 for black
##                foregroundImage.putdata(varMaskListImage)
##
####                    foregroundImage.save(imagePathFilename + csvFile + '_cleaned_pixel-masked.bmp')
####                    foregroundImage = ImageChops.invert(foregroundImage)
##
##                #  put the probability list into the range 0 - 255
##                varProbList = array(varProbList) * 255
##                newProbImage = Image.new('L', imSize, 'white')
##                #  probability list is greyscale
##                newProbImage.putdata(varProbList)
##                newProbImage.save(foreMaskPathFilename + photoFile +'_' + csvFile + '_cleaned_probability.bmp')
##
##                varProbList = []

# *****************************************************************
# compare to the masks to get the correct and incorrect pixel counts
# of both 'just pixels' and segmented images
# *****************************************************************
               # textInput = raw_input("Input your name: ")
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

                if maskFlag:  #  if there is a mask to apply
                    # open the mask image with foreground areas in white,
                    # background in black
                    foreMaskImage = Image.open(foreMaskPathFilename).convert("L")
                else:
                    foreMaskImage = Image.new("L",imSize, 255)

                # turn the foreground mask into a list
                foreMaskList = list(foreMaskImage.getdata())
                foreMaskImage = ''

                for i in range(0, len(foreR)):
                    # mask says it's foreground
                    if foreMaskList[i] == 255:
                        foreMaskCount = foreMaskCount + 1

                        ###  segment says it's foreground.
                        if varMaskList[i] == 1:
                            foreCount = foreCount + 1

                    else:  #  mask says it's not foreground.
                        backMaskCount = backMaskCount + 1
                        # segment says it's foreground, a mistake
                        if varMaskList[i] == 1:
                            # back foreground count
                            badForeCount = badForeCount + 1
                        else:
                            # segment correctly says it's not foreground
                            # good background count
                            backCount = backCount + 1

# *****************************************************************
# Output stuff
# *****************************************************************

                print '\nColor space:', csvFile
                print 'pixels looked at:', count, \
                        ', black pixels:', blackPixelCount, \
                        ', sum:', count + blackPixelCount
                print 'pixels not in big probability array:', foreBigNans
                print 'pixels in foreground mask:', foreMaskCount
                print 'correct foreground pixels:', foreCount
                print 'incorrect foreground pixels:', badForeCount
                print 'correct background pixels:', backCount
                print 'blobs before cleanup:', segmentBeforeCount, \
                        ', blobs after cleanup:', segmentCount
                allPixelMasterArray.append([imagePathFilename,
                                            count,
                                            blackPixelCount,
                                            foreMaskCount,
                                            foreCount,
                                            badForeCount,
                                            backCount,
                                            segmentBeforeCount,
                                            segmentCount,
                                            foreBigNans])

            print '\nWriting output file...'
            # file name to write to
            textPathFilename = \
                    os.path.join ( os.path.split(foreMaskPathFilename)[0],
                            csvFile + '_1mask_probability_segmented_'
                            + fileNamePrepend + '_.csv' )
            writerFile = open(textPathFilename, 'wb')
            writer = csv.writer(writerFile, delimiter = ',', quoting=csv.QUOTE_NONE)
            for i in range(0,len(allPixelMasterArray)):
                writer.writerow(allPixelMasterArray[i])
            writerFile.close()

if __name__ == "__main__":
    main()
    x = raw_input('Done!  Enter any key to quit.')
