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
from scipy import ndimage
import ColorConverter
from browse_files import *

# General steps for this script (For each image and colors space:
# 0 LOAD THE 1-D, 2-D, AND 3-D COLOR SPACES PROBABILITIES INTO A DICT
# 1 CREATE A MASK IMAGE USING PROBABILITIES
# 2 ELIMINATE NOISE FROM CREATED MASK & SEGMENT
# 3 COMPARE CALCULATED MASK WITH ORIGINAL MASK

# This program will compare two masks for one image.  Masks are mutually
# exclusive but may not complete, in that the foreground is a subset of
# all possible foregrounds.  Thus, a foreground mask of leaves may not
# have all leaves selected.
def main( savePixels=False):
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


    # 0 LOAD THE 1-D, 2-D, AND 3-D COLOR SPACES PROBABILITIES INTO A DICT
    print('Working on %s.'% fgMaskDir)
    # get CSV files
    fgCsvFiles = [name for name in os.listdir(fgMaskDir) if name.endswith(".csv")]

    # get the mask file
    fgMaskFiles = [name for name in os.listdir(fgMaskDir) if name.endswith(".bmp")]

    for fgCsvFile in fgCsvFiles:  #  for each color space file...
        print("Data array name: %s"%fgCsvFile)
        # for keeping track of pixels in mask
        allPixelMasterArray = \
                [ ['Path and filename', 'number pixels examined',
                   'number of black pixels', 'foremask pixels',
                   'correct foreground', 'incorrect foreground',
                   'correct background', 'segments before clean',
                   'segments after clean', 'foreBigNans',
                   'QsegFore', 'QsegBack'] ]

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

        # Process individual image files associated with masks
        for fgMaksFile in fgMaskFiles:
            # 1 CREATE A MASK IMAGE USING PROBABILITIES
            imgFile = \ # imgFile is maskFile but with a jpg extension.
                    os.path.splitext(os.path.join(photoPath,fgMaksFile))[0] + '.jpg'
            print("\nProcessing mask:%s and image %s"%(fgMaksFile,imgFile))

            # Calc R, G, B pixel values.
            fgImg = Image.open(imgFile).load()
            fgImgSrc = fgImg.split()

            imSize = fgImg.size
            foreR = list(fgImgSrc[0].getdata())
            foreG = list(fgImgSrc[1].getdata())
            foreB = list(fgImgSrc[2].getdata())
            varMaskList = list(fgImgSrc[0].getdata())

            del(fgImg, fgImgSrc)

            print("Converting colors and matching with array in %s"%fgCsvFile)

            # if a color is not encountered, keep a record of "NaN's"
            foreBigNans = 0
            blackPixelCount = 0
            count = 0
            varProbList = [] # prob img generating list (0 to 255)
            for i in range(0, len(foreR)):
                # process only if white (mask is black)
                if not (foreR[i] == 0 and foreG[i] == 0 and foreB[i] == 0):
                    # convert each pixel RGBs to different color spaces
                    (tupleFlag, XX, YY, ZZ) = \
                            getColorComponent(fgCsvFile=fgCsvFile,
                                    fgR = foreR[i], fgG = foreG[i], fgB = foreB[i])

                    # find pixel values in the probability array
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

                    count += 1 # count the non-black pixels.

                else: # keep track of black pixels.
                    foreProbability = 0
                    blackPixelCount = blackPixelCount + 1

                # put the probability value into a list for all pixels
                varProbList.append(foreProbability)

                # create mask image from prob: (forground)1 if >.5, 0 if <.5
                varMaskList[i] = int(round(foreProbability))

            print('Cleaning and segmenting...')

            if (savePixels):
                #  write pixels back to image files
                foreMaskPathFilename = os.path.join(fgMaskDir, fgMaksFile)
                maskedFile = \
                        os.path.join(imgFile, fgCsvFile+'_pixel-masked.bmp')
                probFile = os.path.join(foreMaskPathFilename, \
                        fgCsvFile + '_probability.bmp')
                saveImgFile(varMaskList, varProbList, imSize, \
                        maskedFile, probFile)
            # 2 ELIMINATE NOISE FROM CREATED MASK & SEGMENT

            erosElem = array([[1,1,1], [1,1,1], [1,1,1]]) #Erosion element
            dialElem = array([[0,1,0], [1,1,1], [0,1,0]]) #Dialation elemetn

            # Turn to a numpy array and resize
            varMaskList = array(varMaskList)
            varMaskList.resize((imSize[1],imSize[0]))

            #  Count blobs before cleanup
            segmentBeforeCount = ndimage.label(varMaskList, dialElem)[1]

            #  remove stray small groups of foreground pixels
            varMaskList = ndimage.binary_dilation(
                    varMaskList, structure = dialElem, iterations = 1)
            # remove pixel noise by eroding one-pass
            varMaskList = ndimage.binary_erosion(
                    varMaskList, structure = erosElem, iterations = 1)
            varMaskList = ndimage.binary_erosion(
                    varMaskList, structure = erosElem, iterations = 1)
            # add back border pixels in blobs that survived
            varMaskList = ndimage.binary_dilation(
                    varMaskList, structure = dialElem, iterations = 1)

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
                    varMaskList, structure = dialElem, border_value = 1)
            varMaskList = ndimage.binary_erosion(
                    varMaskList, structure = erosElem, border_value = 1)
            varMaskList = ndimage.binary_erosion(
                    varMaskList, structure = erosElem, border_value = 1)
            varMaskList = ndimage.binary_dilation(
                    varMaskList, structure = dialElem, border_value = 1)

            varMaskList = varMaskList.ravel()
            # invert the list for the next step
            for i in range(0, len(varMaskList)):
                if varMaskList[i] > 0:
                    varMaskList[i] = 0
                else:
                    varMaskList[i] = 1

            varMaskList = array(varMaskList)
            varMaskList.resize((imSize[1],imSize[0]))
            # segment the array into continuous regions of
            # increasing integer values, skip the array and
            # return the number of blobs found
            segmentCount = \
                    ndimage.label(varMaskList, dialElem)[1]

            if (savePixels):
                #write the pixels back to image files after removing noise
                foreMaskPathFilename = os.path.join(fgMaskDir, fgMaksFile)
                maskedFile = os.path.join(imgFile, \
                            fgCsvFile+'_cleaned_pixel-masked.bmp')
                probFile = os.path.join(foreMaskPathFilename, \
                            fgCsvFile+'_cleaned_probability.bmp')
                saveImgFile(varMaskList, varProbList, imSize, \
                        maskedFile, probFile)

            # 3 COMPARE CALCULATED MASK WITH ORIGINAL MASK
            print('Comparing to masks...')
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

            # open the mask image with FG areas in white, BG in black
            foreMaskPathFilename = os.path.join(fgMaskDir, fgMaksFile)
            foreMaskImage = Image.open(foreMaskPathFilename).convert("1")

            # turn the foreground mask into a list
            foreMaskList = list(foreMaskImage.getdata())
            foreMaskImage = ''

            # open the mask image with FG areas in white, BG in black
            backMaskPathFilename = os.path.join(bgMaskDir, fgMaksFile)
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

            # *******************************************************
            #  Output stuff
            # *******************************************************
            print("\nColor space: %s\n"
                    "pixels looked at: %d\n"
                    ", black pixels: %d\n"
                    ", sum: %d\n"
                    "pixels not in big probability array: %d\n"
                    "pixels in foreground mask: %d\n"
                    "correct foreground pixels: %d ( = %d %%)\n"
                    "incorrect foreground pixels: %d\n"
                    "correct background pixels: %d ( = %d %%)\n"
                    "blobs before cleanup: %d\n"
                    ", blobs after cleanup: %d\n"
                    %(fgCsvFile, count, blackPixelCount, count+blackPixelCount,
                        foreBigNans, foreMaskCount, foreCount, QsegFore,
                        badForeCount, backCount, QsegBack, segmentBeforeCount,
                        segmentCount) )

            allPixelMasterArray.append(
                    [imgFile, count, blackPixelCount, foreMaskCount, foreCount,
                     badForeCount, backCount, segmentBeforeCount, segmentCount,
                     foreBigNans, QsegFore, QsegBack])

        print('\nWriting output file...')

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
def getColorComponent(fgCsvFile="", fgR=0, fgG=0, fgB=0, write256 = True ):
    tupleFlag = 0
    XX, YY, ZZ = 0, 0, 0
    r = fgR/255.0,
    g = fgG/255.0,
    b = fgB/255.0)

    if fgCsvFile == "":
        return (-1, XX, YY, ZZ)

    if fgCsvFile[0:3] == 'RG_':
        XX = fgR
        YY = fgG
        tupleFlag = 1
    elif fgCsvFile[0:3] == 'RB_':
        XX = fgR
        YY = fgB
        tupleFlag = 1
    elif fgCsvFile[0:3] == 'GB_':
        XX = fgG
        YY = fgB
        tupleFlag = 1

    elif fgCsvFile[0:2] == 'R_':
        XX = fgR
    elif fgCsvFile[0:2] == 'G_':
        XX = fgG
    elif fgCsvFile[0:2] == 'B_':
        XX = fgB

    elif fgCsvFile[0:3] == 'BR_':
        if fgR <> 0:
            XX = float(fgB)/fgR
    elif fgCsvFile[0:3] == 'GR_':
        if fgR <> 0:
            XX = float(fgG)/fgR
    elif fgCsvFile[0:5] == 'GRBR_':
        if fgR <> 0:
            XX = float(fgG)/fgR
            YY = float(fgB)/fgR
            tupleFlag = 1

    elif fgCsvFile[0:6] == 'RGB3D_':
        XX = fgR
        YY = fgG
        ZZ = fgB
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


def saveImgFile ( varMaskList, varProbList, imSize, maskedFile, probFile ):
    print('Saving image files...')

    # put the mask list into the range 0 - 255
    varMaskList = array(varMaskList) * 255
    fgImg = Image.new('1', imSize, 'white')
    #  var list should be 255 for preserve and 0 for black
    fgImg.putdata(varMaskList)

    fgImg.save(maskedFile)
    fgImg = ImageChops.invert(fgImg)

    #  put the probability list into the range 0 - 255
    varProbList = array(varProbList) * 255
    newProbImage = Image.new('L', imSize, 'white')
    #  probability list is greyscale
    newProbImage.putdata(varProbList)
    newProbImage.save(probFile)

if __name__ == "__main__":
    main()
    print("Done!")

