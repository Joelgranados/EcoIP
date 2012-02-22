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
#from ImageTk import *
#import pickle
from numpy import *
import ColorConverter
from browse_files import *

# *****************************************************************
# Turn images into various color spaces
# Convert to range 0 - 255
# Convert to derived color spaces (e.g. [Red + Green] / Blue) and
# then convert to 0 - 255
# Accumulate in a master 2D histogram
# Normalize the histogram and use Bayes rule for creating probabilities,
# per subdirectory
# *****************************************************************


def main():

    ##################  Change these flags to modify the program #################

    #  output of color conversion should be scaled, each component, to 0-255
    write256 = True

    TFcode = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]  #  15

    RGB_bool = TFcode[0]
    HSL_bool = TFcode[1]  #  what color spaces to run
    Yxy_bool = TFcode[2]
    NRGB_bool = TFcode[3]
    NRGB2D_bool = TFcode[4]
    Lab_bool = TFcode[5]
    ExRGB_bool = TFcode[6]
    ATD1_bool = TFcode[7]
    ATD2_bool = TFcode[8]
    Atd_bool = TFcode[9]
    NDI123_bool = TFcode[10]
    CIVE_bool = TFcode[11]
    shadow_bool = TFcode[12]
    i123_bool = TFcode[13]
    Ingling_bool = TFcode[14]

    ########################################################################

    #  Open dialog to find the photo subdirectory.
    #  Assumed: \masks\ holds more folders.
    #  Each subfolder holds mask files that have the
    # same name as each photo, but are bmp's.

    photoPath = fileBrowser('folder', 'Select directory for photos...')
    if photoPath == '':
        print
        print "No directory selected, program aborted."
        print
        return

    photoPath = photoPath +'\\'
    photoList = os.listdir(photoPath)

    foreMaskPathFilename = \
            fileBrowser('file', 'Select the mask file, cancel if no mask.')
    if foreMaskPathFilename == '':
        print
        print "No mask selected."
        print
        maskFlag = 0
        foreMaskPathFilename = photoPath + '\\junk.csv'
    else:
        maskFlag = 1

    fileNamePrepend = raw_input('Enter file output name:')
    print("Output file will be in mask directory "
            "or if no mask, then in photo directory")

    outFileList = []
    colorList = []
    header = ['filename','foreground mask pixels','background mask pixles']
    writerFile = open( os.path.join(os.path.split(foreMaskPathFilename)[0],
                                    fileNamePrepend + '.csv'), 
                       'wb')
    writer = csv.writer(writerFile, delimiter = ',', quoting=csv.QUOTE_NONE)

    if RGB_bool:
        outFileList = outFileList + ['RGB']
        header = header + ['RGB_R mean','stdev',
                'RGB_G mean','stdev','RGB_B mean','stdev']
    if HSL_bool:
        outFileList = outFileList + ['HSL']
        header = header + ['HSL_H mean','stdev','HSL_S mean',
                'stdev','HSL_L mean','stdev']
    if Yxy_bool:
        outFileList = outFileList + ['Yxy']
        header = header + ['Yxy_Y mean','stdev','Yxy_x mean',
                'stdev','Yxy_y mean','stdev']
    if NRGB_bool:
        outFileList = outFileList + ['NRGB']
        header = header + ['NRGB_NR mean','stdev','NRGB_NG mean',
                'stdev','NRGB_NB mean','stdev']
    if NRGB2D_bool:
        outFileList = outFileList + ['NRGB2D']
        header = header + ['NRGB_2D_1 mean','stdev','NRGB_2D_2 mean','stdev']
    if Lab_bool:
        outFileList = outFileList + ['Lab']
        header = header + ['Lab_L mean','stdev','Lab_a mean',
                'stdev','Lab_b mean','stdev']
    if Ingling_bool:
        outFileList = outFileList + ['Ingling']
        header = header + ['Ingling_V mean','stdev','Ingling_r_g mean',
                'stdev','Ingling_b_y mean','stdev']
    if ExRGB_bool:
        outFileList = outFileList + ['ExRGB']
        header = header + ['ExRGB_ExG mean','stdev','ExRGB_1 mean',
                'stdev','ExRGB_2 mean','stdev']
    if ATD1_bool:
        outFileList = outFileList + ['ATD1']
        header = header + ['ATD_A1 mean','stdev','ATD_T1 mean',
                'stdev','ATD_D1 mean','stdev']
    if Atd_bool:
        outFileList = outFileList + ['Atd']
        header = header + ['Atd_A1 mean','stdev','Atd_t mean',
                'stdev','Atd_d mean','stdev']
    if ATD2_bool:
        outFileList = outFileList + ['ATD2']
        header = header + ['ATD_A2 mean','stdev','ATD_T2 mean',
                'stdev','ATD_D2 mean','stdev']
    if NDI123_bool:
        outFileList = outFileList + ['NDI123']
        header = header + ['NDI_1 mean','stdev','NDI_2 mean',
                'stdev','NDI_3 mean','stdev']
    if i123_bool:
        outFileList = outFileList + ['i123']
        header = header + ['i123_1 mean','stdev','i123_2 mean',
                'stdev','i123_3 mean','stdev']
    if CIVE_bool:
        outFileList = outFileList + ['CIVE']
        header = header + ['CIVE mean','stdev']
    if shadow_bool:
        outFileList = outFileList + ['shadow']
        header = header + ['shadow mean','stdev']

    writer.writerow(header)
    rowList = []

    photoFileIter = iter(photoList)
    loopFiles = True
    #  iterate through all the files
    while loopFiles:
        rowList = []
        openFile = True
        # if there are photo files that make the program crash,
        # catch the exception and move to the next
        while openFile:
            try:
                photoFile = photoFileIter.next()
                #  could be other types of files in the subdirectory
                if os.path.splitext(photoPath + photoFile)[1] == '.jpg':
                    #  make the path and file name for opening a file in a subdirectory
                    imagePathFilename = photoPath + photoFile
                    print
                    print 'Processing image:', imagePathFilename
                    image = Image.open(imagePathFilename)  #  open the image and mask files
                    image.load()
                    openFile = False

            except StopIteration:
                loopFiles = False
                break

            except:
                print
                print 'Skipped file!', imagePathFilename
                print

        #  end of file encountered, break out of top while loop
        if not loopFiles: break

        rowList = rowList + [imagePathFilename]

        if maskFlag:  #  if there is a mask to apply
            #  open the mask image with foreground areas in white, background in black
            maskImage = Image.open(foreMaskPathFilename).convert("L")
        else:
            size = image.size
            maskImage = Image.new("L",size, 255)
        maskSource = array(list(maskImage.getdata()))

        imageSource = image.split()  #  split image into RGB
        #  get pixel values for every pixel in image
        foreR_original = array(list(imageSource[0].getdata()))
        foreG_original = array(list(imageSource[1].getdata()))
        foreB_original = array(list(imageSource[2].getdata()))

        foreR = foreR_original  #  make copies for putting in new color spaces
        foreG = foreG_original
        foreB = foreB_original

        firstColorSpaceFlag = True
        for colorSpace in outFileList:  #  one color space at at time
            print 'Working on', colorSpace

            Flag2D = 0
            Flag1D = 0
            foreCount = -1
            backCount = 0

            # convert RGBs to different color spaces with the output
            # range 0 - 255 for each color component (see subroutines)

            # send pixel data to subroutines to calculate colors
            for i in range(0, len(foreR_original)):
                sys.stdout.write('')

                # if the pixel is in the foreground of the mask
                if maskSource[i] == 255:
                    foreCount = foreCount + 1
                    r = float(foreR_original[i])/255.0
                    g = float(foreG_original[i])/255.0
                    b = float(foreB_original[i])/255.0

                    # *********************  Accumulate  *********************

                    if colorSpace == 'HSL':
                        foreR[foreCount], foreG[foreCount], foreB[foreCount] = \
                                ColorConverter.rgb_to_HSL(r, g, b, write256)

                    elif colorSpace == 'Yxy':
                        foreR[foreCount], foreG[foreCount], foreB[foreCount] = \
                                ColorConverter.rgb_to_Yxy(r, g, b, write256)

                    elif colorSpace == 'NRGB':
                        foreR[foreCount], foreG[foreCount], foreB[foreCount] = \
                                ColorConverter.rgb_to_NRGB(r, g, b, write256)

                    elif colorSpace == 'NRGB2D':
                        foreR[foreCount], foreG[foreCount] = \
                                ColorConverter.rbg_to_NRGB_2D(r, g, b, write256)
                        foreB[foreCount] = 0
                        Flag2D = 1

                    elif colorSpace == 'Lab':
                        foreR[foreCount], foreG[foreCount], foreB[foreCount] = \
                                ColorConverter.rgb_to_Lab(r, g, b, write256)

                    elif colorSpace == 'Ingling':
                        foreR[foreCount], foreG[foreCount], foreB[foreCount] = \
                                ColorConverter.rgb_to_Ingling(r, g, b, write256)

                    elif colorSpace == 'ExRGB':
                        dummy1, dummy2, foreR[foreCount], dummy3 = \
                                ColorConverter.rgb_to_ExRGB(r, g, b, write256)
                        foreG[foreCount], foreB[foreCount] = \
                                ColorConverter.rgb_to_ExRGB_2D(r, g, b, write256)

                    elif colorSpace == 'ATD1':
                        foreR[foreCount], foreG[foreCount], foreB[foreCount], \
                                dummy1, dummy2, dummy3, dummy4, dummy5 = \
                                ColorConverter.rgb_to_ATD(r, g, b, write256)

                    elif colorSpace == 'ATD2':
                        dummy1, dummy2, dummy3, dummy4, dummy5, \
                                foreR[foreCount],foreG[foreCount],foreB[foreCount] = \
                                ColorConverter.rgb_to_ATD(r, g, b, write256)
                    elif colorSpace == 'Atd':
                        foreR[foreCount], dummy2, dummy3, foreG[foreCount], \
                                foreB[foreCount], dummy4, dummy5, dummy6 = \
                                ColorConverter.rgb_to_ATD(r, g, b, write256)

                    elif colorSpace == 'NDI123':
                        foreR[foreCount], foreG[foreCount], foreB[foreCount] = \
                                ColorConverter.rgb_to_NDI123(r, g, b, write256)

                    elif colorSpace == 'i123':
                        foreR[foreCount], foreG[foreCount], foreB[foreCount] = \
                                ColorConverter.rgb_to_i1i2i3(r, g, b, write256)

                    elif colorSpace == 'CIVE':
                        foreR[foreCount] = ColorConverter.rgb_to_CIVE(r, g, b, write256)
                        foreG[foreCount] = 0
                        foreB[foreCount] = 0
                        Flag1D = 1

                    elif colorSpace == 'shadow':
                        foreR[foreCount] = ColorConverter.rgb_to_shadow(r, g, b, write256)
                        foreG[foreCount] = 0
                        foreB[foreCount] = 0
                        Flag1D = 1


                else:  #  pixel is not in foreground in mask
                    backCount = backCount + 1

# *****************************************************************

            Xmean = mean(foreR[0:foreCount+1])
            Ymean = mean(foreG[0:foreCount+1])
            Zmean = mean(foreB[0:foreCount+1])
            Xstd = std(foreR[0:foreCount+1])
            Ystd = std(foreG[0:foreCount+1])
            Zstd = std(foreB[0:foreCount+1])

            if firstColorSpaceFlag:
                rowList = rowList + [foreCount+1, backCount]
                firstColorSpaceFlag = False
            if ((not Flag2D) and (not Flag1D)):
                rowList = rowList + [Xmean, Xstd, Ymean, Ystd, Zmean, Zstd]
            elif Flag2D:
                rowList = rowList + [Xmean, Xstd, Ymean, Ystd]
            elif Flag1D:
                rowList = rowList + [Xmean, Xstd]

        writer.writerow(rowList)

    writerFile.close()
    print 'File closed.'

if __name__ == "__main__":
    main()
    x = raw_input('Done!  Enter any key to quit.')
