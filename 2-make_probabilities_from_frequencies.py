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

    # This program will take all the csv files in one directory and divide
    # them by their equivalent in another, matching up the color value in
    # the first column, keeping the frequencyas counts of the first file,
    # and dividing the frequencies listed in the third column of the first
    # file by the sum of those and the frequencies of the second.  Thus,
    # if the second file  has no matching value, then the probability is
    # 1.0 and if they are equal, the probability is 0.5.

    # *****************************************************************
    # Open dialog to find the first and second subdirectories.
    # *****************************************************************

    fgDir = getMainDir(message='Select the foreground directory: ')
    if fgDir == '':
        print("\nNo directory selected, program aborted.\n")
        return

    bgDir = getMainDir(message='Select the background directory: ')
    if bgDir == '':
        print("\nNo directory selected, program aborted.\n")
        return

    # *****************************************************************
    # Load the 1_d and 2-D color spaces probabilities into a dictionary
    # with the color space tuples (XX, YY) as keys
    # *****************************************************************

    # get the mask file names in each subdirectory
    #fgCsvFiles = \
    #        [name for name in os.listdir(fgDir) if name.endswith(".csv") ]

    csvFiles = [ (os.path.join(fgDir,name), os.path.join(bgDir,name))
                   for name in os.listdir(fgDir)
                   if name.endswith(".csv") ]

    # for each color space file...
    for csvFile in csvFiles:
        if os.path.exists(csvFile[0]) and os.path.exists(csvFile[1]):
            calcProb(fgFile=csvFile[0], bgFile=csvFile[1])
        else:
            print ("Either %s or %s does not exists" % (csvFile[0], csvFile[1]))

def calcProb ( fgFile="", bgFile="" ):
    print ( "Calculating probability for %s" % os.path.split(fgFile)[1] )

    # load up the first list with color value and frequency
    fgCountDict = {} # count of pixels
    fgFrecDict = {} # frequency of pixels

    #  open the foreground data file
    fdTmp = open(fgFile, 'rb')
    fgReader = csv.reader(fdTmp, delimiter = ',')

    # File format is: Xcol, Ycol (if it exists), Count, Frequency
    fgReader.next() # skip header row
    for fgRow in fgReader:
        if len(fgRow) == 3:
            tmpkey = (float(fgRow[0]),)
            fgCountDict[tmpkey] = int(float(fgRow[1]))
            fgFrecDict[tmpkey] = float(fgRow[2])

        elif len(fgRow) == 4:
            tmpkey = (float(fgRow[0]),float(fgRow[1]))
            fgCountDict[tmpkey] = float(fgRow[2])
            fgFrecDict[tmpkey] = float(fgRow[3])

        elif len(fgRow) == 5:
            tmpkey = ( float(fgRow[0]), float(fgRow[1]), float(fgRow[2]) )
            fgCountDict[tmpkey] = float(fgRow[3])
            fgFrecDict[tmpkey] = float(fgRow[4])

    fdTmp.close()

    # over-write the original file!!!
    fgFd = open(fgFile, 'wb')
    fgWriter = csv.writer(fgFd, delimiter = ',',quoting=csv.QUOTE_NONE)

    # open the second data file
    bgFd = open(bgFile, 'rb')
    bgReader = csv.reader(bgFd, delimiter = ',')

    newRow = [ 'Frequency', 'Probs_vs_%s'%bgFile.split('/')[-4], 'Count' ]
    secondHeader = bgReader.next()
    if len(secondHeader) >= 3:
        newRow.insert(0, secondHeader[0])
    if len(secondHeader) >= 4:
        newRow.insert(1, secondHeader[1])
    if len(secondHeader) >= 5:
        newRow.insert(2, secondHeader[2])
    fgWriter.writerow(newRow)

    # Loop through the second file and match it with the items in
    # the first file, creating a probability [0:1]
    for bgRow in bgReader:
        if len(bgRow) == 3:
            index = (float(bgRow[0]),)
        elif len(bgRow) == 4:
            index = (float(bgRow[0]), float(bgRow[1]))
        elif len(bgRow) == 5:
            index = ( float(bgRow[0]), float(bgRow[1]), float(bgRow[2]) )

        # The BG Frequency is always the penultimate column.
        bgFrec = float(bgRow[len(bgRow)-1])

        if index in fgFrecDict:
            newRow = list(index) # convert set into list.
            newRow.append(fgFrecDict[index]) # (num pixels / total pixels)
            # write frequency/frequency of background for that color
            newRow.append( fgFrecDict[index] / (bgFrec+fgFrecDict[index]) )
            newRow.append(fgCountDict[index]) # write original count
            del fgFrecDict[index] #remove to keep track
        else:
            if len(bgRow) == 3:
                newRow = [float(bgRow[0]), float(bgRow[2]), 0.0, 0]
            elif len(bgRow) == 4:
                newRow = \
                        [float(bgRow[0]),float(bgRow[1]),float(bgRow[3]),0.0,0]
            elif len(bgRow) == 5:
                newRow = \
                        [float(bgRow[0]), float(bgRow[1]), float(bgRow[2]), \
                                float(bgRow[3]), 0.0, 0]

        fgWriter.writerow(newRow)

    # for the remaining items that were in the first
    # list but did not occur in the second, write them
    # to the file with a probablity of 1
    for key, value in fgFrecDict.iteritems():
        newRow = []
        for i in key: # Key should be a tuple (iterable)
            newRow.append(i)

        # freq, 1, count of FG
        newRow.extend( [value, 1.0, fgCountDict[key]] )

        fgWriter.writerow(newRow)

    fgFd.close()
    bgFd.close()

if __name__ == "__main__":
    main()
    x = raw_input('Done!  Enter any key to quit.')
