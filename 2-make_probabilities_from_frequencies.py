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
    firstCountDict = {} # count of pixels
    firstDictionary = {} # frequency of pixels

    #  open the foreground data file
    fdTmp = open(fgFile, 'rb')
    fgReader = csv.reader(fdTmp, delimiter = ',')

    # File format is: Xcol, Ycol (if it exists), Count, Frequency
    fgReader.next() # skip header row
    for fgRow in fgReader:
        if len(fgRow) == 3:
            tmpkey = float(fgRow[0])
            firstCountDict[tmpkey] = int(float(fgRow[1]))
            firstDictionary[tmpkey] = float(fgRow[2])

        elif len(fgRow) == 4:
            tmpkey = (float(fgRow[0]),float(fgRow[1]))
            firstCountDict[tmpkey] = float(fgRow[2])
            firstDictionary[tmpkey] = float(fgRow[3])

        elif len(fgRow) == 5:
            tmpkey = ( float(fgRow[0]), float(fgRow[1]), float(fgRow[2]) )
            firstCountDict[tmpkey] = float(fgRow[3])
            firstDictionary[tmpkey] = float(fgRow[4])

    fdTmp.close()

    # over-write the original file!!!
    writerFirstFile = open(fgFile, 'wb')
    writer = csv.writer(writerFirstFile, delimiter = ',',quoting=csv.QUOTE_NONE)

    # open the second data file
    readerSecondFile = open(bgFile, 'rb')
    secondReader = csv.reader(readerSecondFile, delimiter = ',')

    newList = [ 'Frequency', 'Probs_vs_%s'%bgFile.split('/')[-4], 'Count' ]
    secondHeader = secondReader.next()
    if len(secondHeader) >= 3:
        newList.insert(0, secondHeader[0])
    if len(secondHeader) >= 4:
        newList.insert(1, secondHeader[1])
    if len(secondHeader) >= 5:
        newList.insert(2, secondHeader[2])
    writer.writerow(newList)

    # Loop through the second file and match it with the items in
    # the first file, creating a probability [0:1]
    for secondDataItem in secondReader:
        # if the color space is a single vector
        if len(secondDataItem) == 3:
            index = float(secondDataItem[0])
            # If the background item is in the first dictionary,
            # then make the probability
            if index in firstDictionary:
                newList = [index]  #  write the color vector value

                # write the frequency of the value
                # (number of pixels / total count)
                newList.append(firstDictionary[index])

                # write frequency/frequency of background for that color
                newList.append( firstDictionary[index]
                                / ( float(secondDataItem[2])
                                    + firstDictionary[index] ) )

                # write original count
                newList.append(firstCountDict[index])
                writer.writerow(newList)
                # remove the item from the first dictionary
                # to keep track of things
                del firstDictionary[index]
            else:
                # if the item is not in the first dictionary,
                # then write the probability of zero
                writer.writerow([index, float(secondDataItem[2]), 0.0, 0])

        # if the color space is a double
        elif len(secondDataItem) == 4:
            index = (float(secondDataItem[0]), float(secondDataItem[1]))
            if index in firstDictionary:
                #  write the color vector values
                newList = [index[0], index[1]]
                newList.append(firstDictionary[index])
                newList.append( firstDictionary[index]
                                / ( float(secondDataItem[3])
                                    + firstDictionary[index] ) )
                newList.append(firstCountDict[index])
                writer.writerow(newList)
                del firstDictionary[index]

            else:
                writer.writerow([float(secondDataItem[0]),
                                 float(secondDataItem[1]),
                                 float(secondDataItem[3]),
                                 0.0,
                                 0])

        # if the color space is a triple
        elif len(secondDataItem) == 5:
            index = ( float(secondDataItem[0]),
                      float(secondDataItem[1]),
                      float(secondDataItem[2]) )
            if index in firstDictionary:
                # write the color vector values
                newList = [index[0], index[1], index[2]]
                newList.append(firstDictionary[index])
                newList.append(firstDictionary[index]
                               / ( float(secondDataItem[4])
                                   + firstDictionary[index] ) )
                newList.append(firstCountDict[index])
                writer.writerow(newList)
                del firstDictionary[index]
            else:
                writer.writerow([float(secondDataItem[0]),
                                 float(secondDataItem[1]),
                                 float(secondDataItem[2]),
                                 float(secondDataItem[3]),
                                 0.0,
                                 0])

    # for the remaining items that were in the first
    # list but did not occur in the second, write them
    # to the file wiht a probablity of 1
    for items in firstDictionary.iteritems():
        newList = []
        # if the first item in the dictionary is a tuple
        # (it is "iterable"), then start with that tuple
        if hasattr(items[0], '__iter__'):
            for ii in items[0]:
                newList.append(ii)
        else:
            # otherwise, just take the first item
            newList.append(items[0])
        newList.append(items[1])  # append the frequency
        newList.append(1.0)  #  append '1'
        # append the count of the foreground
        newList.append(firstCountDict[items[0]])
        writer.writerow(newList)

    writerFirstFile.close()
    readerSecondFile.close()

if __name__ == "__main__":
    main()
    x = raw_input('Done!  Enter any key to quit.')
