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
#import ImageTk
from numpy import *
from scipy import *
from scipy import ndimage
import math
import ColorConverter
from browse_files import *

def main():

    #  This program will take all the csv files in one directory and divide them by their equivalent in another,
    #  matching up the color value in the first column, keeping the frequencyas counts of the first file, and dividing the
    #  frequencies listed in the third column of the first file by the sum of those and the frequencies of the second.  Thus, if the second file
    #  has no matching value, then the probability is 1.0 and if they are equal, the probability is 0.5.

    #  *********************************************************************************************************************
    #  Open dialog to find the first and second subdirectories.
    #  *********************************************************************************************************************  

    rootDirName = fileBrowser('folder', 'Select the foreground directory')
    if rootDirName == '':
        print
        print "No directory selected, program aborted."
        print
        return
    
    firstPath = rootDirName + '/'

    rootDirName = fileBrowser('folder', 'Select the background directory')
    if rootDirName == '':
        print
        print "No directory selected, program aborted."
        print
        return
    
    secondPath = rootDirName + '/'
    
##    firstPath = 'C:/Documents and Settings/Eric Howard/Desktop/Martins_images/PHOTOS/masks/plant/'
##    secondPath = 'C:/Documents and Settings/Eric Howard/Desktop/Martins_images/PHOTOS/masks/background/'

    #  *********************************************************************************************************************
    #  Load the 1_d and 2-D color spaces probabilities into a dictionary with the color space tuples (XX, YY) as keys
    #  *********************************************************************************************************************            

    firstFileList = os.listdir(firstPath)  #  get the mask file names in each subdirectory
    secondFileList = os.listdir(secondPath)  #  get the mask file names in each subdirectory
    for csvFile in firstFileList:  #  for each color space file...
        if os.path.splitext(firstPath + csvFile)[1] == '.csv':  #  could be other types of files in the subdirectory
            firstFileListName = (firstPath + csvFile)
            secondFileListName = (secondPath + csvFile)
            print 'File name:', csvFile
            readerFirstFile = open(firstFileListName, 'rb')
            firstReader = csv.reader(readerFirstFile, delimiter = ',')  #  open the first data file

            firstDictionary = {}  #  load up the first list with color value and frequency
            firstCountDict = {}
            headerFlag = 1

            for dataItem in firstReader:  #  File format is: Xcol, Ycol (if it exists), Count, Frequency
                if headerFlag == 1:  #  skip the first row, has only header text
                    headerFlag = 0
                else:
                    if len(dataItem) == 3:
                        firstCountDict[float(dataItem[0])] = int(float(dataItem[1]))  # count of pixels
                        firstDictionary[float(dataItem[0])] = float(dataItem[2])  #  frequency of pixels
                    elif len(dataItem) == 4:
                        firstCountDict[(float(dataItem[0]),float(dataItem[1]))] = float(dataItem[2])
                        firstDictionary[(float(dataItem[0]),float(dataItem[1]))] = float(dataItem[3])
                    elif len(dataItem) == 5:
                        firstCountDict[(float(dataItem[0]),float(dataItem[1]),float(dataItem[2]))] = float(dataItem[3])
                        firstDictionary[(float(dataItem[0]),float(dataItem[1]),float(dataItem[2]))] = float(dataItem[4])
            readerFirstFile.close()

            writerFirstFile = open(firstPath + csvFile, 'wb')  ## !! over-write the original file
            writer = csv.writer(writerFirstFile, delimiter = ',',quoting=csv.QUOTE_NONE)
            
            readerSecondFile = open(secondFileListName, 'rb')
            secondReader = csv.reader(readerSecondFile, delimiter = ',')  #  open the second data file

            headerFlag = 1
            secondDirName = os.path.split(secondPath)[0]
            secondDirName = secondDirName.split('/')[len(secondDirName.split('/'))-2]

            for secondDataItem in secondReader:  #  loop through the second file and match it with the items in the first file, creating a probability [0:1]
                if headerFlag == 1:
                    if len(secondDataItem) == 3:
                        newList = [secondDataItem[0], 'Frequency', 'Probs_vs_' + secondDirName, 'Count']
                    elif len(secondDataItem) == 4:
                        newList = [secondDataItem[0], secondDataItem[1], 'Frequency', 'Probs_vs_' + secondDirName, 'Count']
                    elif len(secondDataItem) == 5:
                        newList = [secondDataItem[0], secondDataItem[1], secondDataItem[2], 'Frequency', 'Probs_vs_' + secondDirName, 'Count']
                    writer.writerow(newList)
                    headerFlag = 0
                else:
                    if len(secondDataItem) == 3:  #  if the color space is a single vector
                        index = float(secondDataItem[0])
                        if index in firstDictionary:  #  if the background item is in the first dictionary, then make the probability
                            newList = [index]  #  write the color vector value
                            newList.append(firstDictionary[index])  #  write the frequency of the value (number of pixels / total count)
                            newList.append(firstDictionary[index] / (float(secondDataItem[2]) + firstDictionary[index]))  #  write frequency / frequency of background for that color
                            newList.append(firstCountDict[index])  #  write original count
                            writer.writerow(newList)                                            
                            del firstDictionary[index]  #  remove the item from the first dictionary to keep track of things
                        else:  #  if the item is not in the first dictionary, then write the probability of zero
                            writer.writerow([index, float(secondDataItem[2]), 0.0, 0])
                    elif len(dataItem) == 4:  #  if the color space is a double
                        index = (float(secondDataItem[0]), float(secondDataItem[1]))
                        if index in firstDictionary:
                            newList = [index[0], index[1]]  #  write the color vector values
                            newList.append(firstDictionary[index])
                            newList.append(firstDictionary[index] / (float(secondDataItem[3]) + firstDictionary[index]))
                            newList.append(firstCountDict[index])
                            writer.writerow(newList) 
                            del firstDictionary[index]
                        else:
                            writer.writerow([float(secondDataItem[0]), float(secondDataItem[1]), float(secondDataItem[3]), 0.0, 0])
                    elif len(dataItem) == 5:  #  if the color space is a triple
                        index = (float(secondDataItem[0]), float(secondDataItem[1]), float(secondDataItem[2]))
                        if index in firstDictionary:
                            newList = [index[0], index[1], index[2]]  #  write the color vector values
                            newList.append(firstDictionary[index])
                            newList.append(firstDictionary[index] / (float(secondDataItem[4]) + firstDictionary[index]))
                            newList.append(firstCountDict[index])
                            writer.writerow(newList) 
                            del firstDictionary[index]
                        else:
                            writer.writerow([float(secondDataItem[0]), float(secondDataItem[1]), float(secondDataItem[2]), float(secondDataItem[3]), 0.0, 0])

            for items in firstDictionary.iteritems():  #  for the remaining items that were in the first list but did not occur in the second, write them to the file wiht a probablity of 1
                newList = []
                if hasattr(items[0], '__iter__'):  #  if the first item in the dictionary is a tuple (it is "iterable"), then start with that tuple
                    for ii in items[0]:
                        newList.append(ii)
                else:
                    newList.append(items[0])  #  otherwise, just take the first item
                newList.append(items[1])  # append the frequency
                newList.append(1.0)  #  append '1'
                newList.append(firstCountDict[items[0]])  #  append the count of the foreground
                writer.writerow(newList) 
               
            writerFirstFile.close()
            readerSecondFile.close()

if __name__ == "__main__":
    main()
    x = raw_input('Done!  Enter any key to quit.')
