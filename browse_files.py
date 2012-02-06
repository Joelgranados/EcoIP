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
if sys.platform == "win32":
    import win32gui
    from win32com.shell import shell, shellcon

def fileBrowser(browse = 'folder', message = ''):

    #mydocs_pidl = shell.SHGetFolderLocation (0, shellcon.CSIDL_PERSONAL, 0, 0)
    drives_pidl = shell.SHGetFolderLocation (0, shellcon.CSIDL_DRIVES, 0, 0)

    ##pidl, display_name, image_list = shell.SHBrowseForFolder (
    ##  win32gui.GetDesktopWindow (),
    ##  mydocs_pidl,
    ##  "Select a file or folder",
    ##  shellcon.BIF_BROWSEINCLUDEFILES,
    ##  None,
    ##  None
    ##)

    if browse == 'file':
        pidl, display_name, image_list = shell.SHBrowseForFolder (
            win32gui.GetDesktopWindow (),
            drives_pidl,
            message,
            shellcon.BIF_BROWSEINCLUDEFILES,
            None,
            None
        )

    elif browse == 'folder':
        pidl, display_name, image_list = shell.SHBrowseForFolder (
            win32gui.GetDesktopWindow (),
            drives_pidl,
            message,
            0,
            None,
            None
        )

    else:
        return ""

    if (pidl, display_name, image_list) == (None, None, None):
      return ""  # "Nothing selected"
    else:
      path = shell.SHGetPathFromIDList(pidl)
      return path
    # print "Selected:", path
    #  print "Display name:", display_name
    #  os.startfile (path)

def getMainDir(message="Select directory for photos..."):
    if sys.platform == "win32":
        return fileBrowser('folder', message)
    elif sys.platform == "linux2":
        picdir = raw_input(message)
        return os.path.realpath(picdir)


