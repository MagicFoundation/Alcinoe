/*
 *  Copyright 2004 The WebRTC Project Authors. All rights reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */

#ifndef RTC_BASE_FILEUTILS_H_
#define RTC_BASE_FILEUTILS_H_

#include <string>

#if defined(WEBRTC_WIN)
#include <windows.h>
#else
#include <dirent.h>
#include <stdio.h>
#include <sys/stat.h>
#endif  // WEBRTC_WIN

#include "rtc_base/constructormagic.h"

namespace rtc {

class Pathname;

//////////////////////////
// Directory Iterator   //
//////////////////////////

// A DirectoryIterator is created with a given directory. It originally points
// to the first file in the directory, and can be advanecd with Next(). This
// allows you to get information about each file.

class DirectoryIterator {
  friend class Filesystem;

 public:
  // Constructor
  DirectoryIterator();
  // Destructor
  virtual ~DirectoryIterator();

  // Starts traversing a directory
  // dir is the directory to traverse
  // returns true if the directory exists and is valid
  // The iterator will point to the first entry in the directory
  virtual bool Iterate(const Pathname& path);

  // Advances to the next file
  // returns true if there were more files in the directory.
  virtual bool Next();

  // returns true if the file currently pointed to is a directory
  virtual bool IsDirectory() const;

  // returns the name of the file currently pointed to
  virtual std::string Name() const;

 private:
  std::string directory_;
#if defined(WEBRTC_WIN)
  WIN32_FIND_DATA data_;
  HANDLE handle_;
#else
  DIR* dir_;
  struct dirent* dirent_;
  struct stat stat_;
#endif
};

class FilesystemInterface {
 public:
  virtual ~FilesystemInterface() {}

  // This will attempt to delete the path located at filename.
  // It DCHECKs and returns false if the path points to a folder or a
  // non-existent file.
  virtual bool DeleteFile(const Pathname& filename) = 0;

  // This moves a file from old_path to new_path, where "old_path" is a
  // plain file. This DCHECKs and returns false if old_path points to a
  // directory, and returns true if the function succeeds.
  virtual bool MoveFile(const Pathname& old_path, const Pathname& new_path) = 0;

  // Returns true if pathname refers to a directory
  virtual bool IsFolder(const Pathname& pathname) = 0;

  // Returns true if pathname refers to a file
  virtual bool IsFile(const Pathname& pathname) = 0;

  // Determines the size of the file indicated by path.
  virtual bool GetFileSize(const Pathname& path, size_t* size) = 0;
};

class Filesystem {
 public:
  static bool DeleteFile(const Pathname& filename) {
    return GetFilesystem()->DeleteFile(filename);
  }

  static bool MoveFile(const Pathname& old_path, const Pathname& new_path) {
    return GetFilesystem()->MoveFile(old_path, new_path);
  }

  static bool IsFolder(const Pathname& pathname) {
    return GetFilesystem()->IsFolder(pathname);
  }

  static bool IsFile(const Pathname& pathname) {
    return GetFilesystem()->IsFile(pathname);
  }

  static bool GetFileSize(const Pathname& path, size_t* size) {
    return GetFilesystem()->GetFileSize(path, size);
  }

 private:
  static FilesystemInterface* GetFilesystem();
  RTC_DISALLOW_IMPLICIT_CONSTRUCTORS(Filesystem);
};

}  // namespace rtc

#endif  // RTC_BASE_FILEUTILS_H_
