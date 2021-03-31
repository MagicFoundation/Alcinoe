LOCAL_PATH := $(call my-dir)


#
# Statically Linked
#

include $(CLEAR_VARS)
LOCAL_MODULE            := sqlite3-a
LOCAL_MODULE_FILENAME   := libsqlite3
LOCAL_SRC_FILES         := ../../sqlite3mc.c
LOCAL_CFLAGS            += -w -Wno-error -DSQLITE_ENABLE_FTS3 -DNDEBUG -DNO_TCL -D_CRT_SECURE_NO_DEPRECATE -DSQLITE_TEMP_STORE=1
# LOCAL_ALLOW_UNDEFINED_SYMBOLS :=true
include $(BUILD_STATIC_LIBRARY)
