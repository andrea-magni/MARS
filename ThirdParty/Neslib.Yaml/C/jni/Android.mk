LOCAL_PATH:= $(call my-dir)/..
include $(CLEAR_VARS)

LOCAL_MODULE     := yaml_android
LOCAL_CFLAGS     := -O3 -DYAML_DECLARE_STATIC -DHAVE_CONFIG_H
LOCAL_C_INCLUDES := $(LOCAL_PATH)/src
LOCAL_SRC_FILES  := src/api.c src/dumper.c src/emitter.c src/loader.c src/parser.c src/reader.c src/scanner.c src/writer.c
  
include $(BUILD_STATIC_LIBRARY)