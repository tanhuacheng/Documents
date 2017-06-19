1. Android 使用现有库

    ```make
    Android 源码中

    LOCAL_PATH := $(call my-dir)
    include $(CLEAR_VARS)
    LOCAL_MODULE_TAGS := optional
    LOCAL_PREBUILT_LIBS := path/to/libraries.a path/to/libraries.so
    include $(BUILD_MULTI_PREBUILT)

    或使用 NDK

    LOCAL_PATH := $(call my-dir)
    include $(CLEAR_VARS)
    LOCAL_MODULE_TAGS := optional
    LOCAL_MODULE := name-of-library
    LOCAL_SRC_FILES := path/to/libraries.a
    include $(PREBUILT_STATIC_LIBRARY)
    ```
