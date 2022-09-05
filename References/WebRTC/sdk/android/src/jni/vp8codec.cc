/*
 *  Copyright 2017 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */

#include <jni.h>

#include "modules/video_coding/codecs/vp8/include/vp8.h"
#include "sdk/android/generated_vp8_jni/jni/VP8Decoder_jni.h"
#include "sdk/android/generated_vp8_jni/jni/VP8Encoder_jni.h"
#include "sdk/android/src/jni/jni_helpers.h"

namespace webrtc {
namespace jni {

static jlong JNI_VP8Encoder_CreateEncoder(JNIEnv* jni,
                                          const JavaParamRef<jclass>&) {
  return jlongFromPointer(VP8Encoder::Create().release());
}

static jlong JNI_VP8Decoder_CreateDecoder(JNIEnv* jni,
                                          const JavaParamRef<jclass>&) {
  return jlongFromPointer(VP8Decoder::Create().release());
}

}  // namespace jni
}  // namespace webrtc
