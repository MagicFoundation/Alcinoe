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

#include "modules/video_coding/codecs/vp9/include/vp9.h"
#include "sdk/android/generated_vp9_jni/jni/VP9Decoder_jni.h"
#include "sdk/android/generated_vp9_jni/jni/VP9Encoder_jni.h"
#include "sdk/android/src/jni/jni_helpers.h"

namespace webrtc {
namespace jni {

static jlong JNI_VP9Encoder_CreateEncoder(JNIEnv* jni,
                                          const JavaParamRef<jclass>& w) {
  return jlongFromPointer(VP9Encoder::Create().release());
}

static jboolean JNI_VP9Encoder_IsSupported(JNIEnv* jni,
                                           const JavaParamRef<jclass>&) {
  return !SupportedVP9Codecs().empty();
}

static jlong JNI_VP9Decoder_CreateDecoder(JNIEnv* jni,
                                          const JavaParamRef<jclass>& w) {
  return jlongFromPointer(VP9Decoder::Create().release());
}

static jboolean JNI_VP9Decoder_IsSupported(JNIEnv* jni,
                                           const JavaParamRef<jclass>&) {
  return !SupportedVP9Codecs().empty();
}

}  // namespace jni
}  // namespace webrtc
