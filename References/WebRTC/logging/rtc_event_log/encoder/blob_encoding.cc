/*
 *  Copyright (c) 2018 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */

#include "logging/rtc_event_log/encoder/blob_encoding.h"

#include <algorithm>

#include "rtc_base/logging.h"

namespace webrtc {

const size_t kMaxVarIntLengthBytes = 10;  // ceil(64 / 7.0) is 10.

namespace {

// Encode a given uint64_t as a varint. From least to most significant,
// each batch of seven bits are put into the lower bits of a byte, and the last
// remaining bit in that byte (the highest one) marks whether additional bytes
// follow (which happens if and only if there are other bits in |input| which
// are non-zero).
// Notes: If input == 0, one byte is used. If input is uint64_t::max, exactly
// kMaxVarIntLengthBytes are used.
std::string EncodeVarInt(uint64_t input) {
  std::string output;
  output.reserve(kMaxVarIntLengthBytes);

  do {
    uint8_t byte = static_cast<uint8_t>(input & 0x7f);
    input >>= 7;
    if (input > 0) {
      byte |= 0x80;
    }
    output += byte;
  } while (input > 0);

  RTC_DCHECK_GE(output.size(), 1u);
  RTC_DCHECK_LE(output.size(), kMaxVarIntLengthBytes);

  return output;
}

// Inverse of EncodeVarInt().
// If decoding is successful, a non-zero number is returned, indicating the
// number of bytes read from |input|, and the decoded varint is written
// into |output|.
// If not successful, 0 is returned, and |output| is not modified.
size_t DecodeVarInt(absl::string_view input, uint64_t* output) {
  RTC_DCHECK(output);

  uint64_t decoded = 0;
  for (size_t i = 0; i < input.length() && i < kMaxVarIntLengthBytes; ++i) {
    decoded += (static_cast<uint64_t>(input[i] & 0x7f)
                << static_cast<uint64_t>(7 * i));
    if (!(input[i] & 0x80)) {
      *output = decoded;
      return i + 1;
    }
  }

  return 0;
}

}  // namespace

std::string EncodeBlobs(const std::vector<std::string>& blobs) {
  RTC_DCHECK(!blobs.empty());

  size_t result_length_bound = kMaxVarIntLengthBytes * blobs.size();
  for (const auto& blob : blobs) {
    // Providing an input so long that it would cause a wrap-around is an error.
    RTC_DCHECK_GE(result_length_bound + blob.length(), result_length_bound);
    result_length_bound += blob.length();
  }

  std::string result;
  result.reserve(result_length_bound);

  // First, encode all of the lengths.
  for (absl::string_view blob : blobs) {
    result += EncodeVarInt(blob.length());
  }

  // Second, encode the actual blobs.
  for (absl::string_view blob : blobs) {
    result.append(blob.data(), blob.length());
  }

  RTC_DCHECK_LE(result.size(), result_length_bound);
  return result;
}

std::vector<absl::string_view> DecodeBlobs(absl::string_view encoded_blobs,
                                           size_t num_of_blobs) {
  if (encoded_blobs.empty()) {
    RTC_LOG(LS_WARNING) << "Corrupt input; empty input.";
    return std::vector<absl::string_view>();
  }

  if (num_of_blobs == 0u) {
    RTC_LOG(LS_WARNING)
        << "Corrupt input; number of blobs must be greater than 0.";
    return std::vector<absl::string_view>();
  }

  size_t read_idx = 0;

  // Read the lengths of all blobs.
  std::vector<uint64_t> lengths(num_of_blobs);
  for (size_t i = 0; i < num_of_blobs; ++i) {
    if (read_idx >= encoded_blobs.length()) {
      RTC_DCHECK_EQ(read_idx, encoded_blobs.length());
      RTC_LOG(LS_WARNING) << "Corrupt input; excessive number of blobs.";
      return std::vector<absl::string_view>();
    }

    const size_t read_bytes =
        DecodeVarInt(encoded_blobs.substr(read_idx), &lengths[i]);
    if (read_bytes == 0) {
      RTC_LOG(LS_WARNING) << "Corrupt input; varint decoding failed.";
      return std::vector<absl::string_view>();
    }

    read_idx += read_bytes;

    // Note: It might be that read_idx == encoded_blobs.length(), if this
    // is the last iteration, and all of the blobs are the empty string.
    RTC_DCHECK_LE(read_idx, encoded_blobs.length());
  }

  // Read the blobs themselves.
  std::vector<absl::string_view> blobs(num_of_blobs);
  for (size_t i = 0; i < num_of_blobs; ++i) {
    if (read_idx + lengths[i] < read_idx) {  // Wrap-around detection.
      RTC_LOG(LS_WARNING) << "Corrupt input; unreasonably large blob sequence.";
      return std::vector<absl::string_view>();
    }

    if (read_idx + lengths[i] > encoded_blobs.length()) {
      RTC_LOG(LS_WARNING) << "Corrupt input; blob sizes exceed input size.";
      return std::vector<absl::string_view>();
    }

    blobs[i] = encoded_blobs.substr(read_idx, lengths[i]);
    read_idx += lengths[i];
  }

  if (read_idx != encoded_blobs.length()) {
    RTC_LOG(LS_WARNING) << "Corrupt input; unrecognized trailer.";
    return std::vector<absl::string_view>();
  }

  return blobs;
}

}  // namespace webrtc
