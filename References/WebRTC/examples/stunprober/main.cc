/*
 *  Copyright 2015 The WebRTC Project Authors. All rights reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <map>
#include <memory>
#include <sstream>  // no-presubmit-check TODO(webrtc:8982)

#include "p2p/base/basicpacketsocketfactory.h"
#include "p2p/stunprober/stunprober.h"
#include "rtc_base/checks.h"
#include "rtc_base/flags.h"
#include "rtc_base/helpers.h"
#include "rtc_base/logging.h"
#include "rtc_base/nethelpers.h"
#include "rtc_base/network.h"
#include "rtc_base/ssladapter.h"
#include "rtc_base/strings/string_builder.h"
#include "rtc_base/stringutils.h"
#include "rtc_base/thread.h"
#include "rtc_base/timeutils.h"

using stunprober::StunProber;
using stunprober::AsyncCallback;

WEBRTC_DEFINE_bool(help, false, "Prints this message");
WEBRTC_DEFINE_int(interval,
                  10,
                  "Interval of consecutive stun pings in milliseconds");
WEBRTC_DEFINE_bool(shared_socket,
                   false,
                   "Share socket mode for different remote IPs");
WEBRTC_DEFINE_int(pings_per_ip,
                  10,
                  "Number of consecutive stun pings to send for each IP");
WEBRTC_DEFINE_int(
    timeout,
    1000,
    "Milliseconds of wait after the last ping sent before exiting");
WEBRTC_DEFINE_string(
    servers,
    "stun.l.google.com:19302,stun1.l.google.com:19302,stun2.l.google.com:19302",
    "Comma separated STUN server addresses with ports");

namespace {

const char* PrintNatType(stunprober::NatType type) {
  switch (type) {
    case stunprober::NATTYPE_NONE:
      return "Not behind a NAT";
    case stunprober::NATTYPE_UNKNOWN:
      return "Unknown NAT type";
    case stunprober::NATTYPE_SYMMETRIC:
      return "Symmetric NAT";
    case stunprober::NATTYPE_NON_SYMMETRIC:
      return "Non-Symmetric NAT";
    default:
      return "Invalid";
  }
}

void PrintStats(StunProber* prober) {
  StunProber::Stats stats;
  if (!prober->GetStats(&stats)) {
    RTC_LOG(LS_WARNING) << "Results are inconclusive.";
    return;
  }

  RTC_LOG(LS_INFO) << "Shared Socket Mode: " << stats.shared_socket_mode;
  RTC_LOG(LS_INFO) << "Requests sent: " << stats.num_request_sent;
  RTC_LOG(LS_INFO) << "Responses received: " << stats.num_response_received;
  RTC_LOG(LS_INFO) << "Target interval (ns): "
                   << stats.target_request_interval_ns;
  RTC_LOG(LS_INFO) << "Actual interval (ns): "
                   << stats.actual_request_interval_ns;
  RTC_LOG(LS_INFO) << "NAT Type: " << PrintNatType(stats.nat_type);
  RTC_LOG(LS_INFO) << "Host IP: " << stats.host_ip;
  RTC_LOG(LS_INFO) << "Server-reflexive ips: ";
  for (auto& ip : stats.srflx_addrs) {
    RTC_LOG(LS_INFO) << "\t" << ip;
  }

  RTC_LOG(LS_INFO) << "Success Precent: " << stats.success_percent;
  RTC_LOG(LS_INFO) << "Response Latency:" << stats.average_rtt_ms;
}

void StopTrial(rtc::Thread* thread, StunProber* prober, int result) {
  thread->Quit();
  if (prober) {
    RTC_LOG(LS_INFO) << "Result: " << result;
    if (result == StunProber::SUCCESS) {
      PrintStats(prober);
    }
  }
}

}  // namespace

int main(int argc, char* argv[]) {
  rtc::FlagList::SetFlagsFromCommandLine(&argc, argv, true);
  if (FLAG_help) {
    rtc::FlagList::Print(nullptr, false);
    return 0;
  }

  std::vector<rtc::SocketAddress> server_addresses;
  std::istringstream servers(FLAG_servers);
  std::string server;
  while (getline(servers, server, ',')) {
    rtc::SocketAddress addr;
    if (!addr.FromString(server)) {
      RTC_LOG(LS_ERROR) << "Parsing " << server << " failed.";
      return -1;
    }
    server_addresses.push_back(addr);
  }

  rtc::InitializeSSL();
  rtc::InitRandom(rtc::Time32());
  rtc::Thread* thread = rtc::ThreadManager::Instance()->WrapCurrentThread();
  std::unique_ptr<rtc::BasicPacketSocketFactory> socket_factory(
      new rtc::BasicPacketSocketFactory());
  std::unique_ptr<rtc::BasicNetworkManager> network_manager(
      new rtc::BasicNetworkManager());
  rtc::NetworkManager::NetworkList networks;
  network_manager->GetNetworks(&networks);
  StunProber* prober =
      new StunProber(socket_factory.get(), rtc::Thread::Current(), networks);
  auto finish_callback = [thread](StunProber* prober, int result) {
    StopTrial(thread, prober, result);
  };
  prober->Start(server_addresses, FLAG_shared_socket, FLAG_interval,
                FLAG_pings_per_ip, FLAG_timeout,
                AsyncCallback(finish_callback));
  thread->Run();
  delete prober;
  return 0;
}
