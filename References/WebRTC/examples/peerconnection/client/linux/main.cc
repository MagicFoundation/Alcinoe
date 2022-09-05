/*
 *  Copyright 2012 The WebRTC Project Authors. All rights reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree. An additional intellectual property rights grant can be found
 *  in the file PATENTS.  All contributing project authors may
 *  be found in the AUTHORS file in the root of the source tree.
 */

#include <gtk/gtk.h>

#include "examples/peerconnection/client/conductor.h"
#include "examples/peerconnection/client/flagdefs.h"
#include "examples/peerconnection/client/linux/main_wnd.h"
#include "examples/peerconnection/client/peer_connection_client.h"

#include "rtc_base/ssladapter.h"
#include "rtc_base/thread.h"

class CustomSocketServer : public rtc::PhysicalSocketServer {
 public:
  explicit CustomSocketServer(GtkMainWnd* wnd)
      : wnd_(wnd), conductor_(NULL), client_(NULL) {}
  virtual ~CustomSocketServer() {}

  void SetMessageQueue(rtc::MessageQueue* queue) override {
    message_queue_ = queue;
  }

  void set_client(PeerConnectionClient* client) { client_ = client; }
  void set_conductor(Conductor* conductor) { conductor_ = conductor; }

  // Override so that we can also pump the GTK message loop.
  bool Wait(int cms, bool process_io) override {
    // Pump GTK events.
    // TODO(henrike): We really should move either the socket server or UI to a
    // different thread.  Alternatively we could look at merging the two loops
    // by implementing a dispatcher for the socket server and/or use
    // g_main_context_set_poll_func.
    while (gtk_events_pending())
      gtk_main_iteration();

    if (!wnd_->IsWindow() && !conductor_->connection_active() &&
        client_ != NULL && !client_->is_connected()) {
      message_queue_->Quit();
    }
    return rtc::PhysicalSocketServer::Wait(0 /*cms == -1 ? 1 : cms*/,
                                           process_io);
  }

 protected:
  rtc::MessageQueue* message_queue_;
  GtkMainWnd* wnd_;
  Conductor* conductor_;
  PeerConnectionClient* client_;
};

int main(int argc, char* argv[]) {
  gtk_init(&argc, &argv);
// g_type_init API is deprecated (and does nothing) since glib 2.35.0, see:
// https://mail.gnome.org/archives/commits-list/2012-November/msg07809.html
#if !GLIB_CHECK_VERSION(2, 35, 0)
  g_type_init();
#endif
// g_thread_init API is deprecated since glib 2.31.0, see release note:
// http://mail.gnome.org/archives/gnome-announce-list/2011-October/msg00041.html
#if !GLIB_CHECK_VERSION(2, 31, 0)
  g_thread_init(NULL);
#endif

  rtc::FlagList::SetFlagsFromCommandLine(&argc, argv, true);
  if (FLAG_help) {
    rtc::FlagList::Print(NULL, false);
    return 0;
  }

  // Abort if the user specifies a port that is outside the allowed
  // range [1, 65535].
  if ((FLAG_port < 1) || (FLAG_port > 65535)) {
    printf("Error: %i is not a valid port.\n", FLAG_port);
    return -1;
  }

  GtkMainWnd wnd(FLAG_server, FLAG_port, FLAG_autoconnect, FLAG_autocall);
  wnd.Create();

  CustomSocketServer socket_server(&wnd);
  rtc::AutoSocketServerThread thread(&socket_server);

  rtc::InitializeSSL();
  // Must be constructed after we set the socketserver.
  PeerConnectionClient client;
  rtc::scoped_refptr<Conductor> conductor(
      new rtc::RefCountedObject<Conductor>(&client, &wnd));
  socket_server.set_client(&client);
  socket_server.set_conductor(conductor);

  thread.Run();

  // gtk_main();
  wnd.Destroy();

  // TODO(henrike): Run the Gtk main loop to tear down the connection.
  /*
  while (gtk_events_pending()) {
    gtk_main_iteration();
  }
  */
  rtc::CleanupSSL();
  return 0;
}
