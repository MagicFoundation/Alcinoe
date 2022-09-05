package com.alcinoe.firebase.iid;

public interface ALFirebaseInstanceIdServiceListener {
  void onTokenRefresh(final String refreshedToken);
}