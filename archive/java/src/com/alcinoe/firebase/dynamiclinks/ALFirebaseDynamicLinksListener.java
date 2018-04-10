package com.alcinoe.firebase.dynamiclinks;

public interface ALFirebaseDynamicLinksListener {
  void onGetDynamicLinkSuccess(final String deepLink, final String invitationId);
  void onGetDynamicLinkError(final int errorCode, final String errorMsg);
}