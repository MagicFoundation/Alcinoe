package com.alcinoe.googleplayservices;

public interface ALAppInviteInvitationResultListener {
  void onSuccess(final String deepLink, final String invitationId);
  void onError(final int level, final int code);
}