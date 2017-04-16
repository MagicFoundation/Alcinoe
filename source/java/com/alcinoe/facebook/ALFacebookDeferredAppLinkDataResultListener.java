package com.alcinoe.facebook;

public interface ALFacebookDeferredAppLinkDataResultListener {
  void onSuccess(final String targetUri, final String promotionCode);
  void onError(final int code);
}