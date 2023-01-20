package com.alcinoe.installreferrer;

public interface ALInstallReferrerListener {
  void onGetInstallReferrerSuccess(final String referrer, long clickTimestampSeconds, long installBeginTimestampSeconds);
  void onGetInstallReferrerError(int responseCode);
}