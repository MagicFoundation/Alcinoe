package android.support.v4.net;

import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.vending.licensing.APKExpansionPolicy;

class ConnectivityManagerCompatGingerbread {
    ConnectivityManagerCompatGingerbread() {
    }

    public static boolean isActiveNetworkMetered(ConnectivityManager cm) {
        NetworkInfo info = cm.getActiveNetworkInfo();
        if (info == null) {
            return true;
        }
        switch (info.getType()) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
            case DetectedActivity.ON_FOOT /*2*/:
            case DetectedActivity.STILL /*3*/:
            case DetectedActivity.UNKNOWN /*4*/:
            case DetectedActivity.TILTING /*5*/:
            case Participant.STATUS_UNRESPONSIVE /*6*/:
                return true;
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return false;
            default:
                return true;
        }
    }
}
