package com.google.android.gms.common;

import android.app.Activity;
import android.app.AlertDialog.Builder;
import android.app.Dialog;
import android.app.PendingIntent;
import android.content.Context;
import android.content.DialogInterface.OnCancelListener;
import android.content.DialogInterface.OnClickListener;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.support.v4.app.FragmentActivity;
import android.support.v4.media.TransportMediator;
import android.util.Base64;
import android.util.Log;
import com.google.android.gms.R;
import com.google.android.gms.common.api.CommonStatusCodes;
import com.google.android.gms.drive.DriveFile;
import com.google.android.gms.games.GamesStatusCodes;
import com.google.android.gms.games.multiplayer.Participant;
import com.google.android.gms.internal.ef;
import com.google.android.gms.internal.ek;
import com.google.android.gms.internal.fr;
import com.google.android.gms.location.DetectedActivity;
import com.google.android.gms.wallet.NotifyTransactionStatusRequest.Status.Error;
import com.google.android.vending.licensing.APKExpansionPolicy;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.security.cert.CertificateException;
import java.security.cert.CertificateExpiredException;
import java.security.cert.CertificateFactory;
import java.security.cert.CertificateNotYetValidException;
import java.security.cert.X509Certificate;
import java.util.Arrays;
import java.util.NoSuchElementException;
import java.util.Scanner;

public final class GooglePlayServicesUtil {
    public static final String GMS_ERROR_DIALOG = "GooglePlayServicesErrorDialog";
    public static final String GOOGLE_PLAY_SERVICES_PACKAGE = "com.google.android.gms";
    public static final int GOOGLE_PLAY_SERVICES_VERSION_CODE = 4323000;
    public static final String GOOGLE_PLAY_STORE_PACKAGE = "com.android.vending";
    static final byte[][] yL;
    static final byte[][] yM;
    static final byte[][] yN;
    static final byte[][] yO;
    private static final byte[][] yP;
    private static final byte[][] yQ;
    static final byte[][] yR;
    public static boolean yS;
    public static boolean yT;
    static boolean yU;
    private static int yV;
    private static final Object yW;

    static {
        yL = new byte[][]{Z("0\u0082\u0004C0\u0082\u0003+\u00a0\u0003\u0002\u0001\u0002\u0002\t\u0000\u00c2\u00e0\u0087FdJ0\u008d0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0004\u0005\u00000t1\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00140\u0012\u0006\u0003U\u0004\n\u0013\u000bGoogle Inc.1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u0003\u0013\u0007Android0\u001e\u0017\r080821231334Z\u0017\r360107231334Z0t1\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00140\u0012\u0006\u0003U\u0004\n\u0013\u000bGoogle Inc.1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u0003\u0013\u0007Android0\u0082\u0001 0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0001\u0005\u0000\u0003\u0082\u0001\r\u00000\u0082\u0001\b\u0002\u0082\u0001\u0001\u0000\u00abV.\u0000\u00d8;\u00a2\b\u00ae\n\u0096o\u0012N)\u00da\u0011\u00f2\u00abV\u00d0\u008fX\u00e2\u00cc\u00a9\u0013\u0003\u00e9\u00b7T\u00d3r\u00f6@\u00a7\u001b\u001d\u00cb\u0013\tgbNFV\u00a7wj\u0092\u0019=\u00b2\u00e5\u00bf\u00b7$\u00a9\u001ew\u0018\u008b\u000ejG\u00a4;3\u00d9`\u009bw\u00181E\u00cc\u00df{.Xft\u00c9\u00e1V[\u001fLjYU\u00bf\u00f2Q\u00a6=\u00ab\u00f9\u00c5\\'\"\"R\u00e8u\u00e4\u00f8\u0015Jd_\u0089qh\u00c0\u00b1\u00bf\u00c6\u0012\u00ea\u00bfxWi\u00bb4\u00aay\u0084\u00dc~.\u00a2vL\u00ae\u0083\u0007\u00d8\u00c1qT\u00d7\u00ee_d\u00a5\u001aD\u00a6\u0002\u00c2I\u0005AW\u00dc\u0002\u00cd_\\\u000eU\u00fb\u00ef\u0085\u0019\u00fb\u00e3'\u00f0\u00b1Q\u0016\u0092\u00c5\u00a0o\u0019\u00d1\u0083\u0085\u00f5\u00c4\u00db\u00c2\u00d6\u00b9?h\u00cc)y\u00c7\u000e\u0018\u00ab\u0093\u0086k;\u00d5\u00db\u0089\u0099U*\u000e;L\u0099\u00dfX\u00fb\u0091\u008b\u00ed\u00c1\u0082\u00ba5\u00e0\u0003\u00c1\u00b4\u00b1\r\u00d2D\u00a8\u00ee$\u00ff\u00fd38r\u00abR!\u0098^\u00da\u00b0\u00fc\r\u000b\u0014[j\u00a1\u0092\u0085\u008ey\u0002\u0001\u0003\u00a3\u0081\u00d90\u0081\u00d60\u001d\u0006\u0003U\u001d\u000e\u0004\u0016\u0004\u0014\u00c7}\u008c\u00c2!\u0017V%\u009a\u007f\u00d3\u0082\u00dfk\u00e3\u0098\u00e4\u00d7\u0086\u00a50\u0081\u00a6\u0006\u0003U\u001d#\u0004\u0081\u009e0\u0081\u009b\u0080\u0014\u00c7}\u008c\u00c2!\u0017V%\u009a\u007f\u00d3\u0082\u00dfk\u00e3\u0098\u00e4\u00d7\u0086\u00a5\u00a1x\u00a4v0t1\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00140\u0012\u0006\u0003U\u0004\n\u0013\u000bGoogle Inc.1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u0003\u0013\u0007Android\u0082\t\u0000\u00c2\u00e0\u0087FdJ0\u008d0\f\u0006\u0003U\u001d\u0013\u0004\u00050\u0003\u0001\u0001\u00ff0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0004\u0005\u0000\u0003\u0082\u0001\u0001\u0000m\u00d2R\u00ce\u00ef\u00850,6\n\u00aa\u00ce\u0093\u009b\u00cf\u00f2\u00cc\u00a9\u0004\u00bb]z\u0016a\u00f8\u00aeF\u00b2\u0099B\u0004\u00d0\u00ffJh\u00c7\u00ed\u001aS\u001e\u00c4YZb<\u00e6\u0007c\u00b1g)zz\u00e3W\u0012\u00c4\u0007\u00f2\b\u00f0\u00cb\u0010\u0094)\u0012M{\u0010b\u0019\u00c0\u0084\u00ca>\u00b3\u00f9\u00ad_\u00b8q\u00ef\u0092&\u009a\u008b\u00e2\u008b\u00f1mD\u00c8\u00d9\u00a0\u008el\u00b2\u00f0\u0005\u00bb?\u00e2\u00cb\u0096D~\u0086\u008es\u0010v\u00adE\u00b3?`\t\u00ea\u0019\u00c1a\u00e6&A\u00aa\u0099'\u001d\u00fdR(\u00c5\u00c5\u0087\u0087]\u00db\u007fE'X\u00d6a\u00f6\u00cc\f\u00cc\u00b75.BL\u00c46\\R52\u00f72Q7Y<J\u00e3A\u00f4\u00dbA\u00ed\u00da\r\u000b\u0010q\u00a7\u00c4@\u00f0\u00fe\u009e\u00a0\u001c\u00b6'\u00cagCi\u00d0\u0084\u00bd/\u00d9\u0011\u00ff\u0006\u00cd\u00bf,\u00fa\u0010\u00dc\u000f\u0089:\u00e3Wb\u0091\u0090H\u00c7\u00ef\u00c6LqD\u0017\u0083B\u00f7\u0005\u0081\u00c9\u00deW:\u00f5[9\r\u00d7\u00fd\u00b9A\u00861\u0089]_u\u009f0\u0011&\u0087\u00ffb\u0014\u0010\u00c0i0\u008a"), Z("0\u0082\u0004\u00a80\u0082\u0003\u0090\u00a0\u0003\u0002\u0001\u0002\u0002\t\u0000\u00d5\u0085\u00b8l}\u00d3N\u00f50\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0004\u0005\u00000\u0081\u00941\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00100\u000e\u0006\u0003U\u0004\n\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u0003\u0013\u0007Android1\"0 \u0006\t*\u0086H\u0086\u00f7\r\u0001\t\u0001\u0016\u0013android@android.com0\u001e\u0017\r080415233656Z\u0017\r350901233656Z0\u0081\u00941\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00100\u000e\u0006\u0003U\u0004\n\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u0003\u0013\u0007Android1\"0 \u0006\t*\u0086H\u0086\u00f7\r\u0001\t\u0001\u0016\u0013android@android.com0\u0082\u0001 0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0001\u0005\u0000\u0003\u0082\u0001\r\u00000\u0082\u0001\b\u0002\u0082\u0001\u0001\u0000\u00d6\u00ce.\b\n\u00bf\u00e21M\u00d1\u008d\u00b3\u00cf\u00d3\u0018\\\u00b4=3\u00fa\ft\u00e1\u00bd\u00b6\u00d1\u00db\u0089\u0013\u00f6,\\9\u00dfV\u00f8F\u0081=e\u00be\u00c0\u00f3\u00caBk\u0007\u00c5\u00a8\u00edZ9\u0090\u00c1g\u00e7k\u00c9\u0099\u00b9'\u0089K\u008f\u000b\"\u0000\u0019\u0094\u00a9)\u0015\u00e5r\u00c5m*0\u001b\u00a3o\u00c5\u00fc\u0011:\u00d6\u00cb\u009et5\u00a1m#\u00ab}\u00fa\u00ee\u00e1e\u00e4\u00df\u001f\n\u008d\u00bd\u00a7\n\u0086\u009dQlN\u009d\u0005\u0011\u0096\u00ca|\fU\u007f\u0017[\u00c3u\u00f9H\u00c5j\u00ae\u0086\b\u009b\u00a4O\u008a\u00a6\u00a4\u00dd\u009a}\u00bf,\n5\"\u0082\u00ad\u0006\u00b8\u00cc\u0018^\u00b1Uy\u00ee\u00f8m\b\u000b\u001da\u0089\u00c0\u00f9\u00af\u0098\u00b1\u00c2\u00eb\u00d1\u0007\u00eaE\u00ab\u00dbh\u00a3\u00c7\u0083\u008a^T\u0088\u00c7lS\u00d4\u000b\u0012\u001d\u00e7\u00bb\u00d3\u000eb\f\u0018\u008a\u00e1\u00aaa\u00db\u00bc\u0087\u00dd<d_/U\u00f3\u00d4\u00c3u\u00ec@p\u00a9?qQ\u00d86p\u00c1j\u0097\u001a\u00be^\u00f2\u00d1\u0018\u0090\u00e1\u00b8\u00ae\u00f3)\u008c\u00f0f\u00bf\u009el\u00e1D\u00ac\u009a\u00e8m\u001c\u001b\u000f\u0002\u0001\u0003\u00a3\u0081\u00fc0\u0081\u00f90\u001d\u0006\u0003U\u001d\u000e\u0004\u0016\u0004\u0014\u008d\u001c\u00c5\u00be\u0095LC<a\u0086:\u0015\u00b0L\u00bc\u0003\u00f2O\u00e0\u00b20\u0081\u00c9\u0006\u0003U\u001d#\u0004\u0081\u00c10\u0081\u00be\u0080\u0014\u008d\u001c\u00c5\u00be\u0095LC<a\u0086:\u0015\u00b0L\u00bc\u0003\u00f2O\u00e0\u00b2\u00a1\u0081\u009a\u00a4\u0081\u00970\u0081\u00941\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00100\u000e\u0006\u0003U\u0004\n\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u0003\u0013\u0007Android1\"0 \u0006\t*\u0086H\u0086\u00f7\r\u0001\t\u0001\u0016\u0013android@android.com\u0082\t\u0000\u00d5\u0085\u00b8l}\u00d3N\u00f50\f\u0006\u0003U\u001d\u0013\u0004\u00050\u0003\u0001\u0001\u00ff0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0004\u0005\u0000\u0003\u0082\u0001\u0001\u0000\u0019\u00d3\f\u00f1\u0005\u00fbx\u0092?L\r}\u00d2##=@\u0096z\u00cf\u00ce\u0000\b\u001d[\u00d7\u00c6\u00e9\u00d6\u00ed k\u000e\u0011 \u0095\u0006Al\u00a2D\u0093\u0099\u0013\u00d2kJ\u00a0\u00e0\u00f5$\u00ca\u00d2\u00bb\\nL\u00a1\u0001j\u0015\u0091n\u00a1\u00ec]\u00c9Z^:\u0001\u00006\u00f4\u0092H\u00d5\u0010\u009b\u00bf.\u001ea\u0081\u0086g:;\u00e5m\u00af\u000bw\u00b1\u00c2)\u00e3\u00c2U\u00e3\u00e8L\u0090]#\u0087\u00ef\u00ba\t\u00cb\u00f1; +NZ\"\u00c92cHJ#\u00d2\u00fc)\u00fa\u009f\u00199u\u00973\u00af\u00d8\u00aa\u0016\u000fB\u0096\u00c2\u00d0\u0016>\u0081\u0082\u0085\u009cfC\u00e9\u00c1\u0096/\u00a0\u00c1\u008333[\u00c0\u0090\u00ff\u009ak\"\u00de\u00d1\u00adDB)\u00a59\u00a9N\u00ef\u00ad\u00ab\u00d0e\u00ce\u00d2K>Q\u00e5\u00dd{fx{\u00ef\u0012\u00fe\u0097\u00fb\u00a4\u0084\u00c4#\u00fbO\u00f8\u00ccIL\u0002\u00f0\u00f5\u0005\u0016\u0012\u00ffe)9>\u008eF\u00ea\u00c5\u00bb!\u00f2w\u00c1Q\u00aa_*\u00a6'\u00d1\u00e8\u009d\u00a7\n\u00b6\u00035i\u00de;\u0098\u0097\u00bf\u00ff|\u00a9\u00da>\u0012C\u00f6\u000b")};
        yM = new byte[][]{Z("0\u0082\u0002R0\u0082\u0001\u00bb\u0002\u0004I4\u0098~0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0004\u0005\u00000p1\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u000b0\t\u0006\u0003U\u0004\b\u0013\u0002CA1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00140\u0012\u0006\u0003U\u0004\n\u0013\u000bGoogle, Inc1\u00140\u0012\u0006\u0003U\u0004\u000b\u0013\u000bGoogle, Inc1\u00100\u000e\u0006\u0003U\u0004\u0003\u0013\u0007Unknown0\u001e\u0017\r081202020758Z\u0017\r360419020758Z0p1\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u000b0\t\u0006\u0003U\u0004\b\u0013\u0002CA1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00140\u0012\u0006\u0003U\u0004\n\u0013\u000bGoogle, Inc1\u00140\u0012\u0006\u0003U\u0004\u000b\u0013\u000bGoogle, Inc1\u00100\u000e\u0006\u0003U\u0004\u0003\u0013\u0007Unknown0\u0081\u009f0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0001\u0005\u0000\u0003\u0081\u008d\u00000\u0081\u0089\u0002\u0081\u0081\u0000\u009fH\u0003\u0019\u0090\u00f9\u00b1G&8N\u0004S\u00d1\u008f\u008c\u000b\u00bf\u008d\u00c7{%\u0004\u00a4\u00b1 |LlD\u00ba\u00bc\u0000\u00ad\u00c6a\u000f\u00a6\u00b6\u00ab-\u00a8\u000e3\u00f2\u00ee\u00f1k&\u00a3\u00f6\u00b8[\u009a\u00fa\u00ca\u0090\u009f\u00fb\u00be\u00b3\u00f4\u00c9O~\u0081\"\u00a7\u0098\u00e0\u00eb\u00a7\\\u00ed=\u00d2)\u00fase\u00f4\u0015\u0016AZ\u00a9\u00c1a}\u00d5\u0083\u00ce\u0019\u00ba\u00e8\u00a0\u00bb\u00d8\u0085\u00fc\u0017\u00a9\u00b4\u00bd&@\u0080Q!\u00aa\u00db\u0093w\u00de\u00b4\u0000\u00138\u0014\u0018\u0088.\u00c5\"\u0082\u00fcX\r\u0002\u0003\u0001\u0000\u00010\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0004\u0005\u0000\u0003\u0081\u0081\u0000@\u0086f\u009e\u00d61\u00daC\u0084\u00dd\u00d0a\u00d2&\u00e0s\u00b9\u008c\u00c4\u00b9\u009d\u00f8\u00b5\u00e4\u00be\u009e<\u00be\u0097P\u001e\u0083\u00df\u001co\u00a9Y\u00c0\u00ce`\\O\u00d2\u00acm\u001c\u0084\u00ce\u00de Gl\u00ba\u00b1\u009b\u00e8\u00f2 :\u00ffw\u0017\u00ade-\u008f\u00cc\u0089\u0007\b\u00d1!m\u00a8DWY&I\u00e0\u00e9\u00d3\u00c4\u00bbL\u00f5\u008d\u00a1\u009d\u00b1\u00d4\u00fcA\u00bc\u00b9XOd\u00e6_A\r\u0005)\u00fd[h\u0083\u008c\u0014\u001d\n\u009b\u00d1\u00db\u0011\u0091\u00cb*\r\u00f7\u0090\u00ea\f\u00b1-\u00b3\u00a4"), Z("0\u0082\u0004\u00a80\u0082\u0003\u0090\u00a0\u0003\u0002\u0001\u0002\u0002\t\u0000\u0084~O\u00f2\u00d6\u00b5\u00de\u008e0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0005\u0005\u00000\u0081\u00941\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00100\u000e\u0006\u0003U\u0004\n\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u0003\u0013\u0007Android1\"0 \u0006\t*\u0086H\u0086\u00f7\r\u0001\t\u0001\u0016\u0013android@android.com0\u001e\u0017\r100120010135Z\u0017\r370607010135Z0\u0081\u00941\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00100\u000e\u0006\u0003U\u0004\n\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u0003\u0013\u0007Android1\"0 \u0006\t*\u0086H\u0086\u00f7\r\u0001\t\u0001\u0016\u0013android@android.com0\u0082\u0001 0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0001\u0005\u0000\u0003\u0082\u0001\r\u00000\u0082\u0001\b\u0002\u0082\u0001\u0001\u0000\u00d8(q|6\u00d1\u0017\u000f\u00d4M\n{\u000f\u0007\u0011&\u00e8[\u00bf\u00df3\u00b04`\u0000Z\u0094\u00cc\u00fbe\u00a5\u00db\u00a0\u00b2C\u00df`\u00b1\u0091\u00bf\u009d\u0006\u00df\u001d\u008a\\\n3\u00e2\u00d1c\u00f5\u0013\u00df\u001d\"SA\u00ea<3y\"\u00e8\\\u0002\u00ec4\u00ce\u00d9L\u00b8\u0007#\u00a6#\u00ffK\u00af\u00fb\u00b4\u00e5\u00ef\u00e6w;>\u00a2\u00be\u00b8\u00bc\u00b2\u0002g\u00cf\u00e7\u0085Q\u001f\u0083.\u00f9\u0087\u00abu\u0094\u00fe\u001e)\u00cf\u00bcM\b:\u001f\u0012R\u0000ws\u0096\u00f2\u0016[i{\u0000\u00a3\u00a0\u00c1:\u00cc0\u008a\u0093\u00f2!c\u00c1n\u009c=J\u00b2\u0014\u009f6LE\u00c0C\u00142p9\u00f1\u00da\t`\u0093\u00f1\u00b3\u00fc\u0018\u00b6V\u0010\u0095\u00c6\"_\u00c7\u0010+\u0098|o\u0013\u00a4]$\u00e3\u00e0\u00c5N\u0085\u009dg\u00e3[g\b'\u0013\u00d2\u00d6\u00f0W\u00dd4W\u00d1\u009f\u00c4\u00fe\u008d\u00dd\u00ec\u008c:O?\u0097#\u0005\u0019\u00a7\n(64\u00ac5\u0081\u00a3J\u00bd\u00a1}\u0084Z\n\t\u0085\u00fb\u00f8\u0006\u000b\u0003j'x`\u0081c\u00fa\f7\u00b9\u00e7\u00f2\u00a1\u000ev\u00bcw\u0002\u0001\u0003\u00a3\u0081\u00fc0\u0081\u00f90\u001d\u0006\u0003U\u001d\u000e\u0004\u0016\u0004\u0014\u00b5\u00c7\u00f9\u0012ox\r:\u00fb\u00caess?\u00f5\"k\u009b\u001770\u0081\u00c9\u0006\u0003U\u001d#\u0004\u0081\u00c10\u0081\u00be\u0080\u0014\u00b5\u00c7\u00f9\u0012ox\r:\u00fb\u00caess?\u00f5\"k\u009b\u00177\u00a1\u0081\u009a\u00a4\u0081\u00970\u0081\u00941\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00100\u000e\u0006\u0003U\u0004\n\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00100\u000e\u0006\u0003U\u0004\u0003\u0013\u0007Android1\"0 \u0006\t*\u0086H\u0086\u00f7\r\u0001\t\u0001\u0016\u0013android@android.com\u0082\t\u0000\u0084~O\u00f2\u00d6\u00b5\u00de\u008e0\f\u0006\u0003U\u001d\u0013\u0004\u00050\u0003\u0001\u0001\u00ff0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0005\u0005\u0000\u0003\u0082\u0001\u0001\u0000L>\u00a7e}&\u00e6\u00bb\u00d7\u0011\f\u008f\u0019\u00df\u001f\u008d\u00a1\t}3\u0086\u000fi\u00de\u00bf\u00ca\u00dbF\u00a3~\u0087\u00e5\u00b3\u000f\u00bb4{\u001cuU\u00bc\u00bb<\u0099T\u0014\u0080F\u0096_\u009cy*\u0002\u00d0\u00db\u00e5\u00a6Ga\u00b3yG\u00abk\u00ff\u00b0\u00ba\u00c6\u00a2\u00c1\u00a0\u00cd\u00f8b\u00f8w\u00a9g\r\u00fdo\u0006.@n\u00ce\u0018\u0006\f`I\u008d\u00fc6\u009f'\u0011q\u0098\u00e5o\u00cb\u00a1R\u00e6\u0005\u008d\u00ce\u0094\u00ceY\u001f\u00c4\u00f4\u00a9\u0098+3\u00ba\u00d8\u0019mwoU\u00b7\u00d0\u001a\u00cf1\u00dd\u00d7\f\u00ec\u00b7\u0089xv\u0006e\u0010\u00f9I\u00a5RJ11\u00b3\u00cbeA\u00cf\u008b5B\u000e\u00bc\u00c4R%Y\u0096?Bfi\u0005rfbO\u00b3\u0098\u00cf\u00dbR\u0017\u0088\u001d\u0011\u001cn\u0003F\u0016\u00f8Q!\u0018\u00d0\u00a2\u00a6\u009d\u0013\u00d7\u0092\u00f0\u00cd\u0011\u00db\u00d5\u008e#\u0083ZT\u00a5J\u00c2Q\u00e7\u00d2,Dj?\u00ee\u0014\u0012\u0010\u00e9DGK@c\u0007\u00bb&\u0084+Ok\u00d3U\u0082\u001cs\u0096Q\u00ff\u00a2`[\u0005\u00e2$\u0095\u00d7\u0015\u00d8z\u0091\u00f6")};
        yN = new byte[][]{Z("0\u0082\u0002\u00a70\u0082\u0002e\u00a0\u0003\u0002\u0001\u0002\u0002\u0004P\u0005|B0\u000b\u0006\u0007*\u0086H\u00ce8\u0004\u0003\u0005\u0000071\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00100\u000e\u0006\u0003U\u0004\n\u0013\u0007Android1\u00160\u0014\u0006\u0003U\u0004\u0003\u0013\rAndroid Debug0\u001e\u0017\r120717145250Z\u0017\r220715145250Z071\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00100\u000e\u0006\u0003U\u0004\n\u0013\u0007Android1\u00160\u0014\u0006\u0003U\u0004\u0003\u0013\rAndroid Debug0\u0082\u0001\u00b70\u0082\u0001,\u0006\u0007*\u0086H\u00ce8\u0004\u00010\u0082\u0001\u001f\u0002\u0081\u0081\u0000\u00fd\u007fS\u0081\u001du\u0012)R\u00dfJ\u009c.\u00ec\u00e4\u00e7\u00f6\u0011\u00b7R<\u00efD\u0000\u00c3\u001e?\u0080\u00b6Q&iE]@\"Q\u00fbY=\u008dX\u00fa\u00bf\u00c5\u00f5\u00ba0\u00f6\u00cb\u009bUl\u00d7\u0081;\u0080\u001d4o\u00f2f`\u00b7k\u0099P\u00a5\u00a4\u009f\u009f\u00e8\u0004{\u0010\"\u00c2O\u00bb\u00a9\u00d7\u00fe\u00b7\u00c6\u001b\u00f8;W\u00e7\u00c6\u00a8\u00a6\u0015\u000f\u0004\u00fb\u0083\u00f6\u00d3\u00c5\u001e\u00c3\u00025T\u0013Z\u0016\u00912\u00f6u\u00f3\u00ae+a\u00d7*\u00ef\u00f2\"\u0003\u0019\u009d\u00d1H\u0001\u00c7\u0002\u0015\u0000\u0097`P\u008f\u0015#\u000b\u00cc\u00b2\u0092\u00b9\u0082\u00a2\u00eb\u0084\u000b\u00f0X\u001c\u00f5\u0002\u0081\u0081\u0000\u00f7\u00e1\u00a0\u0085\u00d6\u009b=\u00de\u00cb\u00bc\u00ab\\6\u00b8W\u00b9y\u0094\u00af\u00bb\u00fa:\u00ea\u0082\u00f9WL\u000b=\u0007\u0082gQYW\u008e\u00ba\u00d4YO\u00e6q\u0007\u0010\u0081\u0080\u00b4I\u0016q#\u00e8L(\u0016\u0013\u00b7\u00cf\t2\u008c\u00c8\u00a6\u00e1<\u0016z\u008bT|\u008d(\u00e0\u00a3\u00ae\u001e+\u00b3\u00a6u\u0091n\u00a3\u007f\u000b\u00fa!5b\u00f1\u00fbbz\u0001$;\u00cc\u00a4\u00f1\u00be\u00a8Q\u0090\u0089\u00a8\u0083\u00df\u00e1Z\u00e5\u009f\u0006\u0092\u008bf^\u0080{U%d\u0001L;\u00fe\u00cfI*\u0003\u0081\u0084\u0000\u0002\u0081\u0080j\u00d1\u001b\u00d7\u00d5f\u00d2z\u00f49\u00c0.Ah\u00ac\u00fdE\u00b4\u00be\u0085\u00bc\u0099\u008c{\u009b\u008e\u001cwTi?\u008c\rB\u008a\u00a4\u00fc\u00e1\u0010\u0084\u00818BO\u00a6\u008c\u00d10RN\u00ef\u00f6\u00f178c\u0082/\u00a67)\u008b\u00feMF\u00a0\u00b8fe\u00ee\u00f0A\u00179\u0001\u0003[\u001c\u0080j\u00a3\u0018\u0018\r0:\u00a8\u00cc\u009eY#\u00e0jo\u00ab\u00fauh<E;\u00b2\u0007w|\u00f2\u00fd\u00e7\u00cf\u00b1\u009b\u001408\u0014\u00aa\u001d\u00f7\u00b4=[\"+W\u0006\u00b4\u008b\u00940\u000b\u0006\u0007*\u0086H\u00ce8\u0004\u0003\u0005\u0000\u0003/\u00000,\u0002\u0014\t\u00d2\u00d1\u00b0G\u0002)\u00b5\u00be\u00d2\u0090&a\u00d1\u0012\u00f2p\u00c5\u00e6\u001d\u0002\u0014gP\u0002\u0006\u00a7\u0080P\u00bax\u00ae\u00c7\u0017O\u0016\u0004\u007f\u0084\u00ea\u00a2\u00f7")};
        yO = new byte[][]{Z("0\u0082\u0004L0\u0082\u00034\u00a0\u0003\u0002\u0001\u0002\u0002\t\u0000\u00a8\u00cd\u0017\u00c9=\u00a5\u00d9\u00900\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0005\u0005\u00000w1\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00140\u0012\u0006\u0003U\u0004\n\u0013\u000bGoogle Inc.1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00130\u0011\u0006\u0003U\u0004\u0003\u0013\nGoogle NFC0\u001e\u0017\r110324010653Z\u0017\r380809010653Z0w1\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00140\u0012\u0006\u0003U\u0004\n\u0013\u000bGoogle Inc.1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00130\u0011\u0006\u0003U\u0004\u0003\u0013\nGoogle NFC0\u0082\u0001 0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0001\u0005\u0000\u0003\u0082\u0001\r\u00000\u0082\u0001\b\u0002\u0082\u0001\u0001\u0000\u00c3\u000f\u0088\u00ad\u00d9\u00b4\u0092\tj,XjZ\u009a\u00805k\u00fa\u0002iX\u00f8\u00ff\f]\u00fa\u00f5\u009fI&\u008a\u00d8p\u00de\u00e8!\u00a5>\u001f[\u0017\u000f\u00c9bE\u00a3\u00c9\u0082\u00a7\u00cbE'\u0005;\u00e3^4\u00f3\u0096\u00d2K\"\u0091\u00ec\fR\u008dn&\u0092te\u00e0hu\u00eab\u001f\u007f\u00f9\u008c@\u00e34[ I\u0007\u00cc\u0093Tt:\u00cd\u00aa\u00ceeV_H\u00bat\u00cdA!\u00cd\u00c8v\u00df5\"\u00ba\u00db\t\\ \u00d94\u00c5j>\\9>\u00e5\u00f0\u00e0/\u008f\u00e0b\u001f\u0091\u008d\u001f5\u00a8$\u0089%,o\u00a6\u00b63\u0092\u00a7hk>Ha-\u0006\u00a9\u00cfoI\u00bf\u00f1\u001d]\u0096(\u009c\u009d\u00fe\u0014\u00acWbC\u0096\u0097\u00dd)\u00ea\u00fd\u00b9\u0081\r\u00e3&5\u0013\u00a9\u0005\u00ac\u008e\u008e\u00af \u0090~Fu\nZ\u00b7\u00bf\u009aw&/G\u00b0?Z<nm{Q4?i\u00c7\u00f7%\u00f7\u000b\u00cc\u001bJ\u00d5\u0092%\u000bpZ\u0086\u00e6\u00e8>\u00e2\u00ae7\u00feW\u0001\u00bc\u00bd\u00b2o\u00ee\u00fd\u00ff\u00f6\u000fj[\u00df\u00b5\u00b6G\u0093\u0002\u0001\u0003\u00a3\u0081\u00dc0\u0081\u00d90\u001d\u0006\u0003U\u001d\u000e\u0004\u0016\u0004\u0014\u001c\u00ce\u00ce\u000e\u00eaM\u00c1\u0012\u001f\u00c7Q_\r\n\fr\u00e0\u008c\u00c9m0\u0081\u00a9\u0006\u0003U\u001d#\u0004\u0081\u00a10\u0081\u009e\u0080\u0014\u001c\u00ce\u00ce\u000e\u00eaM\u00c1\u0012\u001f\u00c7Q_\r\n\fr\u00e0\u008c\u00c9m\u00a1{\u00a4y0w1\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00140\u0012\u0006\u0003U\u0004\n\u0013\u000bGoogle Inc.1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00130\u0011\u0006\u0003U\u0004\u0003\u0013\nGoogle NFC\u0082\t\u0000\u00a8\u00cd\u0017\u00c9=\u00a5\u00d9\u00900\f\u0006\u0003U\u001d\u0013\u0004\u00050\u0003\u0001\u0001\u00ff0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0005\u0005\u0000\u0003\u0082\u0001\u0001\u0000\u00a4p\u00c7(\u00e1\u00d3\u001b\u0006\u00d9\u00afj\u00e7h\u00b5e\u0004lW\u0080k\u0098CrI1\u00d7]L\u00a1\f2\u0015 \u00d3<\u00cf\u00ed*\u00a6Tb#L\u009e\u00f9\u00b6\u00f9\u0010\u00ccgk\u0099\u00cb\u007f\u0098\u0095\u00d6\u00c0gcWO\u00bbx3\u0012u\u00dc\\\u00f3\u008f\u00ba\u00a9\u0018\u00d7\u0093\u008c\u0005\u001f\u00fb\u00a2\u00ad\u00e8\u00f3\u0003\u00cd\u00e8\u00d9\u00e6\u008a\u0004\u008d\u001f\u00db\u009e|\u009f*I\u00b2\"\u00c6\u008f\u00ffB+\u00f1Ui\u00b8^\u00ee\u00ed\u00b0J\u00a3\bs\u00db\u00e6K\u009c\u009et\u00f8\u00f2\u00c2\u00f6\u00c4\u0001$\u00aa\u00a8\u00d1x\r\u0018Q+T\n\u00dd(\u00b3\u00e9X\u0019q\u00a4\u0017\r\u00d8h\u00cf_1\u00e4G\u0012\u00b2\u00c2;\u00b5\u00107\u00d7\u00ef\u009f\u0087\u00a6\u00e5\u00bd\u00b3^,\u00ebk\u00b0\"cl\u0017\u00a5j\u0096\u00bczP%\u008c\u000b\u00d2\u00ed{1UZ\u0018E.\u00172\u001a\rR\u0083\u008c\u0082\u00f6?t-t\u00ffyXj\\\u00bb\u007f\u00afq\u0098\u00a8K\u00cftC\u0010\u00e9\u00e9'Y\u007f\u0000\u00a2=\u00d0\u0006`\u0080\f\"8\u00d9\u000b/\u00b3r\u00df\u00db\u00bau\u00bd\u0085."), Z("0\u0082\u0004L0\u0082\u00034\u00a0\u0003\u0002\u0001\u0002\u0002\t\u0000\u00dev\u0095\u0004\u001dvP\u00c00\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0005\u0005\u00000w1\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00140\u0012\u0006\u0003U\u0004\n\u0013\u000bGoogle Inc.1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00130\u0011\u0006\u0003U\u0004\u0003\u0013\nGoogle NFC0\u001e\u0017\r110324010324Z\u0017\r380809010324Z0w1\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00140\u0012\u0006\u0003U\u0004\n\u0013\u000bGoogle Inc.1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00130\u0011\u0006\u0003U\u0004\u0003\u0013\nGoogle NFC0\u0082\u0001 0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0001\u0005\u0000\u0003\u0082\u0001\r\u00000\u0082\u0001\b\u0002\u0082\u0001\u0001\u0000\u00e6\u00ff=\u00ef\u00e9*\u00a1\rq\u00eb\u000f\u00a6@\u008b\u00c06\u00b7\u00e2C\u00ee\u00edh\u00a6\u00a4v=\u00c7\u00a5*1u|\u00da\u00c6\u001f\u00e5\u0010\u00bbs\u00c7\u0016\u00e4\u0000\u0001\u0004&[4\u007f\u00ce\u00ce\u00f4\u00c4+\u00f1\u00e17\u009d\u00d0\u00a8v\u00f0(\"\u007f\u00bb\u00c1\u00f9\u00bd\u00d5\u00d7\u0013\u00b2\u00f6\u00a95\u00a3y\u00d2\u00cb\u00a9\u00c9o\u0092\u00d2\u00d0x|\u0011\u00f1\u00eb\u0019T\u0080\b\u00a6\u00a0r\u00b3K\u0091\u0083l\u00fa\n\u00e1'g\u0080\u00e9\u0000u0\u0016i\u0086\u00a1\u001c\u009c\u00efF\u00ce\u00f7\u00c7\u0004\u0080m\u00de\u00941\u00fb`(M\u0012\n\u00b0\u00e7\u00de\u001dc?\u0007h}F\u008cQ\u0013\u009a\u00ff\u00fd\u00c6\u00bc\u009a |\u00a9\u0004\u00b8\u00be\u001d\u00a0\u00aa{N\u0097uoC`d\u0088\u00be\\\u00ae<h\u00e8\u00bbyB\u00cd\u00f5\u0016\u0007\u00c90\u00a2\u00fc\u00dae[u\u00d0u\u009c\u00ba\u0089\u00ad\u0006\u00e79\u00bd\u000b\u00a2\u009b\u001f@B\u0096\u00c2\u00c0\u00a8Z\u0084\u007fZ\u00b0\u00d0g\u00c6\u00c3\u00ec\u009cI! B\u00acc\u00a7\u00e5;Tle\u00b4`\u0080\u00b4\u00e3\u00e6\u0080\u00e2>\u001fw\u00cf\u00e7\u00f6\u00detK\u001ae\u0002\u0001\u0003\u00a3\u0081\u00dc0\u0081\u00d90\u001d\u0006\u0003U\u001d\u000e\u0004\u0016\u0004\u0014\u00a2\u00e8\u0090d\u00b0]\b\u0086\\4\u00db\u0093\n\u009d\u0084\u0000P\u0011z\u00ec0\u0081\u00a9\u0006\u0003U\u001d#\u0004\u0081\u00a10\u0081\u009e\u0080\u0014\u00a2\u00e8\u0090d\u00b0]\b\u0086\\4\u00db\u0093\n\u009d\u0084\u0000P\u0011z\u00ec\u00a1{\u00a4y0w1\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00140\u0012\u0006\u0003U\u0004\n\u0013\u000bGoogle Inc.1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Android1\u00130\u0011\u0006\u0003U\u0004\u0003\u0013\nGoogle NFC\u0082\t\u0000\u00dev\u0095\u0004\u001dvP\u00c00\f\u0006\u0003U\u001d\u0013\u0004\u00050\u0003\u0001\u0001\u00ff0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0005\u0005\u0000\u0003\u0082\u0001\u0001\u00007q\u0087\f\u00e8|<R\u00ea\u0084\u0089\u00920\u00c6\u00e9b\u00d9KM_\u0012\u0093\u00c2]\u0088&\u0015A\u00fd\u0090\u00b5U]\u0012\u0085\u00ce\u00f3\u00b81,?]\u00f6\u0091\u00a8\u00aa\u00e0L\u00b9\u0081\u00b3\u0005\u00e4'\u00fd\u001d-\u009e\u0019\u0087\u00e1\u00d2\u0090x\u00f1<\u0084R\u0099\u000f\u0018!\u0098\u0002c\u00d8\u00d4\u00bd6Q\u0093H\u00d8\u00d8\u00ba&\u00d8\u00b9\u009f\u00bf\t\u00f5\u00fd>\u00bb\u000e\u00a3\u00c2\u00f0\u00c97o\u001e\u001f\u00cav\u00f3\u00a6\u00a4\u0005B\u009d\b\u001bu*z\u0090\u00b7V\u00e9\u00abD\u00daA\u00ab\u00c8\u00e1\u00e8\u00f8\u008a\u00c2u\u008d\u00a7C\u00fbs\u00e6Pq\u009aW\u0084\f\u00cbkz\u00dd!\u00b9\u009f\u00c6\u0081\u00e4V\u00e1\u0087,\"=\\\u0007J\u00dfU\u00f6\u00ab\u00da&\u008c-\u008bd\u00ea\n\u0088E\u00ee\u00cd\u0096\u008f\u0092\u00b4\u0093\u0012~u\u00c7S\u00c3\u00ff0\u00cb\u00c6x\u00b5\u001c\u009fR\u0096\u0014r\u00f1}\u00a2\n\r\u00c6'J\u00a2F44\u00c1\u00a9\u00b6\u0014\u00dfi}\u008f\u00f5\u00ca\u0081\u0001\u00e7\u00a2\\}\u00b3\u00fb\u0005]eV\u009c\u0004\u00b0\u001d8\u009c\u00ab\u00baW\u00b3\u00a1p>\u00c2\u00e7J\u0088\u00d34")};
        yP = a(yL, yM, yN, yO);
        yQ = new byte[][]{yL[0], yM[0], yO[0]};
        yR = new byte[][]{Z("0\u0082\u0002_0\u0082\u0001\u00c8\u00a0\u0003\u0002\u0001\u0002\u0002\u0004K\u0019\u00b1\u009d0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0005\u0005\u00000t1\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00150\u0013\u0006\u0003U\u0004\n\u0013\fGoogle, Inc.1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Unknown1\u000f0\r\u0006\u0003U\u0004\u0003\u0013\u0006Bazaar0\u001e\u0017\r091205010429Z\u0017\r370422010429Z0t1\u000b0\t\u0006\u0003U\u0004\u0006\u0013\u0002US1\u00130\u0011\u0006\u0003U\u0004\b\u0013\nCalifornia1\u00160\u0014\u0006\u0003U\u0004\u0007\u0013\rMountain View1\u00150\u0013\u0006\u0003U\u0004\n\u0013\fGoogle, Inc.1\u00100\u000e\u0006\u0003U\u0004\u000b\u0013\u0007Unknown1\u000f0\r\u0006\u0003U\u0004\u0003\u0013\u0006Bazaar0\u0081\u009f0\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0001\u0005\u0000\u0003\u0081\u008d\u00000\u0081\u0089\u0002\u0081\u0081\u0000\u00a9\b\u0088\u00de\u0096\u00e354w\t\u00ddK%\u001ez)\u00a8G7k.\\\u00ba[[\u00d5\u0004>\u0083\u0088\u0001\u0002\u0098\u0019\u0014\u0094\u0013\u00fa3\u00aa\u00e5D;\u0003SJ\u00ceJ\u00adoP\u0097\u0012I\u00d9\u008ev\u00a3\u009a~L\u00cc\u00e1\u00d7\u001b\u00a7\u00be>ugM\u00b5\u00f1\u0007Z\u0098sp\u0001FH\u00a7\u00cep<-\u00c7\u00884\u0089\u0005\u0092\u0012\u00af\u009cl[(\u00ab\u00d5O\u0083d\u0011\u00c81\u00a2\u009fP\u000f(\u0002\u00d1l\u00e6\u00d1\u0085o\u0086p\u00aa\u00fc\u00a2eA\u0083{9\u0002\u0003\u0001\u0000\u00010\r\u0006\t*\u0086H\u0086\u00f7\r\u0001\u0001\u0005\u0005\u0000\u0003\u0081\u0081\u0000I\u0084\u00c6\u00f3AG\u0001#b:'O\u00e9\u00e17=u1\u00cc\r\u00fc\u00e9\u00a7j\u00e6\u007f\u00fbp[@L\u00bd\u001b\u00c1\u0016\u008c\u00ab\u0018\u00bb\u0011\u00c3x\u0095\u00bf\u00b4\u00f3l\u00c1L\u00ec\u001d,\u00c5Qj\u000e\u00ce\u00d4\u0007N\u00b5h\u0082\u0089Pd\u0000\u00af\u00f8\u00dc\u00c8\u00efT\u0004\u0012\u0002\u00fd\u00ef\u00f1\u00fd\u0082\u00e0\u00f3#\u0010r\u00fd\u00cc\u00deJ6\u008b\u00e0\u00c6\u00c3\u00f9\u00b8\u00b3\u00aa\rh<:\u00bf\u00da\u009a\u00b7\u00bb\u00882\u00e9\u00be^6\u0019\u00ba\u0092\u00dd:\u00cc\u0003j\u00ad\u00b5\u00a6\u0019\u00afP")};
        yS = false;
        yT = false;
        yU = false;
        yV = -1;
        yW = new Object();
    }

    private GooglePlayServicesUtil() {
    }

    static boolean E(int i) {
        switch (F(i)) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                return !do();
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return true;
            case DetectedActivity.ON_FOOT /*2*/:
                return false;
            default:
                return false;
        }
    }

    private static int F(int i) {
        return i == -1 ? 2 : i;
    }

    private static byte[] Z(String str) {
        try {
            return str.getBytes("ISO-8859-1");
        } catch (UnsupportedEncodingException e) {
            throw new AssertionError(e);
        }
    }

    public static Dialog a(int i, Activity activity, int i2, OnCancelListener onCancelListener, int i3) {
        Builder message = new Builder(activity).setMessage(b(activity, i, i3));
        if (onCancelListener != null) {
            message.setOnCancelListener(onCancelListener);
        }
        OnClickListener efVar = new ef(activity, a(activity, i, i3), i2);
        CharSequence b = b(activity, i);
        if (b != null) {
            message.setPositiveButton(b, efVar);
        }
        switch (i) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                return null;
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return message.setTitle(R.string.common_google_play_services_install_title).create();
            case DetectedActivity.ON_FOOT /*2*/:
                return message.setTitle(R.string.common_google_play_services_update_title).create();
            case DetectedActivity.STILL /*3*/:
                return message.setTitle(R.string.common_google_play_services_enable_title).create();
            case DetectedActivity.UNKNOWN /*4*/:
            case Participant.STATUS_UNRESPONSIVE /*6*/:
                return message.create();
            case DetectedActivity.TILTING /*5*/:
                Log.e("GooglePlayServicesUtil", "An invalid account was specified when connecting. Please provide a valid account.");
                return message.setTitle(R.string.common_google_play_services_invalid_account_title).create();
            case Error.AVS_DECLINE /*7*/:
                Log.e("GooglePlayServicesUtil", "Network error occurred. Please retry request later.");
                return message.setTitle(R.string.common_google_play_services_network_error_title).create();
            case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                Log.e("GooglePlayServicesUtil", "Internal error occurred. Please see logs for detailed information");
                return message.create();
            case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                Log.e("GooglePlayServicesUtil", "Google Play services is invalid. Cannot recover.");
                return message.setTitle(R.string.common_google_play_services_unsupported_title).create();
            case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                Log.e("GooglePlayServicesUtil", "Developer error occurred. Please see logs for detailed information");
                return message.create();
            case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                Log.e("GooglePlayServicesUtil", "The application is not licensed to the user.");
                return message.create();
            case CommonStatusCodes.DATE_INVALID /*12*/:
                Log.e("GooglePlayServicesUtil", "The date of the device is not valid.");
                return message.setTitle(R.string.common_google_play_services_unsupported_title).create();
            default:
                Log.e("GooglePlayServicesUtil", "Unexpected error code " + i);
                return message.create();
        }
    }

    public static Intent a(Context context, int i, int i2) {
        switch (i) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
            case DetectedActivity.ON_FOOT /*2*/:
                return E(i2) ? u(context) ? ek.ai(GOOGLE_PLAY_SERVICES_PACKAGE) : ek.ah("com.google.android.apps.bazaar") : ek.ah(GOOGLE_PLAY_SERVICES_PACKAGE);
            case DetectedActivity.STILL /*3*/:
                return ek.af(GOOGLE_PLAY_SERVICES_PACKAGE);
            case CommonStatusCodes.DATE_INVALID /*12*/:
                return ek.eh();
            default:
                return null;
        }
    }

    public static boolean a(Resources resources) {
        if (resources == null) {
            return false;
        }
        return (fr.eJ() && ((resources.getConfiguration().screenLayout & 15) > 3)) || b(resources);
    }

    private static byte[] a(PackageInfo packageInfo, byte[]... bArr) {
        try {
            CertificateFactory instance = CertificateFactory.getInstance("X509");
            if (packageInfo.signatures.length != 1) {
                Log.w("GooglePlayServicesUtil", "Package has more than one signature.");
                return null;
            }
            try {
                try {
                    ((X509Certificate) instance.generateCertificate(new ByteArrayInputStream(packageInfo.signatures[0].toByteArray()))).checkValidity();
                    byte[] toByteArray = packageInfo.signatures[0].toByteArray();
                    for (byte[] bArr2 : bArr) {
                        if (Arrays.equals(bArr2, toByteArray)) {
                            return bArr2;
                        }
                    }
                    if (Log.isLoggable("GooglePlayServicesUtil", 2)) {
                        Log.v("GooglePlayServicesUtil", "Signature not valid.  Found: \n" + Base64.encodeToString(toByteArray, 0));
                    }
                    return null;
                } catch (CertificateExpiredException e) {
                    Log.w("GooglePlayServicesUtil", "Certificate has expired.");
                    return null;
                } catch (CertificateNotYetValidException e2) {
                    Log.w("GooglePlayServicesUtil", "Certificate is not yet valid.");
                    return null;
                }
            } catch (CertificateException e3) {
                Log.w("GooglePlayServicesUtil", "Could not generate certificate.");
                return null;
            }
        } catch (CertificateException e4) {
            Log.w("GooglePlayServicesUtil", "Could not get certificate instance.");
            return null;
        }
    }

    private static byte[][] a(byte[][]... bArr) {
        int i = 0;
        for (byte[][] length : bArr) {
            i += length.length;
        }
        byte[][] bArr2 = new byte[i][];
        int length2 = bArr.length;
        int i2 = 0;
        int i3 = 0;
        while (i2 < length2) {
            byte[][] bArr3 = bArr[i2];
            i = i3;
            i3 = 0;
            while (i3 < bArr3.length) {
                int i4 = i + 1;
                bArr2[i] = bArr3[i3];
                i3++;
                i = i4;
            }
            i2++;
            i3 = i;
        }
        return bArr2;
    }

    public static String b(Context context, int i) {
        Resources resources = context.getResources();
        switch (i) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return resources.getString(R.string.common_google_play_services_install_button);
            case DetectedActivity.ON_FOOT /*2*/:
                return resources.getString(R.string.common_google_play_services_update_button);
            case DetectedActivity.STILL /*3*/:
                return resources.getString(R.string.common_google_play_services_enable_button);
            default:
                return resources.getString(17039370);
        }
    }

    public static String b(Context context, int i, int i2) {
        Resources resources = context.getResources();
        String string;
        switch (i) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                string = a(context.getResources()) ? resources.getString(R.string.common_google_play_services_install_text_tablet) : resources.getString(R.string.common_google_play_services_install_text_phone);
                return E(i2) ? string + " (via Bazaar)" : string;
            case DetectedActivity.ON_FOOT /*2*/:
                string = resources.getString(R.string.common_google_play_services_update_text);
                return E(i2) ? string + " (via Bazaar)" : string;
            case DetectedActivity.STILL /*3*/:
                return resources.getString(R.string.common_google_play_services_enable_text);
            case DetectedActivity.TILTING /*5*/:
                return resources.getString(R.string.common_google_play_services_invalid_account_text);
            case Error.AVS_DECLINE /*7*/:
                return resources.getString(R.string.common_google_play_services_network_error_text);
            case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                return resources.getString(R.string.common_google_play_services_unsupported_text);
            case CommonStatusCodes.DATE_INVALID /*12*/:
                return resources.getString(R.string.common_google_play_services_unsupported_date_text);
            default:
                return resources.getString(R.string.common_google_play_services_unknown_issue);
        }
    }

    private static boolean b(Resources resources) {
        Configuration configuration = resources.getConfiguration();
        return fr.eL() && (configuration.screenLayout & 15) <= 3 && configuration.smallestScreenWidthDp >= 600;
    }

    public static boolean do() {
        return yS ? yT : "user".equals(Build.TYPE);
    }

    public static Dialog getErrorDialog(int errorCode, Activity activity, int requestCode) {
        return a(errorCode, activity, requestCode, null, -1);
    }

    public static Dialog getErrorDialog(int errorCode, Activity activity, int requestCode, OnCancelListener cancelListener) {
        return a(errorCode, activity, requestCode, cancelListener, -1);
    }

    public static PendingIntent getErrorPendingIntent(int errorCode, Context context, int requestCode) {
        Intent a = a(context, errorCode, -1);
        return a == null ? null : PendingIntent.getActivity(context, requestCode, a, DriveFile.MODE_READ_ONLY);
    }

    public static String getErrorString(int errorCode) {
        switch (errorCode) {
            case APKExpansionPolicy.MAIN_FILE_URL_INDEX /*0*/:
                return "SUCCESS";
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
                return "SERVICE_MISSING";
            case DetectedActivity.ON_FOOT /*2*/:
                return "SERVICE_VERSION_UPDATE_REQUIRED";
            case DetectedActivity.STILL /*3*/:
                return "SERVICE_DISABLED";
            case DetectedActivity.UNKNOWN /*4*/:
                return "SIGN_IN_REQUIRED";
            case DetectedActivity.TILTING /*5*/:
                return "INVALID_ACCOUNT";
            case Participant.STATUS_UNRESPONSIVE /*6*/:
                return "RESOLUTION_REQUIRED";
            case Error.AVS_DECLINE /*7*/:
                return "NETWORK_ERROR";
            case TransportMediator.FLAG_KEY_MEDIA_PLAY_PAUSE /*8*/:
                return "INTERNAL_ERROR";
            case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                return "SERVICE_INVALID";
            case CommonStatusCodes.DEVELOPER_ERROR /*10*/:
                return "DEVELOPER_ERROR";
            case CommonStatusCodes.LICENSE_CHECK_FAILED /*11*/:
                return "LICENSE_CHECK_FAILED";
            case CommonStatusCodes.DATE_INVALID /*12*/:
                return "DATE_INVALID";
            default:
                return "UNKNOWN_ERROR_CODE";
        }
    }

    public static String getOpenSourceSoftwareLicenseInfo(Context context) {
        InputStream openInputStream;
        try {
            openInputStream = context.getContentResolver().openInputStream(new Uri.Builder().scheme("android.resource").authority(GOOGLE_PLAY_SERVICES_PACKAGE).appendPath("raw").appendPath("oss_notice").build());
            String next = new Scanner(openInputStream).useDelimiter("\\A").next();
            if (openInputStream == null) {
                return next;
            }
            openInputStream.close();
            return next;
        } catch (NoSuchElementException e) {
            if (openInputStream != null) {
                openInputStream.close();
            }
            return null;
        } catch (Exception e2) {
            return null;
        } catch (Throwable th) {
            if (openInputStream != null) {
                openInputStream.close();
            }
        }
    }

    public static Context getRemoteContext(Context context) {
        try {
            return context.createPackageContext(GOOGLE_PLAY_SERVICES_PACKAGE, 3);
        } catch (NameNotFoundException e) {
            return null;
        }
    }

    public static Resources getRemoteResource(Context context) {
        try {
            return context.getPackageManager().getResourcesForApplication(GOOGLE_PLAY_SERVICES_PACKAGE);
        } catch (NameNotFoundException e) {
            return null;
        }
    }

    public static int isGooglePlayServicesAvailable(Context context) {
        PackageManager packageManager = context.getPackageManager();
        try {
            context.getResources().getString(R.string.common_google_play_services_unknown_issue);
        } catch (Throwable th) {
            Log.e("GooglePlayServicesUtil", "The Google Play services resources were not found. Check your project configuration to ensure that the resources are included.");
        }
        if (System.currentTimeMillis() < 1227312000288L) {
            return 12;
        }
        t(context);
        try {
            PackageInfo packageInfo = packageManager.getPackageInfo(GOOGLE_PLAY_SERVICES_PACKAGE, 64);
            try {
                if (a(packageManager.getPackageInfo(GOOGLE_PLAY_STORE_PACKAGE, 64), yL) == null) {
                    Log.w("GooglePlayServicesUtil", "Google Play Store signature invalid.");
                    return 9;
                }
                if (a(packageInfo, a(packageManager.getPackageInfo(GOOGLE_PLAY_STORE_PACKAGE, 64), yL)) == null) {
                    Log.w("GooglePlayServicesUtil", "Google Play services signature invalid.");
                    return 9;
                } else if (packageInfo.versionCode < GOOGLE_PLAY_SERVICES_VERSION_CODE) {
                    Log.w("GooglePlayServicesUtil", "Google Play services out of date.  Requires 4323000 but found " + packageInfo.versionCode);
                    return 2;
                } else {
                    try {
                        return !packageManager.getApplicationInfo(GOOGLE_PLAY_SERVICES_PACKAGE, 0).enabled ? 3 : 0;
                    } catch (NameNotFoundException e) {
                        Log.wtf("GooglePlayServicesUtil", "Google Play services missing when getting application info.");
                        e.printStackTrace();
                        return 1;
                    }
                }
            } catch (NameNotFoundException e2) {
                Log.w("GooglePlayServicesUtil", "Google Play Store is missing.");
                return 9;
            }
        } catch (NameNotFoundException e3) {
            Log.w("GooglePlayServicesUtil", "Google Play services is missing.");
            return 1;
        }
    }

    public static boolean isUserRecoverableError(int errorCode) {
        switch (errorCode) {
            case APKExpansionPolicy.PATCH_FILE_URL_INDEX /*1*/:
            case DetectedActivity.ON_FOOT /*2*/:
            case DetectedActivity.STILL /*3*/:
            case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
            case CommonStatusCodes.DATE_INVALID /*12*/:
                return true;
            default:
                return false;
        }
    }

    public static void s(Context context) throws GooglePlayServicesRepairableException, GooglePlayServicesNotAvailableException {
        int isGooglePlayServicesAvailable = isGooglePlayServicesAvailable(context);
        if (isGooglePlayServicesAvailable != 0) {
            Intent a = a(context, isGooglePlayServicesAvailable, -1);
            Log.e("GooglePlayServicesUtil", "GooglePlayServices not available due to error " + isGooglePlayServicesAvailable);
            if (a == null) {
                throw new GooglePlayServicesNotAvailableException(isGooglePlayServicesAvailable);
            }
            throw new GooglePlayServicesRepairableException(isGooglePlayServicesAvailable, "Google Play Services not available", a);
        }
    }

    public static boolean showErrorDialogFragment(int errorCode, Activity activity, int requestCode) {
        return showErrorDialogFragment(errorCode, activity, requestCode, null);
    }

    public static boolean showErrorDialogFragment(int errorCode, Activity activity, int requestCode, OnCancelListener cancelListener) {
        boolean z = false;
        Dialog errorDialog = getErrorDialog(errorCode, activity, requestCode, cancelListener);
        if (errorDialog == null) {
            return z;
        }
        try {
            z = activity instanceof FragmentActivity;
        } catch (NoClassDefFoundError e) {
        }
        if (z) {
            b.a(errorDialog, cancelListener).show(((FragmentActivity) activity).getSupportFragmentManager(), GMS_ERROR_DIALOG);
        } else if (fr.eJ()) {
            ErrorDialogFragment.newInstance(errorDialog, cancelListener).show(activity.getFragmentManager(), GMS_ERROR_DIALOG);
        } else {
            throw new RuntimeException("This Activity does not support Fragments.");
        }
        return true;
    }

    private static void t(Context context) {
        ApplicationInfo applicationInfo = null;
        try {
            applicationInfo = context.getPackageManager().getApplicationInfo(context.getPackageName(), TransportMediator.FLAG_KEY_MEDIA_NEXT);
        } catch (Throwable e) {
            Log.wtf("GooglePlayServicesUtil", "This should never happen.", e);
        }
        Bundle bundle = applicationInfo.metaData;
        if (bundle != null) {
            int i = bundle.getInt("com.google.android.gms.version");
            if (i != GOOGLE_PLAY_SERVICES_VERSION_CODE) {
                throw new IllegalStateException("The meta-data tag in your app's AndroidManifest.xml does not have the right value.  Expected 4323000 but found " + i + ".  You must have the" + " following declaration within the <application> element: " + "    <meta-data android:name=\"" + "com.google.android.gms.version" + "\" android:value=\"@integer/google_play_services_version\" />");
            }
            return;
        }
        throw new IllegalStateException("A required meta-data tag in your app's AndroidManifest.xml does not exist.  You must have the following declaration within the <application> element:     <meta-data android:name=\"com.google.android.gms.version\" android:value=\"@integer/google_play_services_version\" />");
    }

    private static boolean u(Context context) {
        if (yS) {
            return yU;
        }
        try {
            return a(context.getPackageManager().getPackageInfo("com.google.android.apps.bazaar", 64), yR) != null;
        } catch (NameNotFoundException e) {
            return false;
        }
    }
}
