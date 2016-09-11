package android.support.v4.text;

import android.support.v4.media.TransportMediator;
import com.google.android.gms.games.GamesStatusCodes;
import java.nio.CharBuffer;
import java.util.Locale;

public class TextDirectionHeuristicsCompat {
    public static final TextDirectionHeuristicCompat ANYRTL_LTR;
    public static final TextDirectionHeuristicCompat FIRSTSTRONG_LTR;
    public static final TextDirectionHeuristicCompat FIRSTSTRONG_RTL;
    public static final TextDirectionHeuristicCompat LOCALE;
    public static final TextDirectionHeuristicCompat LTR;
    public static final TextDirectionHeuristicCompat RTL;
    private static final int STATE_FALSE = 1;
    private static final int STATE_TRUE = 0;
    private static final int STATE_UNKNOWN = 2;

    private interface TextDirectionAlgorithm {
        int checkRtl(CharSequence charSequence, int i, int i2);
    }

    private static class AnyStrong implements TextDirectionAlgorithm {
        public static final AnyStrong INSTANCE_LTR;
        public static final AnyStrong INSTANCE_RTL;
        private final boolean mLookForRtl;

        public int checkRtl(CharSequence cs, int start, int count) {
            boolean haveUnlookedFor = false;
            int e = start + count;
            for (int i = start; i < e; i += TextDirectionHeuristicsCompat.STATE_FALSE) {
                switch (TextDirectionHeuristicsCompat.isRtlText(Character.getDirectionality(cs.charAt(i)))) {
                    case TextDirectionHeuristicsCompat.STATE_TRUE /*0*/:
                        if (!this.mLookForRtl) {
                            haveUnlookedFor = true;
                            break;
                        }
                        return TextDirectionHeuristicsCompat.STATE_TRUE;
                    case TextDirectionHeuristicsCompat.STATE_FALSE /*1*/:
                        if (this.mLookForRtl) {
                            haveUnlookedFor = true;
                            break;
                        }
                        return TextDirectionHeuristicsCompat.STATE_FALSE;
                    default:
                        break;
                }
            }
            if (!haveUnlookedFor) {
                return TextDirectionHeuristicsCompat.STATE_UNKNOWN;
            }
            if (this.mLookForRtl) {
                return TextDirectionHeuristicsCompat.STATE_FALSE;
            }
            return TextDirectionHeuristicsCompat.STATE_TRUE;
        }

        private AnyStrong(boolean lookForRtl) {
            this.mLookForRtl = lookForRtl;
        }

        static {
            INSTANCE_RTL = new AnyStrong(true);
            INSTANCE_LTR = new AnyStrong(false);
        }
    }

    private static class FirstStrong implements TextDirectionAlgorithm {
        public static final FirstStrong INSTANCE;

        public int checkRtl(CharSequence cs, int start, int count) {
            int result = TextDirectionHeuristicsCompat.STATE_UNKNOWN;
            int e = start + count;
            for (int i = start; i < e && result == TextDirectionHeuristicsCompat.STATE_UNKNOWN; i += TextDirectionHeuristicsCompat.STATE_FALSE) {
                result = TextDirectionHeuristicsCompat.isRtlTextOrFormat(Character.getDirectionality(cs.charAt(i)));
            }
            return result;
        }

        private FirstStrong() {
        }

        static {
            INSTANCE = new FirstStrong();
        }
    }

    private static abstract class TextDirectionHeuristicImpl implements TextDirectionHeuristicCompat {
        private final TextDirectionAlgorithm mAlgorithm;

        protected abstract boolean defaultIsRtl();

        public TextDirectionHeuristicImpl(TextDirectionAlgorithm algorithm) {
            this.mAlgorithm = algorithm;
        }

        public boolean isRtl(char[] array, int start, int count) {
            return isRtl(CharBuffer.wrap(array), start, count);
        }

        public boolean isRtl(CharSequence cs, int start, int count) {
            if (cs == null || start < 0 || count < 0 || cs.length() - count < start) {
                throw new IllegalArgumentException();
            } else if (this.mAlgorithm == null) {
                return defaultIsRtl();
            } else {
                return doCheck(cs, start, count);
            }
        }

        private boolean doCheck(CharSequence cs, int start, int count) {
            switch (this.mAlgorithm.checkRtl(cs, start, count)) {
                case TextDirectionHeuristicsCompat.STATE_TRUE /*0*/:
                    return true;
                case TextDirectionHeuristicsCompat.STATE_FALSE /*1*/:
                    return false;
                default:
                    return defaultIsRtl();
            }
        }
    }

    private static class TextDirectionHeuristicInternal extends TextDirectionHeuristicImpl {
        private final boolean mDefaultIsRtl;

        private TextDirectionHeuristicInternal(TextDirectionAlgorithm algorithm, boolean defaultIsRtl) {
            super(algorithm);
            this.mDefaultIsRtl = defaultIsRtl;
        }

        protected boolean defaultIsRtl() {
            return this.mDefaultIsRtl;
        }
    }

    private static class TextDirectionHeuristicLocale extends TextDirectionHeuristicImpl {
        public static final TextDirectionHeuristicLocale INSTANCE;

        public TextDirectionHeuristicLocale() {
            super(null);
        }

        protected boolean defaultIsRtl() {
            if (TextUtilsCompat.getLayoutDirectionFromLocale(Locale.getDefault()) == TextDirectionHeuristicsCompat.STATE_FALSE) {
                return true;
            }
            return false;
        }

        static {
            INSTANCE = new TextDirectionHeuristicLocale();
        }
    }

    static {
        LTR = new TextDirectionHeuristicInternal(false, null);
        RTL = new TextDirectionHeuristicInternal(true, null);
        FIRSTSTRONG_LTR = new TextDirectionHeuristicInternal(false, null);
        FIRSTSTRONG_RTL = new TextDirectionHeuristicInternal(true, null);
        ANYRTL_LTR = new TextDirectionHeuristicInternal(false, null);
        LOCALE = TextDirectionHeuristicLocale.INSTANCE;
    }

    private static int isRtlText(int directionality) {
        switch (directionality) {
            case STATE_TRUE /*0*/:
                return STATE_FALSE;
            case STATE_FALSE /*1*/:
            case STATE_UNKNOWN /*2*/:
                return STATE_TRUE;
            default:
                return STATE_UNKNOWN;
        }
    }

    private static int isRtlTextOrFormat(int directionality) {
        switch (directionality) {
            case STATE_TRUE /*0*/:
            case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
            case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                return STATE_FALSE;
            case STATE_FALSE /*1*/:
            case STATE_UNKNOWN /*2*/:
            case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
            case 17:
                return STATE_TRUE;
            default:
                return STATE_UNKNOWN;
        }
    }
}
