package android.support.v4.text;

import android.support.v4.media.TransportMediator;
import com.google.android.gms.games.GamesStatusCodes;
import java.util.Locale;

public final class BidiFormatter {
    private static final int DEFAULT_FLAGS = 2;
    private static final BidiFormatter DEFAULT_LTR_INSTANCE;
    private static final BidiFormatter DEFAULT_RTL_INSTANCE;
    private static TextDirectionHeuristicCompat DEFAULT_TEXT_DIRECTION_HEURISTIC = null;
    private static final int DIR_LTR = -1;
    private static final int DIR_RTL = 1;
    private static final int DIR_UNKNOWN = 0;
    private static final String EMPTY_STRING = "";
    private static final int FLAG_STEREO_RESET = 2;
    private static final char LRE = '\u202a';
    private static final char LRM = '\u200e';
    private static final String LRM_STRING;
    private static final char PDF = '\u202c';
    private static final char RLE = '\u202b';
    private static final char RLM = '\u200f';
    private static final String RLM_STRING;
    private final TextDirectionHeuristicCompat mDefaultTextDirectionHeuristicCompat;
    private final int mFlags;
    private final boolean mIsRtlContext;

    public static final class Builder {
        private int mFlags;
        private boolean mIsRtlContext;
        private TextDirectionHeuristicCompat mTextDirectionHeuristicCompat;

        public Builder() {
            initialize(BidiFormatter.isRtlLocale(Locale.getDefault()));
        }

        public Builder(boolean rtlContext) {
            initialize(rtlContext);
        }

        public Builder(Locale locale) {
            initialize(BidiFormatter.isRtlLocale(locale));
        }

        private void initialize(boolean isRtlContext) {
            this.mIsRtlContext = isRtlContext;
            this.mTextDirectionHeuristicCompat = BidiFormatter.DEFAULT_TEXT_DIRECTION_HEURISTIC;
            this.mFlags = BidiFormatter.FLAG_STEREO_RESET;
        }

        public Builder stereoReset(boolean stereoReset) {
            if (stereoReset) {
                this.mFlags |= BidiFormatter.FLAG_STEREO_RESET;
            } else {
                this.mFlags &= -3;
            }
            return this;
        }

        public Builder setTextDirectionHeuristic(TextDirectionHeuristicCompat heuristic) {
            this.mTextDirectionHeuristicCompat = heuristic;
            return this;
        }

        private static BidiFormatter getDefaultInstanceFromContext(boolean isRtlContext) {
            return isRtlContext ? BidiFormatter.DEFAULT_RTL_INSTANCE : BidiFormatter.DEFAULT_LTR_INSTANCE;
        }

        public BidiFormatter build() {
            if (this.mFlags == BidiFormatter.FLAG_STEREO_RESET && this.mTextDirectionHeuristicCompat == BidiFormatter.DEFAULT_TEXT_DIRECTION_HEURISTIC) {
                return getDefaultInstanceFromContext(this.mIsRtlContext);
            }
            return new BidiFormatter(this.mFlags, this.mTextDirectionHeuristicCompat, null);
        }
    }

    private static class DirectionalityEstimator {
        private static final byte[] DIR_TYPE_CACHE;
        private static final int DIR_TYPE_CACHE_SIZE = 1792;
        private int charIndex;
        private final boolean isHtml;
        private char lastChar;
        private final int length;
        private final String text;

        static {
            DIR_TYPE_CACHE = new byte[DIR_TYPE_CACHE_SIZE];
            for (int i = BidiFormatter.DIR_UNKNOWN; i < DIR_TYPE_CACHE_SIZE; i += BidiFormatter.DIR_RTL) {
                DIR_TYPE_CACHE[i] = Character.getDirectionality(i);
            }
        }

        DirectionalityEstimator(String text, boolean isHtml) {
            this.text = text;
            this.isHtml = isHtml;
            this.length = text.length();
        }

        int getEntryDir() {
            this.charIndex = BidiFormatter.DIR_UNKNOWN;
            int embeddingLevel = BidiFormatter.DIR_UNKNOWN;
            int embeddingLevelDir = BidiFormatter.DIR_UNKNOWN;
            int firstNonEmptyEmbeddingLevel = BidiFormatter.DIR_UNKNOWN;
            while (this.charIndex < this.length && firstNonEmptyEmbeddingLevel == 0) {
                switch (dirTypeForward()) {
                    case BidiFormatter.DIR_UNKNOWN /*0*/:
                        if (embeddingLevel != 0) {
                            firstNonEmptyEmbeddingLevel = embeddingLevel;
                            break;
                        }
                        return BidiFormatter.DIR_LTR;
                    case BidiFormatter.DIR_RTL /*1*/:
                    case BidiFormatter.FLAG_STEREO_RESET /*2*/:
                        if (embeddingLevel != 0) {
                            firstNonEmptyEmbeddingLevel = embeddingLevel;
                            break;
                        }
                        return BidiFormatter.DIR_RTL;
                    case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                        break;
                    case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                        embeddingLevel += BidiFormatter.DIR_RTL;
                        embeddingLevelDir = BidiFormatter.DIR_LTR;
                        break;
                    case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                    case (byte) 17:
                        embeddingLevel += BidiFormatter.DIR_RTL;
                        embeddingLevelDir = BidiFormatter.DIR_RTL;
                        break;
                    case (byte) 18:
                        embeddingLevel += BidiFormatter.DIR_LTR;
                        embeddingLevelDir = BidiFormatter.DIR_UNKNOWN;
                        break;
                    default:
                        firstNonEmptyEmbeddingLevel = embeddingLevel;
                        break;
                }
            }
            if (firstNonEmptyEmbeddingLevel == 0) {
                return BidiFormatter.DIR_UNKNOWN;
            }
            if (embeddingLevelDir != 0) {
                return embeddingLevelDir;
            }
            while (this.charIndex > 0) {
                switch (dirTypeBackward()) {
                    case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                        if (firstNonEmptyEmbeddingLevel != embeddingLevel) {
                            embeddingLevel += BidiFormatter.DIR_LTR;
                            break;
                        }
                        return BidiFormatter.DIR_LTR;
                    case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                    case (byte) 17:
                        if (firstNonEmptyEmbeddingLevel != embeddingLevel) {
                            embeddingLevel += BidiFormatter.DIR_LTR;
                            break;
                        }
                        return BidiFormatter.DIR_RTL;
                    case (byte) 18:
                        embeddingLevel += BidiFormatter.DIR_RTL;
                        break;
                    default:
                        break;
                }
            }
            return BidiFormatter.DIR_UNKNOWN;
        }

        int getExitDir() {
            this.charIndex = this.length;
            int embeddingLevel = BidiFormatter.DIR_UNKNOWN;
            int lastNonEmptyEmbeddingLevel = BidiFormatter.DIR_UNKNOWN;
            while (this.charIndex > 0) {
                switch (dirTypeBackward()) {
                    case BidiFormatter.DIR_UNKNOWN /*0*/:
                        if (embeddingLevel != 0) {
                            if (lastNonEmptyEmbeddingLevel != 0) {
                                break;
                            }
                            lastNonEmptyEmbeddingLevel = embeddingLevel;
                            break;
                        }
                        return BidiFormatter.DIR_LTR;
                    case BidiFormatter.DIR_RTL /*1*/:
                    case BidiFormatter.FLAG_STEREO_RESET /*2*/:
                        if (embeddingLevel != 0) {
                            if (lastNonEmptyEmbeddingLevel != 0) {
                                break;
                            }
                            lastNonEmptyEmbeddingLevel = embeddingLevel;
                            break;
                        }
                        return BidiFormatter.DIR_RTL;
                    case GamesStatusCodes.STATUS_GAME_NOT_FOUND /*9*/:
                        break;
                    case GamesStatusCodes.STATUS_INTERRUPTED /*14*/:
                    case GamesStatusCodes.STATUS_TIMEOUT /*15*/:
                        if (lastNonEmptyEmbeddingLevel != embeddingLevel) {
                            embeddingLevel += BidiFormatter.DIR_LTR;
                            break;
                        }
                        return BidiFormatter.DIR_LTR;
                    case TransportMediator.FLAG_KEY_MEDIA_PAUSE /*16*/:
                    case (byte) 17:
                        if (lastNonEmptyEmbeddingLevel != embeddingLevel) {
                            embeddingLevel += BidiFormatter.DIR_LTR;
                            break;
                        }
                        return BidiFormatter.DIR_RTL;
                    case (byte) 18:
                        embeddingLevel += BidiFormatter.DIR_RTL;
                        break;
                    default:
                        if (lastNonEmptyEmbeddingLevel != 0) {
                            break;
                        }
                        lastNonEmptyEmbeddingLevel = embeddingLevel;
                        break;
                }
            }
            return BidiFormatter.DIR_UNKNOWN;
        }

        private static byte getCachedDirectionality(char c) {
            return c < '\u0700' ? DIR_TYPE_CACHE[c] : Character.getDirectionality(c);
        }

        byte dirTypeForward() {
            this.lastChar = this.text.charAt(this.charIndex);
            if (Character.isHighSurrogate(this.lastChar)) {
                int codePoint = Character.codePointAt(this.text, this.charIndex);
                this.charIndex += Character.charCount(codePoint);
                return Character.getDirectionality(codePoint);
            }
            this.charIndex += BidiFormatter.DIR_RTL;
            byte dirType = getCachedDirectionality(this.lastChar);
            if (!this.isHtml) {
                return dirType;
            }
            if (this.lastChar == '<') {
                return skipTagForward();
            }
            if (this.lastChar == '&') {
                return skipEntityForward();
            }
            return dirType;
        }

        byte dirTypeBackward() {
            this.lastChar = this.text.charAt(this.charIndex + BidiFormatter.DIR_LTR);
            if (Character.isLowSurrogate(this.lastChar)) {
                int codePoint = Character.codePointBefore(this.text, this.charIndex);
                this.charIndex -= Character.charCount(codePoint);
                return Character.getDirectionality(codePoint);
            }
            this.charIndex += BidiFormatter.DIR_LTR;
            byte dirType = getCachedDirectionality(this.lastChar);
            if (!this.isHtml) {
                return dirType;
            }
            if (this.lastChar == '>') {
                return skipTagBackward();
            }
            if (this.lastChar == ';') {
                return skipEntityBackward();
            }
            return dirType;
        }

        private byte skipTagForward() {
            int initialCharIndex = this.charIndex;
            while (this.charIndex < this.length) {
                String str = this.text;
                int i = this.charIndex;
                this.charIndex = i + BidiFormatter.DIR_RTL;
                this.lastChar = str.charAt(i);
                if (this.lastChar == '>') {
                    return (byte) 12;
                }
                if (this.lastChar == '\"' || this.lastChar == '\'') {
                    char quote = this.lastChar;
                    while (this.charIndex < this.length) {
                        str = this.text;
                        i = this.charIndex;
                        this.charIndex = i + BidiFormatter.DIR_RTL;
                        char charAt = str.charAt(i);
                        this.lastChar = charAt;
                        if (charAt == quote) {
                            break;
                        }
                    }
                }
            }
            this.charIndex = initialCharIndex;
            this.lastChar = '<';
            return (byte) 13;
        }

        private byte skipTagBackward() {
            int initialCharIndex = this.charIndex;
            while (this.charIndex > 0) {
                String str = this.text;
                int i = this.charIndex + BidiFormatter.DIR_LTR;
                this.charIndex = i;
                this.lastChar = str.charAt(i);
                if (this.lastChar == '<') {
                    return (byte) 12;
                }
                if (this.lastChar == '>') {
                    break;
                } else if (this.lastChar == '\"' || this.lastChar == '\'') {
                    char quote = this.lastChar;
                    while (this.charIndex > 0) {
                        str = this.text;
                        i = this.charIndex + BidiFormatter.DIR_LTR;
                        this.charIndex = i;
                        char charAt = str.charAt(i);
                        this.lastChar = charAt;
                        if (charAt == quote) {
                            break;
                        }
                    }
                }
            }
            this.charIndex = initialCharIndex;
            this.lastChar = '>';
            return (byte) 13;
        }

        private byte skipEntityForward() {
            while (this.charIndex < this.length) {
                String str = this.text;
                int i = this.charIndex;
                this.charIndex = i + BidiFormatter.DIR_RTL;
                char charAt = str.charAt(i);
                this.lastChar = charAt;
                if (charAt == ';') {
                    break;
                }
            }
            return (byte) 12;
        }

        private byte skipEntityBackward() {
            int initialCharIndex = this.charIndex;
            while (this.charIndex > 0) {
                String str = this.text;
                int i = this.charIndex + BidiFormatter.DIR_LTR;
                this.charIndex = i;
                this.lastChar = str.charAt(i);
                if (this.lastChar != '&') {
                    if (this.lastChar == ';') {
                        break;
                    }
                }
                return (byte) 12;
            }
            this.charIndex = initialCharIndex;
            this.lastChar = ';';
            return (byte) 13;
        }
    }

    static {
        DEFAULT_TEXT_DIRECTION_HEURISTIC = TextDirectionHeuristicsCompat.FIRSTSTRONG_LTR;
        LRM_STRING = Character.toString(LRM);
        RLM_STRING = Character.toString(RLM);
        DEFAULT_LTR_INSTANCE = new BidiFormatter(false, FLAG_STEREO_RESET, DEFAULT_TEXT_DIRECTION_HEURISTIC);
        DEFAULT_RTL_INSTANCE = new BidiFormatter(true, FLAG_STEREO_RESET, DEFAULT_TEXT_DIRECTION_HEURISTIC);
    }

    public static BidiFormatter getInstance() {
        return new Builder().build();
    }

    public static BidiFormatter getInstance(boolean rtlContext) {
        return new Builder(rtlContext).build();
    }

    public static BidiFormatter getInstance(Locale locale) {
        return new Builder(locale).build();
    }

    private BidiFormatter(boolean isRtlContext, int flags, TextDirectionHeuristicCompat heuristic) {
        this.mIsRtlContext = isRtlContext;
        this.mFlags = flags;
        this.mDefaultTextDirectionHeuristicCompat = heuristic;
    }

    public boolean isRtlContext() {
        return this.mIsRtlContext;
    }

    public boolean getStereoReset() {
        return (this.mFlags & FLAG_STEREO_RESET) != 0;
    }

    private String markAfter(String str, TextDirectionHeuristicCompat heuristic) {
        boolean isRtl = heuristic.isRtl((CharSequence) str, (int) DIR_UNKNOWN, str.length());
        if (!this.mIsRtlContext && (isRtl || getExitDir(str) == DIR_RTL)) {
            return LRM_STRING;
        }
        if (!this.mIsRtlContext || (isRtl && getExitDir(str) != DIR_LTR)) {
            return EMPTY_STRING;
        }
        return RLM_STRING;
    }

    private String markBefore(String str, TextDirectionHeuristicCompat heuristic) {
        boolean isRtl = heuristic.isRtl((CharSequence) str, (int) DIR_UNKNOWN, str.length());
        if (!this.mIsRtlContext && (isRtl || getEntryDir(str) == DIR_RTL)) {
            return LRM_STRING;
        }
        if (!this.mIsRtlContext || (isRtl && getEntryDir(str) != DIR_LTR)) {
            return EMPTY_STRING;
        }
        return RLM_STRING;
    }

    public boolean isRtl(String str) {
        return this.mDefaultTextDirectionHeuristicCompat.isRtl((CharSequence) str, (int) DIR_UNKNOWN, str.length());
    }

    public String unicodeWrap(String str, TextDirectionHeuristicCompat heuristic, boolean isolate) {
        boolean isRtl = heuristic.isRtl((CharSequence) str, (int) DIR_UNKNOWN, str.length());
        StringBuilder result = new StringBuilder();
        if (getStereoReset() && isolate) {
            result.append(markBefore(str, isRtl ? TextDirectionHeuristicsCompat.RTL : TextDirectionHeuristicsCompat.LTR));
        }
        if (isRtl != this.mIsRtlContext) {
            result.append(isRtl ? RLE : LRE);
            result.append(str);
            result.append(PDF);
        } else {
            result.append(str);
        }
        if (isolate) {
            result.append(markAfter(str, isRtl ? TextDirectionHeuristicsCompat.RTL : TextDirectionHeuristicsCompat.LTR));
        }
        return result.toString();
    }

    public String unicodeWrap(String str, TextDirectionHeuristicCompat heuristic) {
        return unicodeWrap(str, heuristic, true);
    }

    public String unicodeWrap(String str, boolean isolate) {
        return unicodeWrap(str, this.mDefaultTextDirectionHeuristicCompat, isolate);
    }

    public String unicodeWrap(String str) {
        return unicodeWrap(str, this.mDefaultTextDirectionHeuristicCompat, true);
    }

    private static boolean isRtlLocale(Locale locale) {
        return TextUtilsCompat.getLayoutDirectionFromLocale(locale) == DIR_RTL;
    }

    private static int getExitDir(String str) {
        return new DirectionalityEstimator(str, false).getExitDir();
    }

    private static int getEntryDir(String str) {
        return new DirectionalityEstimator(str, false).getEntryDir();
    }
}
