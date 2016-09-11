package com.google.android.gms.plus.model.moments;

import com.google.android.gms.common.data.Freezable;
import com.google.android.gms.internal.im;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public interface ItemScope extends Freezable<ItemScope> {

    public static class Builder {
        private String FH;
        private double KX;
        private double KY;
        private String Oc;
        private final Set<Integer> RM;
        private im RN;
        private List<String> RO;
        private im RP;
        private String RQ;
        private String RR;
        private String RS;
        private List<im> RT;
        private int RU;
        private List<im> RV;
        private im RW;
        private List<im> RX;
        private String RY;
        private String RZ;
        private String SA;
        private String SB;
        private im SC;
        private String SD;
        private String SE;
        private String SF;
        private String SG;
        private im Sa;
        private String Sb;
        private String Sc;
        private List<im> Sd;
        private String Se;
        private String Sf;
        private String Sg;
        private String Sh;
        private String Si;
        private String Sj;
        private String Sk;
        private String Sl;
        private im Sm;
        private String Sn;
        private String So;
        private String Sp;
        private im Sq;
        private im Sr;
        private im Ss;
        private List<im> St;
        private String Su;
        private String Sv;
        private String Sw;
        private String Sx;
        private im Sy;
        private String Sz;
        private String lt;
        private String mName;
        private String pS;
        private String uS;

        public Builder() {
            this.RM = new HashSet();
        }

        public ItemScope build() {
            return new im(this.RM, this.RN, this.RO, this.RP, this.RQ, this.RR, this.RS, this.RT, this.RU, this.RV, this.RW, this.RX, this.RY, this.RZ, this.Sa, this.Sb, this.Sc, this.lt, this.Sd, this.Se, this.Sf, this.Sg, this.FH, this.Sh, this.Si, this.Sj, this.Sk, this.Sl, this.Sm, this.Sn, this.So, this.uS, this.Sp, this.Sq, this.KX, this.Sr, this.KY, this.mName, this.Ss, this.St, this.Su, this.Sv, this.Sw, this.Sx, this.Sy, this.Sz, this.SA, this.SB, this.SC, this.SD, this.SE, this.Oc, this.pS, this.SF, this.SG);
        }

        public Builder setAbout(ItemScope about) {
            this.RN = (im) about;
            this.RM.add(Integer.valueOf(2));
            return this;
        }

        public Builder setAdditionalName(List<String> additionalName) {
            this.RO = additionalName;
            this.RM.add(Integer.valueOf(3));
            return this;
        }

        public Builder setAddress(ItemScope address) {
            this.RP = (im) address;
            this.RM.add(Integer.valueOf(4));
            return this;
        }

        public Builder setAddressCountry(String addressCountry) {
            this.RQ = addressCountry;
            this.RM.add(Integer.valueOf(5));
            return this;
        }

        public Builder setAddressLocality(String addressLocality) {
            this.RR = addressLocality;
            this.RM.add(Integer.valueOf(6));
            return this;
        }

        public Builder setAddressRegion(String addressRegion) {
            this.RS = addressRegion;
            this.RM.add(Integer.valueOf(7));
            return this;
        }

        public Builder setAssociated_media(List<ItemScope> associated_media) {
            this.RT = associated_media;
            this.RM.add(Integer.valueOf(8));
            return this;
        }

        public Builder setAttendeeCount(int attendeeCount) {
            this.RU = attendeeCount;
            this.RM.add(Integer.valueOf(9));
            return this;
        }

        public Builder setAttendees(List<ItemScope> attendees) {
            this.RV = attendees;
            this.RM.add(Integer.valueOf(10));
            return this;
        }

        public Builder setAudio(ItemScope audio) {
            this.RW = (im) audio;
            this.RM.add(Integer.valueOf(11));
            return this;
        }

        public Builder setAuthor(List<ItemScope> author) {
            this.RX = author;
            this.RM.add(Integer.valueOf(12));
            return this;
        }

        public Builder setBestRating(String bestRating) {
            this.RY = bestRating;
            this.RM.add(Integer.valueOf(13));
            return this;
        }

        public Builder setBirthDate(String birthDate) {
            this.RZ = birthDate;
            this.RM.add(Integer.valueOf(14));
            return this;
        }

        public Builder setByArtist(ItemScope byArtist) {
            this.Sa = (im) byArtist;
            this.RM.add(Integer.valueOf(15));
            return this;
        }

        public Builder setCaption(String caption) {
            this.Sb = caption;
            this.RM.add(Integer.valueOf(16));
            return this;
        }

        public Builder setContentSize(String contentSize) {
            this.Sc = contentSize;
            this.RM.add(Integer.valueOf(17));
            return this;
        }

        public Builder setContentUrl(String contentUrl) {
            this.lt = contentUrl;
            this.RM.add(Integer.valueOf(18));
            return this;
        }

        public Builder setContributor(List<ItemScope> contributor) {
            this.Sd = contributor;
            this.RM.add(Integer.valueOf(19));
            return this;
        }

        public Builder setDateCreated(String dateCreated) {
            this.Se = dateCreated;
            this.RM.add(Integer.valueOf(20));
            return this;
        }

        public Builder setDateModified(String dateModified) {
            this.Sf = dateModified;
            this.RM.add(Integer.valueOf(21));
            return this;
        }

        public Builder setDatePublished(String datePublished) {
            this.Sg = datePublished;
            this.RM.add(Integer.valueOf(22));
            return this;
        }

        public Builder setDescription(String description) {
            this.FH = description;
            this.RM.add(Integer.valueOf(23));
            return this;
        }

        public Builder setDuration(String duration) {
            this.Sh = duration;
            this.RM.add(Integer.valueOf(24));
            return this;
        }

        public Builder setEmbedUrl(String embedUrl) {
            this.Si = embedUrl;
            this.RM.add(Integer.valueOf(25));
            return this;
        }

        public Builder setEndDate(String endDate) {
            this.Sj = endDate;
            this.RM.add(Integer.valueOf(26));
            return this;
        }

        public Builder setFamilyName(String familyName) {
            this.Sk = familyName;
            this.RM.add(Integer.valueOf(27));
            return this;
        }

        public Builder setGender(String gender) {
            this.Sl = gender;
            this.RM.add(Integer.valueOf(28));
            return this;
        }

        public Builder setGeo(ItemScope geo) {
            this.Sm = (im) geo;
            this.RM.add(Integer.valueOf(29));
            return this;
        }

        public Builder setGivenName(String givenName) {
            this.Sn = givenName;
            this.RM.add(Integer.valueOf(30));
            return this;
        }

        public Builder setHeight(String height) {
            this.So = height;
            this.RM.add(Integer.valueOf(31));
            return this;
        }

        public Builder setId(String id) {
            this.uS = id;
            this.RM.add(Integer.valueOf(32));
            return this;
        }

        public Builder setImage(String image) {
            this.Sp = image;
            this.RM.add(Integer.valueOf(33));
            return this;
        }

        public Builder setInAlbum(ItemScope inAlbum) {
            this.Sq = (im) inAlbum;
            this.RM.add(Integer.valueOf(34));
            return this;
        }

        public Builder setLatitude(double latitude) {
            this.KX = latitude;
            this.RM.add(Integer.valueOf(36));
            return this;
        }

        public Builder setLocation(ItemScope location) {
            this.Sr = (im) location;
            this.RM.add(Integer.valueOf(37));
            return this;
        }

        public Builder setLongitude(double longitude) {
            this.KY = longitude;
            this.RM.add(Integer.valueOf(38));
            return this;
        }

        public Builder setName(String name) {
            this.mName = name;
            this.RM.add(Integer.valueOf(39));
            return this;
        }

        public Builder setPartOfTVSeries(ItemScope partOfTVSeries) {
            this.Ss = (im) partOfTVSeries;
            this.RM.add(Integer.valueOf(40));
            return this;
        }

        public Builder setPerformers(List<ItemScope> performers) {
            this.St = performers;
            this.RM.add(Integer.valueOf(41));
            return this;
        }

        public Builder setPlayerType(String playerType) {
            this.Su = playerType;
            this.RM.add(Integer.valueOf(42));
            return this;
        }

        public Builder setPostOfficeBoxNumber(String postOfficeBoxNumber) {
            this.Sv = postOfficeBoxNumber;
            this.RM.add(Integer.valueOf(43));
            return this;
        }

        public Builder setPostalCode(String postalCode) {
            this.Sw = postalCode;
            this.RM.add(Integer.valueOf(44));
            return this;
        }

        public Builder setRatingValue(String ratingValue) {
            this.Sx = ratingValue;
            this.RM.add(Integer.valueOf(45));
            return this;
        }

        public Builder setReviewRating(ItemScope reviewRating) {
            this.Sy = (im) reviewRating;
            this.RM.add(Integer.valueOf(46));
            return this;
        }

        public Builder setStartDate(String startDate) {
            this.Sz = startDate;
            this.RM.add(Integer.valueOf(47));
            return this;
        }

        public Builder setStreetAddress(String streetAddress) {
            this.SA = streetAddress;
            this.RM.add(Integer.valueOf(48));
            return this;
        }

        public Builder setText(String text) {
            this.SB = text;
            this.RM.add(Integer.valueOf(49));
            return this;
        }

        public Builder setThumbnail(ItemScope thumbnail) {
            this.SC = (im) thumbnail;
            this.RM.add(Integer.valueOf(50));
            return this;
        }

        public Builder setThumbnailUrl(String thumbnailUrl) {
            this.SD = thumbnailUrl;
            this.RM.add(Integer.valueOf(51));
            return this;
        }

        public Builder setTickerSymbol(String tickerSymbol) {
            this.SE = tickerSymbol;
            this.RM.add(Integer.valueOf(52));
            return this;
        }

        public Builder setType(String type) {
            this.Oc = type;
            this.RM.add(Integer.valueOf(53));
            return this;
        }

        public Builder setUrl(String url) {
            this.pS = url;
            this.RM.add(Integer.valueOf(54));
            return this;
        }

        public Builder setWidth(String width) {
            this.SF = width;
            this.RM.add(Integer.valueOf(55));
            return this;
        }

        public Builder setWorstRating(String worstRating) {
            this.SG = worstRating;
            this.RM.add(Integer.valueOf(56));
            return this;
        }
    }

    ItemScope getAbout();

    List<String> getAdditionalName();

    ItemScope getAddress();

    String getAddressCountry();

    String getAddressLocality();

    String getAddressRegion();

    List<ItemScope> getAssociated_media();

    int getAttendeeCount();

    List<ItemScope> getAttendees();

    ItemScope getAudio();

    List<ItemScope> getAuthor();

    String getBestRating();

    String getBirthDate();

    ItemScope getByArtist();

    String getCaption();

    String getContentSize();

    String getContentUrl();

    List<ItemScope> getContributor();

    String getDateCreated();

    String getDateModified();

    String getDatePublished();

    String getDescription();

    String getDuration();

    String getEmbedUrl();

    String getEndDate();

    String getFamilyName();

    String getGender();

    ItemScope getGeo();

    String getGivenName();

    String getHeight();

    String getId();

    String getImage();

    ItemScope getInAlbum();

    double getLatitude();

    ItemScope getLocation();

    double getLongitude();

    String getName();

    ItemScope getPartOfTVSeries();

    List<ItemScope> getPerformers();

    String getPlayerType();

    String getPostOfficeBoxNumber();

    String getPostalCode();

    String getRatingValue();

    ItemScope getReviewRating();

    String getStartDate();

    String getStreetAddress();

    String getText();

    ItemScope getThumbnail();

    String getThumbnailUrl();

    String getTickerSymbol();

    String getType();

    String getUrl();

    String getWidth();

    String getWorstRating();

    boolean hasAbout();

    boolean hasAdditionalName();

    boolean hasAddress();

    boolean hasAddressCountry();

    boolean hasAddressLocality();

    boolean hasAddressRegion();

    boolean hasAssociated_media();

    boolean hasAttendeeCount();

    boolean hasAttendees();

    boolean hasAudio();

    boolean hasAuthor();

    boolean hasBestRating();

    boolean hasBirthDate();

    boolean hasByArtist();

    boolean hasCaption();

    boolean hasContentSize();

    boolean hasContentUrl();

    boolean hasContributor();

    boolean hasDateCreated();

    boolean hasDateModified();

    boolean hasDatePublished();

    boolean hasDescription();

    boolean hasDuration();

    boolean hasEmbedUrl();

    boolean hasEndDate();

    boolean hasFamilyName();

    boolean hasGender();

    boolean hasGeo();

    boolean hasGivenName();

    boolean hasHeight();

    boolean hasId();

    boolean hasImage();

    boolean hasInAlbum();

    boolean hasLatitude();

    boolean hasLocation();

    boolean hasLongitude();

    boolean hasName();

    boolean hasPartOfTVSeries();

    boolean hasPerformers();

    boolean hasPlayerType();

    boolean hasPostOfficeBoxNumber();

    boolean hasPostalCode();

    boolean hasRatingValue();

    boolean hasReviewRating();

    boolean hasStartDate();

    boolean hasStreetAddress();

    boolean hasText();

    boolean hasThumbnail();

    boolean hasThumbnailUrl();

    boolean hasTickerSymbol();

    boolean hasType();

    boolean hasUrl();

    boolean hasWidth();

    boolean hasWorstRating();
}
