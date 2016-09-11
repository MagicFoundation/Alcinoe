package com.google.android.gms.drive.query;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.query.internal.LogicalFilter;
import com.google.android.gms.drive.query.internal.Operator;
import java.util.ArrayList;
import java.util.List;

public class Query implements SafeParcelable {
    public static final Creator<Query> CREATOR;
    LogicalFilter EL;
    String EM;
    final int wj;

    public static class Builder {
        private String EM;
        private final List<Filter> EN;

        public Builder() {
            this.EN = new ArrayList();
        }

        public Builder addFilter(Filter filter) {
            this.EN.add(filter);
            return this;
        }

        public Query build() {
            return new Query(new LogicalFilter(Operator.Ff, this.EN), this.EM);
        }

        public Builder setPageToken(String token) {
            this.EM = token;
            return this;
        }
    }

    static {
        CREATOR = new a();
    }

    Query(int versionCode, LogicalFilter clause, String pageToken) {
        this.wj = versionCode;
        this.EL = clause;
        this.EM = pageToken;
    }

    Query(LogicalFilter clause, String pageToken) {
        this(1, clause, pageToken);
    }

    public int describeContents() {
        return 0;
    }

    public Filter getFilter() {
        return this.EL;
    }

    public String getPageToken() {
        return this.EM;
    }

    public void writeToParcel(Parcel out, int flags) {
        a.a(this, out, flags);
    }
}
