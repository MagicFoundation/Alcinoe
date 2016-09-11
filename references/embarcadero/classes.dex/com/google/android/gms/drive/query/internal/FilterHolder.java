package com.google.android.gms.drive.query.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.query.Filter;

public class FilterHolder implements SafeParcelable {
    public static final Creator<FilterHolder> CREATOR;
    final ComparisonFilter<?> ER;
    final FieldOnlyFilter ES;
    final LogicalFilter ET;
    final NotFilter EU;
    final InFilter<?> EV;
    private final Filter EW;
    final int wj;

    static {
        CREATOR = new c();
    }

    FilterHolder(int versionCode, ComparisonFilter<?> comparisonField, FieldOnlyFilter fieldOnlyFilter, LogicalFilter logicalFilter, NotFilter notFilter, InFilter<?> containsFilter) {
        this.wj = versionCode;
        this.ER = comparisonField;
        this.ES = fieldOnlyFilter;
        this.ET = logicalFilter;
        this.EU = notFilter;
        this.EV = containsFilter;
        if (this.ER != null) {
            this.EW = this.ER;
        } else if (this.ES != null) {
            this.EW = this.ES;
        } else if (this.ET != null) {
            this.EW = this.ET;
        } else if (this.EU != null) {
            this.EW = this.EU;
        } else if (this.EV != null) {
            this.EW = this.EV;
        } else {
            throw new IllegalArgumentException("At least one filter must be set.");
        }
    }

    public FilterHolder(Filter filter) {
        this.wj = 1;
        this.ER = filter instanceof ComparisonFilter ? (ComparisonFilter) filter : null;
        this.ES = filter instanceof FieldOnlyFilter ? (FieldOnlyFilter) filter : null;
        this.ET = filter instanceof LogicalFilter ? (LogicalFilter) filter : null;
        this.EU = filter instanceof NotFilter ? (NotFilter) filter : null;
        this.EV = filter instanceof InFilter ? (InFilter) filter : null;
        if (this.ER == null && this.ES == null && this.ET == null && this.EU == null && this.EV == null) {
            throw new IllegalArgumentException("Invalid filter type or null filter.");
        }
        this.EW = filter;
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel out, int flags) {
        c.a(this, out, flags);
    }
}
