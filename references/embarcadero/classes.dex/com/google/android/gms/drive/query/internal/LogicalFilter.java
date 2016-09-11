package com.google.android.gms.drive.query.internal;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.drive.query.Filter;
import java.util.ArrayList;
import java.util.List;

public class LogicalFilter implements SafeParcelable, Filter {
    public static final Creator<LogicalFilter> CREATOR;
    private List<Filter> EN;
    final Operator EO;
    final List<FilterHolder> EY;
    final int wj;

    static {
        CREATOR = new f();
    }

    LogicalFilter(int versionCode, Operator operator, List<FilterHolder> filterHolders) {
        this.wj = versionCode;
        this.EO = operator;
        this.EY = filterHolders;
    }

    public LogicalFilter(Operator operator, Filter filter, Filter... additionalFilters) {
        this.wj = 1;
        this.EO = operator;
        this.EY = new ArrayList(additionalFilters.length + 1);
        this.EY.add(new FilterHolder(filter));
        this.EN = new ArrayList(additionalFilters.length + 1);
        this.EN.add(filter);
        for (Filter filter2 : additionalFilters) {
            this.EY.add(new FilterHolder(filter2));
            this.EN.add(filter2);
        }
    }

    public LogicalFilter(Operator operator, Iterable<Filter> filters) {
        this.wj = 1;
        this.EO = operator;
        this.EN = new ArrayList();
        this.EY = new ArrayList();
        for (Filter filter : filters) {
            this.EN.add(filter);
            this.EY.add(new FilterHolder(filter));
        }
    }

    public int describeContents() {
        return 0;
    }

    public void writeToParcel(Parcel out, int flags) {
        f.a(this, out, flags);
    }
}
