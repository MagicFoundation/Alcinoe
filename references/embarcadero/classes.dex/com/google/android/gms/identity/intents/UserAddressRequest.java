package com.google.android.gms.identity.intents;

import android.os.Parcel;
import android.os.Parcelable.Creator;
import com.google.android.gms.common.internal.safeparcel.SafeParcelable;
import com.google.android.gms.identity.intents.model.CountrySpecification;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public final class UserAddressRequest implements SafeParcelable {
    public static final Creator<UserAddressRequest> CREATOR;
    List<CountrySpecification> Ky;
    private final int wj;

    public final class Builder {
        final /* synthetic */ UserAddressRequest Kz;

        private Builder(UserAddressRequest userAddressRequest) {
            this.Kz = userAddressRequest;
        }

        public Builder addAllowedCountrySpecification(CountrySpecification countrySpecification) {
            if (this.Kz.Ky == null) {
                this.Kz.Ky = new ArrayList();
            }
            this.Kz.Ky.add(countrySpecification);
            return this;
        }

        public Builder addAllowedCountrySpecifications(Collection<CountrySpecification> countrySpecifications) {
            if (this.Kz.Ky == null) {
                this.Kz.Ky = new ArrayList();
            }
            this.Kz.Ky.addAll(countrySpecifications);
            return this;
        }

        public UserAddressRequest build() {
            if (this.Kz.Ky != null) {
                this.Kz.Ky = Collections.unmodifiableList(this.Kz.Ky);
            }
            return this.Kz;
        }
    }

    static {
        CREATOR = new a();
    }

    UserAddressRequest() {
        this.wj = 1;
    }

    UserAddressRequest(int versionCode, List<CountrySpecification> allowedCountrySpecifications) {
        this.wj = versionCode;
        this.Ky = allowedCountrySpecifications;
    }

    public static Builder newBuilder() {
        UserAddressRequest userAddressRequest = new UserAddressRequest();
        userAddressRequest.getClass();
        return new Builder(null);
    }

    public int describeContents() {
        return 0;
    }

    public int getVersionCode() {
        return this.wj;
    }

    public void writeToParcel(Parcel out, int flags) {
        a.a(this, out, flags);
    }
}
