package com.google.android.gms.drive.query;

import com.google.android.gms.drive.metadata.CollectionMetadataField;
import com.google.android.gms.drive.metadata.MetadataField;
import com.google.android.gms.drive.metadata.OrderedMetadataField;
import com.google.android.gms.drive.query.internal.ComparisonFilter;
import com.google.android.gms.drive.query.internal.FieldOnlyFilter;
import com.google.android.gms.drive.query.internal.InFilter;
import com.google.android.gms.drive.query.internal.LogicalFilter;
import com.google.android.gms.drive.query.internal.NotFilter;
import com.google.android.gms.drive.query.internal.Operator;

public class Filters {
    public static Filter and(Filter filter, Filter... additionalFilters) {
        return new LogicalFilter(Operator.Ff, filter, additionalFilters);
    }

    public static Filter and(Iterable<Filter> filters) {
        return new LogicalFilter(Operator.Ff, filters);
    }

    public static Filter contains(MetadataField<String> field, String value) {
        return new ComparisonFilter(Operator.Fi, (MetadataField) field, (Object) value);
    }

    public static <T> Filter eq(MetadataField<T> field, T value) {
        return new ComparisonFilter(Operator.Fa, (MetadataField) field, (Object) value);
    }

    public static <T extends Comparable<T>> Filter greaterThan(OrderedMetadataField<T> field, T value) {
        return new ComparisonFilter(Operator.Fd, (MetadataField) field, (Object) value);
    }

    public static <T extends Comparable<T>> Filter greaterThanEquals(OrderedMetadataField<T> field, T value) {
        return new ComparisonFilter(Operator.Fe, (MetadataField) field, (Object) value);
    }

    public static <T> Filter in(CollectionMetadataField<T> field, T value) {
        return new InFilter((CollectionMetadataField) field, (Object) value);
    }

    public static <T extends Comparable<T>> Filter lessThan(OrderedMetadataField<T> field, T value) {
        return new ComparisonFilter(Operator.Fb, (MetadataField) field, (Object) value);
    }

    public static <T extends Comparable<T>> Filter lessThanEquals(OrderedMetadataField<T> field, T value) {
        return new ComparisonFilter(Operator.Fc, (MetadataField) field, (Object) value);
    }

    public static Filter not(Filter toNegate) {
        return new NotFilter(toNegate);
    }

    public static Filter or(Filter filter, Filter... additionalFilters) {
        return new LogicalFilter(Operator.Fg, filter, additionalFilters);
    }

    public static Filter or(Iterable<Filter> filters) {
        return new LogicalFilter(Operator.Fg, filters);
    }

    public static Filter sharedWithMe() {
        return new FieldOnlyFilter(SearchableField.EH);
    }
}
