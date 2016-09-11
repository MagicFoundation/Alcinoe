package com.google.android.gms.drive.widget;

import android.content.Context;
import android.database.CursorIndexOutOfBoundsException;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;
import com.google.android.gms.common.data.DataBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class DataBufferAdapter<T> extends BaseAdapter {
    private final int Fj;
    private int Fk;
    private final int Fl;
    private final List<DataBuffer<T>> Fm;
    private final LayoutInflater Fn;
    private boolean Fo;
    private final Context mContext;

    public DataBufferAdapter(Context context, int resource) {
        this(context, resource, 0, new ArrayList());
    }

    public DataBufferAdapter(Context context, int resource, int textViewResourceId) {
        this(context, resource, textViewResourceId, new ArrayList());
    }

    public DataBufferAdapter(Context context, int resource, int textViewResourceId, List<DataBuffer<T>> objects) {
        this.Fo = true;
        this.mContext = context;
        this.Fk = resource;
        this.Fj = resource;
        this.Fl = textViewResourceId;
        this.Fm = objects;
        this.Fn = (LayoutInflater) context.getSystemService("layout_inflater");
    }

    public DataBufferAdapter(Context context, int resource, int textViewResourceId, DataBuffer<T>... buffers) {
        this(context, resource, textViewResourceId, Arrays.asList(buffers));
    }

    public DataBufferAdapter(Context context, int resource, List<DataBuffer<T>> objects) {
        this(context, resource, 0, (List) objects);
    }

    public DataBufferAdapter(Context context, int resource, DataBuffer<T>... buffers) {
        this(context, resource, 0, Arrays.asList(buffers));
    }

    private View a(int i, View view, ViewGroup viewGroup, int i2) {
        View inflate = view == null ? this.Fn.inflate(i2, viewGroup, false) : view;
        try {
            TextView textView = this.Fl == 0 ? (TextView) inflate : (TextView) inflate.findViewById(this.Fl);
            Object item = getItem(i);
            if (item instanceof CharSequence) {
                textView.setText((CharSequence) item);
            } else {
                textView.setText(item.toString());
            }
            return inflate;
        } catch (Throwable e) {
            Log.e("DataBufferAdapter", "You must supply a resource ID for a TextView");
            throw new IllegalStateException("DataBufferAdapter requires the resource ID to be a TextView", e);
        }
    }

    public void append(DataBuffer<T> buffer) {
        this.Fm.add(buffer);
        if (this.Fo) {
            notifyDataSetChanged();
        }
    }

    public void clear() {
        for (DataBuffer close : this.Fm) {
            close.close();
        }
        this.Fm.clear();
        if (this.Fo) {
            notifyDataSetChanged();
        }
    }

    public Context getContext() {
        return this.mContext;
    }

    public int getCount() {
        int i = 0;
        for (DataBuffer count : this.Fm) {
            i = count.getCount() + i;
        }
        return i;
    }

    public View getDropDownView(int position, View convertView, ViewGroup parent) {
        return a(position, convertView, parent, this.Fk);
    }

    public T getItem(int position) throws CursorIndexOutOfBoundsException {
        int i = position;
        for (DataBuffer dataBuffer : this.Fm) {
            int count = dataBuffer.getCount();
            if (count <= i) {
                i -= count;
            } else {
                try {
                    return dataBuffer.get(i);
                } catch (CursorIndexOutOfBoundsException e) {
                    throw new CursorIndexOutOfBoundsException(position, getCount());
                }
            }
        }
        throw new CursorIndexOutOfBoundsException(position, getCount());
    }

    public long getItemId(int position) {
        return (long) position;
    }

    public View getView(int position, View convertView, ViewGroup parent) {
        return a(position, convertView, parent, this.Fj);
    }

    public void notifyDataSetChanged() {
        super.notifyDataSetChanged();
        this.Fo = true;
    }

    public void setDropDownViewResource(int resource) {
        this.Fk = resource;
    }

    public void setNotifyOnChange(boolean notifyOnChange) {
        this.Fo = notifyOnChange;
    }
}
