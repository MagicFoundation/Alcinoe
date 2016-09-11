package android.bluetooth;

import android.annotation.SuppressLint;

@SuppressLint({"NewApi"})
public class RTLBluetoothGattCallback extends BluetoothGattCallback {
    private RTLBluetoothGattListener mListener;

    public RTLBluetoothGattCallback(RTLBluetoothGattListener listener) {
        this.mListener = listener;
    }

    public void onCharacteristicChanged(BluetoothGatt gatt, BluetoothGattCharacteristic characteristic) {
        this.mListener.onCharacteristicChanged(gatt, characteristic);
    }

    public void onCharacteristicRead(BluetoothGatt gatt, BluetoothGattCharacteristic characteristic, int status) {
        this.mListener.onCharacteristicRead(gatt, characteristic, status);
    }

    public void onCharacteristicWrite(BluetoothGatt gatt, BluetoothGattCharacteristic characteristic, int status) {
        this.mListener.onCharacteristicWrite(gatt, characteristic, status);
    }

    public void onConnectionStateChange(BluetoothGatt gatt, int status, int newState) {
        this.mListener.onConnectionStateChange(gatt, status, newState);
    }

    public void onDescriptorRead(BluetoothGatt gatt, BluetoothGattDescriptor descriptor, int status) {
        this.mListener.onDescriptorRead(gatt, descriptor, status);
    }

    public void onDescriptorWrite(BluetoothGatt gatt, BluetoothGattDescriptor descriptor, int status) {
        this.mListener.onDescriptorWrite(gatt, descriptor, status);
    }

    public void onReadRemoteRssi(BluetoothGatt gatt, int rssi, int status) {
        this.mListener.onReadRemoteRssi(gatt, rssi, status);
    }

    public void onReliableWriteCompleted(BluetoothGatt gatt, int status) {
        this.mListener.onReliableWriteCompleted(gatt, status);
    }

    public void onServicesDiscovered(BluetoothGatt gatt, int status) {
        this.mListener.onServicesDiscovered(gatt, status);
    }
}
