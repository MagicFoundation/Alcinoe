package android.bluetooth;

import android.annotation.SuppressLint;

@SuppressLint({"NewApi"})
public class RTLBluetoothGattServerCallback extends BluetoothGattServerCallback {
    private RTLBluetoothGattServerListener mListener;

    public RTLBluetoothGattServerCallback(RTLBluetoothGattServerListener listener) {
        this.mListener = listener;
    }

    public void onCharacteristicReadRequest(BluetoothDevice device, int requestId, int offset, BluetoothGattCharacteristic characteristic) {
        this.mListener.onCharacteristicReadRequest(device, requestId, offset, characteristic);
    }

    public void onCharacteristicWriteRequest(BluetoothDevice device, int requestId, BluetoothGattCharacteristic characteristic, boolean preparedWrite, boolean responseNeeded, int offset, byte[] value) {
        this.mListener.onCharacteristicWriteRequest(device, requestId, characteristic, preparedWrite, responseNeeded, offset, value);
    }

    public void onConnectionStateChange(BluetoothDevice device, int status, int newState) {
        this.mListener.onConnectionStateChange(device, status, newState);
    }

    public void onDescriptorReadRequest(BluetoothDevice device, int requestId, int offset, BluetoothGattDescriptor descriptor) {
        this.mListener.onDescriptorReadRequest(device, requestId, offset, descriptor);
    }

    public void onDescriptorWriteRequest(BluetoothDevice device, int requestId, BluetoothGattDescriptor descriptor, boolean preparedWrite, boolean responseNeeded, int offset, byte[] value) {
        this.mListener.onDescriptorWriteRequest(device, requestId, descriptor, preparedWrite, responseNeeded, offset, value);
    }

    public void onExecuteWrite(BluetoothDevice device, int requestId, boolean execute) {
        this.mListener.onExecuteWrite(device, requestId, execute);
    }

    public void onServiceAdded(int status, BluetoothGattService service) {
        this.mListener.onServiceAdded(status, service);
    }
}
