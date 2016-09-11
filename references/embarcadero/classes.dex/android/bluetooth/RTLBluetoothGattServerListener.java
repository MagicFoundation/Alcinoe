package android.bluetooth;

public interface RTLBluetoothGattServerListener {
    void onCharacteristicReadRequest(BluetoothDevice bluetoothDevice, int i, int i2, BluetoothGattCharacteristic bluetoothGattCharacteristic);

    void onCharacteristicWriteRequest(BluetoothDevice bluetoothDevice, int i, BluetoothGattCharacteristic bluetoothGattCharacteristic, boolean z, boolean z2, int i2, byte[] bArr);

    void onConnectionStateChange(BluetoothDevice bluetoothDevice, int i, int i2);

    void onDescriptorReadRequest(BluetoothDevice bluetoothDevice, int i, int i2, BluetoothGattDescriptor bluetoothGattDescriptor);

    void onDescriptorWriteRequest(BluetoothDevice bluetoothDevice, int i, BluetoothGattDescriptor bluetoothGattDescriptor, boolean z, boolean z2, int i2, byte[] bArr);

    void onExecuteWrite(BluetoothDevice bluetoothDevice, int i, boolean z);

    void onServiceAdded(int i, BluetoothGattService bluetoothGattService);
}
