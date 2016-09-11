package android.bluetooth;

public interface RTLBluetoothGattListener {
    void onCharacteristicChanged(BluetoothGatt bluetoothGatt, BluetoothGattCharacteristic bluetoothGattCharacteristic);

    void onCharacteristicRead(BluetoothGatt bluetoothGatt, BluetoothGattCharacteristic bluetoothGattCharacteristic, int i);

    void onCharacteristicWrite(BluetoothGatt bluetoothGatt, BluetoothGattCharacteristic bluetoothGattCharacteristic, int i);

    void onConnectionStateChange(BluetoothGatt bluetoothGatt, int i, int i2);

    void onDescriptorRead(BluetoothGatt bluetoothGatt, BluetoothGattDescriptor bluetoothGattDescriptor, int i);

    void onDescriptorWrite(BluetoothGatt bluetoothGatt, BluetoothGattDescriptor bluetoothGattDescriptor, int i);

    void onReadRemoteRssi(BluetoothGatt bluetoothGatt, int i, int i2);

    void onReliableWriteCompleted(BluetoothGatt bluetoothGatt, int i);

    void onServicesDiscovered(BluetoothGatt bluetoothGatt, int i);
}
