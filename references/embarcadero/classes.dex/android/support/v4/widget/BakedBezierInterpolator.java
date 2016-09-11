package android.support.v4.widget;

import android.view.animation.Interpolator;

final class BakedBezierInterpolator implements Interpolator {
    private static final BakedBezierInterpolator INSTANCE;
    private static final float STEP_SIZE;
    private static final float[] VALUES;

    static {
        INSTANCE = new BakedBezierInterpolator();
        VALUES = new float[]{0.0f, 2.0E-4f, 9.0E-4f, 0.0019f, 0.0036f, 0.0059f, 0.0086f, 0.0119f, 0.0157f, 0.0209f, 0.0257f, 0.0321f, 0.0392f, 0.0469f, 0.0566f, 0.0656f, 0.0768f, 0.0887f, 0.1033f, 0.1186f, 0.1349f, 0.1519f, 0.1696f, 0.1928f, 0.2121f, 0.237f, 0.2627f, 0.2892f, 0.3109f, 0.3386f, 0.3667f, 0.3952f, 0.4241f, 0.4474f, 0.4766f, 0.5f, 0.5234f, 0.5468f, 0.5701f, 0.5933f, 0.6134f, 0.6333f, 0.6531f, 0.6698f, 0.6891f, 0.7054f, 0.7214f, 0.7346f, 0.7502f, 0.763f, 0.7756f, 0.7879f, 0.8f, 0.8107f, 0.8212f, 0.8326f, 0.8415f, 0.8503f, 0.8588f, 0.8672f, 0.8754f, 0.8833f, 0.8911f, 0.8977f, 0.9041f, 0.9113f, 0.9165f, 0.9232f, 0.9281f, 0.9328f, 0.9382f, 0.9434f, 0.9476f, 0.9518f, 0.9557f, 0.9596f, 0.9632f, 0.9662f, 0.9695f, 0.9722f, 0.9753f, 0.9777f, 0.9805f, 0.9826f, 0.9847f, 0.9866f, 0.9884f, 0.9901f, 0.9917f, 0.9931f, 0.9944f, 0.9955f, 0.9964f, 0.9973f, 0.9981f, 0.9986f, 0.9992f, 0.9995f, 0.9998f, 1.0f, 1.0f};
        STEP_SIZE = 1.0f / ((float) (VALUES.length - 1));
    }

    public static final BakedBezierInterpolator getInstance() {
        return INSTANCE;
    }

    private BakedBezierInterpolator() {
    }

    public float getInterpolation(float input) {
        if (input >= 1.0f) {
            return 1.0f;
        }
        if (input <= 0.0f) {
            return 0.0f;
        }
        int position = Math.min((int) (((float) (VALUES.length - 1)) * input), VALUES.length - 2);
        return VALUES[position] + ((VALUES[position + 1] - VALUES[position]) * ((input - (((float) position) * STEP_SIZE)) / STEP_SIZE));
    }
}
