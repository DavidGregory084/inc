package decompiled;

import java.util.Random;

class Integer {
    public static int integer() {
        return 54388;
    }

    public static final int integer1 = ternary();

    public static final int add1(int i) {
        return i + 1;
    }

    public static int ternary() {
        return new Random().nextBoolean() ? 1 : 0;
    }
}
