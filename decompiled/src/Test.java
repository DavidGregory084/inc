package decompiled;

import java.util.Random;
import java.util.function.Function;

class Test {
    public static Integer add1(Integer x) {
        return x.intValue() + 1;
    }

    public static Function<Integer, Integer> fn = Test::add1;
}
