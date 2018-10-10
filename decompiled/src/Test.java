package decompiled;

import java.util.Random;
import java.util.function.Function;

class Test {
    public static Function<Integer, Function<Integer, Function<Integer, Integer>>> add1 = i -> j -> k -> Integer.valueOf(i.intValue() + j.intValue() + k.intValue());
    public static final int six = add1.apply(1).apply(2).apply(3);
}
