package decompiled;

import java.util.Random;
import java.util.function.Function;

class Test {
    public static Function<Integer, Function<Integer, Integer>> add1 = i -> j -> Integer.valueOf(i.intValue() + j);
    public static final int two = add1.apply(1).apply(2);
}
