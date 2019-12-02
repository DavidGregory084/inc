package inc.rts;

@FunctionalInterface
public interface Function2<A0, A1, R> extends Function<R> {
    public R apply(A0 arg0, A1 arg1);
}
