package inc.rts;

@FunctionalInterface
public interface Function0<R> extends Function<R> {
    public R apply();
}
