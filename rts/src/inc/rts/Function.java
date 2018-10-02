package inc.rts;

public interface Function<R> {
    R result();
    void apply(Continuation c);
}
