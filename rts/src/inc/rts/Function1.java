package inc.rts;

public interface Function1<A0, R> extends Function<R> {
    public R result();
    public void setArg0(A0 arg0);
    public void apply(Continuation c);
}
