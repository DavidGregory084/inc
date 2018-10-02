package inc.rts;

public interface Function2<A0, A1, R> extends Function<R> {
    public R result();
    public void setArg0(A0 arg0);
    public void setArg1(A1 arg1);
    public void apply(Continuation c);
}
