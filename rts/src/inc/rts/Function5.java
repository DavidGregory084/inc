package inc.rts;

public interface Function5<A0, A1, A2, A3, A4, R> extends Function<R> {
    public R result();
    public void setArg0(A0 arg0);
    public void setArg1(A1 arg1);
    public void setArg2(A2 arg2);
    public void setArg3(A3 arg3);
    public void setArg4(A4 arg4);
    public void apply(Continuation c);
}
