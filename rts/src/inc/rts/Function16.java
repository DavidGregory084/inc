package inc.rts;

public interface Function16<A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, R> extends Function<R> {
    public R result();
    public void setArg0(A0 arg0);
    public void setArg1(A1 arg1);
    public void setArg2(A2 arg2);
    public void setArg3(A3 arg3);
    public void setArg4(A4 arg4);
    public void setArg5(A5 arg5);
    public void setArg6(A6 arg6);
    public void setArg7(A7 arg7);
    public void setArg8(A8 arg8);
    public void setArg9(A9 arg9);
    public void setArg10(A10 arg10);
    public void setArg11(A11 arg11);
    public void setArg12(A12 arg12);
    public void setArg13(A13 arg13);
    public void setArg14(A14 arg14);
    public void setArg15(A15 arg15);
    public void apply(Continuation c);
}
