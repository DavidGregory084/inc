package inc.rts;

public class Continuation {
    protected Continuation() {
    }

    public static final ThreadLocal<Continuation> instance =
        new ThreadLocal<Continuation>() {
            @Override protected Continuation initialValue() {
                return new Continuation();
            }
        };

    public Function continuation = null;
}
