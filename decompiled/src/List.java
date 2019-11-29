package decompiled;

abstract class List<A> {
    public abstract boolean isEmpty();

    final static class Cons<A> extends List<A> {
        final A value;

        private Cons(final A a) {
            value = a;
        }

        public boolean isEmpty() {
            return false;
        }
    }

    final static class Nil<A> extends List<A> {
        private Nil() {
        }

        public boolean isEmpty() {
            return true;
        }
    }
}
