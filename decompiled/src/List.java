package decompiled;

class Collection {
    static abstract class List<A> {
        static final class Cons<A> extends List<A> {
            final A a;

            private Cons(final A a) {
                this.a = a;
            }
        }

        static final class Nil<A> extends List<A> {
            private Nil() {
            }
        }
    }
}
