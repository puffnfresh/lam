package lam;

public abstract class Iso<A, B> {
    public abstract A from(final B b);
    public abstract B to(final A a);

    public Lens<A, B> asLens() {
        return new Lens<A, B>() {
            public A set(final A a, final B b) {
                return from(b);
            }

            public B get(final A a) {
                return to(a);
            }
        };
    }

    public Prism<A, B> asPrism() {
        return new Prism<A, B>() {
            public Maybe<B> view(final A a) {
                return Maybe.just(to(a));
            }

            public A review(final B b) {
                return from(b);
            }
        };
    }
}
