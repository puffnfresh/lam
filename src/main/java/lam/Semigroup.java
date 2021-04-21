package lam;

public abstract class Semigroup<A> {
    public abstract A append(final A a, final A b);

    public static <A> Semigroup<A> first() {
        return new Semigroup<A>() {
            public A append(final A a, final A b) {
                return a;
            }
        };
    }
}
