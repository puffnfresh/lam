package lam;

public abstract class Prism<A, B> {
    public abstract Maybe<B> view(final A a);
    public abstract A review(final B b);
}
