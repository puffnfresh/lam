package lam;

public abstract class Lens<A, B> {
    public abstract A set(final A a, final B b);
    public abstract B get(final A a);
}
