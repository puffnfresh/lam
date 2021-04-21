package lam;

import java.util.function.Function;

public class Lambda<A, B> {
    public final Function<A, B> f;

    public Lambda(final Function<A, B> f) {
        this.f = f;
    }

    public static <A, B> Lambda<A, B> λ(final Function<A, B> f) {
        return new Lambda<>(f);
    }

    public B $(final A a) {
        return f.apply(a);
    }

    public <C> Lambda<A, Lambda<A, C>> on(final Lambda<B, Lambda<B, C>> l) {
        return λ(a -> λ(b -> l.$(f.apply(a)).$(f.apply(b))));
    }

    public static <A, B> Lambda<A, B> constant(final B b) {
        return λ(a -> b);
    }

    public static <A, B, C> Lambda<B, Lambda<A, C>> flip(final Lambda<A, Lambda<B, C>> lambda) {
        return λ(b -> λ(a -> lambda.$(a).$(b)));
    }
}
