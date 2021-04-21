package lam;

import java.util.function.Function;

public abstract class Maybe<A> {
    public abstract <B> B fold(final B nothing, final Function<A, B> just);

    public static <A> Maybe<A> nothing() {
        return new Maybe<A>() {
            public <B> B fold(final B nothing, final Function<A, B> just) {
                return nothing;
            }
        };
    }

    public static <A> Maybe<A> just(final A a) {
        return new Maybe<A>() {
            public <B> B fold(final B nothing, final Function<A, B> just) {
                return just.apply(a);
            }
        };
    }

    public <B> Maybe<B> bind(final Function<A, Maybe<B>> f) {
        return fold(nothing(), f);
    }

    public static <A> Maybe<A> point(final A a) {
        return just(a);
    }
}
