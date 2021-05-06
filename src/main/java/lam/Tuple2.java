package lam;

import java.util.function.Function;

public abstract class Tuple2<A, B> {
    public abstract <C> C fold(final Function<A, Function<B, C>> f);

    public static <A, B> Tuple2<A, B> tuple2(final A _1, final B _2) {
        return new Tuple2<A, B>() {
            public final <C> C fold(final Function<A, Function<B, C>> f) {
                return f.apply(_1).apply(_2);
            }
        };
    }

    public static <A, B> Lens<Tuple2<A, B>, A> _1() {
        return new Lens<Tuple2<A, B>, A>() {
            public Tuple2<A, B> set(final Tuple2<A, B> s, final A t) {
                return s.fold(a -> b -> tuple2(t, b));
            }
            public A get(final Tuple2<A, B> s) {
                return s.fold(a -> b -> a);
            }
        };
    }

    public static <A, B> Lens<Tuple2<A, B>, B> _2() {
        return new Lens<Tuple2<A, B>, B>() {
            public Tuple2<A, B> set(final Tuple2<A, B> s, final B t) {
                return s.fold(a -> b -> tuple2(a, t));
            }
            public B get(final Tuple2<A, B> s) {
                return s.fold(a -> b -> b);
            }
        };
    }
}
