package lam;

import java.util.function.Function;

public abstract class List<A> {
    public abstract <B> B fold(final B nil, final Function<A, Function<B, B>> cons);

    public static <A> List<A> nil() {
        return new List<A>() {
            public <B> B fold(final B nil, final Function<A, Function<B, B>> cons) {
                return nil;
            }
        };
    }

    public static <A> List<A> cons(final A head, final List<A> tail) {
        return new List<A>() {
            public <B> B fold(final B nil, final Function<A, Function<B, B>> cons) {
                return cons.apply(head).apply(tail.fold(nil, cons));
            }
        };
    }

    public List<A> concat(final List<A> rest) {
        return fold(rest, a -> b -> cons(a, b));
    }

    public <B> List<B> bind(final Function<A, List<B>> f) {
        return fold(nil(), a -> b -> f.apply(a).concat(b));
    }

    public static <A> List<A> point(final A a) {
        return cons(a, nil());
    }

    public <B> B foldMap(final Monoid<B> m, final Function<A, B> f) {
        return fold(m.zero(), a -> b -> m.append(f.apply(a), b));
    }
}
