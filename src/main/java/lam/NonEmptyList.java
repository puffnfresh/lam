package lam;

import java.util.function.Function;

public abstract class NonEmptyList<A> {
    public abstract <B> B fold(final Function<A, B> singleton, final Function<A, Function<B, B>> cons);

    public static <A> NonEmptyList<A> singleton(final A head) {
        return new NonEmptyList<A>() {
            public <B> B fold(final Function<A, B> singleton, final Function<A, Function<B, B>> cons) {
                return singleton.apply(head);
            }
        };
    }

    public static <A> NonEmptyList<A> cons(final A head, final NonEmptyList<A> tail) {
        return new NonEmptyList<A>() {
            public <B> B fold(final Function<A, B> singleton, final Function<A, Function<B, B>> cons) {
                return cons.apply(head).apply(tail.fold(singleton, cons));
            }
        };
    }

    public NonEmptyList<A> concat(final NonEmptyList<A> rest) {
        return fold(a -> cons(a, rest), a -> b -> cons(a, b));
    }

    public <B> NonEmptyList<B> bind(final Function<A, NonEmptyList<B>> f) {
        return fold(f, a -> b -> f.apply(a).concat(b));
    }

    public static <A> NonEmptyList<A> point(final A a) {
        return singleton(a);
    }

    public <B> B foldMap1(final Semigroup<B> m, final Function<A, B> f) {
        return fold(f, a -> b -> m.append(f.apply(a), b));
    }
}
