package lam;

import java.util.function.Function;

public abstract class Either<A, B> {
    public abstract <C> C fold(final Function<A, C> left, final Function<B, C> right);

    public static <A, B> Either<A, B> left(final A a) {
        return new Either<A, B>() {
            public <C> C fold(final Function<A, C> left, final Function<B, C> right) {
                return left.apply(a);
            }
        };
    }

    public static <A, B> Either<A, B> right(final B b) {
        return new Either<A, B>() {
            public <C> C fold(final Function<A, C> left, final Function<B, C> right) {
                return right.apply(b);
            }
        };
    }

    public Either<B, A> swap() {
        return fold(Either::right, Either::left);
    }

    public boolean isLeft() {
        return fold(a -> true, b -> false);
    }

    public boolean isRight() {
        return !isLeft();
    }

    public <C> Either<A, C> bind(final Function<B, Either<A, C>> f) {
        return fold(Either::left, f);
    }

    public static <A, B> Either<A, B> point(final B b) {
        return right(b);
    }

    public <C, D> Either<C, D> bimap(final Function<A, C> ac, final Function<B, D> bd) {
        return fold(ac.andThen(Either::left), bd.andThen(Either::right));
    }

    public B folded(final Monoid<B> m) {
        return fold(a -> m.zero(), a -> a);
    }

    public <C> C foldMap(final Monoid<C> m, final Function<B, C> f) {
        return fold(a -> m.zero(), f);
    }
}
