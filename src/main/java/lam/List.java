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

    public static <A> Prism<List<A>, Tuple2<A, List<A>>> tuple2() {
        return new Prism<List<A>, Tuple2<A, List<A>>>() {
            public Maybe<Tuple2<A, List<A>>> view(final List<A> s) {
                return s.fold(
                    Maybe.nothing(),
                    a -> b ->
                        b.map((final Tuple2<A, List<A>> u) ->
                            Tuple2.tuple2(a, cons(Tuple2.<A, List<A>>_1().get(u), Tuple2.<A, List<A>>_2().get(u)))
                        ).orElse(
                            Maybe.just(Tuple2.tuple2(a, List.nil()))
                        )
                );
            }
            public List<A> review(final Tuple2<A, List<A>> t) {
                return cons(Tuple2.<A, List<A>>_1().get(t), Tuple2.<A, List<A>>_2().get(t));
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
