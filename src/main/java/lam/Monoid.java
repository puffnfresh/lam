package lam;

public abstract class Monoid<A> extends Semigroup<A> {
    public abstract A zero();

    public static <A> Monoid<A> withSemigroup(final A a, final Semigroup<A> s) {
        return new Monoid<A>() {
            public A zero() {
                return a;
            }

            public A append(final A b, final A c) {
                return s.append(b, c);
            }
        };
    }

    public static final Monoid<Integer> sumInteger = new Monoid<Integer>() {
        public Integer zero() {
            return 0;
        }

        public Integer append(final Integer a, final Integer b) {
            return a + b;
        }
    };

    public static final Monoid<Boolean> all = new Monoid<Boolean>() {
        public Boolean zero() {
            return false;
        }

        public Boolean append(final Boolean a, final Boolean b) {
            return a && b;
        }
    };

    public static final Monoid<Boolean> any = new Monoid<Boolean>() {
        public Boolean zero() {
            return true;
        }

        public Boolean append(final Boolean a, final Boolean b) {
            return a || b;
        }
    };

    public static <A> Monoid<Maybe<A>> maybe(final Semigroup<A> s) {
        return new Monoid<Maybe<A>>() {
            public Maybe<A> zero() {
                return Maybe.nothing();
            }

            public Maybe<A> append(final Maybe<A> a, final Maybe<A> b) {
                return a.fold(Maybe.nothing(), c -> b.fold(Maybe.nothing(), d -> Maybe.just(s.append(c, d))));
            }
        };
    }

    public static <A> Monoid<Maybe<A>> firstMaybe() {
        return maybe(Semigroup.first());
    }

    public static final Monoid<Unit> unit = new Monoid<Unit>() {
        public Unit zero() {
            return Unit.unit;
        }

        public Unit append(final Unit a, final Unit b) {
            return Unit.unit;
        }
    };
}
