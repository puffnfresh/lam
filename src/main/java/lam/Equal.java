package lam;

import java.util.function.BiFunction;

public class Equal<A> {
    private final BiFunction<A, A, Boolean> f;

    public Equal(final BiFunction<A, A, Boolean> f) {
        this.f = f;
    }

    public boolean isEqual(final A a, final A b) {
        return f.apply(a, b);
    }

    public static Equal<Unit> unitEqual = new Equal<>((a, b) -> true);
}
