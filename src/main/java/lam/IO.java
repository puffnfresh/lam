package lam;

import java.util.function.Function;
import lam.sam.ExceptionSupplier;

public class IO<A> {
    private final ExceptionSupplier<A> underlying;

    public IO(final ExceptionSupplier<A> supplier) {
        underlying = supplier;
    }

    public A unsafePerform() throws Exception {
        return underlying.unsafeGet();
    }

    public A unsafePerformThrow() {
        try {
            return underlying.unsafeGet();
        } catch(final Exception e) {
            throw new RuntimeException(e);
        }
    }

    public IO<Either<Throwable, A>> catchNonFatal() {
        return new IO<>(() -> {
                try {
                    return Either.right(underlying.unsafeGet());
                } catch(final Throwable t) {
                    if(isFatal(t)) {
                        throw t;
                    } else {
                        return Either.left(t);
                    }
                }
        });
    }

    private boolean isFatal(final Throwable t) {
        return
            (t instanceof VirtualMachineError) ||
            (t instanceof ThreadDeath) ||
            (t instanceof InterruptedException) ||
            (t instanceof LinkageError);
    }

    public <B> IO<B> bind(final Function<A, IO<B>> f) {
        return null;
    }

    public static <A> IO<A> point(final A a) {
        return new IO<>(() -> a);
    }
}
