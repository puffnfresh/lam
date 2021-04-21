package lam.sam;

@FunctionalInterface
public interface ExceptionSupplier<A> {
    public A unsafeGet() throws Exception;
}
