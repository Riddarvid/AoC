package days.day13;

import java.math.BigInteger;

public class CrtTerm {
    private final BigInteger remainder;
    private final BigInteger modulus;

    public CrtTerm(int remainder, int modulus) {
        this.remainder = BigInteger.valueOf(remainder);
        this.modulus = BigInteger.valueOf(modulus);
    }

    public BigInteger getRemainder() {
        return remainder;
    }

    public BigInteger getModulus() {
        return modulus;
    }
}
