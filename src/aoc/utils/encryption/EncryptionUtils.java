package aoc.utils.encryption;

public class EncryptionUtils {
    public static String caesarDecrypt(String toDecrypt, int shift) {
        StringBuilder sb = new StringBuilder();
        for (char c : toDecrypt.toCharArray()) {
            if (Character.isAlphabetic(c)) {
                c += shift;
                if (c > 'z') {
                    c -= 26;
                }
            }
            sb.append(c);
        }
        return sb.toString();
    }

    public static String caesarEncrypt(String toEncrypt, int shift) {
        StringBuilder sb = new StringBuilder();
        for (char c : toEncrypt.toCharArray()) {
            if (Character.isAlphabetic(c)) {
                c -= shift;
                if (c < 'a') {
                    c += 26;
                }
            }
            sb.append(c);
        }
        return sb.toString();
    }
}
