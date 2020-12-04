package days.day4;

import java.util.HashSet;
import java.util.Set;

public class Passport {
    private Integer birthYear;
    private Integer issueYear;
    private Integer expirationYear;
    private String height;
    private String hairColor;
    private String eyeColor;
    private String passportID;
    private Integer countryID;

    private static Set<String> validColors;

    static {
        validColors = new HashSet<>();
        validColors.add("amb");
        validColors.add("blu");
        validColors.add("brn");
        validColors.add("gry");
        validColors.add("grn");
        validColors.add("hzl");
        validColors.add("oth");
    }

    public void setBirthYear(int birthYear) {
        this.birthYear = birthYear;
    }

    public void setIssueYear(int issueYear) {
        this.issueYear = issueYear;
    }

    public void setExpirationYear(int expirationYear) {
        this.expirationYear = expirationYear;
    }

    public void setHeight(String height) {
        this.height = height;
    }

    public void setHairColor(String hairColor) {
        this.hairColor = hairColor;
    }

    public void setEyeColor(String eyeColor) {
        this.eyeColor = eyeColor;
    }

    public void setPassportID(String passportID) {
        this.passportID = passportID;
    }

    public void setCountryID(int countryID) {
        this.countryID = countryID;
    }

    public boolean allFieldsPresent() {
        return birthYear != null && issueYear != null && expirationYear != null && height != null && hairColor != null
                && eyeColor != null && passportID != null;
    }

    private boolean isBirthYearValid() {
        return birthYear >= 1920 && birthYear <= 2002;
    }

    private boolean isIssueYearValid() {
        return issueYear >= 2010 && issueYear <= 2020;
    }

    private boolean isExpirationYearValid() {
        return expirationYear >= 2020 && expirationYear <= 2030;
    }

    private boolean isHeightValid() {
        if (!(height.endsWith("cm") || height.endsWith("in"))) {
            return false;
        }
        int value = Integer.parseInt(height.substring(0, height.length() - 2));
        if (height.endsWith("cm")) {
            return value >= 150 && value <= 193;
        } else {
            return value >= 59 && value <= 76;
        }
    }

    private boolean isHairColorValid() {
        if (!hairColor.startsWith("#")) {
            return false;
        }
        if (hairColor.length() != 7) {
            return false;
        }
        for (char c : hairColor.substring(1).toCharArray()) {
            if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))) {
                return false;
            }
        }
        return true;
    }

    private boolean isEyeColorValid() {
        return validColors.contains(eyeColor);
    }

    private boolean isPassportIDValid() {
        if (passportID.length() != 9) {
            return false;
        }
        for (char c : passportID.toCharArray()) {
            if (!Character.isDigit(c)) {
                return false;
            }
        }
        return true;
    }

    public boolean isValid() {
        if (!allFieldsPresent()) {
            return false;
        }
        if (!isBirthYearValid()) {
            return false;
        }
        if (!isIssueYearValid()) {
            return false;
        }
        if (!isExpirationYearValid()) {
            return false;
        }
        if (!isHeightValid()) {
            return false;
        }
        if (!isHairColorValid()) {
            return false;
        }
        if (!isEyeColorValid()) {
            return false;
        }
        if (!isPassportIDValid()) {
            return false;
        }
        return true;
    }
}
