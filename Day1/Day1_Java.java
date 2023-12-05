package Day1;

import java.util.List;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Day1_Java {
    public static List<String> readCalibrationDocumentFromFile(String filename) throws IOException {
        Path path = Paths.get(filename);
        return Files.readAllLines(path);
    }

    public static int sumCalibrationValues(List<String> calibrationDocument) {
        int totalSum = 0;

        for (String line : calibrationDocument) {
            // Find the first and last digits of each line
            char firstDigit = '\0', lastDigit = '\0';

            for (char c : line.toCharArray()) {
                if (Character.isDigit(c)) {
                    if (firstDigit == '\0') {
                        firstDigit = c;
                    }
                    lastDigit = c;
                }
            }

            // Combine the first and last digits to form a two-digit number
            int calibrationValue = (firstDigit - '0') * 10 + (lastDigit - '0');

            // Add the calibration value to the total sum
            totalSum += calibrationValue;
        }

        return totalSum;
    }

    public static void main(String[] args) {
        // Example usage
        try {
            List<String> calibrationDocument = readCalibrationDocumentFromFile("/home/galya777/IdeaProjects/AdventOfCode2023/Day1_input.txt");

            int result = sumCalibrationValues(calibrationDocument);
            System.out.println(result);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
