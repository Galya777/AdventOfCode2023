#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <cctype>

std::vector<std::string> readCalibrationDocumentFromFile(const std::string& filename) {
    std::vector<std::string> calibrationDocument;
    std::ifstream file(filename);

    if (file.is_open()) {
        std::string line;
        while (std::getline(file, line)) {
            calibrationDocument.push_back(line);
        }
        file.close();
    } else {
        std::cerr << "Unable to open file: " << filename << std::endl;
    }

    return calibrationDocument;
}

int sumCalibrationValues(const std::vector<std::string>& calibrationDocument) {
    int totalSum = 0;

    for (const std::string& line : calibrationDocument) {
        // Find the first and last digits of each line
        char firstDigit = '\0', lastDigit = '\0';

        for (char c : line) {
            if (isdigit(c)) {
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
int main() {
    // Example usage
    std::vector<std::string> calibrationDocument = readCalibrationDocumentFromFile("/home/galya777/IdeaProjects/AdventOfCode2023/Day1_input.txt");

    int result = sumCalibrationValues(calibrationDocument);
    std::cout << result << std::endl;

    return 0;
}
