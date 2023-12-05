//----------------------------------------
// --- Day 2: Cube Conundrum ---
// https://adventofcode.com/2023/day/2
//----------------------------------------

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sstream>
#include <regex>

using namespace std;

//only 12 red cubes, 13 green cubes, and 14 blue cubes
bool checkString(const std::string &s)
{
    std::smatch m;

    if (std::regex_search(s, m, std::regex("[0-9]* red"))) {
        if (stoi(m[0]) > 12)
            return false;
    }

    if (std::regex_search(s, m, std::regex("[0-9]* green"))) {
        if (stoi(m[0]) > 13)
            return false;
    }

    if (std::regex_search(s, m, std::regex("[0-9]* blue"))) {
        if (stoi(m[0]) > 14)
            return false;
    }

    return true;
}

int main()
{
    std::ifstream ifs;
    ifs.open("day02.in");
    if (not ifs) {
        ifs.open("../day02.in");
    }

    std::string line;
    unsigned sum = 0;
    while (std::getline(ifs, line)) {
        int id = 0;
        char buf[1024];
        sscanf(line.c_str(), "Game %d: %[^\t\n]", &id, &buf);

        //std::cout << "Game " << id << "; "        << " " << buf << endl;

        std::string s(buf);
        std::string delimiter = "; ";

        size_t pos = 0;
        std::string token;
        bool found = false;
        while (((pos = s.find(delimiter)) != std::string::npos) and not found) {
            token = s.substr(0, pos);
            if (not checkString(token)) {
                found = true;
            }
            s.erase(0, pos + delimiter.length());
        }
        if (not found and not checkString(s)) {
            found = true;
        }

        if (not found) {
            sum += id;
        }
    }
    std::cout << sum << endl;
    return 0;
}