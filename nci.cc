#include <iostream>
#include <fstream>

#include "parser.h"

int main(int argc, char* argv[])
{
    std::istream* input;
    std::ifstream file;

    if (argc < 2)
    {
        input = &std::cin;
    }
    else
    {
        file.open(argv[1]);
        input = &file;
    }

    std::string source(std::istreambuf_iterator<char>(*input), {});
    try
    {
        auto st = parse(source.c_str());
        std::cout << st;
    }
    catch (const Parser::Error& e)
    {
        std::cerr << e.what() << " " << e.location << "\n";
    }

    return 0;
}

