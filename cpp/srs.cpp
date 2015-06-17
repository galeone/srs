#include <iostream>
#include <string>
#include <cstdlib>
#include <cwchar>
#include <clocale>
#include <SWI-cpp.h>

#define DEBUG

void _die() { exit(EXIT_FAILURE); }
#define die(format, ...) _die()

using namespace std;

wstring p_get(PlTerm t)
{
    wstring s(L"");

    switch(t.type())
    {
        case PL_VARIABLE:
        case PL_ATOM:
        case PL_INTEGER:
        case PL_FLOAT:
            s += wstring((wchar_t *)t);
            break;
        case PL_STRING:
            s += L"\"" + wstring((wchar_t *)t) + L"\"";
            break;
        case PL_TERM:
            {
                const char *name = t.name();
                size_t convertedChars = 0;
                wchar_t *wc_name = new wchar_t[sizeof name];
                mbstowcs(wc_name, name, sizeof name);
                s += wc_name;
                s += L"(";
                for(int n=1; n <= t.arity(); n++) {
                    if (n > 1) {
                        s += L", ";
                    }
                    s += p_get(t[n]);
                }
                s += L")";
                break;
            }
    }

    return s;
}

int main(int argc, char **argv) {
    setlocale(LC_ALL, "en_US.utf8");
    PlEngine engine(argc, argv);

#ifdef DEBUG
    wcout << "[+] prolog engine initizlized with success!" << endl;
#endif

    const int arity = 8;
    PlTermv termv(arity);
    PlQuery q("get_weigths",termv);

    wstring term_s[arity];
    // t1
    int64_t userID;
    //t2
    char tag[70];
    //t3..t7 // weigths -> to int64_t convert to float
    int64_t tagged_w, rated_positive_w, rated_negative_w, commented_w, searched_w;
    //t8, Timestamp
    int64_t timestamp;

    while(q.next_solution()) {
        //http://www.swi-prolog.org/pldoc/man?section=foreign-term-analysis
        for(size_t i = 0; i < arity; ++i) {
            term_s[i] = p_get(termv[i]);
#ifdef DEBUG
            wcout << term_s[i] << " ";
            if(i == arity - 1) wcout << "\n";
#endif
        }
    }

    PL_halt(EXIT_SUCCESS);
    return EXIT_SUCCESS;
}
