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
                size_t len = strlen(name) + 1;
                wchar_t *wc_name = new wchar_t[len]();
                mbstowcs(wc_name, name, len);
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
    wcout << L"[+] prolog engine initizlized with success!" << endl;
#endif

    const int arity = 8;
    PlTermv termv(arity);
    PlQuery q("get_weigths",termv);

    // t1
    int64_t user;
    //t2
    wstring tag;
    //t3..t7 // weigths -> to int64_t convert to float
    double tagged_w, rated_positive_w, rated_negative_w, commented_w, searched_w;
    //t8, Timestamp
    int64_t timestamp, counter = 0;

#ifdef DEBUG
    wstring term_s[arity];
#endif

    while(q.next_solution()) {
        ++counter;
        //http://www.swi-prolog.org/pldoc/man?section=foreign-term-analysis
        user = (long)termv[0][1];
        tag  = p_get(termv[1][1]);
        tagged_w = (double)termv[2][1];
        rated_positive_w = (double)termv[3][1];
        rated_negative_w = (double)termv[4][1];
        commented_w = (double)termv[5][1];
        searched_w = (double)termv[6][1];
        //timestamp ??

#ifdef DEBUG
        wcout << L"user: " << user << "\nTag: " <<tag << "\nTagged: " <<tagged_w <<
            "\nRated Positive: " << rated_positive_w << "\nRated Negative: " << rated_negative_w <<
            "\nCommented: " << commented_w << "\nSearched: " << searched_w << "\n";
#endif
    }

#ifdef DEBUG
    wcout << "[+] Total cluster elements: " << counter << endl;
#endif

    PL_halt(EXIT_SUCCESS);
    return EXIT_SUCCESS;
}
