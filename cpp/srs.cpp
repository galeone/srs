#include <iostream>
#include <string>
#include <cstdlib>
#include <cwchar>
#include <clocale>
#include <chrono>
#include <ctime>
#include <SWI-cpp.h>

#define DEBUG
#define BASE_MAX 100.0

void _die() { exit(EXIT_FAILURE); }
#define die(format, ...) _die()

using namespace std;

#ifdef DEBUG
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
#endif

static struct tm *get_tm() {
    auto now = chrono::system_clock::now();
    time_t now_c = chrono::system_clock::to_time_t(now);
    return localtime(&now_c);
}

unsigned int current_year() {
    return get_tm()->tm_year + 1900;
}

unsigned int current_month() {
    return get_tm()->tm_mon + 1;
}

float _base(unsigned int year) {
    return year >= current_year() ? BASE_MAX : _base(year + 1) / 2;
}

int main(int argc, char **argv) {
    setlocale(LC_ALL, "en_US.utf8");
    PlEngine engine(argc, argv);

#ifdef DEBUG
    wcout << L"[-] prolog engine initizlized with success!" << endl;
#endif

    const int arity = 8, // of get_frequencies
          number_of_frequencies = 5;

    // t1
    int64_t user;
    //t2
    wstring tag;
    //t3..t7 // weigths -> to int64_t convert to float
    double tagged_w, rated_positive_w, rated_negative_w, commented_w, searched_w;
    // t8, timestamp
    // Extract year and month, day?
    unsigned int year, month, day;

    // Cluster elements
    int64_t counter = 0, monthly_cluster_elements_counter = 0;

    // Calculate weights for couple (user, tag) in last 12 month
    // long type due to prolog
    long end_month = current_month(), end_year  = current_year(),
         start_month = end_month,
         start_year = end_year - 1,
         month_counter = 0;

    float base = _base(start_year);


#ifdef DEBUG
    wcout << "[-]Search year: " << start_year << " - month: " << start_month << "\n";
    wstring term_s[arity];
#endif

    while(month_counter <= 12) {

#ifdef DEBUG
        wcout << "[-] Searching in " << start_year << " - " << start_month << "\n";
        wcout << "[-] Base: " << base << "\n";
#endif

        PlTermv gf_termv(arity);
        PlTermv date_termv(7);

        date_termv[0] = PlTerm(start_year);
        date_termv[1] = PlTerm(start_month);

        gf_termv[7] = PlCompound("timestamp", date_termv);
        PlQuery q("get_frequencies",gf_termv);

        while(q.next_solution()) {
            ++monthly_cluster_elements_counter;
            //http://www.swi-prolog.org/pldoc/man?section=foreign-term-analysis
            user             = (long)gf_termv[0][1];
            tag              = p_get(gf_termv[1][1]);
            tagged_w         = (double)gf_termv[2][1];
            rated_positive_w = (double)gf_termv[3][1];
            rated_negative_w = (double)gf_termv[4][1];
            commented_w      = (double)gf_termv[5][1];
            searched_w       = (double)gf_termv[6][1];
            year             = (long)gf_termv[7][1];
            month            = (long)gf_termv[7][2];
            day              = (long)gf_termv[7][3];

#ifdef DEBUG
            wcout << L"[-]\t(" << user << ", " <<tag << ")" << "\n[-]\tDate: " << year << " " << month << " " << day << "\n" 
                << "[-]\tTagged: " <<tagged_w <<
                "\n[-]\tRated Positive: " << rated_positive_w << "\n[-]\tRated Negative: " << rated_negative_w <<
                "\n[-]\tCommented: " << commented_w << "\n[-]\tSearched: " << searched_w << "\n\n";
#endif
        }

#ifdef DEBUG
        wcout << "[+] Monthly cluster total elements: " << monthly_cluster_elements_counter << endl;
#endif

        ++month_counter;
        counter += monthly_cluster_elements_counter;
        monthly_cluster_elements_counter = 0;
        if(start_month == 12) {
            start_month = 1;
            ++start_year;
            base = _base(start_year);
        } else {
            ++start_month;
        }

    }

#ifdef DEBUG
    wcout << "[+] Yearly cluster total elments: " << counter << endl;
#endif


    PL_halt(EXIT_SUCCESS);
    return EXIT_SUCCESS;
}
