#include <iostream>
#include <string>
#include <cstdlib>
#include <cwchar>
#include <clocale>
#include <cmath>
#include <map>
#include <vector>
#include <cassert>
#include <SWI-cpp.h>
#include "classes/Base.h"

#define DEBUG

void _die() { exit(EXIT_FAILURE); }
#define die(format, ...) _die()

using namespace std;
using namespace srs;

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

typedef pair<long, wstring> ut_pair;
typedef map<ut_pair, vector<float>> monthly_map;
typedef map<ut_pair, long> cardinality_map;

int main(int argc, char **argv) {
    setlocale(LC_ALL, "en_US.utf8");
    PlEngine engine(argc, argv);

#ifdef DEBUG
    wcout << L"[-] prolog engine initizlized with success!" << endl;
#endif

    const int arity = 8; // of get_frequencies

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

    Today today;

    // Calculate weights for couple (user, tag) in last 12 month
    // long type due to prolog
    long end_month = today.month(), end_year  = today.year(),
         start_month = end_month,
         start_year = end_year - 1,
         month_counter = 0;

    float base = Base().set(start_year).get();

#ifdef DEBUG
    wcout << "[-]Search year: " << start_year << " - month: " << start_month << "\n";
    wstring term_s[arity];
#endif

    monthly_map m;
    cardinality_map c;

#define FREQ_NUM 5

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

            auto pair = ut_pair(user, tag);
            if(m.find(pair) == m.end()) {
                m[pair] = vector<float>(13);
            }

            if(c.find(pair) == c.end()) {
                c[pair] = 0;
            }

            m.find(pair)->second[month_counter] += tagged_w + rated_positive_w + rated_negative_w + commented_w + searched_w;
            c[pair]++;

#ifdef DEBUG
            wcout << L"[-]\t(" << user << ", " <<tag << ")" << "\n[-]\tDate: " << year << " " << month << " " << day << "\n" 
                << "[-]\tTagged: " <<tagged_w <<
                "\n[-]\tRated Positive: " << rated_positive_w << "\n[-]\tRated Negative: " << rated_negative_w <<
                "\n[-]\tCommented: " << commented_w << "\n[-]\tSearched: " << searched_w << "\n\n";
#endif
        }
        // Compute the exponent at the end of current month
        // a questo punto ho per ogni coppia (utente, argomento) la somma delle frequenze per quell'argomento nel mese corrente
        // nel vettore associato alla coppia, nella posizione del mese corrente.
        // Inoltre ho la cardinalità dell'insieme (aka il numero di occorrenze della coppia nel mese)
        // Per calcolare il peso, prendiamo 
        for(auto const &it : m) {
#ifdef DEBUG
            wcout << "[-]Computing monthly weight for pair: " << get<0>(it.first) << ", " << get<1>(it.first) << "\n";
            wcout << "[-]\tFrequencies sum: " << m[it.first][month_counter] << " (month " << month_counter << ") \n";
            wcout << "[-]\tNumber of pair occurrence (in month): " << c[it.first] << "\n";
            // nell posizione correte (month_counter) della tal coppia, ci metto dentro il risultato
#endif
            float den = c[it.first] * FREQ_NUM,
                  num = m[it.first][month_counter];
            float exp = den == 0 ? 0 : num/den;
            assert(exp <= 1.0);
            m[it.first][month_counter] = pow(base, exp);
#ifdef DEBUG
            wcout << "[-]\tExponent: " << exp << "\n";
            wcout << "[-]\tWeight: " << m[it.first][month_counter] << "\n";

#endif
        }

        // svuoto il conteggio delle occorrenze
        c.clear();
        // non m in quanto contiene l'array di 13(12?) elementi che mi servono per il peso complessivo
        // normalizzato

#ifdef DEBUG
        wcout << "[+] Monthly cluster total elements: " << monthly_cluster_elements_counter << endl;
#endif

        ++month_counter;
        counter += monthly_cluster_elements_counter;
        monthly_cluster_elements_counter = 0;
        if(start_month == 12) {
            start_month = 1;
            ++start_year;
            base = Base().set(start_year).get();
        } else {
            ++start_month;
        }

    }

#ifdef DEBUG
    wcout << "[+] Yearly cluster total elments: " << counter << endl;
#endif

    for(auto const &it : m) {
#ifdef DEBUG
        wcout << "[-] Computing weight for pair: " << get<0>(it.first) << ", " << get<1>(it.first) << "\n";
#endif
        float weight = 0;
        for(auto const &wit : m[it.first]) {
            weight += wit;
        }
#ifdef DEBUG
        wcout << "[-]\tComputed weight: " << weight << "\n";
#endif
        float normalizedWeight = weight / (FREQ_NUM * 13 / 2);
#ifdef DEBUG
        wcout << "[-]\tNormalized weight: " << normalizedWeight << "\n";
#endif
    }

    PL_halt(EXIT_SUCCESS);
    return EXIT_SUCCESS;
}
