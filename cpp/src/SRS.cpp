#include "SRS.h"

using namespace std;
using namespace srs;

wstring SRS::_term(PlTerm t)
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
                    s += _term(t[n]);
                }
                s += L")";
                break;
            }
    }

    return s;
}

SRS::SRS(float alpha, float max) : _alpha(alpha), _max(max) {
    _plans = plans();
}

void SRS::updateDB() {
    PlTermv termv(0);
    PlQuery q("populate",termv);
    q.next_solution();
}

void SRS::generatePlans() {
    // Prolog terms
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

#ifdef DEBUG
    wcout << "[-] Starting search from year: " << start_year << " - month: " << start_month << "\n";
#endif

    monthly_map m;
    cardinality_map c;
    user_topic_count utc;

    float base = Base(_max, _alpha, Today::Field::MONTH).set(make_pair(start_year, start_month)).get();

    while(month_counter <= 12) {
#ifdef DEBUG
        wcout << "[-] Searching in " << start_year << " - " << start_month << "\n";
        wcout << "[-] Base: " << base << "\n";
#endif
        PlTermv gf_termv(8);
        PlTermv date_termv(7);

        date_termv[0] = PlTerm(start_year);
        date_termv[1] = PlTerm(start_month);

        gf_termv[7] = PlCompound("timestamp", date_termv);
        PlQuery q("get_frequencies",gf_termv);

        while(q.next_solution()) {
            ++monthly_cluster_elements_counter;
            //http://www.swi-prolog.org/pldoc/man?section=foreign-term-analysis
            user             = (long)gf_termv[0][1];
            tag              = _term(gf_termv[1][1]);
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
                m[pair] = make_pair(vector<float>(13, 0), 0);
            }

            utc[user]++; // totale delle occorrenze d'uso dei tag
            // non solo la prima volta, altrimenti esce fuori che durante l'anno
            // ha usato 2 volte un tag ma in totale ha usato un solo argomento
            // quindi 2/1 non è una frequenza valida

            c[pair]++;

            m.find(pair)->second.first[month_counter] += tagged_w + rated_positive_w + rated_negative_w + commented_w + searched_w;


#ifdef DEBUG
            wcout << L"[-]\t(" << user << ", " <<tag << ")" << "\n[-]\tDate: " << year << " " << month << " " << day << "\n" <<
                "[-]\tTagged: " <<tagged_w << "\n[-]\tRated Positive: " << rated_positive_w << "\n[-]\tRated Negative: " <<
                rated_negative_w << "\n[-]\tCommented: " << commented_w << "\n[-]\tSearched: " << searched_w << "\n\n";
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
            wcout << "[-]\tFrequencies sum: " << m[it.first].first[month_counter] << " (month " << month_counter << ") \n";
            wcout << "[-]\tNumber of pair occurrence (in month): " << c[it.first] << "\n";
            // nell posizione correte (month_counter) della tal coppia, ci metto dentro il risultato
#endif
            float den = c[it.first] * 5, // 5 = number of frequencies
                  num = m[it.first].first[month_counter];
            float exp = den == 0 ? 0 : num/den;
#ifdef DEBUG
            assert(exp <= 1.0);
            wcout << "[-]\tExponent: " << exp << "\n";
#endif
            m[it.first].first[month_counter] = exp == 0 ? 0 : pow(base, exp);
#ifdef DEBUG
            wcout << "[-]\tWeight: " << m[it.first].first[month_counter] << "\n";
#endif
            // Per ogni coppia salvo il numero di occorrenze nel mese (sommo a quelle del mese precedente
            // per avere il numero totale di occorrenze dell'argomento da parte dell'utente nell'anno
            m[it.first].second += c[it.first];
        }

        // svuoto il conteggio delle occorrenze
        c.clear();
        // non m in quanto contiene l'array di (12) elementi che mi servono per il peso complessivo
        // normalizzato

#ifdef DEBUG
        wcout << "[-] Monthly cluster total elements: " << monthly_cluster_elements_counter << endl;
#endif

        ++month_counter;
        counter += monthly_cluster_elements_counter;
        monthly_cluster_elements_counter = 0;
        if(start_month == 12) {
            start_month = 1;
            ++start_year;
        } else {
            ++start_month;
        }
        base = Base(_max, _alpha, Today::Field::MONTH).set(make_pair(start_year, start_month)).get();
    }
    // fine iterazione per 12 mesi

    // (M/alpha) * (1 + 1/alpha + 1/alpha^2 + ... + 1/alpha^11)
    float normalizationFactor = _max, sum = 1;
    for(int i=1;i<=11;++i) {
        sum += pow(_alpha, -i);
    }
    normalizationFactor *= sum;
    normalizationFactor = round(normalizationFactor);

#ifdef DEBUG
    wcout << "[-] Normalization factor: " << normalizationFactor << "\n";
#endif
    // Mi serve la coppia (utente, argomento) -> occorrenze annuali di argomento (l'ho)
    // che diviso il totale degli argomenti per utente (utente, X) -> conteggiato (l'ho in utc[user])
    // mi da la frequenza del'argomento tra gli argomenti dell'utente
    //
    // Creo N (numero di topic) piani.
    // Ogni piano conterrà la coppia (peso normalizzato, frequenza annuale) -> che identifica un utente
    for(auto const &it : m) {
        wstring topic = get<1>(it.first);
        long    user  = get<0>(it.first);
#ifdef DEBUG
        wcout << "[-] Computing weight for pair: " << user << ", " << topic << "\n";
#endif
        float weight = 0;
        for(auto const &wit : m[it.first].first) {
            weight += wit;
        }

#ifdef DEBUG
        wcout << "[-]\tComputed weight: " << weight << "\n";
        assert(weight > 0);
#endif
        float normalizedWeight = weight / normalizationFactor;
        float topicFrequency   = (float)m[it.first].second / utc[get<0>(it.first)];
#ifdef DEBUG
        wcout << "[-]\tNormalized weight: " << normalizedWeight << "\n";
        wcout << "[-]\tAnnual occurrence of topic: " << m[it.first].second << "\n";
        wcout << "[-]\tAnnual frequency of topic:  " << topicFrequency << "\n";
        assert(normalizedWeight <= 1.0);
        assert(topicFrequency <= 1.0);
#endif
        // Nel piano T (topic), la coppia (normalizedWeight, topicFrequency) identifica user
        _plans[topic][user] = make_pair(normalizedWeight, topicFrequency);
    }

#ifdef DEBUG
    wcout << "[-] Yearly cluster total elments: " << counter << endl;
    wcout << "[-] Total of plans generated: " << _plans.size() << endl;
#endif
}

SRS::~SRS() {
    PL_halt(EXIT_SUCCESS);
}

SRS::users SRS::getRecommendation(long me) {
    users ret;
    return ret;
}
