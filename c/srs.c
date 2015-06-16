#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <SWI-Prolog.h>

#define DEBUG

void _die() { exit(EXIT_FAILURE); }
#define die(format, ...) _die()

void p_get(term_t t, char s[200])
{
    functor_t functor;
    size_t len;
    int arity, n;
    memset(s, '\0', 200);
    char *c;
    switch( PL_term_type(t) )
    {
        case PL_VARIABLE:
        case PL_ATOM:
        case PL_INTEGER:
        case PL_FLOAT:
            PL_get_chars(t, &c, CVT_ALL);
            sprintf(s, "%s", c);
            break;
        case PL_STRING:
            PL_get_string_chars(t, &c, &len);
            sprintf(s,"\"%s\"", c);
            break;
        case PL_TERM:
            {
                atom_t name;
                term_t a = PL_new_term_ref();
                PL_get_name_arity(t, &name, &arity);
                sprintf(s,"%s(", PL_atom_chars(name));
                for(n=1; n<=arity; n++) {
                    PL_get_arg(n, t, a);
                    if ( n > 1 ) {
                        strcat(s, ", ");
                    }
                    char d[200];
                    p_get(a,d);
                    strcat(s, d);
                }
                strcat(s,")");
                break;
            }
    }
}

int main(int argc, char *argv[]) {
    if(argc != 1) {
        die("%s path/of/engine.pl",argv[0]);
    }

    char *prolog_init[2];
    prolog_init[0] = argv[0];
    prolog_init[1] = NULL;

    if(!PL_initialise(1,prolog_init)) {
        PL_halt(EXIT_FAILURE);
        die("error initializing prolog engine\n");
    }

#ifdef DEBUG
    puts("[+] prolog engine initizlized with success!");
#endif

    // call predicate get_weigths/8
    char *p_name = "get_weigths", *p_module = "srs";
    int arity = 8;
    predicate_t get_weigths_p = PL_predicate(p_name, arity, p_module);
    // pointer to the first element of a vector of 8 terms (returning variables)
    term_t term = PL_new_term_refs(arity);
    // every term is a prolog term ( type(X) )
    // thus we store in a string the values and after we parse this value to extract the value
    char term_s[8][200];
    // t1
    int64_t userID;
    //t2
    char tag[70];
    //t3..t7 // weigths -> to int64_t convert to float
    int64_t tagged_w, rated_positive_w, rated_negative_w, commented_w, searched_w;
    //t8, Timestamp
    int64_t timestamp;

    qid_t get_weigths_q = PL_open_query(NULL, (PL_Q_NORMAL|PL_Q_CATCH_EXCEPTION), get_weigths_p, term);

    while(PL_next_solution(get_weigths_q)) {
        //http://www.swi-prolog.org/pldoc/man?section=foreign-term-analysis
        for(size_t i = 0; i < 8; ++i) {
            p_get(term + i, term_s[i]);
#ifdef DEBUG
            printf("%s ",term_s[i]);
            if(i == 7) putchar('\n');
#endif
        }
    }
    PL_close_query(get_weigths_q);

    PL_halt(EXIT_SUCCESS);
    return EXIT_SUCCESS;
}
