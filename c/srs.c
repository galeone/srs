#include <stdio.h>
#include <stdlib.h>

#include <SWI-Prolog.h>
// swipl-ld TODO

void _die() { exit(EXIT_FAILURE); }
#define die(format, ...) _die(fprintf(stderr, "[!] ") && fprintf(stderr, format, __VA_ARGS__) && fprintf(stderr, "\n"))


int main(int argc, char **argv) {
    if(argc != 1) {
        die("%s path/of/engine.pl",argv[0]);
    }

    static char *engine = { argv[1], NULL };

    if(!PL_initialise(1,engine)) {
        PL_halt(EXIT_FAILURE);
        die("error initializing prolog engine\n");
    }

    puts("[+] prolog engine initizlized with success!");
    
    // call predicate get_weights/8
    predicate_t populate_p = PL_predicate("get_weights", 8, "srs");
    // pointer to the first element of a vector of 8 terms (returning variables)
    term_t row = PL_new_term_refs(8);

    qid_t populate_q = PL_open_query(NULL, PL_Q_NORMAL, populate_p, row);

    while(PL_next_solution(populate_q)) {
        //http://www.swi-prolog.org/pldoc/man?section=foreign-term-analysis
        //TODO
        //PL_get_*(term i = row + i)
    }
    PL_close_query(populate_q);


    PL_halt(EXIT_SUCCESS);
    return EXIT_SUCCESS;
}
