#ifndef TODAY_H
#define TODAY_H

#include <chrono>
#include <ctime>

namespace srs {
    class Today {
        private:
            struct tm *_tm;

        public:
            unsigned int year();
            unsigned int month();
            unsigned int day();
            Today();
    };
}

#endif
