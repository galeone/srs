#ifndef BASE_H
#define BASE_H

#include "Today.h"

namespace srs {
    class Base {
        public:
            static constexpr float MAX   = 100;

            float get();
            Base& set(unsigned int);
            Base();

        private:
            unsigned int _year;
            Today _today;
    };
}

#endif
