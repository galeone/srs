#ifndef BASE_H
#define BASE_H

#include <utility>
#include "Today.h"

namespace srs {
    class Base {
        public:
            float get();
            Base& set(std::pair<long int, long int>);
            Base(float, float, Today::Field);

        private:
            long int _year, _month;
            float _max, _alpha;
            Today _today;
            Today::Field _dayField;
    };
}

#endif
