#include "Base.h"

using namespace srs;

Base::Base(float max, float alpha, Today::Field dayField) {
    _alpha = alpha;
    _max   = max;
    _dayField = dayField;
    _today = Today();
}

Base& Base::set(std::pair<long int, long int> year_month) {
    _year  = year_month.first;
    _month = year_month.second;
    return *this;
}

float Base::get() {
    switch(_dayField) {
        case Today::Field::YEAR:
            if(_year >= _today.year()) {
               return _max;
            }
            ++_year;
            return get()/_alpha;

        case Today::Field::MONTH:
            if(_year >= _today.year() && _month >= _today.month()) {
                return _max;
            }

            if(_month == 12) {
                ++_year;
                _month = 1;
            } else {
                _month++;
            }
            return get()/_alpha;       
    }
    std::cerr << "Invalid dayField\n";
    abort();
}
