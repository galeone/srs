#include "Base.h"

using namespace srs;

Base::Base() {
    _today = Today();
}

Base& Base::set(unsigned int year) {
    _year = year;
    return *this;
}

float Base::get() {
    if(_year >= _today.year())
       return MAX;
    ++_year;
    return get()/2;
}
