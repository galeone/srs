#include "Today.h"

using namespace srs;

Today::Today() {
    auto now = std::chrono::system_clock::now();
    time_t now_c = std::chrono::system_clock::to_time_t(now);
    _tm = localtime(&now_c);
}

unsigned int Today::year() {
    return _tm->tm_year + 1900;
}

unsigned int Today::month() {
    return _tm->tm_mon + 1;
}

unsigned int Today::day() {
    return _tm->tm_mday;
}

