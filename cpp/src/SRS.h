#ifndef SRS_H
#define SRS_H

#include <iostream>
#include <string>
#include <cstdlib>
#include <cwchar>
#include <clocale>
#include <cmath>
#include <map>
#include <vector>
#include <cassert>
#include <algorithm>
#include <SWI-cpp.h>
#include "Base.h"

namespace srs {

    class SRS {
        public:
            typedef std::vector<long> users;

            SRS(float, float);
            void updateDB();
            void generatePlans();
            users getRecommendation(long);
            ~SRS();

        private:
            typedef std::pair<long, std::wstring> ut_pair;

            //std::vector of frequenices,number of occurence of Topic by User
            typedef std::pair<std::vector<float>, long> topic_pair;
            typedef std::map<ut_pair, topic_pair> monthly_map;
            typedef std::map<ut_pair, long> cardinality_map;
            typedef std::map<long, long> user_topic_count;
            typedef std::map<std::wstring, std::map<long, std::pair<float, float>>> plans;

            float _alpha;
            float _max;
            plans _plans;
            std::wstring _term(PlTerm);
            float _euclideanDistance(float, float, float, float);
            users _getUsersSortedByAffinity(long);
    };
}

#endif
