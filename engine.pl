% Import the required database and ask the engine to do the work
% Do you want to use srs with an other social network? Define a module that exports
% the same predicate of nerdz and replace the line below
:- use_module(nerdz).

% populating the srs database, computing the cost for each item

open_db  :- odbc_connect(srs, _, [ alias(srs), open(once) ]).

% Compute the relation value between topics and users.
% Its a cartesian product (topics X users). To reduce srs database size
% we store only existing relations (user(A) talks about tag(T)).
% If we don't find any match, the value is 0.

% Populate the db every 20 minutes (with new values)

% Check if 20 minutes from the last update are passed
populate(L) :- open_db, odbc_query(srs,
                'SELECT EXTRACT(EPOCH FROM NOW() - last_update)::INT FROM srs_data LIMIT 1',
                row(ElapsedTime), [ types([integer]) ]),
                ElapsedTime >= 1200 , % if its true, populate
                write('Computing weights...'), nl,
                write('Annual weights:'), nl, nl,
                findall(
                    (user(A), post(P), tag(T), Year),
                    classify(user(A), post(P), tag(T), timestamp(Year, _ , _ , _, _, _, _)), L), !.
                    % find_duplicate(L), no duplicate exists. OK

% test
find_duplicate([H|T]) :- not(member(H, T)).

year(Year) :- get_time(Stamp),
              stamp_date_time(Stamp, DateTime, local),
              date_time_value(year, DateTime, Year).

:- dynamic base_res/2.
base(Year, B)   :- base_res(Year, B), !.
base(Year, 100) :- year(Actual), Actual =< Year,
                    assert(base_res(Year, 100)), !.

base(Year, B)   :- NextYear is Year + 1, base(NextYear, NextBase), B is NextBase / 2,
                    assert(base_res(Year, B)).

% TODO: find a good formula to calculate the value.
% TODO: use different tables to store different informations 

close_db :- odbc_disconnect(srs).
