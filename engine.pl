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
                'SELECT EXTRACT(EPOCH FROM NOW() - last_update)::INT, last_update FROM srs_data LIMIT 1',
                row(ElapsedTime, LastUpdate), [ types([integer, integer]) ]),
                ElapsedTime >= 1200 , % if its true, populate
                write('Computing weights for dates between last_update and NOW'), nl,
                write('Annual weights:'), nl, nl,
                findall(
                    (user(A), post(P), tag(T), Timestamp),
                    (classify(user(A), post(P), tag(T), Timestamp), Timestamp > LastUpdate) , L), !.
                % For every user(A), compute weight and insert in srs db
                
                % Save NOW in last_update
frequency(Count, Tot, Freq) :- Freq is Count / Tot.
value(Base, Exp, Value) :- Value is Base ** Exp.

topic_value(_, _, []) :- !.
topic_value(user(A), tag(T), range(Start, End), [(user(A), post(_), tag(T), Timestamp)|Tail]) :-
    year(Timestamp, Year),
    base(Year, B),
    count(user(A), tag(T), range(Start, End), TagCount),
    count(user(A), range(Start, End), TotalCount),
    Exp is TagCount / TotalCount, value(B, Exp, Value),
    odbc_prepare(srs, 'INSERT INTO topic_user_value("topic", "user", "value") VALUES(?,?,?)', [varchar(70), bigint, real], Statement),
    odbc_execute(Statement, [A, T, Value], AffectedRow),
    AffectedRow =:= 1,
    odbc_free_statement(Statement), !, 
    topic_value(user(A), tag(T), range(Start, End), Tail).

topic_value(user(A), tag(T), range(Start, End), [_|Tail]) :- topic_value(user(A), tag(T), range(Start, End), Tail).

% test
find_duplicate([H|T]) :- not(member(H, T)).

get_date_time_value(Key, Value) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(Key, DateTime, Value).

year(Year) :- get_date_time_value(year, Year).
year(Timestamp, Year) :- stamp_date_time(Timestamp, DateTime, local),
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
