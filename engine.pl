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
populate :- open_db, odbc_query(srs,
                'SELECT EXTRACT(EPOCH FROM NOW() - yearly_last_update)::INT, yearly_last_update FROM srs_data LIMIT 1',
                row(ElapsedTime, _), [ types([integer, integer]) ]),
                ElapsedTime >= 1200 , % if its true, populate
                write('Computing weights for dates between yearly_last_update and NOW'), nl,
                write('Annual weights:'), nl, nl,
                findall(
                    (user(A), tag(T), year(Year)),
                    classify(user(A), tag(T), year(Year)), L), !,
                        %get_time(Today), YearAgo is Today - 31536000,
                        %write('Today: '), write(Today), write(' Year ago: '), write(YearAgo), nl,
                    % For every user(A), compute tag frequency in the year(tag) and insert in srs db
                    topic_value(L),
                    % Save NOW in last_update
                    odbc_query(srs,'UPDATE srs_data SET yearly_last_update = NOW()'),
                    close_db.
                
                %filter_tag(_, _, _, [], _).
                %filter_tag(user(A), tag(T), Year, [(user(B), post(P), tag(TT), Timestamp)|Tail], [(user(B), post(P), tag(TT), Timestamp)|NewList]) :- 
                %    (not(year(Timestamp, Year)), !; A \= B, !; T \= TT, !), !, filter_tag(user(A), tag(T), Year, Tail, NewList).
                %filter_tag(user(A), tag(T), Year, [_|Tail], NewList) :- !, filter_tag(user(A), tag(T), Year, Tail, NewList).

frequency(TagCount, TotalCount, Value) :- Value is TagCount / TotalCount.

topic_value([]) :- !.
topic_value([(user(A), tag(T), year(Year))|Tail]) :-
    write('Tag Year  '), write(Year), nl, write(user(A)), nl,
    date_time_stamp(date(Year, 1, 1), Start), date_time_stamp(date(Year, 12, 31, 23, 59, 60.0, 0, _, _), End),
    count(user(A), tag(T), range(Start, End), TagCountInRange), write(tag(T)), write(' count (in range): '), write(TagCountInRange), nl,
    count(user(A), tag, range(Start, End), TotalTagCountInRange), write('Total count (in range): '), write(TotalTagCountInRange), nl,
    frequency(TagCountInRange, TotalTagCountInRange, Freq), write('Frequency: '), write(Freq), nl,
    odbc_prepare(srs,
        'INSERT INTO yearly_user_topic_frequency("topic", "year", "user", "frequency") VALUES(lower(?), ?, ?, ?)',
        [varchar(70), integer, bigint, real], Statement),
    odbc_execute(Statement, [T, Year, A, Freq], affected(AffectedRow)),
    AffectedRow =:= 1,
    odbc_free_statement(Statement), !,
    topic_value(Tail).

% test
find_duplicate([H|T]) :- not(member(H, T)).

get_date_time_value(Key, Value) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(Key, DateTime, Value).

get_date_time_vakye(Stamp, Key, Value) :-
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(Key, DateTime, Value).

:- dynamic base_res/2.
base(Year, B)   :- base_res(Year, B), !.
base(Year, 100) :- get_date_time_value(year, Actual), Actual =< Year,
                    assert(base_res(Year, 100)), !.

base(Year, B)   :- NextYear is Year + 1, base(NextYear, NextBase), B is NextBase / 2,
                    assert(base_res(Year, B)).

% TODO: find a good formula to calculate the value.
% TODO: use different tables to store different informations 

close_db :- odbc_disconnect(srs).
