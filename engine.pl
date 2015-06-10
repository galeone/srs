% Import the required database and ask the engine to do the work
% Do you want to use srs with an other social network? Define a module that exports
% the same predicate of nerdz and replace the line below
:- use_module(nerdz).
consult(config).

% populating the srs database, computing the cost for each item

open_db  :- odbc_connect(srs, _, [ alias(srs), open(once) ]).

% Compute the relation value between topics and users.
% Its a cartesian product (topics X users). To reduce srs database size
% we store only existing relations (user(A) talks about tag(T)).
% If we don't find any match, the value is 0.

% Populate the db every 20 minutes (with new values)

% Check if 20 minutes from the last update are passed
populate :- open_db, odbc_query(srs,
                'SELECT EXTRACT(EPOCH FROM NOW() - "timestamp")::INT, "timestamp" FROM srs_data WHERE "key" = \'LAST_UPDATE\'',
                row(ElapsedTime, LastUpdate), [ types([integer, integer]) ]),
                ElapsedTime >= 1200 , % if its true, populate
                write('Computing weights for dates between '), write(LastUpdate), write(' and NOW'), nl,
                get_time(Now),
                % Avoid cartesian product searching only over existing user activities TODO v
                setof((user(A), tag(T)), (
                        classify(user(A), tag(T), range(LastUpdate, Now)) ;
                        search(user(A), tag(T), range(LastUpdate, Now)) %; do for rated, commet, ecc
                    ), L), !,
                        %get_time(Today), YearAgo is Today - 31536000,
                        %write('Today: '), write(Today), write(' Year ago: '), write(YearAgo), nl,
                    % For every user(A), compute tag frequency in the year(tag) and insert in srs db
                    topic_value(L, range(LastUpdate, Now)),
                    % Save NOW in last_update
                    odbc_query(srs,'UPDATE srs_data SET "timestamp" = NOW() WHERE "key" = \'LAST_UPDATE\''),
                    close_db.
                
                %filter_tag(_, _, _, [], _).
                %filter_tag(user(A), tag(T), Year, [(user(B), post(P), tag(TT), Timestamp)|Tail], [(user(B), post(P), tag(TT), Timestamp)|NewList]) :- 
                %    (not(year(Timestamp, Year)), !; A \= B, !; T \= TT, !), !, filter_tag(user(A), tag(T), Year, Tail, NewList).
                %filter_tag(user(A), tag(T), Year, [_|Tail], NewList) :- !, filter_tag(user(A), tag(T), Year, Tail, NewList).
                %
frequency(_ , 0, 0) :- !.
frequency(Num, Den, Freq) :- Freq is Num / Den.

frequency(user(A), What, range(Start, End), Frequency) :-
    functor(What, Action, _),
    count(user(A), What,   range(Start, End), CountInRange),      write(What), write(' count (in range): '), write(CountInRange), nl,
    count(user(A), Action, range(Start, End), TotalCountInRange), write('Total count (in range): '),         write(TotalCountInRange), nl,
    frequency(CountInRange, TotalCountInRange, Frequency),        write('Tagged Frequency: '),               write(Frequency), nl.

%test
all_zero([X]):- X =:= 0, !.
all_zero([H|T]) :- H =:= 0, !, all_zero(T).

insert_topic_value(user(A), Date, tag(T), Frequencies) :- 
    Frequencies = [
        TagFrequency, SearchFrequency, PositiveRateFrequency, NegativeRateFrequency, CommentFrequency
    ], odbc_prepare(srs,
    'INSERT INTO user_topic_frequencies(
        "topic", "update_date", "user", "tagged", "searched",
        "rated_positive", "rated_negative", "commented")
        VALUES(lower(?), ?, ?, ?, ? ,?, ?, ?)', [
        varchar(70), float > timestamp, bigint, real,
        real, real, real, real
    ], Statement), !, 
    odbc_execute(Statement, [
        T, Date, A, TagFrequency, SearchFrequency, PositiveRateFrequency,
        NegativeRateFrequency, CommentFrequency
    ]),
    odbc_free_statement(Statement), !.

topic_value([],_) :- !.
topic_value([(user(A), tag(T))|Tail], range(Start, End)) :-
    write(user(A)), nl, 
    frequency(user(A), tagged(tag(T)),         range(Start, End), TagFrequency),
    frequency(user(A), searched(tag(T)),       range(Start, End), SearchFrequency),
    frequency(user(A), rated_positive(tag(T)), range(Start, End), PositiveRateFrequency),
    frequency(user(A), rated_negative(tag(T)), range(Start, End), NegativeRateFrequency),
    frequency(user(A), commented(tag(T)),      range(Start, End), CommentFrequency),
    insert_topic_value(user(A), End, tag(T), [
        TagFrequency, SearchFrequency, PositiveRateFrequency, NegativeRateFrequency, CommentFrequency
    ]),
    topic_value(Tail, range(Start, End)).

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

close_db :- odbc_disconnect(srs).
