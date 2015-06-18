% Import the required database and ask the engine to do the work
% Do you want to use srs with an other social network? Define a module that exports
% the same predicate of nerdz and replace the line below
:- module(engine, [
    open_db/0, populate/0, close_db/0, base/2, every_hours/3
    ]).

:- use_module(nerdz).
:- consult(config).

% populating the srs database, computing the cost for each item

open_db  :- odbc_connect(srs, _, [ alias(srs), open(once) ]).

% Compute the relation value between topics and users.
% Its a cartesian product (topics X users). To reduce srs database size
% we store only existing relations (user(A) <action> tag(T)).

every_hours(Start, End, [[]])   :- Start >= End, !.
every_hours(Start, End, [[Start, NewTime]|D]) :- NewTime is Start + 3600, every_hours(NewTime, End, D).

populate([]) :- odbc_query(srs,'UPDATE srs_data SET "timestamp" = NOW() WHERE "key" = \'LAST_UPDATE\''), close_db, !.
populate([[Begin, End]|Tail]) :- 
                write('Computing weights ['), write(Begin), write(','), write(End), write(']'), nl,
                % Avoid cartesian product searching only over existing user activities
                (
                    setof((user(A), tag(T)), (
                        classify(user(A), tag(T), range(Begin, End)) ;
                        search(user(A),   tag(T), range(Begin, End)) ;
                        rated(user(A),    tag(T), range(Begin, End)) ; % positive and negative
                        comment(user(A),  tag(T), range(Begin, End))
                    ), L) ; % setof fails if there's not data, but i need to update the timestamp next
                   L = []
                ),
                topic_value(L, range(Begin, End)), !,
                populate(Tail).

% Populate/0 succed only of the right amount of time elapsed
populate :- open_db, odbc_query(srs,
                'SELECT "timestamp" FROM srs_data WHERE "key" = \'LAST_UPDATE\'',
                row(LastUpdate), [ types([integer]) ]),
                write('Computing weights for every hour between '), write(LastUpdate), write(' and NOW'), nl,
                get_time(Now), every_hours(LastUpdate, Now, Hours), populate(Hours).

% Frequency/3
% f(Num, Den) = 0       if Den is 0
%               Num/Den otherwise 
frequency(_ , 0, 0) :- !.
frequency(Num, Den, Freq) :- Freq is Num / Den.

% Frequency/4
% Compute the frequency of action What, done by user(A), in range(Start, End)
frequency(user(A), What, range(Start, End), AdjFrequency) :-
    functor(What, Action, _),
    % get adjustement coefficient from config
    action_coefficient(Action, Adj),
    count(user(A), What,   range(Start, End), CountInRange),      write(What), write(' count (in range): '), write(CountInRange), nl,
    count(user(A), Action, range(Start, End), TotalCountInRange), write('Total count (in range): '),         write(TotalCountInRange), nl,
    frequency(CountInRange, TotalCountInRange, Frequency),        write('Frequency: '),                      write(Frequency), nl,
    AdjFrequency is Frequency * Adj,                              write('Adjusted Frequency: '),             write(AdjFrequency), nl.

% insert_topic_value/4, save the computed actions [frequencies] of user(A) into srs db, computed at date Date
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

% topic_value/2
% for every couple (user(A), tag(T)) in the input list, compute frequency of actions in range(Start, End)
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
remove_duplicates([],[]) :- !.
remove_duplicates([H|T], D) :- member(H, T), !, remove_duplicates(T, D).
remove_duplicates([H|T], [H|D]) :- !, remove_duplicates(T, D).

contains_duplicate([]).
contains_duplicate([H|T]) :- not(member(H, T)), contains_duplicate(T).

all_zero([X]):- X =:= 0, !.
all_zero([H|T]) :- H =:= 0, !, all_zero(T).

% Utility predicates
get_date_time_value(Key, Value) :-
    get_time(Stamp),
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(Key, DateTime, Value).

get_date_time_vakye(Stamp, Key, Value) :-
    stamp_date_time(Stamp, DateTime, local),
    date_time_value(Key, DateTime, Value).

% Base predicate (prolog or change language?)
:- dynamic base_res/2.
base(Year, B)   :- base_res(Year, B), !.
base(Year, 100) :- get_date_time_value(year, Actual), Actual =< Year,
                    assert(base_res(Year, 100)), !.

base(Year, B)   :- NextYear is Year + 1, base(NextYear, NextBase), B is NextBase / 2,
                    assert(base_res(Year, B)).

close_db :- odbc_disconnect(srs).
