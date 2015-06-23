% Social Recommender System
% An hibrid recommender system. It mixes Content Based Filtering (CB), Collaborative Filtering (CF)
% and tries to be the more social oriented as possible.

% SRS works on its DB, created by the engine.

:- module(srs, [
    get_frequencies/8
]).

:- use_module(engine).

% returns frequencies ordered by user and topic
get_frequencies(user(A), tag(T), weight(Tagged), weight(RatedPositive), weight(RatedNegative), weight(Commented), weight(Searched), Timestamp) :-
    (populate ; !), !, 
        open_db, odbc_query(srs,
        'SELECT "user", "topic", tagged, rated_positive, rated_negative, commented, searched, update_date
        FROM user_topic_frequencies
        ORDER BY "user", "topic", "update_date"',
        row(A,T,Tagged,RatedPositive,RatedNegative,Commented,Searched,Timestamp)).

close :- close_db.
