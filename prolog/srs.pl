% Social Recommender System
% An hibrid recommender system. It mixes Content Based Filtering (CB), Collaborative Filtering (CF)
% and tries to be the more social oriented as possible.

% SRS works on its DB, created by the engine.

:- module(srs, [
    get_frequencies/8, populate/0, get_follow/3
]).

:- use_module(engine).
:- use_module(nerdz).

% returns frequencies ordered by user and topic
get_frequencies(user(A), tag(T), frequency(Tagged), frequency(RatedPositive), frequency(RatedNegative), frequency(Commented), frequency(Searched), Timestamp) :-
        open_db, !, odbc_query(srs,
        'SELECT "user", "topic", tagged, rated_positive, rated_negative, commented, searched, update_date
        FROM user_topic_frequencies
        ORDER BY "user", "topic", "update_date"',
        row(A,T,Tagged,RatedPositive,RatedNegative,Commented,Searched,Timestamp)).

get_follow(user(A), user(B), Timestamp) :- follow(user(A), user(B), Timestamp).
populate :- populate_db.
