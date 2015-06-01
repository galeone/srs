% NERDZ entities extractor

:- module(nerdz, [
            name/1, follow/3, bookmark/3, vote/5,
            comment/4, silent/4, blacklist/3, lurk/4,
            mention/4, classify/4
        ]).

use_module(library(odbc)).

name(nerdz).

open_db :- odbc_connect(nerdz, _, 
                        [
                            alias(db), 
                            open(once)
                        ]).

% Map only relations between entities in predicates.

% user(A) followed user(B) the Time
follow(user(A), user(B), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "to", "time" FROM followers',
                        row(A, B, Time), [
                            types([integer, integer, timestamp])
                        ]).

% user(A) followed project(B) the Time
follow(user(A), project(B), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "to", "time" FROM groups_followers',
                        row(A, B, Time), [
                            types([integer, integer, timestamp])
                        ]).

% user(A) bookmarked post(B) the Time
bookmark(user(A), post(P), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "hpid", "time" FROM bookmarks',
                        row(A, P, Time), [
                            types([integer, integer, timestamp])
                        ]).

% user(A) bookmarked project_post(B) the Time
bookmark(user(A), project_post(P), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "hpid", "time" FROM groups_bookmarks',
                        row(A, P, Time), [
                            types([integer, integer, timestamp])
                        ]).

% user(A) rated comment(C) of user(B) the Time
vote(user(A), comment(C), user(B), rate(Rate), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "hcid", "to", "vote", "time" FROM comment_thumbs',
                        row(A, C, B, Rate, Time), [
                            types([integer, integer, integer, integer, timestamp])
                        ]).

% user(A) rated project_comment(C) of user(B) the Time
vote(user(A), project_comment(C), user(B), rate(Rate), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "hcid", "to", "rate", "time" FROM groups_comment_thumbs',
                        row(A, C, B, Rate, Time), [
                            types([integer, integer, integer, integer, timestamp])
                        ]).

% user(A) rated post(P) of user(B) the Time
vote(user(A), post(P), user(B), rate(Rate), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "hpid", "to", "vote", "time" FROM thumbs',
                        row(A, P, B, Rate, Time), [
                            types([integer, integer, integer, integer, timestamp])
                        ]).

% user(A) rated project_post(P) of user(B) the Time
vote(user(A), project_post(P), user(B), rate(Rate), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "hpid", "to", "vote", "time" FROM groups_thumbs',
                        row(A, P, B, Rate, Time), [
                            types([integer, integer, integer, integer, timestamp])
                        ]).

% user(A) commented on post(P) of user (B) the Time
comment(user(A), post(P), user(B), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "hpid", "to", "time" FROM comments',
                        row(A, P, B, Time), [
                            types([integer, integer, integer, timestamp])
                        ]).

% user(A) commented on project_post(P) of project(B) the Time
comment(user(A), project_post(P), project(B), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "hpid", "to", "time" FROM groups_comments',
                        row(A, P, B, Time), [
                            types([integer, integer, integer, timestamp])
                        ]).

% user(A) silented notification of user(B) on post(P) the Time
silent(user(A), user(B), post(P), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "to", "hpid", "time" FROM comments_no_notify',
                        row(A, B, P, Time), [
                            types([integer, integer, integer, timestamp])
                        ]).

% user(A) silented notification of user(B) on project_post(P) the Time
silent(user(A), user(B), project_post(P), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "to", "hpid", "time" FROM groups_comments_no_notify',
                        row(A, B, P, Time), [
                            types([integer, integer, integer, timestamp])
                        ]).

% user(A) silented notification on post(P) the Time
silent(user(A), post(P), Time) :- open_db, odbc_query(db,
                        'SELECT "user", "hpid", "time" FROM posts_no_notify',
                        row(A, P, Time), [
                            types([integer, integer, timestamp])
                        ]).

% user(A) silented notification on project_post(P) the Time
silent(user(A), project_post(P), Time) :- open_db, odbc_query(db,
                        'SELECT "user", "hpid", "time" FROM groups_posts_no_notify',
                        row(A, P, Time), [
                            types([integer, integer, timestamp])
                        ]).

% user(A) blacklisted user(B) the Time
blacklist(user(A), user(B), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "to", "time" FROM blacklist',
                        row(A, B, Time), [
                            types([integer, integer,timestamp])
                        ]).

% user(A) lurked post(P) of user(B) the Time
lurk(user(A), post(P), user(B), Time)  :- open_db, odbc_query(db,
                        'SELECT "from", "hpid", "to", "time" FROM lurkers',
                        row(A, P, B, Time), [
                            types([integer, integer, integer, timestamp])
                        ]).

% user(A) lurked project_post(P) of user(B) the Time
lurk(user(A), post(P), user(B), Time)  :- open_db, odbc_query(db,
                        'SELECT "from", "hpid", "to", "time" FROM groups_ lurkers',
                        row(A, P, B, Time), [
                            types([integer, integer, integer, timestamp])
                        ]).

% user(A) mentioned user(B) on post(P) the Time
mention(user(A), user(B), post(P), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "to", "u_hpid", "time" FROM mentions WHERE "u_hpid" IS NOT NULL',
                        row(A, B, P, Time), [
                            types([integer, integer, integer, timestamp])
                        ]).

% user(A) mentioned user(B) on project_post(P) the Time
mention(user(A), user(B), project_post(P), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "to", "g_hpid", "time" FROM mentions WHERE "g_hpid" IS NOT NULL',
                        row(A, B, P, Time), [
                            types([integer, integer, integer, timestamp])
                        ]).

% user(A) classified post(P) with Tag the Time
classify(user(A), post(P), tag(Tag), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "u_hpid", "tag", "time" FROM posts_classification WHERE "u_hpid" IS NOT NULL',
                        row(A, P, Tag, Time), [
                            types([integer, integer, atom, timestamp])
                        ]).

% user(A) classified project_post(P) with Tag the Time
classify(user(A), project_post(P), tag(Tag), Time) :- open_db, odbc_query(db,
                        'SELECT "from", "g_hpid", "tag", "time" FROM posts_classification WHERE "g_hpid" IS NOT NULL',
                        row(A, P, Tag, Time), [
                            types([integer, integer, atom, timestamp])
                        ]).
