% NERDZ entities extractor

:- module(nerdz, [
            name/1, follow/3, bookmark/3, vote/5,
            comment/4, silent/4, blacklist/3, lurk/4,
            mention/4, classify/4, classify/3, count/4,
            search/3, users/1, tags/1
        ]).

use_module(library(odbc)).

name(nerdz).

open_db :- odbc_connect(nerdz, _, 
                        [
                            alias(nerdz), 
                            open(once)
                        ]).


% users
users(user(A)) :- open_db, odbc_query(nerdz,
                    'SELECT "counter" FROM users',
                    row(A), [
                        types([integer])
                    ]).

%tags
tags(tag(T)) :- open_db, odbc_query(nerdz,
                    'SELECT DISTINCT LOWER("tag") FROM posts_classification',
                    row(T), [
                        types([atom])
                    ]).

% Map only relations between entities in predicates.

% user(A) followed user(B) the Timestamp
follow(user(A), user(B), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "to", "time" FROM followers',
                        row(A, B, Timestamp), [
                            types([integer, integer, integer])
                        ]).

% user(A) followed project(B) the Timestamp
follow(user(A), project(B), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "to", "time" FROM groups_followers',
                        row(A, B, Timestamp), [
                            types([integer, integer, integer])
                        ]).

% user(A) bookmarked post(B) the Timestamp
bookmark(user(A), post(P), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "hpid", "time" FROM bookmarks',
                        row(A, P, Timestamp), [
                            types([integer, integer, integer])
                        ]).

% user(A) bookmarked project_post(B) the Timestamp
bookmark(user(A), project_post(P), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "hpid", "time" FROM groups_bookmarks',
                        row(A, P, Timestamp), [
                            types([integer, integer, integer])
                        ]).

% user(A) rated comment(C) of user(B) the Timestamp
vote(user(A), comment(C), user(B), rate(Rate), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "hcid", "to", "vote", "time" FROM comment_thumbs',
                        row(A, C, B, Rate, Timestamp), [
                            types([integer, integer, integer, integer, integer])
                        ]).

% user(A) rated project_comment(C) of user(B) the Timestamp
vote(user(A), project_comment(C), user(B), rate(Rate), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "hcid", "to", "rate", "time" FROM groups_comment_thumbs',
                        row(A, C, B, Rate, Timestamp), [
                            types([integer, integer, integer, integer, integer])
                        ]).

% user(A) rated post(P) of user(B) the Timestamp
vote(user(A), post(P), user(B), rate(Rate), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "hpid", "to", "vote", "time" FROM thumbs',
                        row(A, P, B, Rate, Timestamp), [
                            types([integer, integer, integer, integer, integer])
                        ]).

% user(A) rated project_post(P) of user(B) the Timestamp
vote(user(A), project_post(P), user(B), rate(Rate), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "hpid", "to", "vote", "time" FROM groups_thumbs',
                        row(A, P, B, Rate, Timestamp), [
                            types([integer, integer, integer, integer, integer])
                        ]).

% user(A) commented on post(P) of user (B) the Timestamp
comment(user(A), post(P), user(B), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "hpid", "to", "time" FROM comments',
                        row(A, P, B, Timestamp), [
                            types([integer, integer, integer, integer])
                        ]).

% user(A) commented on project_post(P) of project(B) the Timestamp
comment(user(A), project_post(P), project(B), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "hpid", "to", "time" FROM groups_comments',
                        row(A, P, B, Timestamp), [
                            types([integer, integer, integer, integer])
                        ]).

% user(A) silented notification of user(B) on post(P) the Timestamp
silent(user(A), user(B), post(P), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "to", "hpid", "time" FROM comments_no_notify',
                        row(A, B, P, Timestamp), [
                            types([integer, integer, integer, integer])
                        ]).

% user(A) silented notification of user(B) on project_post(P) the Timestamp
silent(user(A), user(B), project_post(P), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "to", "hpid", "time" FROM groups_comments_no_notify',
                        row(A, B, P, Timestamp), [
                            types([integer, integer, integer, integer])
                        ]).

% user(A) silented notification on post(P) the Timestamp
silent(user(A), post(P), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "user", "hpid", "time" FROM posts_no_notify',
                        row(A, P, Timestamp), [
                            types([integer, integer, integer])
                        ]).

% user(A) silented notification on project_post(P) the Timestamp
silent(user(A), project_post(P), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "user", "hpid", "time" FROM groups_posts_no_notify',
                        row(A, P, Timestamp), [
                            types([integer, integer, integer])
                        ]).

% user(A) blacklisted user(B) the Timestamp
blacklist(user(A), user(B), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "to", "time" FROM blacklist',
                        row(A, B, Timestamp), [
                            types([integer, integer,integer])
                        ]).

% user(A) lurked post(P) of user(B) the Timestamp
lurk(user(A), post(P), user(B), Timestamp)  :- open_db, odbc_query(nerdz,
                        'SELECT "from", "hpid", "to", "time" FROM lurkers',
                        row(A, P, B, Timestamp), [
                            types([integer, integer, integer, integer])
                        ]).

% user(A) lurked project_post(P) of user(B) the Timestamp
lurk(user(A), post(P), user(B), Timestamp)  :- open_db, odbc_query(nerdz,
                        'SELECT "from", "hpid", "to", "time" FROM groups_ lurkers',
                        row(A, P, B, Timestamp), [
                            types([integer, integer, integer, integer])
                        ]).

% user(A) mentioned user(B) on post(P) the Timestamp
mention(user(A), user(B), post(P), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "to", "u_hpid", "time" FROM mentions WHERE "u_hpid" IS NOT NULL',
                        row(A, B, P, Timestamp), [
                            types([integer, integer, integer, integer])
                        ]).

% user(A) mentioned user(B) on project_post(P) the Timestamp
mention(user(A), user(B), project_post(P), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "to", "g_hpid", "time" FROM mentions WHERE "g_hpid" IS NOT NULL',
                        row(A, B, P, Timestamp), [
                            types([integer, integer, integer, integer])
                        ]).

% user(A) classified post(P) with Tag the Timestamp
classify(user(A), post(P), tag(Tag), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "u_hpid", "tag", "time" FROM posts_classification WHERE "u_hpid" IS NOT NULL AND
                        "from" IS NOT NULL',
                        row(A, P, Tag, Timestamp), [
                            types([integer, integer, atom, integer])
                        ]).

% user(A) classified project_post(P) with Tag the Timestamp
classify(user(A), project_post(P), tag(Tag), Timestamp) :- open_db, odbc_query(nerdz,
                        'SELECT "from", "g_hpid", "tag", "time" FROM posts_classification WHERE "g_hpid" IS NOT NULL AND
                        "from" IS NOT NULL',
                        row(A, P, Tag, Timestamp), [
                            types([integer, integer, atom, integer])
                        ]).

% user(A) classified posts with tag(t) in Timestamp
classify(user(A), tag(T), range(Start, End)) :- !, open_db, odbc_prepare(nerdz,
                        'SELECT DISTINCT pc.from, pc.tag
                        FROM posts_classification pc
                        INNER JOIN (
                            SELECT MAX("time") as time, lower("tag") as tag, "from"
                            FROM posts_classification
                            WHERE "from" IS NOT NULL
                            GROUP BY lower("tag"), "from"
                        ) AS pcgb
                        ON LOWER(pc.tag) = pcgb.tag AND pc.time = pcgb.time
                        WHERE pc."time" >= ? AND pc."time" <= ?',
                        [float > timestamp, float > timestamp], Statement, [
                            types([integer, atom])
                        ]), !,
                        odbc_execute(Statement, [Start, End], row(A, T)).

% user(A) sarched tag(T) the Timestamp
search(user(A), tag(T), range(Start, End)) :- !, open_db, odbc_prepare(nerdz,
                        'SELECT DISTINCT s."from", s."value"
                        FROM searches s
                        INNER JOIN (
                            SELECT MAX("time") as time, lower("value") as value, "from"
                            FROM searches
                           GROUP BY lower("value"), "from"
                        ) AS sgb
                        ON LOWER(s.value) = sgb.value AND s.time = sgb.time
                        WHERE s."time" >= ? AND s."time" <= ?',
                        [float > timestamp, float > timestamp], Statement, [
                            types([integer, atom])
                        ]), !,
                        odbc_execute(Statement, [Start, End], row(A, T)).

% user(A) sarched tag(T) in range(Start, End)
search(user(A), tag(T), range(Start, End)) :- open_db, odbc_prepare(nerdz,
                        'SELECT DISTINCT "from", "value" FROM searches WHERE "time" >= ? AND "time" <= ?',
                        [float > timestamp, float > timestamp], Statement,[
                            types([integer, atom])
                        ]), !, 
                        odbc_execute(Statement, [Start, End], row(A, T)).

% TC is the number of times tag(T) has been used by user(A) in range(Start, End)
count(user(A), tagged(tag(T)), range(Start, End), TC) :- open_db, odbc_prepare(nerdz,
                        'SELECT COUNT(id) FROM posts_classification WHERE lower("tag") = lower(?) AND
                        "from" = ? AND
                        "time" >= ? AND
                        "time" <= ?',
                        [varchar(45), bigint, float > timestamp, float > timestamp], Statement), !,
                        odbc_execute(Statement, [T, A, Start, End], row(TC)),
                        odbc_free_statement(Statement).

% TC is the number of times tag(T) has been searched by user(A) in range(Start, End)
count(user(A), searched(tag(T)), range(Start, End), TC) :- open_db, odbc_prepare(nerdz,
                        'SELECT COUNT(id) FROM searches WHERE lower("value") = lower(?) AND
                        "from" = ? AND
                        "time" >= ? AND
                        "time" <= ?',
                        [varchar(45), bigint, float > timestamp, float > timestamp], Statement), !,
                        odbc_execute(Statement, [T, A, Start, End], row(TC)),
                        odbc_free_statement(Statement).

% TC is the number of times user(A) commented on posts tagged with tag(T) in range(Start, End)
count(user(A), commented(tag(T)), range(Start, End), TC) :- open_db, odbc_prepare(nerdz,
                    'WITH uc(c) as (
                        SELECT COUNT(comments.hcid)
                        FROM        posts_classification
                        INNER JOIN  posts ON posts.hpid = posts_classification.u_hpid
                        INNER JOIN  comments ON posts.hpid = comments.hpid AND
                        comments.from = ? AND
                        comments.time >= ? AND
                        comments.time <= ?
                        WHERE       LOWER(posts_classification.tag) = LOWER(?)
                    ), gc(c) as (
                        SELECT COUNT(groups_comments.hcid)
                        FROM posts_classification
                        INNER JOIN groups_posts ON groups_posts.hpid = posts_classification.g_hpid
                        INNER JOIN  groups_comments ON groups_posts.hpid = groups_comments.hpid AND
                        groups_comments.from = ? AND
                        groups_comments.time >= ? AND
                        groups_comments.time <= ?
                        WHERE       LOWER(posts_classification.tag) = LOWER(?)
                    )
                    SELECT gc.c + uc.c FROM gc, uc',
                    [
                        bigint, float > timestamp, float > timestamp, varchar(45),
                        bigint, float > timestamp, float > timestamp, varchar(45)
                    ], Statement), !, 
                    odbc_execute(Statement, [
                        A, Start, End, T, 
                        A, Start, End, T
                    ], row(TC)),
                    odbc_free_statement(Statement).


% TC is the number of times an element (comment/post everywhere) has been rated positive by user(A) in range(Start, End)
count(user(A), rated_positive(tag(T)), range(Start, End), TC) :- open_db, odbc_prepare(nerdz,
                        'WITH th(c) as (
                            SELECT COUNT(thumbs.counter)
                            FROM posts_classification
                            INNER JOIN  thumbs ON thumbs.hpid = posts_classification.u_hpid AND
                            thumbs.vote = 1 AND
                            thumbs.from = ? AND
                            thumbs.time >= ? AND
                            thumbs.time <= ?
                            INNER JOIN  posts ON posts.hpid = posts_classification.u_hpid
                            WHERE       LOWER(posts_classification.tag) = LOWER(?)
                        ), gth(c) as (
                            SELECT COUNT(groups_thumbs.vote)
                            FROM posts_classification
                            INNER JOIN  groups_thumbs ON groups_thumbs.hpid = posts_classification.g_hpid AND
                            groups_thumbs.vote = 1 AND
                            groups_thumbs.from = ? AND
                            groups_thumbs.time >= ? AND
                            groups_thumbs.time <= ?
                            INNER JOIN  groups_posts ON groups_posts.hpid = posts_classification.g_hpid
                            WHERE       LOWER(posts_classification.tag) = LOWER(?)
                        ), cth(c) as (
                            SELECT COUNT(comment_thumbs.vote)
                            FROM        posts_classification
                            INNER JOIN  posts ON posts.hpid = posts_classification.u_hpid
                            INNER JOIN  comments ON posts.hpid = comments.hpid
                            INNER JOIN  comment_thumbs ON comment_thumbs.hcid = comments.hcid AND
                            comment_thumbs.vote = 1 AND
                            comment_thumbs.from = ? AND
                            comment_thumbs.time >= ? AND
                            comment_thumbs.time <= ?
                            WHERE       LOWER(posts_classification.tag) = LOWER(?)
                        ), gcth(c) as (
                            SELECT COUNT(groups_comment_thumbs.vote)
                            FROM posts_classification
                            INNER JOIN groups_posts ON groups_posts.hpid = posts_classification.g_hpid
                            INNER JOIN  groups_comments ON groups_posts.hpid = groups_comments.hpid
                            INNER JOIN  groups_comment_thumbs ON groups_comment_thumbs.hcid = groups_comments.hcid AND
                            groups_comment_thumbs.vote = 1 AND
                            groups_comment_thumbs.from = ? AND
                            groups_comment_thumbs.time >= ? AND
                            groups_comment_thumbs.time <= ?
                            WHERE       LOWER(posts_classification.tag) = LOWER(?)
                        )
                        SELECT th.c + gth.c + cth.c + gcth.c FROM th, gth, cth, gcth',
                        [
                            bigint, float > timestamp, float > timestamp, varchar(45),
                            bigint, float > timestamp, float > timestamp, varchar(45),
                            bigint, float > timestamp, float > timestamp, varchar(45),
                            bigint, float > timestamp, float > timestamp, varchar(45)
                        ], Statement), !, 
                        odbc_execute(Statement, [
                            A, Start, End, T,
                            A, Start, End, T,
                            A, Start, End, T,
                            A, Start, End, T
                        ], row(TC)),
                        odbc_free_statement(Statement).

% TC is the number of times an element (comment/post everywhere) has been rated negative by user(A) in range(Start, End)
count(user(A), rated_negative(tag(T)), range(Start, End), TC) :- open_db, odbc_prepare(nerdz,
                        'WITH th(c) as (
                            SELECT COUNT(thumbs.counter)
                            FROM posts_classification
                            INNER JOIN  thumbs ON thumbs.hpid = posts_classification.u_hpid AND
                            thumbs.vote = -1 AND
                            thumbs.from = ? AND
                            thumbs.time >=  ? AND
                            thumbs.time <= ?
                            INNER JOIN  posts ON posts.hpid = posts_classification.u_hpid
                            WHERE       LOWER(posts_classification.tag) = LOWER(?)
                        ), gth(c) as (
                            SELECT COUNT(groups_thumbs.vote)
                            FROM posts_classification
                            INNER JOIN  groups_thumbs ON groups_thumbs.hpid = posts_classification.g_hpid AND
                            groups_thumbs.vote = -1 AND
                            groups_thumbs.from = ? AND
                            groups_thumbs.time >= ? AND
                            groups_thumbs.time <= ?
                            INNER JOIN  groups_posts ON groups_posts.hpid = posts_classification.g_hpid
                            WHERE       LOWER(posts_classification.tag) = LOWER(?)
                        ), cth(c) as (
                            SELECT COUNT(comment_thumbs.vote)
                            FROM        posts_classification
                            INNER JOIN  posts ON posts.hpid = posts_classification.u_hpid
                            INNER JOIN  comments ON posts.hpid = comments.hpid
                            INNER JOIN  comment_thumbs ON comment_thumbs.hcid = comments.hcid AND
                            comment_thumbs.vote = -1 AND
                            comment_thumbs.from = ? AND
                            comment_thumbs.time >= ? AND
                            comment_thumbs.time <= ?
                            WHERE       LOWER(posts_classification.tag) = LOWER(?)
                        ), gcth(c) as (
                            SELECT COUNT(groups_comment_thumbs.vote)
                            FROM posts_classification
                            INNER JOIN groups_posts ON groups_posts.hpid = posts_classification.g_hpid
                            INNER JOIN  groups_comments ON groups_posts.hpid = groups_comments.hpid
                            INNER JOIN  groups_comment_thumbs ON groups_comment_thumbs.hcid = groups_comments.hcid AND
                            groups_comment_thumbs.vote = -1 AND
                            groups_comment_thumbs.from = ? AND
                            groups_comment_thumbs.time >= ? AND
                            groups_comment_thumbs.time <= ?
                            WHERE       LOWER(posts_classification.tag) = LOWER(?)
                        )
                        SELECT th.c + gth.c + cth.c + gcth.c FROM th, gth, cth, gcth',
                        [
                            bigint, float > timestamp, float > timestamp, varchar(45),
                            bigint, float > timestamp, float > timestamp, varchar(45),
                            bigint, float > timestamp, float > timestamp, varchar(45),
                            bigint, float > timestamp, float > timestamp, varchar(45)
                        ], Statement), !, 
                        odbc_execute(Statement, [
                            A, Start, End, T,
                            A, Start, End, T,
                            A, Start, End, T,
                            A, Start, End, T
                        ], row(TC)),
                        odbc_free_statement(Statement).


:- dynamic total_count_res/4.
% TC is the number of times user(A) used some tag betweeen range(Start, End)
count(user(A), tagged, range(Start, End), TC) :- total_count_res(user(A), tagged, range(Start, End), TC), !.
count(user(A), tagged, range(Start, End), TC) :- open_db, odbc_prepare(nerdz,
                    'SELECT COUNT(id) FROM posts_classification WHERE "from" = ? AND
                    "time" >= ? AND
                    "time" <= ?',
                    [bigint, float > timestamp, float > timestamp], Statement), !,
                    odbc_execute(Statement, [A, Start, End], row(TC)),
                    odbc_free_statement(Statement),
                    assert( total_count_res(user(A), tagged, range(Start, End), TC) ).

% TC is the number of times user(A) searched some tag betweeen range(Start, End)
count(user(A), searched, range(Start, End), TC) :- total_count_res(user(A), searched, range(Start, End), TC), !.
count(user(A), searched, range(Start, End), TC) :- open_db, odbc_prepare(nerdz,
                    'SELECT COUNT(id) FROM searches WHERE "from" = ? AND
                    "time" >= ? AND
                    "time" <= ?',
                    [bigint, float > timestamp, float > timestamp], Statement), !,
                    odbc_execute(Statement, [A, Start, End], row(TC)),
                    odbc_free_statement(Statement),
                    assert( total_count_res(user(A), searched, range(Start, End), TC) ).

% TC is the number of times user(A) commented on some classified posts between range(Start, End)
count(user(A), commented, range(Start, End), TC) :- total_count_res(user(A), commented, range(Start, End), TC), !.
count(user(A), commented, range(Start, End), TC) :- open_db, odbc_prepare(nerdz,
                    'WITH uc(c) as (
                        SELECT COUNT(comments.hcid)
                        FROM        posts_classification
                        INNER JOIN  posts ON posts.hpid = posts_classification.u_hpid
                        INNER JOIN  comments ON posts.hpid = comments.hpid AND
                        comments.from = ? AND
                        comments.time >= ? AND
                        comments.time <= ?
                    ), gc(c) as (
                        SELECT COUNT(groups_comments.hcid)
                        FROM posts_classification
                        INNER JOIN groups_posts ON groups_posts.hpid = posts_classification.g_hpid
                        INNER JOIN  groups_comments ON groups_posts.hpid = groups_comments.hpid AND
                        groups_comments.from = ? AND
                        groups_comments.time >= ? AND
                        groups_comments.time <= ?
                    )
                    SELECT gc.c + uc.c FROM gc, uc',
                    [
                        bigint, float > timestamp, float > timestamp,
                        bigint, float > timestamp, float > timestamp
                    ], Statement), !, 
                    odbc_execute(Statement, [
                        A, Start, End,
                        A, Start, End
                    ], row(TC)),
                    odbc_free_statement(Statement),
                    assert( total_count_res(user(A), commented, range(Start, End), TC) ).

% TC is the number of times user(A) rated positive some elements between range(Start, End)
count(user(A), rated_positive, range(Start, End), TC) :- total_count_res(user(A), rated_positive, range(Start, End), TC), !.
count(user(A), rated_positive, range(Start, End), TC) :- open_db, odbc_prepare(nerdz,
                    'WITH th(c) as (
                        SELECT COUNT(thumbs.counter)
                        FROM posts_classification
                        INNER JOIN  thumbs ON thumbs.hpid = posts_classification.u_hpid AND
                        thumbs.vote = 1 AND
                        thumbs.from = ? AND
                        thumbs.time >=  ? AND
                        thumbs.time <= ?
                        INNER JOIN  posts ON posts.hpid = posts_classification.u_hpid
                    ), gth(c) as (
                        SELECT COUNT(groups_thumbs.vote)
                        FROM posts_classification
                        INNER JOIN  groups_thumbs ON groups_thumbs.hpid = posts_classification.g_hpid AND
                        groups_thumbs.vote = 1 AND
                        groups_thumbs.from = ? AND
                        groups_thumbs.time >= ? AND
                        groups_thumbs.time <= ?
                        INNER JOIN  groups_posts ON groups_posts.hpid = posts_classification.g_hpid
                    ), cth(c) as (
                        SELECT COUNT(comment_thumbs.vote)
                        FROM        posts_classification
                        INNER JOIN  posts ON posts.hpid = posts_classification.u_hpid
                        INNER JOIN  comments ON posts.hpid = comments.hpid
                        INNER JOIN  comment_thumbs ON comment_thumbs.hcid = comments.hcid AND
                        comment_thumbs.vote = 1 AND
                        comment_thumbs.from = ? AND
                        comment_thumbs.time >= ? AND
                        comment_thumbs.time <= ?
                    ), gcth(c) as (
                        SELECT COUNT(groups_comment_thumbs.vote)
                        FROM posts_classification
                        INNER JOIN groups_posts ON groups_posts.hpid = posts_classification.g_hpid
                        INNER JOIN  groups_comments ON groups_posts.hpid = groups_comments.hpid
                        INNER JOIN  groups_comment_thumbs ON groups_comment_thumbs.hcid = groups_comments.hcid AND
                        groups_comment_thumbs.vote = 1 AND
                        groups_comment_thumbs.from = ? AND
                        groups_comment_thumbs.time >= ? AND
                        groups_comment_thumbs.time <= ?
                    )
                    SELECT th.c + gth.c + cth.c + gcth.c FROM th, gth, cth, gcth',
                    [
                        bigint, float > timestamp, float > timestamp,
                        bigint, float > timestamp, float > timestamp,
                        bigint, float > timestamp, float > timestamp,
                        bigint, float > timestamp, float > timestamp
                    ], Statement), !, 
                    odbc_execute(Statement, [
                        A, Start, End,
                        A, Start, End,
                        A, Start, End,
                        A, Start, End
                    ], row(TC)),
                    odbc_free_statement(Statement),
                    assert( total_count_res(user(A), rated_positive, range(Start, End), TC) ).

% TC is the number of times user(A) rated negative some elements between range(Start, End)
count(user(A), rated_negative, range(Start, End), TC) :- total_count_res(user(A), rated_negative, range(Start, End), TC), !.
count(user(A), rated_negative, range(Start, End), TC) :- open_db, odbc_prepare(nerdz,
                    'WITH th(c) as (
                        SELECT COUNT(thumbs.counter)
                        FROM posts_classification
                        INNER JOIN  thumbs ON thumbs.hpid = posts_classification.u_hpid AND
                        thumbs.vote = -1 AND
                        thumbs.from = ? AND
                        thumbs.time >=  ? AND
                        thumbs.time <= ?
                        INNER JOIN  posts ON posts.hpid = posts_classification.u_hpid
                    ), gth(c) as (
                        SELECT COUNT(groups_thumbs.vote)
                        FROM posts_classification
                        INNER JOIN  groups_thumbs ON groups_thumbs.hpid = posts_classification.g_hpid AND
                        groups_thumbs.vote = -1 AND
                        groups_thumbs.from = ? AND
                        groups_thumbs.time >= ? AND
                        groups_thumbs.time <= ?
                        INNER JOIN  groups_posts ON groups_posts.hpid = posts_classification.g_hpid
                    ), cth(c) as (
                        SELECT COUNT(comment_thumbs.vote)
                        FROM        posts_classification
                        INNER JOIN  posts ON posts.hpid = posts_classification.u_hpid
                        INNER JOIN  comments ON posts.hpid = comments.hpid
                        INNER JOIN  comment_thumbs ON comment_thumbs.hcid = comments.hcid AND
                        comment_thumbs.vote = -1 AND
                        comment_thumbs.from = ? AND
                        comment_thumbs.time >= ? AND
                        comment_thumbs.time <= ?
                    ), gcth(c) as (
                        SELECT COUNT(groups_comment_thumbs.vote)
                        FROM posts_classification
                        INNER JOIN groups_posts ON groups_posts.hpid = posts_classification.g_hpid
                        INNER JOIN  groups_comments ON groups_posts.hpid = groups_comments.hpid
                        INNER JOIN  groups_comment_thumbs ON groups_comment_thumbs.hcid = groups_comments.hcid AND
                        groups_comment_thumbs.vote = -1 AND
                        groups_comment_thumbs.from = ? AND
                        groups_comment_thumbs.time >= ? AND
                        groups_comment_thumbs.time <= ?
                    )
                    SELECT th.c + gth.c + cth.c + gcth.c FROM th, gth, cth, gcth',
                    [
                        bigint, float > timestamp, float > timestamp,
                        bigint, float > timestamp, float > timestamp,
                        bigint, float > timestamp, float > timestamp,
                        bigint, float > timestamp, float > timestamp
                    ], Statement), !,
                    odbc_execute(Statement, [
                        A, Start, End,
                        A, Start, End,
                        A, Start, End,
                        A, Start, End
                    ], row(TC)),
                    odbc_free_statement(Statement),
                    assert( total_count_res(user(A), rated_negative, range(Start, End), TC) ).

