% Import the required database and ask the engine to do the work
% Do you want to use srs with an other social network? Define a module that exports
% the same predicate of nerdz and replace the line below
:- use_module(nerdz).

% populating the srs database, computing the cost for each item

open_db  :- odbc_connect(srs, _, [ alias(db), open(once) ]).

% Computa the relation value between topics and users.
% Its a cartesian product (topics X users). To reduce srs database size
% we store only existing relations (user(A) talks about tag(T)).
% If we don't find any match, the value is 0.
%
% TODO: find a good formula to calculate the value.
% TODO: use different tables to store different informations 

close_db :- odbc_disconnect(db).
