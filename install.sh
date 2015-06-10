#!/usr/bin/env bash

######################################
# REQUIRES AT LEAST swi-prolog 7.X.X #
######################################

cd /tmp
mkdir srs
cd srs
# Can't use the latest version. See:
# http://www.postgresql.org/message-id/1420793104.20163.7.camel@uniklinik-freiburg.de
wget https://ftp.postgresql.org/pub/odbc/versions/src/psqlodbc-09.03.0300.tar.gz
tar xvf psqlod*
cd psql*
./configure
make
make install

echo "[nerdz]
Driver=PostgreSQL Unicode
Servername=localhost
Port=5432
Database=nerdz
Username=nerdz
Password=nerdz
Protocol=9.3.4
ReadOnly=yes
Debug=1" >> /etc/odbc.ini

echo "[PostgreSQL Unicode]
Description=PostgreSQL ODBC driver (Unicode version)
Driver=/usr/local/lib/psqlodbcw.so
Debug=0
CommLog=1
UsageCount=1" >> /etc/odbcinst.ini

cd ..
rm -rf psqlod*

createuser -U postgres -D -R -S srs
createdb -U postgres -O srs srs

tmp=$(mktemp)

cat <<EOF > $tmp
CREATE TABLE user_topic_frequencies(
    "topic"     character varying(70) NOT NULL,
    "update_date" timestamp(0) without time zone NOT NULL,
    "user"      bigint NOT NULL,
    "tagged" real DEFAULT 0.0 NOT NULL,
    "rated_positive" real DEFAULT 0.0 NOT NULL,
    "rated_negative" real DEFAULT 0.0 NOT NULL,
    "commented" real DEFAULT 0.0 NOT NULL,
    "searched" real DEFAULT 0.0 NOT NULL
);

CREATE TABLE srs_data (
    "key" varchar(50) primary key not null,
    "timestamp" timestamp(0) without time zone,
    "value" varchar(100)
);

INSERT INTO srs_data("key", "timestamp") VALUES('LAST_UPDATE', to_timestamp(0));
EOF

psql -d srs -U srs < $tmp
rm $tmp
