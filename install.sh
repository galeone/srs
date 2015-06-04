#!/usr/bin/env bash

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
CREATE TABLE topic_user_value (
    topic character varying(70) NOT NULL,
    "user" bigint NOT NULL,
    value real DEFAULT 0.0 NOT NULL
);

CREATE TABLE srs_data (
    last_update timestamp(0) without time zone NOT NULL
);

INSERT INTO srs_data(last_update) VALUES(to_timestamp(0));
EOF

psql -d srs -U srs < $tmp
rm $tmp
