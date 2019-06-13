# Intro

* Install cockroachdb (for mac, brew install cockroach)
* Start Cockroach with cockroach start --insecure
* Install postgres utilities (brew install postgres)
* Create database: `psql -h 127.0.0.1 -p 26257 -U root -d system -c "CREATE DATABASE netcomp_sample;"
`
* You can now start netcomp_sample, first time it will create database
* You can use utilities in netcomp_sample_srv_pgsql: insert, get, drop 
