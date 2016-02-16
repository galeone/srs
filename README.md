Social Recommender System
=========================

Social Recommender System (SRS) gives Top-K recommendation of users, to users.

It performs statistical analysis on social network data and store the values on its own Postgresql DB.

This task should be performed occasionally, better if performed on a regular time interval (but works well even with non regular intervals).

Don't worry, there's a cron script for that.

SRS executable acts like a server, you must ask recommendation with a client (telnet or something capable to send ASCII is good).

After collecting enough informations SRS will give users recommendations.

Installation
============

# Requirements

- unixodbc
- swi-prolog >= 7
- postgresql >= 9.4
- sudo or root access

# Installation

## Linux

Under GNU/Linux you can install and setup SRS in one step

```bash
$ ./install
```

Into the `misc/systemd` folder, you can find a systemd service file that you can use to start srs.service and/or enable it on boot.

To have up-to-date recommendations, you should put `cron.hourly/srs-update` into your local
`/etc/cron.hourly/` folder or whatever have the same effect with your cron daemon.

[Cronie](https://fedorahosted.org/cronie/) uses the `cron.<when>` folder. I recommend you (eheh) not to make mess with the cronfile and use these folders instead.

## Microsoft Windows

Under Windows you can't :'D
