-- Copyright (c) 2005 -- 2011, Christopher Mark Gore, all rights reserved.
-- 8729 Lower Marine Road, Saint Jacob, Illinois 62281 USA.
-- Phone: +1 (573) 452-3216
-- Web: http://www.cgore.com
-- Email: cgore@cgore.com

-- This creates the trading database.
-- This assumes PostgreSQL 9.1 or better.

create user livermore with login encrypted password 'stupidhead';
create database livermore with owner = livermore;
\connect livermore

create table fungibles (
  id integer primary key not null,
  ticker varchar(16),
  description varchar(64)
);
alter table fungibles owner to livermore;

create table records (
  id integer primary key not null,
  ticker_id integer not null,
  date date,
  opening_price money,
  high_price money,
  low_price money,
  closing_price money,
  trading_volume integer,
  adjusted_closing_price money
);
alter table records owner to livermore;
