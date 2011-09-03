-- Copyright (c) 2005 -- 2011, Christopher Mark Gore,
-- All rights reserved.
--
-- 8729 Lower Marine Road, Saint Jacob, Illinois 62281 USA.
-- Web: http://cgore.com
-- Email: cgore@cgore.com
--
-- This creates the trading database.
-- This assumes PostgreSQL 8.2 or better.

create user trading with login encrypted password 'stupidhead';
create database trading with owner = trading;
\connect trading trading;
create table daily_trade_data (
  ticker varchar(16),
  date date,
  opening_price money,
  high_price money,
  low_price money,
  closing_price money,
  trading_volume integer,
  adjusted_closing_price money,
  primary key (ticker, date));
create table tickers (
  ticker varchar(16) primary key,
  description varchar(64),
  composite boolean,
  components varchar(16)[]);
