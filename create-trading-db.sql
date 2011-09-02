-- Copyright (C) 2008, all rights reserved.
-- Christopher Mark Gore.
-- 8729 Lower Marine Road, Saint Jacob, Illinois 62281.
-- Phone: (573) 452-3216.
-- E-Mail: <chris-gore@earthlink.net>.
-- WWW: <http://www.cgore.com>.
--
-- This creates the trading database.
-- This assumes PostgreSQL 8.2 or better.
--
-- $Date: 2008-05-11 23:52:56 -0500 (Sun, 11 May 2008) $
-- $HeadURL: file:///var/svn/trading/trunk/create-trading-db.sql $
-- $Revision: 469 $
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
