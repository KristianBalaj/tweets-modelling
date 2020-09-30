CREATE TABLE if not exists accounts(
    id bigint primary key,
    screen_name varchar(200),
    name varchar(200),
    description text,
    followers_count integer,
    friends_count integer,
    statuses_count integer
);

CREATE TABLE if not exists countries(
    id serial primary key,
    code varchar(2) unique,
    name varchar(200)
);

CREATE TABLE if not exists hashtags(
    id serial primary key,
    value text unique
);

CREATE TABLE if not exists tweets(
    id varchar(20) primary key,
    content text,
    location geometry(point, 4326),
    retweet_count integer,
    favorite_count integer,
    happened_at timestamptz,
    author_id bigint,
    country_id integer,
    parent_id varchar(20)
);

CREATE TABLE if not exists tweet_mentions(
    id serial primary key,
    account_id bigint,
    tweet_id varchar(20)
);

CREATE TABLE if not exists tweet_hashtags(
    id serial primary key,
    hashtag_id integer,
    tweet_id varchar(20)
);