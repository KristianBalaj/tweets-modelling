# Tweets data

This tool processes `zipped` files in `.jsonl` format containing tweets' data (The tool is configured to process all data files with the suffix `.jsonl.gz` in the `./data` directory).

Official documentation of the tweet json can be found here [here](https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/overview/tweet-object).

The test data I've been using were 41 `.jsonl.gz` files of size 5.65 GB altogether. After unpacking, the data has 40 GB.
The code now uses the `streaming` library, but materializes the chunks that are imported.
The chunk size is hardcoded to 5000 tweets.

**Disclaimer**: The data has more fields than finally used for parsing (parsed fields can be seen in the models).

## Running

The following is the usage:

`tweets-import --import [Postgres,Elastic] --connection-file FILE`

There are example connection files in the root of the repository:
- `elastic_connection.json`
- `pg_connection.json`

## Elastic search import

This is importing the data from the `.jsonl` files right into the ElasticSearch.
In time of coding this, the last available version of Elastic was used (i.e. 7.10).
It is using the Elastic [bulk API](https://www.elastic.co/guide/en/elasticsearch/reference/7.10/docs-bulk.html). 
I'm using the index bulk command.
The Elastic nodes are defined in the docker-compose (I'm using 3 nodes).

The data is being imported into a single index called `tweets` using the mapping defined in the `./esTweetIndexMapping.json` file.
I'm using 3 shards and 1 replica setup for this index.

The imported data (mentioned in the beginning) has total of 9.1 GB inside the Elastic.

## PosgreSQL import

All the logic for this import is inside the `Postgres` module.
The data can be imported to a `PostgreSQL` database according to the following model:

![Database model](images/model.jpg)
