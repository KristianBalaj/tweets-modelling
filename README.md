# Tweets data PostgreSQL import

This tool processes `zipped` files in `.jsonl` format containing tweets' data. (The tool is configured to process all data files with the suffix `.jsonl.gz` in the `./data` directory)

Official documentation of the tweet json can be found here [here](https://developer.twitter.com/en/docs/twitter-api/v1/data-dictionary/overview/tweet-object).

This data is imported to a `PostgreSQL` database according to the following model:

![Database model](images/model.jpg)


## Notes

**Space complexity** of the program is **asymptotically equal** to ***O(1)***.
The insertion into the database is done via bulk insert. The bulk insert is done by chunks to preserve the constant space complexity. The chunk size is set by the `Database.Database.insertChunkSize` value.
