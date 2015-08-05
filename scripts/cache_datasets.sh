#!/bin/bash
# Runs the cache_datasets R script, writes the results to a log file, and sends each line of the log file
# to LangCog's Slack under #metalab-log channel

cd /home/ubuntu/metalab/
mkdir -p log
LOGDATE=$(date +%Y-%m-%dT%H:%M:%S)
LOGFILE=log/cache_datasets_$LOGDATE
Rscript scripts/cache_datasets.R > $LOGFILE

SLACKPOSTURL=https://hooks.slack.com/services/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

while read -r line
do
    ENTRY=$line
    curl -X POST --data-urlencode 'payload={"channel": "#metalab-log", "username": "cachedatasetbot", "icon_emoji": ":dog:", "text": "'"$ENTRY"'"}' $SLACKPOSTURL
done < $LOGFILE

git add data/
git commit -m "datasets cached by bot"
git push origin master
