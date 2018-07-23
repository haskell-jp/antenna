#!/bin/bash

echo "slack-only-on-error = $SLACK_ONLY_ON_ERROR" >> /etc/ofelia/config.ini
echo "slack-webhook = $SLACK_WEBHOOK" >> /etc/ofelia/config.ini
exec "$@"
