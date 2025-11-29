#!/bin/sh
DOMAIN=craigswank.com
HOST=*
AUTH="REDACTED"
CURRENT_IP=$(curl -s myip.dnsomatic.com)
ZONE_ID=e616e486d5c15d1b27920a9ee7abd13e
CURRENT_DDNS=$(dig +short $HOST.$DOMAIN @resolver1.opendns.com)

echo "current ddns record = $CURRENT_DDNS"
echo "current public IP address = $CURRENT_IP"

if [ "$CURRENT_DDNS" == "$CURRENT_IP" ]; then
    echo "ddns is up-to-date"
    exit 0
fi

curl -XPUT https://api.cloudflare.com/client/v4/zones/${ZONE_ID}/dns_records/d91386079b6cd62a104c5beacdf12c36 \
     -H 'Content-Type: application/json' \
     -H 'Authorization: Bearer '$AUTH'' \
     -d '{"content": "'$CURRENT_IP'", "name": "craigswank.com", "proxied": false, "type": "A", "ttl": 3600}'

curl -XPUT https://api.cloudflare.com/client/v4/zones/${ZONE_ID}/dns_records/889b994d80cc16b47a12d08775af0928 \
     -H 'Content-Type: application/json' \
     -H 'Authorization: Bearer '$AUTH'' \
     -d '{"content": "'$CURRENT_IP'", "name": "*.craigswank.com", "proxied": false, "type": "A", "ttl": 3600}'
