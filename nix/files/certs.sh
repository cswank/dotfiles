#!/bin/sh
export CF_Zone_ID="e616e486d5c15d1b27920a9ee7abd13e"
export CF_Token="REDACTED"
acme.sh --log --issue --dns dns_cf --keylength ec-384 -d '*.craigswank.com'
cp '/root/.acme.sh/*.craigswank.com_ecc/fullchain.cer' /home/proxy/.proxy/fullchain.cer
cp '/root/.acme.sh/*.craigswank.com_ecc/*.craigswank.com.key' /home/proxy/.proxy/server.key
systemctl restart proxy
