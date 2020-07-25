#!/bin/sh
# Override file for Protonmail
ssltype=None
imap=127.0.0.1
iport=1143
smtp=127.0.0.1
sport=1025

out "Getting protonmail bridge fingerprint"
fingerprint=$(msmtp --serverinfo --host=127.0.0.1 --port=1025 --tls --tls-certcheck=off |
                  sed -n 's/^SHA256: //p')

set_msmtp() {
    [ -f "$config_home/msmtp/config" ] || msmtp_header

    cat <<EOF >> "$config_home/msmtp/config"
account $title
host $smtp
port $sport
from $fulladdr
user $login
passwordeval "$password_command"
tls_fingerprint $fingerprint/
$starttlsoff
# End profile

EOF
}
