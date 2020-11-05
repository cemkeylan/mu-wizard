# -*- mode: sh; -*-
# Override file for Gmail
msg_behavior=delete
imap=imap.gmail.com
iport=993
smtp=smtp.gmail.com
sport=587
