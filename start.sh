#!/bin/sh

erl -make || exit 1

if [ $# -ne 2 ]; then
    echo "Usage: $0 <jid> <password>"
    echo
    echo "Beware: exmpp does not yet evaluate SRV records (edit Host in client:init/1)"
    echo
    exit 1
fi

erl -sname sub2text +K true -smp auto \
    -pa ebin \
    -sub2text jid "\"$1\"" -sub2text password "\"$2\"" \
    -s sub2text_app

