#!/bin/bash
. $PWD/envs || . $PWD/../envs

echo "Compiling..."
make -C $CURRENT_DIR || exit 1

echo "Reloading modules..."

erl -pa $CURRENT_DIR \
    -eval "nklib_reloader:remote_reload(\"${APP}_${VSN}@${FQDN}\", \"nklib\")." \
    -eval "nklib_reloader:remote_reload(\"${APP}_${VSN}@${FQDN}\", \"nkpacket\")." \
    -eval "nklib_reloader:remote_reload(\"${APP}_${VSN}@${FQDN}\", \"nkrest\")." \
    -eval "nklib_reloader:remote_reload(\"${APP}_${VSN}@${FQDN}\", \"nkserver\")." \ # Include other dependencies if needed
    -eval "nklib_reloader:remote_reload(\"${APP}_${VSN}@${FQDN}\", \"${APP}\")." \
    -s init stop -noshell \
    -name reloader@127.0.0.1 \
    -setcookie nk \
    -pa $CURRENT_DIR/_build/default/lib/nklib/ebin \
    -pa $CURRENT_DIR/_build/default/lib/$APP/ebin

echo "DONE!"
