#!/bin/bash

set -x

echo "Start"

# Call original script
/entrypoint.sh couchbase-server &

sleep 15

CB_USER=gianluca
CB_PASS=Test1234

CB_URL=127.0.0.1:8091
CB_BUCKET_NAME=test

CB_USER_PWD=qwerty12345
CB_USER_NAME=honest
echo "Creating user"

couchbase-cli cluster-init --services data,index,query  --cluster-username="${CB_USER}" --cluster-password="${CB_PASS}" --cluster-ramsize=512 --cluster-index-ramsize=256 --cluster-fts-ramsize=256
while [ $? -ne 0 ] ; do
sleep 3
couchbase-cli cluster-init --services data,index,query  --cluster-username="${CB_USER}" --cluster-password="${CB_PASS}" --cluster-ramsize=512 --cluster-index-ramsize=256 --cluster-fts-ramsize=256
done 

sleep 1
couchbase-cli bucket-create -c $CB_URL -u "${CB_USER}" -p "${CB_PASS}" --bucket=$CB_BUCKET_NAME --bucket-type=couchbase --bucket-ramsize=256 --enable-flush=1

sleep 3

couchbase-cli user-manage -c 127.0.0.1:8091 -u "${CB_USER}" -p "${CB_PASS}" --set --rbac-username $CB_USER_NAME --rbac-password $CB_USER_PWD --rbac-name $CB_USER_NAME --roles bucket_full_access[$CB_BUCKET_NAME] --auth-domain local

sleep 15
curl -i -u $CB_USER:$CB_PASS -X POST http://$CB_URL/settings/indexes -d 'storageMode=forestdb'

sleep 3

until cbq  --exit-on-error --script 'CREATE INDEX `testIndex` ON `'"$CB_BUCKET_NAME"'` (`name`) WHERE (`type` = "query");' -u "${CB_USER}" -p "${CB_PASS}" -c $CB_URL
do 
sleep 3
done


echo "**********************"
echo "Couchbase Ready To Go!"
echo "**********************"
if [ -z ${NEXMO_CS_DB_EXIT} ]; then wait `pgrep couchbase*` ; fi
