#!/bin/bash

dir=$1
if [[ -z ${dir} ]]; then
    dir='.'
fi

for src in $(find ${dir} -type f -name '*.rs')
do
    expected_rsfmt=$(dirname ${src})/$(basename ${src} .rs).rsfmt
    if [[ ! -f ${expected_rsfmt} ]]; then
        continue
    fi

    echo ${src}
    actual=$(rsfmt ${src})
    result=$?
    if [[ ${result} -ne 0 ]]; then
        exit ${result}
    fi

    expected=$(cat ${expected_rsfmt})
    if [[ "${actual}" != "${expected}" ]]; then
        actual_rsfmt="${TMPDIR}/$(basename ${expected_rsfmt})"
        echo "${actual}" > ${actual_rsfmt}
        diff ${actual_rsfmt} ${expected_rsfmt}
        exit 1
    fi 
done
