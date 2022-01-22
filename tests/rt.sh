#!/bin/bash

PRINT='print'
FMT='fmt'

function test_rsfmt() {
    ty=$1
    src=$2

    expected_file=$(dirname ${src})/$(basename ${src} .rs)
    if [[ "${ty}" == "${PRINT}" ]]; then
        expected_file=${expected_file}.rsp
    else
        expected_file=${expected_file}.rsfmt
    fi

    if [[ ! -f ${expected_file} ]]; then
        #echo "[${expected_file}] not found"
        #exit 1
        continue
    fi

    if [[ "${ty}" == "${PRINT}" ]]; then
        actual=$(rsfmt -p ${src})
    else
        actual=$(rsfmt ${src})
    fi
    result=$?
    if [[ ${result} -ne 0 ]]; then
        exit ${result}
    fi

    expected=$(cat ${expected_file})
    if [[ "${actual}" != "${expected}" ]]; then
        echo "${ty}"
        actual_file="${TMPDIR}/$(basename ${expected_file})"
        echo "${actual}" > ${actual_file}
        diff ${actual_file} ${expected_file}
        exit 1
    fi 
}

function test_print() {
    test_rsfmt "${PRINT}" $1
}

function test_fmt() {
    test_rsfmt "${FMT}" $1
}

path=$1
if [[ -z ${path} ]]; then
    path='.'
fi

for src in $(find ${path} -type f -name '*.rs')
do
    echo ${src}
    test_print ${src}
    test_fmt ${src}
done
