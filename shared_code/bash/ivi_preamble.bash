# This bash file needs to be sourced because it is meant to change the caller's
# enviroment. All "local" variables are prefixed by "tmp_" (none for now). This
# file sets:
#   IVI_VERBOSE
#   IVI_VERBOSE_2
#   IVI_VERBOSE_3
#   VERBOSE_ECHO
#   plumbing 

# Obscure bash code to set IVI_VERBOSE to 1 if it is set but NULL, 0 if unset,
# and retain its value otherwise. Don't overthink bash syntax---it is not good
# for you. 
#
# The default value of IVI_VERBOSE set here can be overriddent by the '-v' flag. 
#
export IVI_VERBOSE=${IVI_VERBOSE-0}
if [[ "${IVI_VERBOSE}" == "" ]]; then
    export IVI_VERBOSE="1"
fi 

# Using more verbose should be rare, but I had issues debugging the robustness
# of rsync in our use cases. 
#
export IVI_VERBOSE_2=${IVI_VERBOSE_2-0}
if [[ "${IVI_VERBOSE_2}" == "" ]]; then
    export IVI_VERBOSE_2="1"
fi 

export IVI_VERBOSE_3=${IVI_VERBOSE_3-0}
if [[ "${IVI_VERBOSE_3}" == "" ]]; then
    export IVI_VERBOSE_3="1"
fi 

# Default. We could use "|&" but this assumes bash 4.0 or above. 
plumbing='2>&1 | tee -a'

for var in "$@"
do
    if [[ "$var" == "-q" ]]; then
        # We could use "&>>" but this assumes bash 4.0 or above. 
        plumbing='2>&1 >>'
    elif [[ "$var" == "-v" ]]; then
        export IVI_VERBOSE="1"
    elif [[ "$var" == "-v2" ]]; then
        export IVI_VERBOSE="1"
        export IVI_VERBOSE_2="1"
    elif [[ "$var" == "-v3" ]]; then
        export IVI_VERBOSE="1"
        export IVI_VERBOSE_3="1"
    fi 
done

if [[ "${IVI_VERBOSE}" == "1" ]]; then
    export VERBOSE_ECHO="echo IVB: "
else 
    export VERBOSE_ECHO=": "
fi

# Since this is sourced, we should unset temporary variables
# (None for now).

