#df -h | grep -e "Filesystem" -e "gxxiao"

#myquota -g gxxiao 

source /u/local/Modules/default/init/modules.sh
source ~/.bashrc

myquota -g gxxiao | awk -v date="$(date)" '{if ($1=$1) print date" "$0}' | grep 318000000 | grep -v Filesystem


