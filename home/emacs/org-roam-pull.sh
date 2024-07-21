source ~/Desktop/nixos/home/emacs/.env

printf "Preparing to PULL orgfiles from server\n"
printf "Local files may be deleted to match server config\n\n"

rsync -avz ${REMOTE_USER}@${REMOTE_IP}:~/Desktop/orgfiles/ ~/Desktop/orgfiles/
