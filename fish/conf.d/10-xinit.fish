#  ┓┏┓  ┏┓┏┓┳┳┓┳┏┳┓
#  ┃┃┫━━ ┃┃ ┃┃┃┃ ┃ 
#  ┻┗┛  ┗┛┗┛┻┛┗┻ ┻ 
#                  


# Start X server if on tty1 and X is not already running

if test (tty) = "/dev/tty1"
    and not pgrep -x Xorg > /dev/null
    exec startx $XINITRC
end
