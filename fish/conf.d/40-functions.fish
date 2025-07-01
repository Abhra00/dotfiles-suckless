#  ┏┓┏┓  ┏┓┳┳┳┓┏┓┏┳┓┳┏┓┳┓┏┓
#  ┃┃┃┫━━┣ ┃┃┃┃┃  ┃ ┃┃┃┃┃┗┓
#  ┗╋┗┛  ┻ ┗┛┛┗┗┛ ┻ ┻┗┛┛┗┗┛
#                          

# Yazicd functions
function yy
	set tmp (mktemp -t "yazi-cwd.XXXXXX")
	yazi $argv --cwd-file="$tmp"
	if read -z cwd < "$tmp"; and [ -n "$cwd" ]; and [ "$cwd" != "$PWD" ]
		builtin cd -- "$cwd"
	end
	rm -f -- "$tmp"
end

# Wrapper of yy
function __yy_wrapper
    commandline -r "yy"
    commandline -f execute
end

# YY binding
for mode in insert default visual
    bind -M $mode \co '__yy_wrapper'
end
