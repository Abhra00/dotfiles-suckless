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

# Functions needed for !! and !$
function __history_previous_command
  switch (commandline -t)
  case "!"
    commandline -t $history[1]; commandline -f repaint
  case "*"
    commandline -i !
  end
end

function __history_previous_command_arguments
  switch (commandline -t)
  case "!"
    commandline -t ""
    commandline -f history-token-search-backward
  case "*"
    commandline -i '$'
  end
end

# The bindings for !! and !$
if [ "$fish_key_bindings" = "fish_vi_key_bindings" ];
  bind -Minsert ! __history_previous_command
  bind -Minsert '$' __history_previous_command_arguments
else
  bind ! __history_previous_command
  bind '$' __history_previous_command_arguments
end

