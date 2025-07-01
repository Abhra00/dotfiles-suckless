#  ┏┓┳┏┓┓┏  ┏┓┏┓┳┓┏┓┳┏┓
#  ┣ ┃┗┓┣┫━━┃ ┃┃┃┃┣ ┃┃┓
#  ┻ ┻┗┛┛┗  ┗┛┗┛┛┗┻ ┻┗┛
#                      


# Load environment variables
source ~/.config/fish/conf.d/00-env.fish

# Start graphical session if needed
source ~/.config/fish/conf.d/10-xinit.fish

# Aliases
source ~/.config/fish/conf.d/20-aliases.fish

# Behaviour
source ~/.config/fish/conf.d/30-behaviour.fish

# Functions
source ~/.config/fish/conf.d/40-functions.fish


# FZF
fzf --fish | source

# ZOXIDE
zoxide init fish | source
