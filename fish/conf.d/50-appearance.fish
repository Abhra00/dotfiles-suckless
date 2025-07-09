#  ┏┓┏┓  ┏┓┏┓┏┓┏┓┏┓┳┓┏┓┳┓┏┓┏┓
#  ┣┓┃┫━━┣┫┃┃┃┃┣ ┣┫┣┫┣┫┃┃┃ ┣ 
#  ┗┛┗┛  ┛┗┣┛┣┛┗┛┛┗┛┗┛┗┛┗┗┛┗┛
#                            

# Autocomplete and highlight colors 
set -g fish_color_normal 	      brwhite
set -g fish_color_autosuggestion      brblack
set -g fish_color_command 	      brgreen
set -g fish_color_error 	      brred
set -g fish_color_param 	      brcyan



# Prompt (Download pure prompt using fundle)
fundle plugin 'pure-fish/pure'
fundle init

# Pure prompt customization

# Modules
set -g pure_show_jobs true

# Colors
set -g pure_color_primary cyan
