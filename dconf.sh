#!/bin/bash
dconf load / <<'EOF'
[org/gnome/desktop/interface]
font-name='Source Sans Pro:style=Regular 11'
icon-theme='Adwaita'
gtk-theme='Adwaita-dark'
EOF
