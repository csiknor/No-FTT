#!/bin/sh

# Navigate to the source code directory
cd /tmp || exit 1

# Compile the Elm code to optimized JavaScript
elm make src/Main.elm --optimize --output=elm.js

# Minify the generated JavaScript using a basic minifier (optional, can use other tools)
uglifyjs elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output elm.min.js

# Ensure proper permissions
chmod 644 elm.min.js

# Replace the script source in index.html
sed -i 's|elm.js|elm.min.js|' index.html