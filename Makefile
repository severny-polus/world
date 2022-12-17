all: main.js

main.js: $(wildcard src/*) flags.js
	elm make src/Main.elm --optimize --output=

unoptimized: $(wildcard src/*) flags.js
	elm make src/Main.elm --output=main.js

flags = flags.js
geodata = landWithoutAntarctica antarctica

flags.js: open_flags $(geodata) close_flags

open_flags:
	echo 'const flags = {' > $(flags)

$(geodata): %: geodata/%.geo.json
	echo "  $@: \`" >> $(flags)
	jq -c . < geodata/$@.geo.json >> $(flags)
	echo "  \`", >> $(flags)

close_flags:
	echo '  end: ""' >> $(flags)
	echo '}' >> $(flags)

