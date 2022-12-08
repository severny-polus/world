main.js: src/
	elm make src/Main.elm --optimize --output=main.js

geodata.js: geodata/land.geo.json
	echo 'const land = `' > geodata.js
	jq -c . < geodata/land.geo.json >> geodata.js
	echo '`' >> geodata.js
