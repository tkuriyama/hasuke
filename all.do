
stack install >&2

echo "\nConverting Json to Html...\n" >&2
for f in scores/Json/*.json;
do
hasuke-make $f >&2;
done

echo "\nMoving generated Html files...\n" >&2
mv -v scores/Json/*.html scores/Generated/ >&2