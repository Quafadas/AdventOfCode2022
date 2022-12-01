# AdventOfCode2022
Attempt Advent of Code 2022


Pasted into the terminal, to setup coursier and scala cli
```
curl -fL https://github.com/coursier/launchers/raw/master/cs-x86_64-pc-linux.gz | gzip -d > cs
chmod +x cs
./cs setup
./cs launch --fork almond:0.13.2 --scala 3.1.3 -- --install --force
curl -sSLf https://virtuslab.github.io/scala-cli-packages/scala-setup.sh | sh
scala-cli setup-ide .
```