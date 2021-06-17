# CHIP-8 emulator implemented in (Chicken) Scheme

Work in Progress

# Building
```sh
source env.sh
chicken-install sdl2
chicken-install args
chicken-install format
chicken-install memory
chicken-install defstruct
```

```sh
chicken-csc main.scm
./main --scale 8 INVADERS
```
