# Leapfrog voice decoder

Renders the "LFC" voice streams as WAV files. Used by the Leapster, the Leapster
LMAX, the Leapster 2, and LeapPad.

Note that is barely imperfect still as some of the values are off by one or two
in [tables.ml](lib/tables.ml). Still working on that!

Fun trivia: the noise table in the decoder was generated with bsd's `random()`
with the default seed!

## Quick start

Install Docker, then clone the repo and build the image:

```bash
git clone https://github.com/nathanfarlow/leapfrog-voice-decoder
cd leapfrog-voice-decoder
docker build -t lfc .
```

Linux and macOS:

```bash
# Render all voice streams in a cart to a directory. These commands assume that
# you have a `cart.bin` in the current working directory.
docker run --rm --user "$(id -u):$(id -g)" -v "$PWD:$PWD" -w "$PWD" lfc \
  convert-all -rom cart.bin -output-dir wavs

# Render just one stream
docker run --rm --user "$(id -u):$(id -g)" -v "$PWD:$PWD" -w "$PWD" lfc \
  convert -rom cart.bin -offset 0x2bde46 -output-file foo.wav
```

Windows, via powershell:

```powershell
docker run --rm -v "${PWD}:/data" -w /data lfc convert-all -rom cart.bin -output-dir wavs
```
