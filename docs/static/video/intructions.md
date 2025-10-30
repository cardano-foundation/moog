# **asciinema**

## Recording video in `cast` format

```bash
asciinema rec video.cast

# terminal activities that are recorded
# ....
# Ctrl-x or exit to stop recording
```

## Playing the recorded video with `cast` format

```bash
asciinema play video.cast
```

## Playing the recorded video using `cast` format with speed

```bash
asciinema play --speed 2 video.cast
```

More info [here](https://docs.asciinema.org/getting-started/#generating-a-gif)

# **agg**

## Producing `gif` from `cast`

```bash
agg video.cast video.gif
```

## Producing `gif` from `cast` accelerated

```bash
agg --speed 5 video.cast video.gif
```

## Producing `gif` from `cast` accelerated with better resolution and selected theme (with bigger gif)

```bash
agg --theme monokai --font-size 30 --speed 5 video.cast video.gif
```

More info [here](https://docs.asciinema.org/manual/agg/usage/)

# **FFmpeg**

## Producing `mp4` from `gif`

```bash
ffmpeg -i video.gif -b:v 0 -crf 25 -f mp4 -vcodec libx264 -pix_fmt yuv420p video.mp4
```

**Note** If there is error `height not divisible by 2 (2044x1075)` during mapping to `mp4` one should try

```bash
ffmpeg -i video.gif -vf "crop=trunc(iw/2)*2:trunc(ih/2)*2" -b:v 0 -crf 25 -f mp4 -vcodec libx264 -pix_fmt yuv420p video.mp4
```

## Producing `webm` from `gif`

```bash
ffmpeg -i video.gif -c vp9 -b:v 0 -crf 41 video.webm
```

## Embedding both `webm` and `mp4` - some browser might have problems with `webm` so `mp4` also worth adding:

```html
<video autoplay loop muted playsinline>
  <source src="video.webm" type="video/webm">
  <source src="video.mp4" type="video/mp4">
</video>
```

A <video> element with these attributes plays automatically, loops endlessly, plays no audio, and plays inline.
That is, not full screen. `webm` can be several time smaller than `mp4` and `gif` and hence quickly uploaded.

# Workflow used

``bash
asciinema rec video.cast
agg --theme monokai --speed 3 video.cast video.gif
ffmpeg -i video.gif -vf "crop=trunc(iw/2)*2:trunc(ih/2)*2" -b:v 0 -crf 25 -f mp4 -vcodec libx264 -pix_fmt yuv420p video.mp4
ffmpeg -i video.gif -c vp9 -b:v 0 -crf 41 video.webm
```