# How to record and embed asciinema casts

## Fixing terminal inverted colors on paste

As you will probably set up a file full of commands to run in the terminal,
you may notice that when you paste commands into the terminal, the colors get
inverted (background becomes foreground color and vice versa). To avoid this, you can disable
bracketed paste mode in your terminal before starting the recording:

```bash
echo "set enable-bracketed-paste off" >> ~/.inputrc
bind -f ~/.inputrc # reload the settings on the fly
```

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

## Compressing the recorded `cast` video

You should make sure pauses longer than 1 second are compressed out of the
video to make it more viewer-friendly. You can use the following Python script to
compress the video by removing long pauses:

```bash
python compress.py video.cast 1.0
```

A backup of the original file will be created as `video.cast.not-compressed.bak`.

## Set the size of the recording to 100x30
```bash
python resize.py video.cast 100 30
```

A backup of the original file will be created as `video.cast.not-resized.bak`.

## Add the `asciinema` import

At the top of the documentation page make sure to add the following import:

```jsx
import AsciinemaEmbed from '@site/src/components/AsciinemaEmbed';
```

## Embedding the video in the documentation page

Where you want to embed the video, add the following snippet:

```jsx
<AsciinemaEmbed
  src="/moog/video/facts-querying.cast"
  options={{ autoplay: false, theme: 'asciinema', speed: 1.0 }}
/>
```