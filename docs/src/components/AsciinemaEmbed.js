import React, { useEffect, useRef } from 'react';
import 'asciinema-player/dist/bundle/asciinema-player.css';

export default function AsciinemaEmbed({ src, options = {} }) {
  const ref = useRef(null);

  useEffect(() => {
    let player;
    (async () => {
      // ✅ Use the top-level import (the one allowed by the exports map)
      const mod = await import('asciinema-player');
      const AsciinemaPlayer = mod.default || mod; // support both CJS/ESM builds
      if (ref.current && AsciinemaPlayer?.create) {
        player = AsciinemaPlayer.create(src, ref.current, options);
      } else {
        console.error('Failed to load AsciinemaPlayer or its create() method');
      }
    })();

    return () => {
      player = null;
    };
  }, [src, options]);

  return <div ref={ref} />;
}

