import React from "react";

export const outputColor = "#40E0D0";

const defaultColorPalette = [
  "#FFD700",
  "#ADFF2F",
  "#FF69B4",
  "#40E0D0",
]

export function useColor(colorPalette: string[] = defaultColorPalette) {
  const [colorIndex, setColorIndex] = React.useState(0);
  return function nextColor() {
    const color = colorPalette[colorIndex];
    setColorIndex(st => (st+1) % colorPalette.length)
    return color;
  }
}
