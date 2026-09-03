"""Generate a simple placeholder pharmacy logo (transparent PNG).

Only used so the prototype runs without needing a real brand asset.
"""

from __future__ import annotations

from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

_BOLD = "/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf"


def make_demo_logo(out_path: str = "demo_logo.png", size: int = 480) -> str:
    img = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    green = (37, 211, 102, 255)

    # Green rounded square background
    d.rounded_rectangle([0, 0, size - 1, size - 1], radius=size // 6, fill=green)

    # White medical cross
    arm = size // 6
    cx = cy = size // 2
    d.rectangle([cx - arm // 2, cy - arm * 1.6, cx + arm // 2, cy + arm * 1.6],
                fill=(255, 255, 255, 255))
    d.rectangle([cx - arm * 1.6, cy - arm // 2, cx + arm * 1.6, cy + arm // 2],
                fill=(255, 255, 255, 255))

    if Path(_BOLD).exists():
        font = ImageFont.truetype(_BOLD, size // 8)
        d.text((cx, size - size // 7), "RX", font=font, fill=(255, 255, 255, 255),
               anchor="mm")

    img.save(out_path)
    return out_path


if __name__ == "__main__":
    print("Wrote:", make_demo_logo())
