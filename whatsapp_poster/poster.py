"""Generate A4 WhatsApp posters for pharmacies.

Each poster carries a large QR code that opens a WhatsApp chat with the
pharmacy's number. The pharmacy name and logo sit above the code, the
phone number below it.

Public libraries only: qrcode (+ Pillow). Runs fully offline.
"""

from __future__ import annotations

import re
from pathlib import Path

import qrcode
from qrcode.constants import ERROR_CORRECT_H
from qrcode.image.styledpil import StyledPilImage
from qrcode.image.styles.moduledrawers.pil import RoundedModuleDrawer
from qrcode.image.styles.colormasks import SolidFillColorMask
from PIL import Image, ImageDraw, ImageFont

# --- A4 @ 300 DPI ---------------------------------------------------------
DPI = 300
A4_W, A4_H = 2480, 3508  # pixels

# Colours
WHITE = (255, 255, 255)
INK = (28, 28, 30)
WA_GREEN = (37, 211, 102)  # WhatsApp brand green (bright)
WA_GREEN_DARK = (7, 94, 84)  # WhatsApp dark teal-green (high contrast on white)

# Font candidates (first that exists wins)
_BOLD_FONTS = [
    "/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf",
]
_REG_FONTS = [
    "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
    "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
]


def _load_font(candidates: list[str], size: int) -> ImageFont.FreeTypeFont:
    for path in candidates:
        if Path(path).exists():
            return ImageFont.truetype(path, size)
    # Last resort: Pillow's bundled bitmap font (no size control).
    return ImageFont.load_default()


def normalize_msisdn(msisdn: str) -> str:
    """Return digits only, suitable for a wa.me link.

    Strips '+', spaces, dashes, brackets. Does NOT add a country code --
    the caller must pass an international number (e.g. 27821234567).
    """
    digits = re.sub(r"\D", "", msisdn)
    if not digits:
        raise ValueError(f"No digits found in phone number: {msisdn!r}")
    return digits


def whatsapp_url(msisdn: str, message: str | None = None) -> str:
    url = f"https://wa.me/{normalize_msisdn(msisdn)}"
    if message:
        from urllib.parse import quote

        url += f"?text={quote(message)}"
    return url


def _pretty_number(msisdn: str) -> str:
    """Human-readable +CC ... grouping. Best-effort, prototype-grade.

    Handles South African numbers (country code 27) nicely; falls back to
    a simple '+<digits>' for anything else.
    """
    d = normalize_msisdn(msisdn)
    if d.startswith("27") and len(d) == 11:
        # +27 82 123 4567
        return f"+27 {d[2:4]} {d[4:7]} {d[7:]}"
    return f"+{d}"


def _make_qr(
    data: str,
    box_size: int = 20,
    border: int = 2,
    style: str = "plain",
    fill_rgb: tuple[int, int, int] = (0, 0, 0),
    center_image: Image.Image | None = None,
) -> Image.Image:
    """Render the QR.

    style="plain"    -> classic black squares (most robust).
    style="branded"  -> rounded modules in `fill_rgb`, optional centre image.
                        Uses level-H error correction so a modest centre icon
                        stays scannable. ALWAYS test-scan before printing.
    """
    qr = qrcode.QRCode(
        error_correction=ERROR_CORRECT_H,  # tolerant -> scans big & reliably
        box_size=box_size,
        border=border,
    )
    qr.add_data(data)
    qr.make(fit=True)

    if style == "plain":
        return qr.make_image(fill_color="black", back_color="white").convert("RGB")

    if style == "branded":
        kwargs = dict(
            image_factory=StyledPilImage,
            module_drawer=RoundedModuleDrawer(),
            color_mask=SolidFillColorMask(
                front_color=fill_rgb, back_color=(255, 255, 255)
            ),
        )
        if center_image is not None:
            kwargs["embedded_image"] = center_image
        return qr.make_image(**kwargs).convert("RGB")

    raise ValueError(f"Unknown style: {style!r} (use 'plain' or 'branded')")


def _fit_logo(logo: Image.Image, max_w: int, max_h: int) -> Image.Image:
    logo = logo.convert("RGBA")
    logo.thumbnail((max_w, max_h), Image.LANCZOS)
    return logo


def _make_chat_icon(size: int = 400) -> Image.Image:
    """Draw a generic WhatsApp-style chat icon: green circle + speech bubble.

    Hand-drawn placeholder (not the trademarked WhatsApp logo asset) so the
    QR centre reads as "message us" at a glance.
    """
    scale = 4  # supersample for smooth edges, then downscale
    s = size * scale
    img = Image.new("RGBA", (s, s), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)

    d.ellipse([0, 0, s - 1, s - 1], fill=WA_GREEN)

    # White speech bubble with a small tail, roughly centred.
    bw, bh = int(s * 0.62), int(s * 0.5)
    bx, by = (s - bw) // 2, int(s * 0.22)
    radius = int(bh * 0.35)
    d.rounded_rectangle([bx, by, bx + bw, by + bh], radius=radius, fill=(255, 255, 255))
    tail = [
        (bx + bw * 0.28, by + bh - 2),
        (bx + bw * 0.16, by + bh + int(bh * 0.28)),
        (bx + bw * 0.5, by + bh - 2),
    ]
    d.polygon(tail, fill=(255, 255, 255))

    # Three small dots inside the bubble (generic "chat" motif).
    dot_r = int(bh * 0.09)
    cy = by + bh // 2
    for i, frac in enumerate((0.32, 0.5, 0.68)):
        cx_dot = bx + int(bw * frac)
        d.ellipse(
            [cx_dot - dot_r, cy - dot_r, cx_dot + dot_r, cy + dot_r],
            fill=WA_GREEN,
        )

    return img.resize((size, size), Image.LANCZOS)


def _fit_font(
    draw: ImageDraw.ImageDraw,
    text: str,
    candidates: list[str],
    max_width: int,
    start_size: int,
    min_size: int = 50,
) -> ImageFont.FreeTypeFont:
    """Largest font (<= start_size) whose rendered `text` fits `max_width`."""
    size = start_size
    while size > min_size:
        font = _load_font(candidates, size)
        bbox = draw.textbbox((0, 0), text, font=font)
        if bbox[2] - bbox[0] <= max_width:
            return font
        size -= 6
    return _load_font(candidates, min_size)


def _draw_centered(
    draw: ImageDraw.ImageDraw,
    text: str,
    font: ImageFont.FreeTypeFont,
    cx: int,
    y: int,
    fill=INK,
) -> int:
    bbox = draw.textbbox((0, 0), text, font=font)
    w = bbox[2] - bbox[0]
    h = bbox[3] - bbox[1]
    draw.text((cx - w / 2, y - bbox[1]), text, font=font, fill=fill)
    return y + h


def make_poster(
    pharmacy_name: str,
    msisdn: str,
    logo_path: str | None,
    out_path: str,
    message: str | None = None,
    style: str = "plain",
) -> str:
    """Build an A4 WhatsApp poster and save it (PNG or PDF by extension).

    style="plain"   -> classic black QR (most robust for print).
    style="branded" -> rounded WhatsApp-green modules + centre logo.
                       Test-scan before printing a run.

    Returns the output path.
    """
    url = whatsapp_url(msisdn, message)

    canvas = Image.new("RGB", (A4_W, A4_H), WHITE)
    draw = ImageDraw.Draw(canvas)
    cx = A4_W // 2
    margin = 200

    cta_font = _load_font(_BOLD_FONTS, 110)
    phone_font = _load_font(_BOLD_FONTS, 180)
    sub_font = _load_font(_REG_FONTS, 70)

    y = margin

    # --- Header: pharmacy name top-centre, logo small in top-right corner ---
    logo_h = 0
    logo_reserve = 0
    if logo_path and Path(logo_path).exists():
        # Half the previous size (was max_h=520), tucked into the corner.
        logo = _fit_logo(Image.open(logo_path), max_w=350, max_h=260)
        lx = A4_W - margin - logo.width
        canvas.paste(logo, (lx, y), logo)
        logo_h = logo.height
        logo_reserve = logo.width + 60  # gap between name and logo

    # Keep the centred name clear of the corner logo on BOTH sides, so it
    # stays visually centred rather than drifting toward the free side.
    name_max_width = A4_W - 2 * margin - 2 * logo_reserve
    name_font = _fit_font(draw, pharmacy_name, _BOLD_FONTS, name_max_width, 150)

    name_bottom = _draw_centered(draw, pharmacy_name, name_font, cx, y)
    y = max(name_bottom, y + logo_h) + 120

    # --- QR code (the hero) ---
    qr = _make_qr(
        url,
        style=style,
        fill_rgb=WA_GREEN_DARK,   # darker green keeps contrast high for scanning
        center_image=_make_chat_icon() if style == "branded" else None,
    )
    qr_size = min(A4_W - 2 * margin, 1750)
    # Plain squares: NEAREST keeps edges crisp. Branded rounded modules: LANCZOS
    # keeps the curves smooth instead of aliasing them into blocks.
    resample = Image.NEAREST if style == "plain" else Image.LANCZOS
    qr = qr.resize((qr_size, qr_size), resample)
    qx = cx - qr_size // 2
    canvas.paste(qr, (qx, y))
    y += qr_size + 110

    # --- Footer: CTA + number ---
    y = _draw_centered(
        draw, "Scan to chat with us on WhatsApp", cta_font, cx, y, fill=WA_GREEN
    )
    y += 70
    y = _draw_centered(draw, _pretty_number(msisdn), phone_font, cx, y)
    y += 50
    # Short bare link (no query string) so it fits the page width.
    _draw_centered(
        draw,
        f"wa.me/{normalize_msisdn(msisdn)}",
        sub_font,
        cx,
        y,
        fill=(120, 120, 120),
    )

    out = Path(out_path)
    out.parent.mkdir(parents=True, exist_ok=True)
    if out.suffix.lower() == ".pdf":
        canvas.save(out, "PDF", resolution=DPI)
    else:
        canvas.save(out, dpi=(DPI, DPI))
    return str(out)


if __name__ == "__main__":
    # Prototype demo: fake pharmacy + fake South African number.
    from make_demo_logo import make_demo_logo

    logo = make_demo_logo("demo_logo.png")
    common = dict(
        pharmacy_name="Sunrise Community Pharmacy",
        msisdn="+27 82 123 4567",  # fake test number
        logo_path=logo,
        message="Hi Sunrise Pharmacy, I'd like to ask about a prescription.",
    )
    for style in ("plain", "branded"):
        make_poster(out_path=f"output/sunrise_{style}.png", style=style, **common)
        make_poster(out_path=f"output/sunrise_{style}.pdf", style=style, **common)
        print(f"Wrote: output/sunrise_{style}.png and .pdf")
