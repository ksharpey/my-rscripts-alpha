# Pharmacy WhatsApp Posters

Generate an A4 poster with a large QR code that opens a WhatsApp chat with a
pharmacy. Pharmacy name top-centre, logo small in the top-right corner, phone
number below the QR code.

Prototype — runs fully offline, public libraries only.

## Install

```bash
pip install "qrcode[pil]" Pillow cairosvg
```

## Use

```python
from poster import make_poster

make_poster(
    pharmacy_name="Sunrise Community Pharmacy",
    msisdn="+27 82 123 4567",           # international format
    logo_path="demo_logo.png",          # PNG (transparency ok), or None
    out_path="output/poster.png",       # .png or .pdf by extension
    message="Hi, I'd like to ask about a prescription.",  # optional prefill
    style="branded",                    # "plain" (default) or "branded"
)
```

### Styles

- `style="plain"` — classic black square QR. Most robust for print/scan.
- `style="branded"` — rounded WhatsApp dark-green modules + the **official
  WhatsApp logo** (green rounded-square badge with the white glyph) in the
  centre, using level-H error correction so the logo stays scannable. A more
  distinctive look. **Always test-scan a branded code on a couple of phones
  before printing a run** — colour + centre logo eat into scan margin.

  ⚠️ The WhatsApp logo is a **Meta/WhatsApp trademark**. Only use `style=
  "branded"` where you have permission to display it, and follow WhatsApp's
  brand guidelines (correct colour, don't distort or recolour the glyph,
  keep clear space around it) — see https://www.whatsappbrand.com/. The
  glyph itself lives at `assets/whatsapp_glyph.svg` (vector trace from the
  MIT-licensed simple-icons project); swap that file if you have an
  official asset from Meta instead.

The pharmacy name auto-shrinks to fit between the two margins reserved for the
corner logo, so a long name never collides with it.

Both styles are produced by the `qrcode` library (python-qrcode) with no extra
dependency beyond `cairosvg` (used to rasterize the WhatsApp glyph). The
corner "eyes" stay square — custom eye shapes / full artistic codes would
need a different tool.

## Demo

```bash
python3 poster.py
```

Writes `output/sunrise_poster.png` and `.pdf` using a fake pharmacy, a fake
South African number, and a generated placeholder logo (`make_demo_logo.py`).

## Notes

- Number must be international format (country code, no `+`/spaces needed —
  they're stripped). `_pretty_number` formats SA (`27...`) nicely; others fall
  back to `+<digits>`.
- The WhatsApp link is `https://wa.me/<digits>[?text=<message>]`.
- QR uses high error correction (level H) so it stays scannable at large size.
- A4 @ 300 DPI (2480×3508 px), print-ready.

## TODO (not-yet-perfect)

- Batch mode (CSV of pharmacies → many posters).
- Font/colour theming per brand.
