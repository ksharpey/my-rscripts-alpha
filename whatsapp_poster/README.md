# Pharmacy WhatsApp Posters

Generate an A4 poster with a large QR code that opens a WhatsApp chat with a
pharmacy. Pharmacy name + logo on top, phone number below.

Prototype — runs fully offline, public libraries only.

## Install

```bash
pip install "qrcode[pil]" Pillow
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
)
```

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
- Real logo embedded in the QR centre (optional, needs a scan re-check).
- Font/colour theming per brand.
