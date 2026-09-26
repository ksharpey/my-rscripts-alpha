"""Export The Local Choice pharmacy list (https://thelocalchoice.co.za/find-store) to CSV.

Uses the branch API the site itself calls. Payment tokens and banking details
returned by the API are deliberately not written out.

Usage: python3 scrape_tlc_pharmacies.py [output.csv]
"""
import csv
import json
import sys
import urllib.request

API = ("https://local-choice-api-prod-72rqggorza-ew.a.run.app/local_choice/"
       "nearest_branches?size=5000&latitude=-29.0&longitude=25.0")

# Internal test branches present in the API.
EXCLUDE = {"The Local Choice Head Office Test Box", "OM BRANCH"}

DAYS = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
        "Saturday", "Sunday", "Public Holidays"]

FIELDS = ["name", "registered_name", "branch_number", "branch_code",
          "rams_practice_no", "phone", "email", "full_address", "town",
          "city", "province", "postal_code", "latitude", "longitude",
          "franchise", "ecommerce", "offline"] + \
         [f"hours_{d.lower().replace(' ', '_')}" for d in DAYS]


def extra(branch, *names):
    wanted = {n.lower() for n in names}
    for e in branch.get("extra_data") or []:
        if e.get("name", "").strip().lower() in wanted:
            return str(e.get("value", "")).strip()
    return ""


def row(branch):
    a = branch.get("address") or {}
    hours = {h.get("day"): f"{h.get('from', '')}-{h.get('to', '')}".strip("-")
             for h in branch.get("working_hours") or []}
    r = {
        "name": branch.get("display_name", "").strip(),
        "registered_name": branch.get("registered_name", ""),
        "branch_number": branch.get("branch_number", ""),
        "branch_code": extra(branch, "branch_code"),
        "rams_practice_no": extra(branch, "Rams_Practise_No", "Rams_Practice_No"),
        "phone": branch.get("contact_number", ""),
        "email": branch.get("email", ""),
        "full_address": a.get("full_address", ""),
        "town": a.get("town", ""),
        "city": a.get("city", ""),
        "province": a.get("province", ""),
        "postal_code": a.get("postal_code", ""),
        "latitude": a.get("latitude", ""),
        "longitude": a.get("longitude", ""),
        "franchise": branch.get("franchise", ""),
        "ecommerce": extra(branch, "eCommerce", "eCommerse"),
        "offline": branch.get("offline", ""),
    }
    for d in DAYS:
        r[f"hours_{d.lower().replace(' ', '_')}"] = hours.get(d, "")
    return r


def main(out="tlc_pharmacies.csv"):
    req = urllib.request.Request(API, headers={"User-Agent": "Mozilla/5.0"})
    with urllib.request.urlopen(req) as resp:
        content = json.load(resp)["content"]
    rows = [row(x["branch"]) for x in content
            if x["branch"].get("display_name", "").strip() not in EXCLUDE]
    rows.sort(key=lambda r: (r["province"] or "", r["city"] or "", r["name"]))
    with open(out, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=FIELDS)
        w.writeheader()
        w.writerows(rows)
    print(f"Wrote {len(rows)} branches to {out}")


if __name__ == "__main__":
    main(*sys.argv[1:])
