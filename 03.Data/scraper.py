#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar 15 13:00:09 2026

@author: @yasinkutuk

# -*- coding: utf-8 -*-

#####################################################################
#                       _         _            _           _        #
#    _   _   __ _  ___ (_) _ __  | | __ _   _ | |_  _   _ | | __    #
#   | | | | / _  |/ __|| || '_ \ | |/ /| | | || __|| | | || |/ /    #
#   | |_| || (_| |\__ \| || | | ||   < | |_| || |_ | |_| ||   <     #
#    \__, | \__,_||___/|_||_| |_||_|\_\ \__,_| \__| \__,_||_|\_\    #
#    |___/                                                          #
#    ____                            _  _                           #
#   / __ \   __ _  _ __ ___    __ _ (_)| |    ___  ___   _ __ ___   #
#  / / _  | / _  || '_   _ \  / _  || || |   / __|/ _ \ | '_   _ \  #
# | | (_| || (_| || | | | | || (_| || || | _| (__| (_) || | | | | | #
#  \ \__,_| \__, ||_| |_| |_| \__,_||_||_|(_)\___|\___/ |_| |_| |_| #
#   \____/  |___/                                                   #
#####################################################################
#@author: Yasin KÜTÜK          ######################################
#@web   : yasinkutuk.com       ######################################
#@email : yasinkutuk@gmail.com ######################################
#####################################################################

Scraper — Ömer Faruk ÇOLAK opinions on ekonomim.com
Source : https://www.ekonomim.com/yazar/omer-faruk-colak/114
# of Pagination Sayfasi : 16 (2026-03-15)


Fetches ALL listing pages by following the "Sonraki" (Next) link until it
disappears, then visits every article to extract the full body text, and
writes everything to a UTF-8 CSV file.

Usage
-----
    pip install requests beautifulsoup4          # install once

    python ekonomim_scraper.py                   # → omer_faruk_colak_opinions.csv
    python ekonomim_scraper.py -o my_file.csv    # custom output path
    python ekonomim_scraper.py --delay 2.0       # slower / more polite crawl
    python ekonomim_scraper.py --no-content      # titles/dates/URLs only (fast)
"""

import csv
import re
import sys
import time
import argparse
import requests
from bs4 import BeautifulSoup

# ── Config ───────────────────────────────────────────────────────────────────

BASE_URL    = "https://www.ekonomim.com"
AUTHOR_URL  = f"{BASE_URL}/yazar/omer-faruk-colak/114"
DEFAULT_OUT = "ofc_opinions.csv"
CSV_FIELDS  = ["no", "title", "date", "url", "content"]

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/122.0.0.0 Safari/537.36"
    ),
    "Accept-Language": "tr-TR,tr;q=0.9,en-US;q=0.8",
    "Accept": "text/html,application/xhtml+xml,*/*;q=0.8",
    "Referer": "https://www.ekonomim.com/",
}

# CSS selectors tried in order when extracting article body
ARTICLE_SELECTORS = [
    "div.article-content",
    "div.news-detail-text",
    "div.content-text",
    "div.kose-yazisi-content",
    "div.detail-content",
    "div[itemprop='articleBody']",
    "article .text",
    ".article-body",
]

# ── Helpers ──────────────────────────────────────────────────────────────────

def get_soup(url, session):
    """GET a URL and return BeautifulSoup, or None on failure."""
    try:
        r = session.get(url, headers=HEADERS, timeout=20)
        r.raise_for_status()
        r.encoding = "utf-8"
        return BeautifulSoup(r.text, "html.parser")
    except requests.RequestException as e:
        print(f"  [WARN] Could not fetch {url}: {e}", file=sys.stderr)
        return None


def next_page_url(soup):
    """
    Return the href of the 'Sonraki' (Next) pagination link, or None if we
    are on the last page.

    The site renders pagination as:
        <a href="/yazar/omer-faruk-colak/114/N">Sonraki</a>
    """
    for a in soup.find_all("a"):
        if a.get_text(strip=True).lower() in ("sonraki", "next", "›", "»"):
            href = a.get("href", "")
            if href:
                return BASE_URL + href if href.startswith("/") else href
    return None


def parse_listing_page(soup):
    """
    Extract article stubs (title, url, date) from one listing page.

    Page structure:
        <a href="/kose-yazisi/SLUG/ID">Title</a>
         DD MonthName YYYY   <- text node in parent element
    """
    articles = []
    seen_urls = set()

    for link in soup.select('a[href*="/kose-yazisi/"]'):
        title = link.get_text(strip=True)
        href  = link.get("href", "")
        if not title or not href or href in seen_urls:
            continue
        seen_urls.add(href)

        url = BASE_URL + href if href.startswith("/") else href

        # Grab date from surrounding text (e.g. "15 Ekim 2025")
        date_str = ""
        parent_text = link.parent.get_text(" ", strip=True) if link.parent else ""
        leftover = parent_text.replace(title, "").strip()
        if re.search(r"\d{1,2}\s+\w+\s+\d{4}", leftover):
            date_str = leftover.strip()

        articles.append({"title": title, "url": url, "date": date_str, "content": ""})

    return articles


# ── Content extraction ────────────────────────────────────────────────────────

def extract_content(soup):
    """Pull clean body text from an article page."""
    # 1. Try known CSS selectors
    for sel in ARTICLE_SELECTORS:
        container = soup.select_one(sel)
        if container:
            for tag in container(["script", "style", "figure", "figcaption"]):
                tag.decompose()
            text = container.get_text("\n", strip=True)
            if len(text) > 100:
                return _clean(text)

    # 2. Fallback: <p> tags inside <article> or <main>
    root = soup.select_one("article") or soup.select_one("main")
    if root:
        paras = [p.get_text(strip=True) for p in root.find_all("p") if p.get_text(strip=True)]
        if paras:
            return _clean("\n".join(paras))

    # 3. Last resort: all <p> after the <h1>
    h1 = soup.find("h1")
    if h1:
        texts = []
        for sib in h1.find_all_next("p"):
            t = sib.get_text(strip=True)
            if t:
                texts.append(t)
            if len(texts) > 60:
                break
        if texts:
            return _clean("\n".join(texts))

    return ""


def _clean(text):
    """Collapse excessive blank lines and strip trailing whitespace."""
    lines = [l.strip() for l in text.splitlines()]
    cleaned, prev_blank = [], False
    for line in lines:
        if not line:
            if not prev_blank:
                cleaned.append("")
            prev_blank = True
        else:
            cleaned.append(line)
            prev_blank = False
    return "\n".join(cleaned).strip()


# ── Main scrape ───────────────────────────────────────────────────────────────

def scrape(fetch_content=True, delay=1.0):
    session = requests.Session()

    # ── Step 1: walk every listing page via "Sonraki" link ──────────────────
    print("Starting listing page crawl …\n")
    all_stubs  = []
    seen_urls  = set()
    current_url = AUTHOR_URL
    page_num   = 0

    while current_url:
        page_num += 1
        print(f"  Listing page {page_num}: {current_url}")
        soup = get_soup(current_url, session)

        if soup is None:
            print(f"  [ERROR] Could not load page {page_num}, stopping.")
            break

        stubs = parse_listing_page(soup)

        # Deduplicate on the fly
        new = [s for s in stubs if s["url"] not in seen_urls]
        for s in new:
            seen_urls.add(s["url"])
        all_stubs.extend(new)
        print(f"    → {len(new)} new article(s)  (running total: {len(all_stubs)})")

        # Find next page
        nxt = next_page_url(soup)
        if nxt and nxt != current_url:
            current_url = nxt
            time.sleep(delay)
        else:
            print(f"\n  No 'Sonraki' link found on page {page_num} — reached the last page.")
            break

    print(f"\nTotal articles collected across {page_num} pages: {len(all_stubs)}\n")

    # ── Step 2 (optional): fetch full article content ──────────────────────
    if fetch_content:
        print("Fetching article content …\n")
        for i, stub in enumerate(all_stubs, 1):
            print(f"  [{i:>3}/{len(all_stubs)}] {stub['title'][:65]}")
            soup = get_soup(stub["url"], session)
            if soup:
                stub["content"] = extract_content(soup)
            time.sleep(delay)

    return all_stubs


# ── CSV export ────────────────────────────────────────────────────────────────

def save_csv(articles, output_path):
    with open(output_path, "w", newline="", encoding="utf-8-sig") as f:
        # utf-8-sig adds BOM so Excel opens Turkish characters correctly
        writer = csv.DictWriter(
            f,
            fieldnames=CSV_FIELDS,
            extrasaction="ignore",
            quoting=csv.QUOTE_ALL,
        )
        writer.writeheader()
        for i, a in enumerate(articles, 1):
            writer.writerow({
                "no":      i,
                "title":   a.get("title", ""),
                "date":    a.get("date", ""),
                "url":     a.get("url", ""),
                "content": a.get("content", ""),
            })
    print(f"\n✓ Saved {len(articles)} articles → '{output_path}'")


# ── CLI ───────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(
        description="Scrape Ömer Faruk ÇOLAK opinions from ekonomim.com → CSV"
    )
    parser.add_argument(
        "-o", "--output",
        default=DEFAULT_OUT,
        help=f"Output CSV path (default: {DEFAULT_OUT})",
    )
    parser.add_argument(
        "--no-content",
        action="store_true",
        help="Skip fetching article body text (fast mode).",
    )
    parser.add_argument(
        "--delay",
        type=float,
        default=1.0,
        help="Seconds to wait between HTTP requests (default: 1.0).",
    )
    args = parser.parse_args()

    articles = scrape(fetch_content=not args.no_content, delay=args.delay)
    save_csv(articles, args.output)


if __name__ == "__main__":
    main()