#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun May 11 21:10:01 2026

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

"""
İktisat ve Toplum Dergisi – Kurucu Başeditör'den scraper
https://iktisatvetoplum.com/category/editorden/

Kurulum:  pip install requests beautifulsoup4 pandas
Çalıştır: python itd_editorden_scraper.py
Çıktılar: itd_editorden.csv  |  itd_editorden.json
"""

"""
İktisat ve Toplum Dergisi – Kurucu Başeditör'den scraper
https://iktisatvetoplum.com/category/editorden/

Kurulum:  pip install requests beautifulsoup4 pandas
Çalıştır: python itd_editorden_scraper.py
Çıktılar: itd_editorden.csv  |  itd_editorden.json
"""

import re, time, json, csv, sys
import requests
from bs4 import BeautifulSoup
from datetime import datetime

# ── Ayarlar ────────────────────────────────────────────────────────────────────
BASE_URL  = "https://iktisatvetoplum.com"
CAT_URL   = f"{BASE_URL}/category/editorden/"
DELAY     = 1.5          # istek arası bekleme (saniye)
OUT_CSV   = "itd_editorden.csv"
OUT_JSON  = "itd_editorden.json"

SESSION = requests.Session()
SESSION.headers.update({
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/124.0.0.0 Safari/537.36"
    ),
    "Accept":          "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    "Accept-Language": "tr-TR,tr;q=0.9,en;q=0.8",
    "Referer":         "https://www.google.com/",
})
# ───────────────────────────────────────────────────────────────────────────────


def get_soup(url: str) -> BeautifulSoup:
    r = SESSION.get(url, timeout=20)
    r.raise_for_status()
    r.encoding = "utf-8"
    return BeautifulSoup(r.text, "html.parser")


def get_total_pages(soup: BeautifulSoup) -> int:
    """Pagination'dan toplam sayfa sayısını bul."""
    nums = []
    for sel in ["a.page-numbers", ".pagination a", "nav.navigation a", ".nav-links a"]:
        for a in soup.select(sel):
            t = a.get_text(strip=True)
            if t.isdigit():
                nums.append(int(t))
    return max(nums) if nums else 1


def extract_article_links(soup: BeautifulSoup) -> list[dict]:
    """
    Bir liste sayfasından makale URL'lerini çek.
    Birden fazla strateji dene — hangisi çalışırsa onu kullan.
    """
    found = {}

    # Strateji 1 – h3 içindeki <a>
    for a in soup.select("h3 a"):
        href = a.get("href", "")
        if href.startswith(BASE_URL) and href not in found:
            found[href] = a.get_text(strip=True)

    # Strateji 2 – h2 içindeki <a> (bazı temalar h2 kullanır)
    if not found:
        for a in soup.select("h2 a"):
            href = a.get("href", "")
            if href.startswith(BASE_URL) and "/category/" not in href and href not in found:
                found[href] = a.get_text(strip=True)

    # Strateji 3 – .entry-title, .post-title, article içi başlık linkleri
    if not found:
        for sel in [".entry-title a", ".post-title a", "article h1 a", "article h2 a", "article h3 a"]:
            for a in soup.select(sel):
                href = a.get("href", "")
                if href.startswith(BASE_URL) and href not in found:
                    found[href] = a.get_text(strip=True)

    # Strateji 4 – URL pattern eşleştirme (son çare, geniş ağ)
    if not found:
        pattern = re.compile(r"https://iktisatvetoplum\.com/[a-z0-9\-]+/?$")
        for a in soup.find_all("a", href=True):
            href = a["href"].rstrip("/") + "/"
            if pattern.match(href) and "/category/" not in href and "/tag/" not in href \
               and "/page/" not in href and "/wp-" not in href and href not in found:
                found[href] = a.get_text(strip=True)

    return [{"title": t.strip(), "url": u} for u, t in found.items() if t.strip()]


def collect_all_urls() -> list[dict]:
    """Tüm liste sayfalarını dolaş ve makale URL listesi döndür."""
    print("► İlk sayfa çekiliyor...")
    soup0 = get_soup(CAT_URL)
    total = get_total_pages(soup0)
    print(f"  Toplam sayfa: {total}")

    all_articles = []
    seen = set()

    for page in range(1, total + 1):
        url = CAT_URL if page == 1 else f"{CAT_URL}page/{page}/"
        print(f"  Sayfa {page}/{total}: {url}")
        soup = get_soup(url) if page > 1 else soup0
        arts = extract_article_links(soup)
        new = [a for a in arts if a["url"] not in seen]
        for a in new:
            seen.add(a["url"])
        all_articles.extend(new)
        print(f"    → Bu sayfada {len(arts)} link, yeni {len(new)}")
        if page < total:
            time.sleep(DELAY)

    return all_articles


def parse_article(url: str) -> dict:
    """Bir makale sayfasını çekip içerik, tarih ve yazar döndür."""
    soup = get_soup(url)

    # Tam metin
    content = (
        soup.select_one(".post_content")
        or soup.select_one("[itemprop='articleBody']")
        or soup.select_one(".entry-content")
        or soup.select_one(".post-content")
        or soup.select_one("article")
    )
    if content:
        for tag in content.select("script,style,.sharedaddy,.jp-relatedposts,.social-share"):
            tag.decompose()
        text = re.sub(r"\n{3,}", "\n\n", content.get_text("\n", strip=True)).strip()
    else:
        text = ""

    # Tarih
    dt = soup.select_one("time[datetime]") or soup.select_one("time.entry-date") or soup.select_one(".post-date")
    pub_date = (dt.get("datetime", dt.get_text(strip=True)) if dt else "")[:10]

    # Yazar
    au = soup.select_one(".author.vcard a") or soup.select_one("[rel='author']") or soup.select_one(".post-author a")
    author = au.get_text(strip=True) if au else ""

    # Sayfa başlığı (doğrulama)
    h1 = soup.select_one("h1.entry-title") or soup.select_one("h1")
    title_v = h1.get_text(strip=True) if h1 else ""

    return {"title_verified": title_v, "author": author, "pub_date": pub_date,
            "full_text": text, "char_count": len(text)}


def main():
    print("=" * 50)
    print("İTD Editör'den Scraper")
    print(f"Başlangıç: {datetime.now():%Y-%m-%d %H:%M:%S}")
    print("=" * 50)

    # 1. Ana sayfayı ziyaret et (çerez/oturum başlat)
    SESSION.get(BASE_URL, timeout=20)
    time.sleep(1)

    # 2. URL listesi
    articles = collect_all_urls()
    print(f"\nToplam {len(articles)} makale bulundu.")

    if not articles:
        print("\n⚠ Hiç makale bulunamadı!")
        print("  Olası neden: site JavaScript ile render ediliyor olabilir.")
        print("  Çözüm: aşağıdaki 'scrapy' veya 'selenium' alternatifine geçin.")
        return

    # 3. İçerikleri çek — her makale çekildikçe anında yaz
    print("\n► Makale içerikleri çekiliyor...", flush=True)

    FIELDS = ["title", "title_verified", "author", "pub_date", "url", "char_count", "full_text"]

    csv_file = open(OUT_CSV, "w", newline="", encoding="utf-8-sig")
    writer = csv.DictWriter(csv_file, fieldnames=FIELDS)
    writer.writeheader()
    csv_file.flush()

    json_file = open(OUT_JSON, "w", encoding="utf-8")
    json_file.write("[\n")

    records = []
    for i, art in enumerate(articles, 1):
        print(f"  [{i:>3}/{len(articles)}] {art['title'][:65]}", flush=True)
        try:
            d = parse_article(art["url"])
            row = {**art, **d}
            status = f"✔  {d['pub_date']}  {d['char_count']:>6} kar."
        except Exception as e:
            row = {**art, "title_verified": "", "author": "",
                   "pub_date": "", "full_text": f"HATA: {e}", "char_count": 0}
            status = f"⚠  HATA: {e}"

        # CSV'ye anında yaz
        writer.writerow({f: row.get(f, "") for f in FIELDS})
        csv_file.flush()

        # JSON'a anında yaz
        prefix = "  " if i == 1 else ", "
        json_file.write(prefix + json.dumps(row, ensure_ascii=False) + "\n")
        json_file.flush()

        records.append(row)
        print(f"       {status}", flush=True)
        time.sleep(DELAY)

    json_file.write("]\n")
    json_file.close()
    csv_file.close()

    n = len(records)
    chars = [r["char_count"] for r in records if r["char_count"] > 0]
    dates = sorted(r["pub_date"] for r in records if r["pub_date"])

    print(f"\n✔ {OUT_CSV}  ({n} satır)")
    print(f"✔ {OUT_JSON}")
    if dates:
        print(f"\nTarih aralığı : {dates[0]} – {dates[-1]}")
    if chars:
        print(f"Ort. karakter : {sum(chars)/len(chars):.0f}")


if __name__ == "__main__":
    main()