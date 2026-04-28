
import os
import requests

# ===== CONFIG =====
start_year = 2011
end_year = 2025


## creare il folder Bando_cig_files dove preferisci in cui verranno inserite una cartella per ogni anno nel range di anni appena qua sopra, con ogni cartella contenente gli zip dei 12 mesi di quell'anno

# bando cig
save_folder = r"Bando_cig_files/"
base_url = "https://dati.anticorruzione.it/opendata/download/dataset/cig-{year}/filesystem/cig_csv_{year}_{month}.zip"

# bando smartcig
# save_folder = r"C:/Users/dakot/OneDrive/Desktop/Lab Aziende Statistica/Lab_aziende/Bando_smartcig_files/"
# base_url = "https://dati.anticorruzione.it/opendata/download/dataset/smartcig-{year}/filesystem/smartcig_csv_{year}_{month}.zip"

os.makedirs(save_folder, exist_ok=True)

for year in range(start_year, end_year + 1):

    # Create folder for the year
    year_folder = os.path.join(save_folder, str(year))
    os.makedirs(year_folder, exist_ok=True)

    for month in range(1, 13):

        mm = f"{month:02d}"
        url = base_url.format(year=year, month=mm)

        filename = f"cig_csv_{year}_{mm}.zip"
        filepath = os.path.join(year_folder, filename)

        if os.path.exists(filepath):
            print(f"Already downloaded: {filepath}")
            continue

        print(f"Downloading {url}")

        try:
            with requests.get(url, stream=True, timeout=60) as r:
                if r.status_code == 200:
                    with open(filepath, "wb") as f:
                        for chunk in r.iter_content(chunk_size=8192):
                            f.write(chunk)
                    print(f"Saved: {filepath}")
                else:
                    print(f"Not found: {filename}")
        except Exception as e:
            print(f"Error downloading {filename}: {e}")


