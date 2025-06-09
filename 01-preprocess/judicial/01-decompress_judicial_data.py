import os
from pathlib import Path
from pyunpack import Archive

# Set the directory (current directory)
directory = Path('../../../data/01-judicial/00-sentencing/raw/')

for file in directory.iterdir():
    if file.suffix in [".zip", ".rar"] or file.name.endswith(".tar.gz"):
        try:
            suffix = file.stem.split('_', 1)[1]  # Everything after the first "_"
            output_folder = directory / suffix
            output_folder.mkdir(exist_ok=True)
            
            print(f"Extracting {file.name} to {output_folder}...")
            Archive(str(file)).extractall(str(output_folder))

            # Optionally delete the archive
            file.unlink()
            print(f"Deleted: {file.name}")
        except Exception as e:
            print(f"Error with {file.name}: {e}")