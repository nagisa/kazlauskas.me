import os
import unicodedata

letters = set()

for (where, _, files) in os.walk("_site"):
    for file in files:
        with open(os.path.join(where, file), "rb") as input:
            try:
                data = input.read().decode()
                for character in data:
                    letters.add(character)
            except UnicodeDecodeError:
                continue

ascii = { letter for letter in letters if ord(letter) < 0x7f }
letters -= ascii

print('fontRanges = [ "0-7f"\n{}]'.format("\n".join('             , "{:x}"'.format(ord(l)) for l in sorted(letters))))
