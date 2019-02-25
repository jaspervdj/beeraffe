#!/usr/bin/python

import subprocess
import sys
import os

# This is in the `unicode-character-database` package on Arch Linux.
UNICODE_DATABASE = '/usr/share/unicode-character-database/UnicodeData.txt'
WORDS = 'data/words.txt'
NOTO_EMOJI_DIR = '/home/jasper/Stash/noto-emoji'

def load_unicode_database():
  data = {}
  with open(UNICODE_DATABASE, 'rb') as db:
    for line in db:
      [code, name, _] = line.decode('utf-8').split(';', 2)
      name = name.lower().replace(' ', '-')
      code = code.lower()
      data[name] = code
  return data

def load_words():
  words = {}
  with open(WORDS, 'r') as f:
    for line in f:
      pieces = line.split(':')
      if len(pieces) <= 1:
        words[pieces[0].strip()] = pieces[0].strip()
      else:
        words[pieces[0].strip()] = pieces[1].strip()

  return words

if __name__ == "__main__":
  unicode_database = load_unicode_database()
  words = load_words()

  montage_files = []

  for word, canonical in words.items():
    code = unicode_database[canonical]
    print('{} -> {} -> {}'.format(word, canonical, code), file=sys.stderr)
    png_128 = "{}/png/128/emoji_u{}.png".format(NOTO_EMOJI_DIR, code)
    if not os.path.isfile(png_128):
      raise Exception("{} does not exist".format(png_128))
    else:
      montage_files += [png_128]

  print('Invoking montage...', file=sys.stderr)
  tmp_png = 'build/sprites-tmp-01.png'
  montage_command = \
    ['montage', '-geometry', '32x32+0+0', '-background', 'none'] + \
    montage_files + \
    ['png32:{}'.format(tmp_png)]
  subprocess.run(montage_command, check=True)

  print('Fixing alpha...', file=sys.stderr)
  subprocess.run([
    'convert', '-channel', 'A', '-threshold', '50%', tmp_png,
    'png32:build/sprites.png'
  ], check=True)
