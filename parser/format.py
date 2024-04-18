from pathlib import *
import re

lines = Path("jppml.cf").read_text().splitlines()

new_lines = []

i = 0
while i < len(lines):
  line = lines[i]

  if m := re.fullmatch(r"([^.]+)\.\s+(\S+)\s*::=(.*);\s*-*\s*(.*)", line):
    a, b, c, d, *_ = list(m.groups()) + [""]
    a = f"{a.strip()}."
    b = b.strip()
    c = c.strip()
    d = d.strip() if d else ""
    new_line = f"{a:<9} {b:<9} ::= {c:<29} ;" + (f" -- {d}" if d else "")
  elif m := re.fullmatch(r"(.*);(.*)", line):
    a, b = m.groups()
    a = a.strip()
    b = b.strip()
    print([a, b])
    new_line = f"{a:<54};{b}"
  else:
    new_line = line

  new_lines.append(new_line)

  i = i + 1

# print("\n".join(new_lines))

Path("test.cf").write_text("\n".join(new_lines))
