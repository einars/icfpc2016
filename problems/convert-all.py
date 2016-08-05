#!/usr/bin/env python

import re

r = re.compile("""<textarea class="form-control" rows="10">([\-\d\/,\r\n ]+)</textarea>""")
for i in range(1, 101 + 1):
  print(i)
  f = './.cache/%s' % i
  out = './%03d.txt' % i

  lines = ''.join(open(f))

  co = r.findall(lines)

  with open(out, 'w') as of:
    of.write(co[0])


