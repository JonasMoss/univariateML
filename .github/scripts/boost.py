"""Fetch the pinned standalone header dependency; never involve R."""
import hashlib
from pathlib import Path
import tarfile
import urllib.request

root = Path('build')
root.mkdir(exist_ok=True)
if not (root / 'boost_1_87_0/boost/version.hpp').exists():
    archive = root / 'boost_1_87_0.tar.bz2'
    urllib.request.urlretrieve(
        'https://archives.boost.io/release/1.87.0/source/boost_1_87_0.tar.bz2', archive)
    expected = 'af57be25cb4c4f4b413ed692fe378affb4352ea50fbe294a11ef548f4d527d89'
    if hashlib.sha256(archive.read_bytes()).hexdigest() != expected:
        raise RuntimeError('Boost archive SHA256 mismatch')
    with tarfile.open(archive) as source:
        source.extractall(root, members=(m for m in source if
                          m.name.startswith('boost_1_87_0/boost/')), filter='data')
    archive.unlink()
