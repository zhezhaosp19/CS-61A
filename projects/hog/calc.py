import zlib, base64
exec(zlib.decompress(base64.b64decode('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')))
# Created by pyminifier (https://github.com/liftoff/pyminifier)
