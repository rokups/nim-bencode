## The MIT License (MIT)
##
## Copyright (c) 2016 Rokas Kupstys
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

import tables, strutils, algorithm

type BValueKind* = enum
    bvNone = 0
    bvNumber = 1
    bvBytes = 2
    bvList = 3
    bvDict = 4

type BValue* = ref object
    kind: BValueKind
    bytes: string
    list: seq[BValue]
    dict: Table[string, BValue]
    number: int64

proc new*(_: typedesc[BValue], kind: BValueKind): BValue =
    ## Creates new value of type ``kind``.
    if kind == BValueKind.bvNone:
        raise newException(ValueError, "bvNone is not a valid value type")
    result.new()
    result.kind = kind
    case kind
    of BValueKind.bvBytes:
        result.bytes = ""
    of BValueKind.bvList:
        result.list = newSeq[BValue]()
    of BValueKind.bvDict:
        result.dict = initTable[string, BValue]()
    else:
        discard

converter toNumber*(val: BValue): int64 =
    ## Converts ``val`` to number.
    if val.kind != BValueKind.bvNumber:
        raise newException(ValueError, "This value is not a number")
    return val.number

converter toBytes*(val: BValue): string =
    ## Converts ``val`` to binary string.
    if val.kind != BValueKind.bvBytes:
        raise newException(ValueError, "This value is not binary string")
    return val.bytes

converter fromNumber*(val: int64): BValue =
    ## Converts number ``val`` to ``BValue``.
    result = BValue.new()
    result.number = val
    result.kind = BValueKind.bvNumber

converter fromString*(val: string): BValue =
    ## Converts string ``val`` to ``BValue``.
    result = BValue.new()
    result.bytes = val
    result.kind = BValueKind.bvBytes

proc `[]`*(val: BValue, i: int): var BValue =
    ## Gets value of list at specified index ``i``.
    if val.kind != BValueKind.bvList:
        raise newException(ValueError, "This value is not a list")
    return val.list[i]

proc `[]`*(val: BValue, key: string): var BValue =
    ## Gets value of dict at specified ``key``.
    if val.kind != BValueKind.bvDict:
        raise newException(ValueError, "This value is not a dict")
    return val.dict[key]

proc `[]=`*(val: var BValue, i: int, newVal: BValue) =
    ## Sets value of list to ``newVal`` at specified index ``i``.
    if val.kind != BValueKind.bvList:
        raise newException(ValueError, "This value is not a list")
    val.list[i] = newVal

proc `[]=`*(val: var BValue, key: string, newVal: BValue) =
    ## Sets value of dict to ``newVal`` at specified ``key``.
    if val.kind != BValueKind.bvDict:
        raise newException(ValueError, "This value is not a dict")
    val.dict[key] = newVal

proc add*(val: var BValue, newVal: BValue) =
    ## Appends value ``newVal`` to a list.
    if val.kind != BValueKind.bvList:
        raise newException(ValueError, "This value is not a list")
    val.list.add(newVal)

proc add*(val: var BValue, newVal: int64) =
    ## Adds number to list. This is a workaround procedure because converter does not automatically pick up number litterals.
    val.list.add(newVal)

proc next(i: var int, data: string): char =
    if i >= data.len:
        raise newException(ValueError, "Truncated data (pos $#)" % [$i])
    result = data[i]
    inc i

proc decode*(data: string, i: var int): BValue =
    ## Decodes single element in bencoding-encoded string ``data`` starting at position ``i``.
    result = BValue.new()
    result.kind = BValueKind.bvNone
    var c = next(i, data)
    case c
    of 'i':     # number
        c = next(i, data)
        var negative = c == '-'
        if negative:
            c = next(i, data)
        while c != 'e':
            if c in {'0'..'9'}:
                result.number = result.number * 10 + ord(c) - ord('0')
            else:
                raise newException(ValueError, "Expected 0-9 (pos $#)" % [$(i - 1)])
            c = next(i, data)
        if negative:
            result.number *= -1
        result.kind = BValueKind.bvNumber
    of '0'..'9':  # bytes
        var length = 0
        while c != ':':
            if c in {'0'..'9'}:
                length = length * 10 + ord(c) - ord('0')
            else:
                raise newException(ValueError, "Expected 0-9 (pos $#)" % [$(i - 1)])
            c = next(i, data)
        result.bytes = data[i..i + length - 1]
        result.kind = BValueKind.bvBytes
        i += length
    of 'l':
        result.list = newSeq[BValue]()
        while c != 'e':
            result.list.add(decode(data, i))
            c = data[i]
        result.kind = BValueKind.bvList
    of 'd':
        result.dict = initTable[string, BValue]()
        while c != 'e':
            var p = i
            var key = decode(data, i)
            if key.kind != BValueKind.bvBytes:
                raise newException(ValueError, "Dictionary keys must be binary strings (pos $#)" % [$p])
            result.dict[key.bytes] = decode(data, i)
            c = data[i]
        result.kind = BValueKind.bvDict
    else:
        raise newException(ValueError, "Invalid character (pos $#)" % [$(i - 1)])


proc decode*(data: string): BValue =
    ## Decodes single element in bencoding-encoded string ``data``.
    var i = 0
    return decode(data, i)


proc encode*(val: BValue): string =
    ## Encodes value ``val`` to bencoding-encoded binary string.
    case val.kind
    of BValueKind.bvNumber:
        return 'i' & $val.number & 'e'
    of BValueKind.bvBytes:
        return $val.bytes.len & ':' & val.bytes
    of BValueKind.bvList:
        result = "l"
        for v in val.list:
            result &= encode(v)
        result &= "e"
    of BValueKind.bvDict:
        result = "d"
        var keys = newSeq[string]()
        for k in val.dict.keys():
            keys.add(k)
        sort(keys, proc(a, b: string): int = cmp(a, b))
        for k in keys:
            result &= $k.len & ':' & k
            result &= encode(val.dict[k])
        result &= "e"
    else:
        raise newException(ValueError, "bvNone is not a valid value therefore it can not be encoded")

when isMainModule:
    block:
        var dct = decode("d4:spamli-42e4:spamee")
        doAssert dct.kind == BValueKind.bvDict
        doAssert dct.dict["spam"].kind == BValueKind.bvList
        doAssert dct.dict["spam"][0].kind == BValueKind.bvNumber
        doAssert dct.dict["spam"][0].number == -42
        doAssert dct.dict["spam"][1].kind == BValueKind.bvBytes
        doAssert dct.dict["spam"][1].bytes == "spam"

    block:
        var dct = BValue.new(BValueKind.bvDict)
        dct["spam"] = BValue.new(BValueKind.bvList)
        dct["spam"].add(-42)
        dct["spam"].add("spam")
        doAssert encode(dct) == "d4:spamli-42e4:spamee"
