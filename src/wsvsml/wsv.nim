import std/[unicode,strformat,strutils]

## module to parse and serialize texts, lines and strings by the
## `WSV-Specification <https://dev.stenway.com/WSV/Specification.html>`_

const
  whitespaceInts: seq[int32] = @[
    0x0009, #	Character Tabulation
    0x000A, #	Line Feed
    0x000B, #	Line Tabulation
    0x000C, #	Form Feed
    0x000D, #	Carriage Return
    0x0020, #	Space
    0x0085, #	Next Line
    0x00A0, #	No-Break Space
    0x1680, #	Ogham Space Mark
    0x2000, #	En Quad
    0x2001, #	Em Quad
    0x2002, #	En Space
    0x2003, #	Em Space
    0x2004, #	Three-Per-Em Space
    0x2005, #	Four-Per-Em Space
    0x2006, #	Six-Per-Em Space
    0x2007, #	Figure Space
    0x2008, #	Punctuation Space
    0x2009, #	Thin Space
    0x200A, #	Hair Space
    0x2028, #	Line Separator
    0x2029, #	Paragraph Separator
    0x202F, #	Narrow No-Break Space
    0x205F, #	Medium Mathematical Space
    0x3000 #	Ideographic Space
  ]
  dblQuote: Rune = cast[Rune](0x0022)
  slash: Rune = cast[Rune](0x002F)
  hashsign: Rune = cast[Rune](0x0023)
  newline: Rune = cast[Rune](0x000A)
  hyphenminus: Rune = cast[Rune](0x002D)

  wsvnewline: string = "\"/\""
  wsvDblQuote: string = "\"\""
  wsvHyphenMinus: string = "\"-\""
  wsvNull: string = "--NULL--"
    
type
  WsvString* = distinct string
    ## string value which follows the wsv value-specification
    ## e.g. contains no "\\n", etc.
  
  WsvEncoding* = enum
    ## to sepcify the wsv-document encoding; currently only UTF-8 is supported!
    weUtf8, weUtf16

  WsvLine* = ref object
    values*: seq[WsvString]
    whitespaces*: seq[int32]
    comment*: string
    
  WsvDocument* = ref object
    lines*: seq[WsvLine]
    encoding*: WsvEncoding

  WsvRow* = ref object
    ## like `WsvLine <#WsvLine>`_ but hold the values as *real* strings with wsv-rules
    values*: seq[string]
    whitespaces*: seq[int32]
    comment*: string
  WsvTable* = ref object
    rows*: seq[WsvRow]
    ## To hold Data in 'real-string' way

proc len*(wsvstring: WsvString): int =
  return string(wsvstring).len()
    
proc add*(wsvstring: var WsvString, s: string) =
  string(wsvstring).add(s)

proc add*(w1: var WsvString, w2: WsvString) =
  string(w1).add(string(w2))

proc `==`*(wsvstring1, wsvstring2: WsvString): bool =
  if string(wsvstring1) == string(wsvstring2):
    return true
  else:
    return false
    
proc hasOneOf(s: string, collection: seq[int32]): bool =
  for val in collection:
    if s.contains($(cast[Rune](val))):
      return true
  return false

func isNextRuneDblQuote(runes: seq[Rune], curIndex: int): bool =
  if curIndex+1 >= len(runes):
    return false
  if runes[curIndex+1] == dblQuote:
    return true
  else:
    return false

func isNewlineEscapeSequence(runes: seq[Rune], curIndex: int): bool =
  if curIndex+2 >= len(runes):
    return false
  if (runes[curIndex+1] == slash) and (runes[curIndex+2] == dblQuote):
    return true
  else:
    return false

func isWhitespaceChar(c: int32): bool =
  if c in whitespaceInts:
    return true
  else:
    return false

func isWhitespaceString(s: string): bool =
  for c in s:
    if not int32(c).isWhitespaceChar():
      return false
  return true
  
func isDblQuote(r: Rune): bool =
  if r == dblQuote:
    return true
  else:
    return false

func join(wsvstrings: seq[WsvString], sep: string): WsvString =
  for wsvstring in wsvstrings:
    result.add(wsvstring)
    result.add(WsvString(sep))

proc parseWsvLine(line: string): WsvLine =
  ## parses a string-line in wsv-format (e.g. a serialized line)
  result = WsvLine()
  let runes = toRunes(line)
  var
    currentWord: WsvString = WsvString("")
    isPendingDblQuote = false
  var i = -1
  while i < runes.len()-1:
    i.inc()
    let r = runes[i]
    
proc parseLine(line: string): WsvLine =
  ## parses a string-line with wsv-rules
  result = WsvLine()
  let runes = toRunes(line)
  var
    currentWord: WsvString = WsvString("")
    isPendingDblQuote = false
    lastRune: Rune

  var i = -1
  while i < runes.len()-1:
    inc(i)
    let r = runes[i]
    if isWhitespaceChar(int32(r)):
      if isPendingDblQuote:
        currentWord.add($r)
      else:
        if currentWord.len() > 0:
          result.values.add(currentWord)
          currentWord = WsvString("")
    elif r == hashsign:
      if isPendingDblQuote:
        currentWord.add($r)
      else:
        if currentWord.len() > 0:
          result.values.add(currentWord)
          currentWord = WsvString("")
        result.comment = $runes[i..^1]
        break
    elif r == hyphenminus:
      if isPendingDblQuote:
        currentWord.add($r)
      else:
        if currentWord.len() > 0:
          currentWord.add($r)
        else:
          if int32(runes[i+1]) in whitespaceInts:
            result.values.add(WsvString(wsvNull))
          else:
            currentWord.add($r)
    else:
      if r.isDblQuote:
        if isPendingDblQuote:
          if isNextRuneDblQuote(runes, i):
            currentWord.add(wsvDblQuote)
            inc(i)
          elif isNewlineEscapeSequence(runes, i):
            currentWord.add(wsvnewline)
            i = i+2
          else:
            isPendingDblQuote = false
            currentWord.add($r)
            if currentWord.len() > 0:
              result.values.add(currentWord)
              currentWord = WsvString("")
        else:
          isPendingDblQuote = true
          currentWord.add($r)
      else:
        currentWord.add($r)
    lastRune = r

  if currentWord.len() > 0:
    result.values.add(currentWord)

proc toString*(wsvstring: WsvString): string =
  ## replaces wsv-rules in WsvString an returns value as string
  result = string(wsvstring)
  if result == "":
      return "NIL"
  if result == wsvDblQuote:
      return ""
  if result == wsvHyphenMinus:
    return "-"
  var expectSurroundingDblQuotes = false
  if result.contains(wsvDblQuote):
    expectSurroundingDblQuotes = true
    result = result.replace(wsvDblQuote, "\"")
  if result.contains($hashsign):
    expectSurroundingDblQuotes = true
  if result.hasOneOf(whitespaceInts):
    expectSurroundingDblQuotes = true
  if expectSurroundingDblQuotes:
    if (result[0] != '"') or (result[^1] != '"'):
      echo "malformed wsv-string, bye..."
      system.quit()
    result = result[1..^2]
  result = result.replace(wsvnewline, "\n")

proc asString*(wsvstring: WsvString): string =
  ## wsvstring-value with wsv-rules and returns value as string
  return string(wsvstring)

proc toWsvString*(s: string): WsvString =
  ## uses the wsv-rules on string-value and returns value as WsvString
  if s == "":
    return WsvString(wsvDblQuote)
  if s == "-":
    return WsvString(wsvHyphenMinus)
  var wsvstring: WsvString = WsvString("")
  let runes = toRunes(s)
  var needSurroundingDblQuotes = false
  for rune in runes:
    var value = $rune
    if rune == dblQuote:
      needSurroundingDblQuotes = true
      value = "\"\""
    elif int32(rune) in whitespaceInts: # \n an CR is in Whitspacelist
      needSurroundingDblQuotes = true
    if rune == newline:
      needSurroundingDblQuotes = true
      value = "\"/\""
    wsvstring.add(value)
  if needSurroundingDblQuotes:
    wsvstring.add("\"")
    wsvstring = WsvString(fmt("\"{string(wsvstring)}"))

  return wsvstring

proc toWsvString*(wsvline: WsvLine, separator: string = "\t"): WsvString =
  if separator.isWhitespaceString():
    result = join(wsvLine.values, separator)
  else:
    echo "invalid whitespace-char as separator"
    return WsvString("--invalid-whitespace-char--")

proc asWsvString*(s: string): string =
  ## uses wsv-rules on string and returns string-value as string
  return string(toWsvString(s))

proc asString*(wsvline: WsvLine, separator: string = "    "): string =
  let wsvstring = toWsvString(wsvline, separator)
  result = wsvstring.asString()

proc toStringSeq*(wsvline: WsvLine): seq[string] =
  ## replaces wsv-rules in wsvline.values wsvstrings and
  ## returns them as string sequenz
  for wsvstring in wsvline.values:
    result.add(wsvstring.toString())

proc asStringSeq*(wsvline: WsvLine): seq[string] =
  ## returns wsvline.values as string-sequence.
  ## string values have wsv-rules
  for wsvstring in wsvline.values:
    result.add(wsvstring.asString())

proc toWsvStringSeq*(wsvline: WsvLine): seq[WsvString] =
  ## returns wsvline.values - which is a sequence of WsvStrings
  return wsvline.values

proc toWsvStringSeq*(list: seq[string]): seq[WsvString] =
  ## creates from a sequence of strings a sequence of wsvstrings
  result = @[]
  for s in list:
    result.add(s.toWsvString())

proc newWsvLine*(line: string = ""): WsvLine =
  ## creates from a string a WsvLine:
  ## parses the line with parseLine
  return parseLine(line)

proc toWsvLine*(list: seq[string]): WsvLine =
  result = WsvLine(whitespaces: @[])
  result.values = list.toWsvStringSeq()
    
proc newWsvDocument*(lines: seq[WsvLine] = @[], encoding: WsvEncoding = weUtf8): WsvDocument =
  return WsvDocument(lines: lines, encoding: encoding)

proc parseWsvText*(txt: string): WsvDocument =
  result = newWsvDocument()
  let lines = split(txt, "\n")
  var linecounter = 0
  for line in lines:
    if len(line) == 0:
      continue
    if line[0] == '#':
      var wl = WsvLine(comment: line)
      result.lines.add(wl)
    else:
      var wsvline = parseWsvLine(line)
      result.lines.add(wsvline)
    inc(linecounter)

proc parseWsvFile*(fp: string): WsvDocument =
  try:
    result = parseWsvText(readFile(fp))
  except:
    echo getCurrentExceptionMsg()

proc parseWsvContent*(txt: string): WsvTable =
  discard

proc serializeWsvDoc*(wsvdoc: WsvDocument, fp: string, separator: char = '\t'): string =
  if not int32(separator).isWhitespaceChar:
    echo "invalid whitespace-char as separator"
    return "--invalid-whitespace-char--"
  for wsvline in wsvdoc.lines:
    result.add(wsvline.asString("  "))
    result.add("\n")  

  
proc toTab*(wsvdoc: WsvDocument): seq[seq[string]] =
  discard

proc toWsvTab*(tab: seq[seq[string]]): seq[seq[WsvString]] =
  # result = @[]
  # for list in tab:
  #   result.add(list.toWsvSeq)
  discard


proc toWsvDoc*(tab: seq[seq[string]]): WsvDocument =
  result = WsvDocument(encoding: weUtf8)
  for list in tab:
    result.lines.add(list.toWsvLine())

when isMainModule:
  echo "\tstart string:"
  var s = "Jürgen und seine\n\"Nana\"!"
  echo typeof(s)
  echo s
  echo "--------------------------------"
  echo "\tstring toWsvString()"
  var wsvstring = s.toWsvString()
  echo typeof(wsvstring)
  echo string(wsvstring)
  echo "--------------------------------"
  echo "\tstring asWsvString()"
  var swsv = s.asWsvString()
  echo typeof(swsv)
  echo swsv
  echo "--------------------------------"
  echo "\twsvstring asString()"
  var was = wsvstring.asString()
  echo typeof(was)
  echo was
  echo "--------------------------------"
  echo "\twsvstring.toString()"
  var s2 = wsvstring.toString()
  echo typeof(s2)
  echo s2
