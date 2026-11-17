let float_of_string_opt : string -> float option =
  [%mel.raw
    {|function (s) {
  var res = +s;
  if ((s.length > 0) && (res === res)) return res;
  s = s.replace(/_/g, "");
  res = +s;
  if (((s.length > 0) && (res === res)) || /^[+-]?nan$/i.test(s)) return res;
  var m = /^ *([+-]?)0x([0-9a-f]+)\.?([0-9a-f]*)p([+-]?[0-9]+)/i.exec(s);
  if (m) {
    var m3 = m[3].replace(/0+$/, "");
    var mantissa = parseInt(m[1] + m[2] + m3, 16);
    var exponent = (m[4] | 0) - 4 * m3.length;
    return mantissa * Math.pow(2, exponent);
  }
  if (/^\+?inf(inity)?$/i.test(s)) return Infinity;
  if (/^-inf(inity)?$/i.test(s)) return -Infinity;
  return undefined;
}|}]
