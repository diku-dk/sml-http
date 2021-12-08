signature HTTP = sig

  type ('a,'st) p = ('a,'st) ScanUtil.p

  structure Url : sig
    type t = {scheme: string, host: string,
              port: int option, path: string, query: string}   (* http://domain:port/path?query *)
    val parse    : (t, 'st) p
    val toString : t -> string
  end

  structure Mime : sig
    type t
    val parse      : (t, 'st) p
    val toString   : t -> string
    val fromString : string -> t option
    val fromExt    : string -> t option
  end

  structure Version : sig
    type t = string  (* HTTP/1.0, HTTP/1.1, ... *)
    val parse : (t, 'st) p
    val toString   : t -> string
  end

  structure StatusCode : sig
    type t           (* 3 digits *)
    val reason     : t -> string
    val fromString : string -> t option
    val parse      : (t, 'st) p
    val toString   : t -> string
  end

  structure Header : sig
    type t = string * string
    val parse    : (string*string, 'st) p
    val toString : t -> string
  end

  structure Request : sig
    datatype method = OPTIONS | GET | HEAD | POST | PUT | DELETE | TRACE | CONNECT

    type line = {method: method, version: Version.t, url: Url.t}
    type t = {line:line, headers: (string*string)list, body: string option}

    val parse_method    : (method, 'st) p
    val parse_headers   : ((string*string)list, 'st) p
    val parse_line      : (line, 'st) p
    val parse           : (t, 'st) p
    val methodToString  : method -> string
    val lineToString    : line -> string
    val toString        : t -> string
  end

  structure Response : sig
    type t = {version: Version.t, status: StatusCode.t, headers: (string*string)list, body: string option}
(*    val parse    : (t, 'st) p *)
    val toString : t -> string
  end

end
